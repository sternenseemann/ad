//! Traits for implementing a 9p fileserver
use crate::{
    fs::{FileMeta, FileType, IoUnit, Mode, Perm, Stat, QID_ROOT},
    protocol::{Data, Format9p, Qid, RawStat, Rdata, Tdata, Tmessage, MAX_DATA_LEN},
    unix, Result, Stream,
};
use std::{
    cmp::min,
    collections::btree_map::{BTreeMap, Entry},
    fs,
    mem::size_of,
    net::TcpListener,
    os::unix::net::UnixListener,
    path::{Path, PathBuf},
    sync::{mpsc::Receiver, Arc, Mutex, RwLock},
    thread::{spawn, JoinHandle},
};

/// Marker afid to denode that auth is not required for establishing connections
pub const AFID_NO_AUTH: u32 = u32::MAX;

#[derive(Debug)]
struct Socket {
    path: PathBuf,
    listener: UnixListener,
}

impl Drop for Socket {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

/// The unix socket path that will be used for a given server name.
pub fn socket_path(name: impl AsRef<Path>) -> PathBuf {
    let mut path = unix::namespace().unwrap();
    path.push(name);
    path
}

fn unix_socket(name: impl AsRef<Path>) -> Socket {
    let path = socket_path(name);
    if let Some(socket_dir) = path.parent() {
        let _ = fs::create_dir_all(socket_dir);
    }

    // FIXME: really we should be handling this on exit but we'll need to catch
    // ctrl-c to do that properly. For now this works but it means that if you
    // start a second file server with the same name then it'll remove the socket
    // for the first.
    let _ = fs::remove_file(&path);
    let listener = UnixListener::bind(&path).unwrap();

    Socket { path, listener }
}

fn tcp_socket(port: u16) -> TcpListener {
    let addr = format!("127.0.0.1:{port}");
    TcpListener::bind(addr).unwrap()
}

// Error messages
const E_NO_VERSION_MESSAGE: &str = "first message must be Tversion";
const E_AUTH_NOT_REQUIRED: &str = "authentication not required";
const E_DUPLICATE_FID: &str = "duplicate fid";
const E_UNKNOWN_FID: &str = "unknown fid";
const E_UNKNOWN_ROOT: &str = "unknown root directory";
const E_WALK_NON_DIR: &str = "walk in non-directory";
const E_CREATE_NON_DIR: &str = "create in non-directory";
const E_INVALID_OFFSET: &str = "invalid offset for read on directory";

const UNKNOWN_VERSION: &str = "unknown";
const SUPPORTED_VERSION: &str = "9P2000";

/// An opaque client ID that can be used by server implementations to determine which client a
/// request originated from by comparing equality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClientId(u64);

/// The outcome of a client attempting to [read](Serve9p::read) a given file.
#[derive(Debug)]
pub enum ReadOutcome {
    /// The data is immediately available.
    Immediate(Vec<u8>),
    /// No response should be sent until data is received on the provided channel
    Blocked(Receiver<Vec<u8>>),
}

/// A type capable of handling [9p](http://9p.cat-v.org/) requests in order to implement a
/// 9p virtual filesystem. The [Server] struct is used to handle the lower level protocal and
/// underlying connection, allowing implementers of this trait to focus on the semantics of the
/// virtual filesystem itself.
///
/// # The 9p protocol
/// Please see [the documentation page on cat-v](http://9p.cat-v.org/documentation/) for an
/// overview of how the protocol works along with various papers covering the original implementation
/// from Bell Labs. For simple filesystems you should be able to get away with referring to the
/// docs on each of the methods for this trait, but you are advised to read through the semantics
/// around permissions and file creation as this is something handled by the trait implementer, not
/// [Server].
///
/// ## Client fids and server-side qids
/// [Server] handles establishing and maintaining per-client sessions along with all of their `fids`,
/// as such, [Serve9p] only needs to worry about maintaining `qids` for resources.
///
/// The source code of [Server] is a useful reference for those wanting to learn more.
pub trait Serve9p: Send + 'static {
    // #[allow(unused_variables)]
    // fn auth(&mut self, afid: u32, uname: &str, aname: &str) -> Result<Qid> {
    //     Err("authentication not required".to_string())
    // }

    /// Lookup a child node under a known parent directory by name.
    ///
    /// `9p` Twalk messages received by the server will specify a full path from a known parent
    /// to a target child. This method is called for each element of that path in sequence in order,
    /// stopping either when the target is reached or some element of the path returns an error.
    ///
    /// [Server] will ensure that this method is only called for known parents who have previously
    /// been identified has having [FileType::Directory].
    fn walk(
        &mut self,
        cid: ClientId,
        parent_qid: u64,
        child: &str,
        uname: &str,
    ) -> Result<FileMeta>;

    /// Open an existing file in the requested mode for subsequent I/O via [read](Serve9p::read) and
    /// [write](Serve9p::write) calls.
    ///
    /// The return of this method is an [IoUnit] used to inform the client of the maximum number of
    /// bytes that will be supported per read/write call on this resource.
    fn open(&mut self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit>;

    /// Clunk a currently open file.
    #[allow(unused_variables)]
    fn clunk(&mut self, cid: ClientId, qid: u64) {}

    /// Create a new file in the given parent directory.
    fn create(
        &mut self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(FileMeta, IoUnit)>;

    /// Read `count` bytes from the requested file starting from the given `offset`.
    fn read(
        &mut self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> Result<ReadOutcome>;

    /// List the contents of the given directory.
    fn read_dir(&mut self, cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>>;

    /// Write the given `data` to the requested file starting at `offset`
    fn write(
        &mut self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> Result<usize>;

    /// Remove the requested file from the filesystem.
    fn remove(&mut self, cid: ClientId, qid: u64, uname: &str) -> Result<()>;

    /// Request a machine independent "directory entry" for the given resource.
    fn stat(&mut self, cid: ClientId, qid: u64, uname: &str) -> Result<Stat>;

    /// Attempt to set the machine independent "directory entry" for the given resource.
    fn write_stat(&mut self, cid: ClientId, qid: u64, stat: Stat, uname: &str) -> Result<()>;
}

/// A threaded `9p` server capable of listening on either a TCP socket or UNIX domain socket.
#[derive(Debug)]
pub struct Server<S>
where
    S: Serve9p,
{
    s: Arc<Mutex<S>>,
    msize: u32,
    roots: BTreeMap<String, u64>,
    qids: Arc<RwLock<BTreeMap<u64, FileMeta>>>,
    next_client_id: u64,
}

impl<S> Server<S>
where
    S: Serve9p,
{
    /// Create a new file server with a single anonymous root (name will be "") and
    /// qid of [QID_ROOT].
    pub fn new(s: S) -> Self {
        Self::new_with_roots(s, [("".to_string(), QID_ROOT)].into_iter().collect())
    }

    /// Create a new file server with the given roots for clients to attach to.
    pub fn new_with_roots(s: S, roots: BTreeMap<String, u64>) -> Self {
        let qids = roots
            .iter()
            .map(|(p, &qid)| (qid, FileMeta::dir(p.clone(), qid)))
            .collect();

        Self {
            s: Arc::new(Mutex::new(s)),
            msize: MAX_DATA_LEN as u32,
            roots,
            qids: Arc::new(RwLock::new(qids)),
            next_client_id: 0,
        }
    }

    /// Bind this server to the specified port and serve over a tcp socket.
    pub fn serve_tcp(mut self, port: u16) -> JoinHandle<()> {
        spawn(move || {
            let listener = tcp_socket(port);

            for stream in listener.incoming() {
                let stream = stream.unwrap();
                let session = Session::new_unattached(
                    ClientId(self.next_client_id),
                    self.msize,
                    self.roots.clone(),
                    self.s.clone(),
                    self.qids.clone(),
                    stream,
                );

                self.next_client_id += 1;
                spawn(move || session.handle_connection());
            }
        })
    }

    /// Bind this server to the specified path and serve over a unix socket.
    pub fn serve_socket(mut self, socket_name: impl Into<String>) -> JoinHandle<()> {
        let socket_name = socket_name.into();

        spawn(move || {
            let sock = unix_socket(&socket_name);

            for stream in sock.listener.incoming() {
                let stream = stream.unwrap();
                let session = Session::new_unattached(
                    ClientId(self.next_client_id),
                    self.msize,
                    self.roots.clone(),
                    self.s.clone(),
                    self.qids.clone(),
                    stream,
                );

                self.next_client_id += 1;
                spawn(move || session.handle_connection());
            }
        })
    }
}

/// Marker trait for implementing a type state for Session
trait SessionType: Send {}

#[derive(Debug, Default)]
struct Unattached {
    seen_version: bool,
}

impl SessionType for Unattached {}

#[derive(Debug)]
struct Attached {
    uname: String,
    fids: BTreeMap<u32, u64>,
}
impl SessionType for Attached {}

impl Attached {
    fn new(uname: String, root_fid: u32, root_qid: u64) -> Self {
        Self {
            uname,
            fids: [(root_fid, root_qid)].into_iter().collect(),
        }
    }
}

#[derive(Debug)]
struct Session<T, S, U>
where
    T: SessionType,
    S: Serve9p,
    U: Stream,
{
    client_id: ClientId,
    state: T,
    msize: u32,
    roots: BTreeMap<String, u64>,
    s: Arc<Mutex<S>>,
    qids: Arc<RwLock<BTreeMap<u64, FileMeta>>>,
    stream: U,
}

impl<T, S, U> Session<T, S, U>
where
    T: SessionType,
    S: Serve9p,
    U: Stream,
{
    fn qid(&self, qid: u64) -> Option<Qid> {
        self.qids.read().unwrap().get(&qid).map(|fm| fm.as_qid())
    }

    fn reply(&mut self, tag: u16, resp: Result<Rdata>) {
        self.stream.reply(tag, resp);
    }

    /// The version request negotiates the protocol version and message size to be used on the
    /// connection and initializes the connection for I/O. Tversion must be the first message sent
    /// on the 9P connection, and the client cannot issue any further requests until it has
    /// received the Rversion reply. The tag should be NOTAG (value (ushort)~0) for a version
    /// message.
    /// The client suggests a maximum message size, msize, that is the maximum length, in bytes, it
    /// will ever generate or expect to receive in a single 9P message. This count includes all 9P
    /// protocol data, starting from the size field and extending through the message, but excludes
    /// enveloping transport protocols. The server responds with its own maximum, msize, which must
    /// be less than or equal to the client’s value. Thenceforth, both sides of the connection must
    /// honor this limit.
    /// The version string identifies the level of the protocol. The string must always begin with
    /// the two characters “9P”. If the server does not understand the client’s version string, it
    /// should respond with an Rversion message (not Rerror) with the version string the 7
    /// characters “unknown”.
    /// The server may respond with the client’s version string, or a version string identifying an
    /// earlier defined protocol version. Currently, the only defined version is the 6 characters
    /// “9P2000”. Version strings are defined such that, if the client string contains one or more
    /// period characters, the initial substring up to but not including any single period in the
    /// version string defines a version of the protocol. After stripping any such period-separated
    /// suffix, the server is allowed to respond with a string of the form 9Pnnnn, where nnnn is
    /// less than or equal to the digits sent by the client.
    /// The client and server will use the protocol version defined by the server’s response for
    /// all subsequent communication on the connection.
    /// A successful version request initializes the connection. All outstanding I/O on the
    /// connection is aborted; all active fids are freed (‘clunked’) automatically. The set of
    /// messages between version requests is called a session.
    fn handle_version(&mut self, msize: u32, version: String) -> Result<Rdata> {
        let server_version = if version != SUPPORTED_VERSION {
            UNKNOWN_VERSION
        } else {
            SUPPORTED_VERSION
        };

        Ok(Rdata::Version {
            msize: min(self.msize, msize),
            version: server_version.to_string(),
        })
    }

    /// If the client does wish to authenticate, it must acquire and validate an afid using an auth
    /// message before doing the attach.
    /// The auth message contains afid, a new fid to be established for authentication, and the
    /// uname and aname that will be those of the following attach message. If the server does not
    /// require authentication, it returns Rerror to the Tauth message.
    /// If the server does require authentication, it returns aqid defining a file of type QTAUTH
    /// (see intro(9P)) that may be read and written (using read and write messages in the usual
    /// way) to execute an authentication protocol. That protocol’s definition is not part of 9P
    /// itself.
    /// Once the protocol is complete, the same afid is presented in the attach message for the
    /// user, granting entry. The same validated afid may be used for multiple attach messages with
    /// the same uname and aname.
    #[allow(unused_variables)]
    fn handle_auth(&mut self, afid: u32, uname: String, aname: String) -> Result<Rdata> {
        // TODO: handle auth
        // let aqid = self.s.lock().unwrap().auth(afid, &uname, &aname)?;
        // Ok(Rdata::Auth { aqid })

        Err(E_AUTH_NOT_REQUIRED.to_string())
    }
}

impl<S, U> Session<Unattached, S, U>
where
    S: Serve9p,
    U: Stream,
{
    fn new_unattached(
        client_id: ClientId,
        msize: u32,
        roots: BTreeMap<String, u64>,
        s: Arc<Mutex<S>>,
        qids: Arc<RwLock<BTreeMap<u64, FileMeta>>>,
        stream: U,
    ) -> Self {
        Self {
            client_id,
            state: Unattached::default(),
            msize,
            roots,
            s,
            qids,
            stream,
        }
    }

    fn into_attached(self, ty: Attached) -> Session<Attached, S, U> {
        let Self {
            client_id,
            msize,
            roots,
            s,
            qids,
            stream,
            ..
        } = self;

        Session::new_attached(client_id, ty, msize, roots, s, qids, stream)
    }

    fn handle_connection(mut self) {
        use Tdata::*;

        loop {
            let t = match Tmessage::read_from(&mut self.stream) {
                Ok(t) => t,
                Err(_) => return,
            };

            let Tmessage { tag, content } = t;

            let resp = match content {
                Version { msize, version } => {
                    self.state.seen_version = version == SUPPORTED_VERSION;
                    self.handle_version(msize, version)
                }

                Auth { afid, uname, aname } => {
                    if !self.state.seen_version {
                        self.reply(tag, Err(E_NO_VERSION_MESSAGE.to_string()));
                        continue;
                    }

                    self.handle_auth(afid, uname, aname)
                }

                Attach {
                    fid,
                    afid,
                    uname,
                    aname,
                } => {
                    if !self.state.seen_version {
                        self.reply(tag, Err(E_NO_VERSION_MESSAGE.to_string()));
                        continue;
                    }

                    let (st, aqid) = match self.handle_attach(fid, afid, uname, aname) {
                        Err(e) => {
                            self.reply(tag, Err(e));
                            continue;
                        }

                        Ok((st, aqid)) => (st, aqid),
                    };

                    self.reply(tag, Ok(Rdata::Attach { aqid }));
                    return self.into_attached(st).handle_connection();
                }

                _ => Err("session is unattached".into()),
            };

            self.reply(tag, resp);
        }
    }

    /// The attach message serves as a fresh introduction from a user on the client machine to the
    /// server. The message identifies the user (uname) and may select the file tree to access
    /// (aname). The afid argument specifies a fid previously established by an auth message.
    /// As a result of the attach transaction, the client will have a connection to the root
    /// directory of the desired file tree, represented by fid. An error is returned if fid is
    /// already in use. The server’s idea of the root of the file tree is represented by the
    /// returned qid.
    ///
    /// If the client does not wish to authenticate the connection, or knows that authentication is
    /// not required, the afid field in the attach message should be set to NOFID, defined as
    /// (u32int)~0 in <fcall.h>.
    fn handle_attach(
        &mut self,
        root_fid: u32,
        _afid: u32,
        uname: String,
        aname: String,
    ) -> Result<(Attached, Qid)> {
        let root_qid = match self.roots.get(&aname) {
            Some(qid) => *qid,
            None => return Err(E_UNKNOWN_ROOT.to_string()),
        };

        // TODO: handle checking afids (AFID_NO_AUTH should be accepted if there is no auth)

        let st = Attached::new(uname, root_fid, root_qid);
        let aqid = self.qid(root_qid).expect("to have root qid");

        Ok((st, aqid))
    }
}

macro_rules! file_meta {
    ($self:expr, $fid:expr) => {
        match $self.state.fids.get(&$fid) {
            Some(&qid) => $self.qids.read().unwrap().get(&qid).cloned(),
            None => None,
        }
    };
}

impl<S, U> Session<Attached, S, U>
where
    S: Serve9p,
    U: Stream,
{
    fn new_attached(
        client_id: ClientId,
        state: Attached,
        msize: u32,
        roots: BTreeMap<String, u64>,
        s: Arc<Mutex<S>>,
        qids: Arc<RwLock<BTreeMap<u64, FileMeta>>>,
        stream: U,
    ) -> Self {
        Self {
            client_id,
            state,
            msize,
            roots,
            s,
            qids,
            stream,
        }
    }

    /// Explicitly clunk all
    fn clunk_and_clear(&mut self) {
        let mut guard = self.s.lock().unwrap();
        for &qid in self.state.fids.values() {
            guard.clunk(self.client_id, qid);
        }
        self.state.fids.clear();
    }

    fn handle_connection(mut self) {
        use Tdata::*;

        loop {
            let t = match Tmessage::read_from(&mut self.stream) {
                Ok(t) => t,
                Err(_) => return self.clunk_and_clear(),
            };

            let Tmessage { tag, content } = t;

            let resp = match content {
                Version { msize, version } => {
                    let res = self.handle_version(msize, version);
                    if res.is_ok() {
                        self.clunk_and_clear();
                    }

                    res
                }
                Auth { .. } | Attach { .. } => Err("session is already attached".into()),
                Flush { .. } => Ok(Rdata::Flush {}),

                Walk {
                    fid,
                    new_fid,
                    wnames,
                } => self.handle_walk(fid, new_fid, wnames),
                Clunk { fid } => self.handle_clunk(fid),
                Stat { fid } => self.handle_stat(fid),
                Open { fid, mode } => self.handle_open(fid, Mode::new(mode)),
                Create {
                    fid,
                    name,
                    perm,
                    mode,
                } => self.handle_create(fid, name, Perm::new(perm), Mode::new(mode)),
                Read { fid, offset, count } => match self.handle_read(tag, fid, offset, count) {
                    Ok(Some(resp)) => Ok(resp),
                    Err(err) => Err(err),
                    Ok(None) => continue,
                },
                Write { fid, offset, data } => self.handle_write(fid, offset, data.0),
                Remove { fid } => self.handle_remove(fid),
                Wstat { fid, stat, .. } => self.handle_wstat(fid, stat),
            };

            self.reply(tag, resp);
        }
    }

    /// The walk request carries as arguments an existing fid and a proposed newfid (which must not
    /// be in use unless it is the same as fid) that the client wishes to associate with the result
    /// of traversing the directory hierarchy by ‘walking’ the hierarchy using the successive path
    /// name elements wname.
    ///
    /// The fid must represent a directory unless zero path name elements are specified.
    ///
    /// The fid must be valid in the current session and must not have been opened for I/O by an
    /// open or create message. If the full sequence of nwname elements is walked successfully,
    /// newfid will represent the file that results. If not, newfid (and fid) will be unaffected.
    /// However, if newfid is in use or otherwise illegal, an Rerror is returned.
    ///
    /// The name “..” (dot-dot) represents the parent directory. The name “.” (dot), meaning the
    /// current directory, is not used in the protocol.
    ///
    /// It is legal for nwname to be zero, in which case newfid will represent the same file as fid
    /// and the walk will usually succeed; this is equivalent to walking to dot. The rest of this
    /// discussion assumes nwname is greater than zero.
    ///
    /// The nwname path name elements wname are walked in order, “elementwise”. For the first
    /// elementwise walk to succeed, the file identified by fid must be a directory, and the
    /// implied user of the request must have permission to search the directory (see intro(9P)).
    /// Subsequent elementwise walks have equivalent restrictions applied to the implicit fid that
    /// results from the preceding elementwise walk.
    ///
    /// If the first element cannot be walked for any reason, Rerror is returned. Otherwise, the
    /// walk will return an Rwalk message containing nwqid qids corresponding, in order, to the
    /// files that are visited by the nwqid successful elementwise walks; nwqid is therefore either
    /// nwname or the index of the first elementwise walk that failed. The value of nwqid cannot be
    /// zero unless nwname is zero. Also, nwqid will always be less than or equal to nwname. Only
    /// if it is equal, however, will newfid be affected, in which case newfid will represent the
    /// file reached by the final elementwise walk requested in the message.
    ///
    /// A walk of the name “..” in the root directory of a server is equivalent to a walk with no
    /// name elements.
    ///
    /// If newfid is the same as fid, the above discussion applies, with the obvious difference
    /// that if the walk changes the state of newfid, it also changes the state of fid; and if
    /// newfid is unaffected, then fid is also unaffected.
    ///
    /// To simplify the implementation of the servers, a maximum of sixteen name elements or qids
    /// may be packed in a single message. This constant is called MAXWELEM in fcall(3). Despite
    /// this restriction, the system imposes no limit on the number of elements in a file name,
    /// only the number that may be transmitted in a single message.
    fn handle_walk(&mut self, fid: u32, new_fid: u32, wnames: Vec<String>) -> Result<Rdata> {
        if new_fid != fid && self.state.fids.contains_key(&new_fid) {
            return Err(E_DUPLICATE_FID.to_string());
        }

        let fm = match file_meta!(self, fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };

        if wnames.is_empty() {
            self.state.fids.insert(new_fid, fm.qid);
            return Ok(Rdata::Walk { wqids: vec![] });
        } else if matches!(fm.ty, FileType::Regular) {
            return Err(E_WALK_NON_DIR.to_string());
        }

        let mut wqids = Vec::with_capacity(wnames.len());
        let mut s = self.s.lock().unwrap();
        let mut qids = self.qids.write().unwrap();
        let mut qid = fm.qid;

        for name in wnames.iter() {
            let fm = s.walk(self.client_id, qid, name, &self.state.uname)?;
            qid = fm.qid;
            wqids.push(fm.as_qid());
            qids.insert(qid, fm);
        }

        drop(s);
        drop(qids);

        if wqids.len() == wnames.len() {
            let qid = wqids.last().expect("empty was handled above").path;
            self.state.fids.insert(new_fid, qid);
        }

        Ok(Rdata::Walk { wqids })
    }

    /// If a client clunks a fid we simply remove it from the list of fids
    /// we have for them
    fn handle_clunk(&mut self, fid: u32) -> Result<Rdata> {
        match self.state.fids.entry(fid) {
            Entry::Occupied(ent) => {
                let qid = ent.remove();
                self.s.lock().unwrap().clunk(self.client_id, qid);

                Ok(Rdata::Clunk {})
            }
            Entry::Vacant(_) => Err(E_UNKNOWN_FID.to_string()),
        }
    }

    fn handle_stat(&mut self, fid: u32) -> Result<Rdata> {
        let fm = match file_meta!(self, fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };
        let s = self
            .s
            .lock()
            .unwrap()
            .stat(self.client_id, fm.qid, &self.state.uname)?;
        let stat: RawStat = s.into();
        let size = stat.size + size_of::<u16>() as u16;

        Ok(Rdata::Stat { size, stat })
    }

    fn handle_wstat(&mut self, fid: u32, raw_stat: RawStat) -> Result<Rdata> {
        let stat: Stat = raw_stat.try_into()?;
        let fm = match file_meta!(self, fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };

        self.s
            .lock()
            .unwrap()
            .write_stat(self.client_id, fm.qid, stat, &self.state.uname)?;

        Ok(Rdata::Wstat {})
    }

    fn handle_open(&mut self, fid: u32, mode: Mode) -> Result<Rdata> {
        let fm = match file_meta!(self, fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };
        let iounit =
            self.s
                .lock()
                .unwrap()
                .open(self.client_id, fm.qid, mode, &self.state.uname)?;

        Ok(Rdata::Open {
            qid: fm.as_qid(),
            iounit,
        })
    }

    fn handle_create(&mut self, fid: u32, name: String, perm: Perm, mode: Mode) -> Result<Rdata> {
        let fm = match file_meta!(self, fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };
        if fm.ty != FileType::Directory {
            return Err(E_CREATE_NON_DIR.to_string());
        }

        let (fm, iounit) = self.s.lock().unwrap().create(
            self.client_id,
            fm.qid,
            &name,
            perm,
            mode,
            &self.state.uname,
        )?;

        // fid is now changed to point to the newly created file rather than the parent
        let qid = fm.as_qid();
        self.state.fids.insert(fid, fm.qid);
        self.qids.write().unwrap().entry(fm.qid).or_insert(fm);

        Ok(Rdata::Create { qid, iounit })
    }

    // The read request asks for count bytes of data from the file identified by fid, which must be
    // opened for reading, starting offset bytes after the beginning of the file. The bytes are
    // returned with the read reply message.
    // The count field in the reply indicates the number of bytes returned. This may be less than
    // the requested amount. If the offset field is greater than or equal to the number of bytes in
    // the file, a count of zero will be returned.
    // For directories, read returns an integral number of directory entries exactly as in stat
    // (see stat(9P)), one for each member of the directory. The read request message must have
    // offset equal to zero or the value of offset in the previous read on the directory, plus the
    // number of bytes returned in the previous read. In other words, seeking other than to the
    // beginning is illegal in a directory.
    fn handle_read(
        &mut self,
        tag: u16,
        fid: u32,
        offset: u64,
        count: u32,
    ) -> Result<Option<Rdata>> {
        use FileType::*;

        let fm = match file_meta!(self, fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };

        if offset > u32::MAX as u64 {
            return Err(format!("offset too large: {offset} > {}", u32::MAX));
        }

        let data = match fm.ty {
            Directory => self.read_dir(fm.qid, offset as usize, count as usize)?,
            Regular | AppendOnly | Exclusive => {
                let outcome = self.s.lock().unwrap().read(
                    self.client_id,
                    fm.qid,
                    offset as usize,
                    count as usize,
                    &self.state.uname,
                )?;

                match outcome {
                    ReadOutcome::Immediate(data) => data,
                    ReadOutcome::Blocked(chan) => {
                        // Cloning the stream can
                        let mut stream = match self.stream.try_clone() {
                            Ok(stream) => stream,
                            Err(err) => return Err(err),
                        };

                        spawn(move || {
                            let data = chan.recv().unwrap_or_default();
                            stream.reply(tag, Ok(Rdata::Read { data: Data(data) }));
                        });

                        return Ok(None);
                    }
                }
            }
        };

        Ok(Some(Rdata::Read { data: Data(data) }))
    }

    fn read_dir(&mut self, qid: u64, offset: usize, count: usize) -> Result<Vec<u8>> {
        let stats = self
            .s
            .lock()
            .unwrap()
            .read_dir(self.client_id, qid, &self.state.uname)?;

        let mut buf = Vec::with_capacity(count);
        let mut to_skip = offset;

        for stat in stats.into_iter() {
            self.qids
                .write()
                .unwrap()
                .entry(stat.fm.qid)
                .or_insert(stat.fm.clone());

            let rstat: RawStat = stat.into();
            let mut tmp = Vec::new();
            rstat.write_to(&mut tmp).unwrap();

            if to_skip != 0 {
                if tmp.len() > to_skip {
                    return Err(E_INVALID_OFFSET.to_string());
                } else {
                    to_skip -= tmp.len();
                    continue;
                }
            }

            if buf.len() + tmp.len() > count {
                break;
            }
            buf.extend(tmp);
        }

        Ok(buf)
    }

    fn handle_write(&mut self, fid: u32, offset: u64, data: Vec<u8>) -> Result<Rdata> {
        let fm = match file_meta!(self, fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };

        if offset > u32::MAX as u64 {
            return Err(format!("offset too large: {offset} > {}", u32::MAX));
        }

        let count = self.s.lock().unwrap().write(
            self.client_id,
            fm.qid,
            offset as usize,
            data,
            &self.state.uname,
        )? as u32;

        Ok(Rdata::Write { count })
    }

    fn handle_remove(&mut self, fid: u32) -> Result<Rdata> {
        let fm = match file_meta!(self, fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };

        self.s
            .lock()
            .unwrap()
            .remove(self.client_id, fm.qid, &self.state.uname)?;

        Ok(Rdata::Remove {})
    }
}
