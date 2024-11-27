//! Built-in minimal LSP support for ad
//!
//! See the LSP spec for details of semantics:
//!   https://microsoft.github.io/language-server-protocol/specification
use crate::{
    buffer::Buffer,
    die,
    editor::{Action, Actions, ViewPort},
    input::Event,
    lsp::{
        capabilities::{Capabilities, PositionEncoding},
        client::{LspClient, LspMessage, Status},
        lang::{built_in_configs, LspConfig},
        msg::{Message, Request, RequestId, Response},
        notifications::try_parse_notification,
    },
};
use lsp_types::{GotoDefinitionResponse, Hover, NumberOrString};
use std::{
    collections::HashMap,
    sync::{
        mpsc::{channel, Receiver, Sender},
        Arc, RwLock,
    },
    thread::spawn,
};
use tracing::{error, warn};

mod capabilities;
mod client;
mod lang;
mod msg;
mod notifications;

pub use capabilities::Coords;

const LSP_FILE: &str = "+lsp";

#[derive(Debug)]
enum Req {
    Start {
        lang: String,
        cmd: String,
        args: Vec<String>,
        root: String,
    },
    Stop {
        lsp_id: usize,
    },
    Pending(PendingRequest),
    Message(LspMessage),
}

#[derive(Debug)]
pub struct LspManagerHandle {
    tx_req: Sender<Req>,
    capabilities: Arc<RwLock<HashMap<String, (usize, Capabilities)>>>,
    configs: Vec<LspConfig>,
}

impl LspManagerHandle {
    #[inline]
    fn send(&self, lsp_id: usize, pending: PendingParams) {
        let req = Req::Pending(PendingRequest { lsp_id, pending });
        if let Err(e) = self.tx_req.send(req) {
            die!("LSP manager died: {e}")
        }
    }

    /// Will return None if there is no active client with recorded capabilities for the
    /// given language.
    fn lsp_id_and_encoding_for(&self, b: &Buffer) -> Option<(usize, PositionEncoding)> {
        let lang = &self.config_for_buffer(b)?.lang;

        self.capabilities
            .read()
            .unwrap()
            .get(lang)
            .map(|(id, caps)| (*id, caps.position_encoding))
    }

    fn config_for_buffer(&self, b: &Buffer) -> Option<&LspConfig> {
        let os_ext = b.path()?.extension()?;
        let ext = os_ext.to_str()?;
        self.configs
            .iter()
            .find(|c| c.extensions.iter().any(|e| e == ext))
    }

    fn start_req_for_buf(&self, b: &Buffer) -> Option<Req> {
        let config = self.config_for_buffer(b)?;
        let root = config.root_for_buffer(b)?.to_str()?.to_owned();

        Some(Req::Start {
            lang: config.lang.clone(),
            cmd: config.cmd.clone(),
            args: config.args.clone(),
            root,
        })
    }

    pub fn start_client(&self, b: &Buffer) -> Option<&'static str> {
        match self.start_req_for_buf(b) {
            Some(req) => {
                if let Err(e) = self.tx_req.send(req) {
                    die!("LSP manager died: {e}")
                }
                None
            }

            None => Some("no LSP available for buffer"),
        }
    }

    pub fn stop_client(&mut self, b: &Buffer) {
        if let Some((lsp_id, _)) = self.lsp_id_and_encoding_for(b) {
            if let Err(e) = self.tx_req.send(Req::Stop { lsp_id }) {
                die!("LSP manager died: {e}")
            }
        };
    }

    pub fn show_server_capabilities(&self, b: &Buffer) -> Option<(&'static str, String)> {
        let lang = &self.config_for_buffer(b)?.lang;
        let txt = self
            .capabilities
            .read()
            .unwrap()
            .get(lang)?
            .1
            .as_pretty_json()?;

        Some((LSP_FILE, txt))
    }

    pub fn goto_definition(&mut self, b: &Buffer) {
        if let Some((id, encoding)) = self.lsp_id_and_encoding_for(b) {
            let file = b.full_name();
            let (y, x) = b.dot.active_cur().as_yx(b);
            let (line, character) = encoding.lsp_position(b, y, x);
            let p = PendingParams::GotoDefinition(Pos::new(file, line, character));

            self.send(id, p)
        }
    }

    pub fn hover(&mut self, b: &Buffer) {
        if let Some((id, encoding)) = self.lsp_id_and_encoding_for(b) {
            let file = b.full_name();
            let (y, x) = b.dot.active_cur().as_yx(b);
            let (line, character) = encoding.lsp_position(b, y, x);
            let p = PendingParams::Hover(Pos::new(file, line, character));

            self.send(id, p)
        }
    }
}

#[derive(Debug)]
pub struct LspManager {
    clients: HashMap<usize, LspClient>,
    // lang -> (lspID, server capabilities)
    capabilities: Arc<RwLock<HashMap<String, (usize, Capabilities)>>>,
    // (lspID, ReqID) -> in-flight requests we need a response for
    pending: HashMap<(usize, RequestId), Pending>,
    // lspID -> map of progress token -> title
    progress_tokens: HashMap<usize, HashMap<NumberOrString, String>>,
    tx_req: Sender<Req>,
    tx_events: Sender<Event>,
    next_id: usize,
}

impl LspManager {
    pub fn spawn(tx_events: Sender<Event>) -> LspManagerHandle {
        let (tx_req, rx_req) = channel();
        let manager = Self {
            clients: Default::default(),
            capabilities: Default::default(),
            pending: Default::default(),
            progress_tokens: Default::default(),
            tx_req: tx_req.clone(),
            tx_events,
            next_id: 0,
        };

        let capabilities = manager.capabilities.clone();
        spawn(move || manager.run(rx_req));

        LspManagerHandle {
            tx_req,
            capabilities,
            configs: built_in_configs(),
        }
    }

    fn run(mut self, rx_req: Receiver<Req>) {
        for r in rx_req.into_iter() {
            match r {
                Req::Start {
                    lang,
                    cmd,
                    args,
                    root,
                } => self.start_client(lang, cmd, args, root),
                Req::Stop { lsp_id } => self.stop_client(lsp_id),
                Req::Pending(p) => self.handle_pending(p),

                Req::Message(LspMessage { lsp_id, msg }) => match msg {
                    Message::Request(r) => self.handle_request(lsp_id, r),
                    Message::Response(r) => self.handle_response(lsp_id, r),

                    Message::Notification(n) => {
                        let tokens = self.progress_tokens.entry(lsp_id).or_default();
                        if let Some(actions) = try_parse_notification(n, tokens) {
                            if self.tx_events.send(Event::Actions(actions)).is_err() {
                                warn!("LSP sender actions channel closed: exiting");
                                return;
                            }
                        }
                    }
                },
            }
        }
    }

    fn handle_pending(&mut self, PendingRequest { lsp_id, pending }: PendingRequest) {
        let client = match self.clients.get_mut(&lsp_id) {
            Some(client) => match client.status {
                Status::Running => client,
                Status::Initializing => {
                    self.send_status("LSP server still initializing");
                    return;
                }
            },
            None => return self.send_status("no attached LSP client for buffer"),
        };

        match pending {
            PendingParams::GotoDefinition(pos) => {
                match client.goto_definition(&pos.file, pos.line, pos.character) {
                    Ok(req_id) => {
                        self.pending
                            .insert((client.id, req_id), Pending::GotoDefinition);
                        self.send_status("requesting goto definition");
                    }

                    Err(e) => {
                        self.report_error(format!("unable to request LSP goto definition: {e}"))
                    }
                }
            }

            PendingParams::Hover(pos) => match client.hover(&pos.file, pos.line, pos.character) {
                Ok(req_id) => {
                    self.pending.insert((client.id, req_id), Pending::Hover);
                    self.send_status("requesting hover");
                }

                Err(e) => self.report_error(format!("unable to request LSP hover: {e}")),
            },
        }
    }

    fn handle_request(&mut self, lsp_id: usize, req: Request) {
        use lsp_types::{
            request::{Request as _, WorkDoneProgressCreate},
            WorkDoneProgressCreateParams,
        };

        if req.method == WorkDoneProgressCreate::METHOD {
            match req.extract::<WorkDoneProgressCreate>() {
                Ok((_, WorkDoneProgressCreateParams { token })) => {
                    self.progress_tokens
                        .entry(lsp_id)
                        .or_default()
                        .insert(token, Default::default());
                }

                Err(e) => {
                    error!("malformed request from LSP: {e:?}");
                }
            }
        }
    }

    fn handle_response(&mut self, lsp_id: usize, res: Response) {
        use lsp_types::request::{GotoDefinition, HoverRequest, Initialize};

        // TODO: need to distinguish between errors being returned explicitly and errors in parsing
        // the response itself
        macro_rules! extract {
            ($k:ty, $res:expr) => {
                match $res.extract::<$k>() {
                    Ok(data) => data,
                    Err(e) => {
                        error!("malformed LSP response: {e}");
                        return;
                    }
                }
            };
        }

        let req_id = res.id.clone();
        let p = match self.pending.remove(&(lsp_id, req_id)) {
            Some(p) => p,
            None => {
                error!("got response for unknown LSP request: {res:?}");
                return;
            }
        };

        let actions = match p {
            Pending::Initialize(lang) => {
                let (_id, res) = extract!(Initialize, res);
                let res = res.unwrap_or_default();

                let client = match self.clients.get_mut(&lsp_id) {
                    Some(client) => client,
                    None => {
                        self.send_status(format!("no attached LSP client for {lang}"));
                        return;
                    }
                };

                match Capabilities::try_new(res) {
                    Some(c) => {
                        if let Err(e) = client.ack_initialized() {
                            self.report_error(format!("error initializing LSP client: {e}"));
                            return;
                        }
                        client.status = Status::Running;
                        client.position_encoding = c.position_encoding;
                        self.capabilities.write().unwrap().insert(lang, (lsp_id, c));
                    }

                    None => {
                        error!("LSP client only supports utf-16 offset encoding");
                        self.stop_client(lsp_id);
                    }
                };

                return;
            }

            Pending::GotoDefinition => {
                let (_id, data) = extract!(GotoDefinition, res);
                self.clients
                    .get(&lsp_id)
                    .and_then(|client| handle_goto_definition(data, client.position_encoding))
            }

            Pending::Hover => {
                let (_id, data) = extract!(HoverRequest, res);
                handle_hover(data)
            }
        };

        if let Some(actions) = actions {
            if self.tx_events.send(Event::Actions(actions)).is_err() {
                warn!("LSP sender actions channel closed: exiting");
            }
        }
    }

    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;

        id
    }

    fn send_status(&self, message: impl Into<String>) {
        _ = self.tx_events.send(Event::Action(Action::SetStatusMessage {
            message: message.into(),
        }));
    }

    #[inline]
    fn report_error(&self, message: impl Into<String>) {
        let message = message.into();
        error!("{message}");
        self.send_status(message);
    }

    pub fn start_client(&mut self, lang: String, cmd: String, args: Vec<String>, root: String) {
        let lsp_id = self.next_id();
        let mut client = match LspClient::new(lsp_id, &cmd, args, self.tx_req.clone()) {
            Ok(client) => client,
            Err(e) => {
                return self.report_error(format!("failed to start LSP server: {e}"));
            }
        };
        let req_id = match client.initialize(&root) {
            Ok(id) => id,
            Err(e) => {
                return self.report_error(format!("failed to initialize LSP server: {e}"));
            }
        };

        self.clients.insert(lsp_id, client);
        self.pending
            .insert((lsp_id, req_id), Pending::Initialize(lang));
        self.send_status("LSP server started")
    }

    pub fn stop_client(&mut self, lsp_id: usize) {
        let client = match self.clients.remove(&lsp_id) {
            Some(client) => client,
            None => return self.report_error("no attached LSP server"),
        };

        let cmd = client.cmd.clone();
        let message = match client.shutdown_and_exit() {
            Ok(_) => "LSP server shutdown".to_string(),
            Err(e) => {
                format!("error shutting down LSP({cmd}): {e}")
            }
        };
        self.send_status(message);
    }
}

#[derive(Debug, Clone)]
struct Pos {
    file: String,
    line: u32,
    character: u32,
}

impl Pos {
    fn new(file: impl Into<String>, line: u32, character: u32) -> Self {
        Self {
            file: file.into(),
            line,
            character,
        }
    }
}

#[derive(Debug)]
struct PendingRequest {
    lsp_id: usize,
    pending: PendingParams,
}

#[derive(Debug)]
enum PendingParams {
    GotoDefinition(Pos),
    Hover(Pos),
}

#[derive(Debug)]
enum Pending {
    Initialize(String),
    GotoDefinition,
    Hover,
}

fn handle_goto_definition(
    data: Option<Option<GotoDefinitionResponse>>,
    p: PositionEncoding,
) -> Option<Actions> {
    match data {
        None | Some(None) => None,
        Some(Some(GotoDefinitionResponse::Scalar(loc))) => {
            let (path, coords) = Coords::new(loc, p);

            Some(Actions::Multi(vec![
                Action::OpenFile { path },
                Action::DotSetFromCoords { coords },
                Action::SetViewPort(ViewPort::Center),
            ]))
        }

        Some(Some(GotoDefinitionResponse::Array(mut locs))) => {
            let (path, coords) = Coords::new(locs.remove(0), p);

            Some(Actions::Multi(vec![
                Action::OpenFile { path },
                Action::DotSetFromCoords { coords },
                Action::SetViewPort(ViewPort::Center),
            ]))
        }

        Some(Some(GotoDefinitionResponse::Link(links))) => {
            error!("unhandled goto definition links response: {links:?}");
            None
        }
    }
}

fn handle_hover(data: Option<Option<Hover>>) -> Option<Actions> {
    use lsp_types::{HoverContents, MarkedString};

    let ms_to_string = |ms: MarkedString| match ms {
        MarkedString::String(s) => s,
        MarkedString::LanguageString(ls) => ls.value,
    };

    let txt = match data {
        None | Some(None) => return None,

        Some(Some(Hover {
            contents: HoverContents::Scalar(ms),
            ..
        })) => ms_to_string(ms),

        Some(Some(Hover {
            contents: HoverContents::Array(mss),
            ..
        })) => {
            let strs: Vec<_> = mss.into_iter().map(ms_to_string).collect();
            strs.join("\n")
        }

        Some(Some(Hover {
            contents: HoverContents::Markup(mc),
            ..
        })) => mc.value,
    };

    Some(Actions::Single(Action::OpenVirtualFile {
        name: LSP_FILE.to_string(),
        txt,
    }))
}
