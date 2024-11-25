//! Built-in minimal LSP support for ad
//!
//! See the LSP spec for details of semantics:
//!   https://microsoft.github.io/language-server-protocol/specification
use crate::{
    buffer::Buffer,
    die,
    editor::{Action, Actions, Coords},
    input::Event,
    lsp::{
        client::{LspClient, LspMessage, Status},
        msg::{Message, Request, RequestId, Response},
        notifications::try_parse_notification,
    },
};
use lsp_types::{GotoDefinitionResponse, Hover, Location, NumberOrString, Range};
use std::{
    collections::HashMap,
    sync::mpsc::{channel, Receiver, Sender},
    thread::spawn,
};
use tracing::{error, warn};

mod client;
mod msg;
mod notifications;

const LSP_FILE: &str = "+lsp";

#[derive(Debug)]
enum Req {
    Start {
        lang: String,
        cmd: String,
        root: String,
    },
    Stop {
        lang: String,
    },
    Pending(PendingRequest),
    Message(LspMessage),
}

#[derive(Debug)]
pub struct LspManagerHandle {
    tx_req: Sender<Req>,
}

impl LspManagerHandle {
    #[inline]
    fn send(&self, lang: String, pending: PendingParams) {
        let req = Req::Pending(PendingRequest { lang, pending });
        if let Err(e) = self.tx_req.send(req) {
            die!("LSP manager died: {e}")
        }
    }

    // FIXME: hardcoded for rust just now
    fn lang_from_filepath(&self, file: &str) -> Option<String> {
        if file.ends_with("rs") {
            return Some("rust".to_string());
        }

        None
    }

    pub fn start_client(&self, lang: String, cmd: String, root: String) {
        let req = Req::Start { lang, cmd, root };
        if let Err(e) = self.tx_req.send(req) {
            die!("LSP manager died: {e}")
        }
    }

    pub fn stop_client(&mut self, b: &Buffer) {
        let file = b.full_name();
        if let Some(lang) = self.lang_from_filepath(file) {
            if let Err(e) = self.tx_req.send(Req::Stop { lang }) {
                die!("LSP manager died: {e}")
            }
        };
    }

    pub fn goto_definition(&mut self, b: &Buffer) {
        let file = b.full_name();
        if let Some(lang) = self.lang_from_filepath(file) {
            let (line, character) = b.dot.active_cur().as_yx(b);
            let p = PendingParams::GotoDefinition(Pos::new(file, line as u32, character as u32));
            self.send(lang, p)
        }
    }

    pub fn hover(&mut self, b: &Buffer) {
        let file = b.full_name();
        if let Some(lang) = self.lang_from_filepath(file) {
            let (line, character) = b.dot.active_cur().as_yx(b);
            let p = PendingParams::Hover(Pos::new(file, line as u32, character as u32));
            self.send(lang, p)
        }
    }
}

#[derive(Debug)]
pub struct LspManager {
    clients: HashMap<String, LspClient>,
    // map of (lspID, ReqID) -> in-flight requests we need a response for
    pending: HashMap<(usize, RequestId), Pending>,
    // map of lspID -> map of progress token -> title
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
            pending: Default::default(),
            progress_tokens: Default::default(),
            tx_req: tx_req.clone(),
            tx_events,
            next_id: 0,
        };

        spawn(move || manager.run(rx_req));

        LspManagerHandle { tx_req }
    }

    fn run(mut self, rx_req: Receiver<Req>) {
        for r in rx_req.into_iter() {
            match r {
                Req::Start { lang, cmd, root } => self.start_client(lang, cmd, root),
                Req::Stop { lang } => self.stop_client(lang),
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

    fn handle_pending(&mut self, PendingRequest { lang, pending }: PendingRequest) {
        let client = match self.clients.get_mut(&lang) {
            Some(client) => match client.status {
                Status::Running => client,
                Status::Initializing => {
                    self.send_status("LSP server still initializing");
                    return;
                }
            },
            None => {
                self.send_status(format!("no attached LSP client for {lang}"));
                return;
            }
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
                // TODO: handle error in initializing
                //       handle storing capabilities
                let (_id, _data) = extract!(Initialize, res);

                let client = match self.clients.get_mut(&lang) {
                    Some(client) => client,
                    None => {
                        self.send_status(format!("no attached LSP client for {lang}"));
                        return;
                    }
                };

                if let Err(e) = client.ack_initialized() {
                    self.report_error(format!("error initializing LSP client: {e}"));
                    return;
                }
                client.status = Status::Running;
                return;
            }

            Pending::GotoDefinition => {
                let (_id, data) = extract!(GotoDefinition, res);
                handle_goto_definition(data)
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

    pub fn start_client(&mut self, lang: String, cmd: String, root: String) {
        let lsp_id = self.next_id();
        let mut client = match LspClient::new(lsp_id, &cmd, self.tx_req.clone()) {
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

        self.clients.insert(lang.clone(), client);
        self.pending
            .insert((lsp_id, req_id), Pending::Initialize(lang));
        self.send_status("LSP server started")
    }

    pub fn stop_client(&mut self, lang: String) {
        let client = match self.clients.remove(&lang) {
            Some(client) => client,
            None => return self.report_error("no attached LSP client"),
        };

        let cmd = client.cmd.clone();
        let message = match client.shutdown_and_exit() {
            Ok(_) => "LSP client shutdown".to_string(),
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
    lang: String,
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

fn handle_goto_definition(data: Option<Option<GotoDefinitionResponse>>) -> Option<Actions> {
    match data {
        None | Some(None) => None,
        Some(Some(GotoDefinitionResponse::Scalar(loc))) => {
            let (path, coords) = parse_loc(loc);

            Some(Actions::Multi(vec![
                Action::OpenFile { path },
                Action::DotSetFromCoords { coords },
            ]))
        }

        Some(Some(GotoDefinitionResponse::Array(mut locs))) => {
            let (path, coords) = parse_loc(locs.remove(0));

            Some(Actions::Multi(vec![
                Action::OpenFile { path },
                Action::DotSetFromCoords { coords },
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

fn parse_loc(
    Location {
        uri,
        range: Range { start, end },
    }: Location,
) -> (String, Coords) {
    let filepath = uri.to_string().strip_prefix("file://").unwrap().to_owned();
    let coords = Coords {
        start_row: start.line as usize,
        start_col: start.character as usize,
        end_row: end.line as usize,
        end_col: end.character as usize,
    };

    (filepath, coords)
}
