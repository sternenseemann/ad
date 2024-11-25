//! Built-in minimal LSP support for ad
//!
//! See the LSP spec for details of semantics:
//!   https://microsoft.github.io/language-server-protocol/specification
use crate::{
    buffer::Buffer,
    editor::{Action, Actions, Coords},
    input::Event,
    lsp::{
        client::{LspClient, LspMessage},
        msg::{Message, RequestId, Response},
        notifications::try_parse_notification,
    },
};
use lsp_types::{GotoDefinitionResponse, Hover, Location, Range};
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
    Pending(PendingRequest),
    Message(LspMessage),
}

impl Req {
    fn pending(lsp_id: usize, req_id: RequestId, pending: Pending) -> Req {
        Req::Pending(PendingRequest {
            lsp_id,
            req_id,
            pending,
        })
    }
}

#[derive(Debug)]
pub struct LspManager {
    clients: HashMap<String, LspClient>,
    tx_req: Sender<Req>,
    next_id: usize,
}

impl LspManager {
    pub fn new(tx_events: Sender<Event>) -> Self {
        let (tx_req, rx_req) = channel();
        spawn(move || listener(rx_req, tx_events));

        Self {
            clients: Default::default(),
            tx_req,
            next_id: 0,
        }
    }

    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;

        id
    }

    // FIXME: this is clunky: rx_init should probably be a typestate
    fn client(&mut self, lang: &str) -> Option<&mut LspClient> {
        let client = self.clients.get_mut(lang)?;
        match &mut client.rx_init {
            None => return Some(client),
            Some(rx) => {
                if rx.try_recv().is_err() {
                    return None;
                }
            }
        }
        client.rx_init = None;
        client.ack_initialized().ok()?; // FIXME: handle error

        Some(client)
    }

    // FIXME: hardcoded for rust just now
    fn lang_from_filepath(&self, file: &str) -> Option<String> {
        if file.ends_with("rs") {
            return Some("rust".to_string());
        }

        None
    }

    fn client_from_filepath(&mut self, file: &str) -> Option<&mut LspClient> {
        let lang = self.lang_from_filepath(file)?;
        self.client(&lang)
    }

    pub fn start_client(&mut self, lang: String, cmd: &str, root: &str) -> &'static str {
        let lsp_id = self.next_id();
        let (tx, rx) = channel();
        let mut client = match LspClient::new(lsp_id, cmd, self.tx_req.clone(), rx) {
            Ok(client) => client,
            Err(e) => {
                error!("failed to start LSP server: {e}");
                return "error starting LSP server";
            }
        };
        let req_id = match client.initialize(root) {
            Ok(id) => id,
            Err(e) => {
                error!("failed to initialize LSP server: {e}");
                return "error initializing LSP server";
            }
        };

        let req = Req::pending(lsp_id, req_id, Pending::Initialize(tx));
        _ = self.tx_req.send(req);
        self.clients.insert(lang, client);

        "LSP server started"
    }

    pub fn stop_client(&mut self, b: &Buffer) -> &'static str {
        let file = b.full_name();
        let lang = match self.lang_from_filepath(file) {
            Some(lang) => lang,
            None => return "no attached LSP client",
        };
        let client = match self.clients.remove(&lang) {
            Some(client) => client,
            None => return "no attached LSP client",
        };

        let cmd = client.cmd.clone();
        match client.shutdown_and_exit() {
            Ok(_) => "LSP client shutdown",
            Err(e) => {
                error!("LSP client shutdown ({cmd}): {e}");
                "error shutting down LSP client"
            }
        }
    }

    pub fn goto_definition(&mut self, b: &Buffer) -> &'static str {
        let (line, character) = b.dot.active_cur().as_yx(b);
        let file = b.full_name();
        let client = match self.client_from_filepath(file) {
            Some(client) => client,
            None => return "no attached LSP client",
        };

        match client.goto_definition(file, line as u32, character as u32) {
            Ok(req_id) => {
                let req = Req::pending(client.id, req_id, Pending::GotoDefinition);
                _ = self.tx_req.send(req);
                "requesting goto definition"
            }

            Err(e) => {
                error!("unable to request LSP goto definition: {e}");
                "unable to request goto definition"
            }
        }
    }

    pub fn hover(&mut self, b: &Buffer) -> &'static str {
        let (line, character) = b.dot.active_cur().as_yx(b);
        let file = b.full_name();
        let client = match self.client_from_filepath(file) {
            Some(client) => client,
            None => return "no attached LSP client",
        };

        match client.hover(file, line as u32, character as u32) {
            Ok(req_id) => {
                let req = Req::pending(client.id, req_id, Pending::Hover);
                _ = self.tx_req.send(req);
                "requesting hover"
            }

            Err(e) => {
                error!("unable to request LSP hover: {e}");
                "unable to request hover"
            }
        }
    }
}

fn listener(rx_pending: Receiver<Req>, tx_events: Sender<Event>) {
    // map of (lspID, ReqID) -> in-flight requests we need a response for
    let mut pending: HashMap<(usize, RequestId), Pending> = HashMap::default();
    // map of lspID -> map of progress token -> title
    let mut progress_tokens: HashMap<usize, HashMap<String, String>> = HashMap::default();

    for r in rx_pending.into_iter() {
        match r {
            Req::Pending(p) => {
                pending.insert((p.lsp_id, p.req_id), p.pending);
            }

            Req::Message(LspMessage { lsp_id, msg }) => match msg {
                Message::Request(r) => {
                    error!("dropping unexpected request from LSP: {r:?}");
                }

                Message::Response(r) => {
                    let req_id = r.id.clone();
                    match pending.remove(&(lsp_id, req_id)) {
                        Some(pending) => {
                            if let Some(actions) = pending.try_into_editor_actions(r) {
                                if tx_events.send(Event::Actions(actions)).is_err() {
                                    warn!("LSP sender actions channel closed: exiting");
                                    return;
                                }
                            }
                        }
                        None => {
                            error!("got response for unknown LSP request: {r:?}");
                        }
                    }
                }

                Message::Notification(n) => {
                    let tokens = progress_tokens.entry(lsp_id).or_default();
                    if let Some(actions) = try_parse_notification(n, tokens) {
                        if tx_events.send(Event::Actions(actions)).is_err() {
                            warn!("LSP sender actions channel closed: exiting");
                            return;
                        }
                    }
                }
            },
        }
    }
}

#[derive(Debug)]
pub struct PendingRequest {
    lsp_id: usize,
    req_id: RequestId,
    pending: Pending,
}

#[derive(Debug)]
pub enum Pending {
    Initialize(Sender<()>),
    GotoDefinition,
    Hover,
}

impl Pending {
    pub(crate) fn try_into_editor_actions(self, res: Response) -> Option<Actions> {
        use lsp_types::request::{GotoDefinition, HoverRequest, Initialize};

        macro_rules! extract {
            ($k:ty, $res:expr) => {
                match $res.extract::<$k>() {
                    Ok(data) => data,
                    Err(e) => {
                        error!("malformed LSP response: {e}");
                        return None;
                    }
                }
            };
        }

        match self {
            Self::Initialize(rx) => {
                // TODO: handle error in initializing
                //       handle storing capabilities
                let (_id, _data) = extract!(Initialize, res);
                _ = rx.send(());
                None
            }

            Self::GotoDefinition => {
                let (_id, data) = extract!(GotoDefinition, res);
                handle_goto_definition(data)
            }

            Self::Hover => {
                let (_id, data) = extract!(HoverRequest, res);
                handle_hover(data)
            }
        }
    }
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

    let content = match data {
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
        txt: content,
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
