//! Built-in minimal LSP support for ad
//!
//! See the LSP spec for details of semantics:
//!   https://microsoft.github.io/language-server-protocol/specification
use crate::{
    buffer::{Buffer, Buffers},
    die,
    editor::{Action, Actions, MbSelect, MbSelector, MiniBufferSelection, ViewPort},
    input::Event,
    lsp::{
        capabilities::{Capabilities, PositionEncoding},
        client::{LspClient, LspMessage, Status},
        lang::{built_in_configs, LspConfig},
        msg::{Message, Notification, Request, RequestId, Response},
    },
    util::ReadOnlyLock,
};
use lsp_types::{
    GotoDefinitionResponse, Hover, NumberOrString, ProgressParams, PublishDiagnosticsParams, Uri,
};
use std::{
    collections::HashMap,
    sync::{
        mpsc::{channel, Receiver, Sender},
        Arc, RwLock,
    },
    thread::spawn,
};
use tracing::{debug, error, warn};

mod capabilities;
mod client;
mod lang;
mod msg;

pub use capabilities::Coords;

const LSP_FILE: &str = "+lsp";

#[derive(Debug)]
pub(crate) enum Req {
    Start {
        lang: String,
        cmd: String,
        args: Vec<String>,
        root: String,
        open_bufs: Vec<PendingParams>,
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
    capabilities: ReadOnlyLock<HashMap<String, (usize, Capabilities)>>,
    diagnostics: ReadOnlyLock<HashMap<Uri, Vec<Diagnostic>>>,
    configs: Vec<LspConfig>,
}

impl LspManagerHandle {
    #[cfg(test)]
    pub(crate) fn new_stubbed(tx_req: Sender<Req>) -> Self {
        Self {
            tx_req,
            capabilities: Default::default(),
            diagnostics: Default::default(),
            configs: Default::default(),
        }
    }

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

    fn start_req_for_buf(&self, bs: &Buffers) -> Option<Req> {
        let b = bs.active();
        let config = self.config_for_buffer(b)?;
        let root = config.root_for_buffer(b)?.to_str()?.to_owned();
        let lang = &config.lang;
        let open_bufs: Vec<_> = bs
            .iter()
            .flat_map(|b| match self.config_for_buffer(b) {
                Some(config) if &config.lang == lang => Some(PendingParams::DocumentOpen {
                    path: b.full_name().to_string(),
                    content: b.str_contents(),
                }),
                _ => None,
            })
            .collect();

        Some(Req::Start {
            lang: config.lang.clone(),
            cmd: config.cmd.clone(),
            args: config.args.clone(),
            root,
            open_bufs,
        })
    }

    pub fn start_client(&self, bs: &Buffers) -> Option<&'static str> {
        match self.start_req_for_buf(bs) {
            Some(req) => {
                debug!("starting LSP server");
                if let Err(e) = self.tx_req.send(req) {
                    die!("LSP manager died: {e}")
                }
                None
            }

            None => Some("no LSP available for buffer"),
        }
    }

    pub fn stop_client(&self, b: &Buffer) {
        if let Some((lsp_id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("stopping LSP server {lsp_id}");
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

    pub fn show_diagnostics(&self, b: &Buffer) -> Action {
        self.document_changed(b); // to ensure that diagnostics are up to date
        debug!("showing LSP diagnostics");
        let guard = self.diagnostics.read().unwrap();
        let mut diags: Vec<Diagnostic> = guard.values().flatten().cloned().collect();
        diags.sort_unstable();

        Action::MbSelect(Diagnostics(diags).into_selector())
    }

    pub fn document_opened(&self, b: &Buffer) {
        if let Some((id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didOpen ({id})");
            let path = b.full_name().to_string();
            let content = b.str_contents();

            self.send(id, PendingParams::DocumentOpen { path, content })
        }
    }

    pub fn document_closed(&self, b: &Buffer) {
        if let Some((id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didClose ({id})");
            let path = b.full_name().to_string();

            self.send(id, PendingParams::DocumentClose { path })
        }
    }

    pub fn document_changed(&self, b: &Buffer) {
        if let Some((id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didChange ({id})");
            let path = b.full_name().to_string();
            let content = b.str_contents();

            self.send(
                id,
                PendingParams::DocumentChange {
                    path,
                    content,
                    version: 2,
                },
            )
        }
    }

    pub fn goto_definition(&self, b: &Buffer) {
        if let Some((id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }
            debug!("sending LSP textDocument/definition ({id})");
            self.send(id, PendingParams::GotoDefinition(enc.buffer_pos(b)))
        }
    }

    pub fn hover(&self, b: &Buffer) {
        if let Some((id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }
            debug!("sending LSP textDocument/hover ({id})");
            self.send(id, PendingParams::Hover(enc.buffer_pos(b)))
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
    diagnostics: Arc<RwLock<HashMap<Uri, Vec<Diagnostic>>>>,
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
            diagnostics: Default::default(),
            tx_req: tx_req.clone(),
            tx_events,
            next_id: 0,
        };

        let capabilities = ReadOnlyLock::new(manager.capabilities.clone());
        let diagnostics = ReadOnlyLock::new(manager.diagnostics.clone());
        spawn(move || manager.run(rx_req));

        LspManagerHandle {
            tx_req,
            capabilities,
            diagnostics,
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
                    open_bufs,
                } => self.start_client(lang, cmd, args, root, open_bufs),
                Req::Stop { lsp_id } => self.stop_client(lsp_id),
                Req::Pending(p) => self.handle_pending(p),

                Req::Message(LspMessage { lsp_id, msg }) => match msg {
                    Message::Request(r) => self.handle_request(lsp_id, r),
                    Message::Response(r) => self.handle_response(lsp_id, r),
                    Message::Notification(n) => self.handle_notification(lsp_id, n),
                },
            }
        }
    }

    fn handle_pending(&mut self, PendingRequest { lsp_id, pending }: PendingRequest) {
        // this is a macro as we don't need an attached client for the active buffer in
        // order to show all diagnostics
        macro_rules! client {
            () => {
                match self.clients.get_mut(&lsp_id) {
                    Some(client) => match client.status {
                        Status::Running => client,
                        Status::Initializing => {
                            self.send_status("LSP server still initializing");
                            return;
                        }
                    },
                    None => return self.send_status("no attached LSP client for buffer"),
                }
            };
        }

        match pending {
            PendingParams::DocumentOpen { path, content } => {
                if let Err(e) = client!().document_did_open(path, content) {
                    self.report_error(format!("unable to notify document open: {e}"))
                }
            }

            PendingParams::DocumentClose { path } => {
                if let Err(e) = client!().document_did_close(path) {
                    self.report_error(format!("unable to notify document close: {e}"))
                }
            }

            PendingParams::DocumentChange {
                path,
                content,
                version,
            } => {
                if let Err(e) = client!().document_did_change(path, content, version) {
                    self.report_error(format!("unable to notify document change: {e}"))
                }
            }

            PendingParams::GotoDefinition(pos) => {
                let client = client!();
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

            PendingParams::Hover(pos) => {
                let client = client!();
                match client.hover(&pos.file, pos.line, pos.character) {
                    Ok(req_id) => {
                        self.pending.insert((client.id, req_id), Pending::Hover);
                        self.send_status("requesting hover");
                    }

                    Err(e) => self.report_error(format!("unable to request LSP hover: {e}")),
                }
            }
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

        macro_rules! extract {
            ($k:ty, $res:expr) => {
                match $res.extract::<$k>() {
                    Ok(data) => data,
                    Err((_, e)) => {
                        error!("dropping malformed LSP response: {e:?}");
                        return;
                    }
                }
            };
        }

        let req_id = res.id();
        let p = match self.pending.remove(&(lsp_id, req_id)) {
            Some(p) => p,
            None => {
                error!("got response for unknown LSP request: {res:?}");
                return;
            }
        };

        let actions = match p {
            Pending::Initialize(lang, open_bufs) => {
                let (_id, res) = extract!(Initialize, res);

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
                        for pending in open_bufs {
                            self.handle_pending(PendingRequest { lsp_id, pending });
                        }
                    }

                    // Unknown position encoding that we can't support
                    None => self.stop_client(lsp_id),
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

    pub fn handle_notification(&mut self, lsp_id: usize, n: Notification) {
        use lsp_types::notification::{Notification as _, Progress, PublishDiagnostics};

        let tokens = self.progress_tokens.entry(lsp_id).or_default();

        let actions = match n.method.as_ref() {
            Progress::METHOD => match n.extract::<Progress>() {
                Ok(params) => handle_progress(params, tokens),
                Err(e) => {
                    error!("malformed progress notification: {e}");
                    None
                }
            },

            PublishDiagnostics::METHOD => {
                match n.extract::<PublishDiagnostics>() {
                    Ok(params) => self.update_diagnostics(lsp_id, params),
                    Err(e) => error!("malformed diagnostics notification: {e}"),
                }
                None
            }

            _ => {
                warn!(
                    "unknown notification from LSP: {}",
                    serde_json::to_string(&n).unwrap()
                );
                None
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

    fn start_client(
        &mut self,
        lang: String,
        cmd: String,
        args: Vec<String>,
        root: String,
        open_bufs: Vec<PendingParams>,
    ) {
        let lsp_id = self.next_id();
        let mut client = match LspClient::new(lsp_id, &lang, &cmd, args, self.tx_req.clone()) {
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
            .insert((lsp_id, req_id), Pending::Initialize(lang, open_bufs));
        self.send_status("LSP server started")
    }

    fn stop_client(&mut self, lsp_id: usize) {
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

    /// Currently throwing away a LOT of the information contained in the payload from the server
    /// Servers are in control over the state of diagnostics so any push of diagnostic state for
    /// a given file overwrites our current state
    fn update_diagnostics(&mut self, lsp_id: usize, params: PublishDiagnosticsParams) {
        let encoding = match self.clients.get(&lsp_id) {
            Some(c) => c.position_encoding,
            None => return,
        };

        let PublishDiagnosticsParams {
            uri, diagnostics, ..
        } = params;

        let new_diagnostics: Vec<Diagnostic> = diagnostics
            .into_iter()
            .map(|d| Diagnostic::new(uri.clone(), d, encoding))
            .collect();

        tracing::info!("new diagnostics: {new_diagnostics:?}");

        let mut guard = self.diagnostics.write().unwrap();

        guard.insert(uri, new_diagnostics);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Pos {
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
pub(crate) struct PendingRequest {
    lsp_id: usize,
    pending: PendingParams,
}

#[derive(Debug)]
pub(crate) enum PendingParams {
    DocumentOpen {
        path: String,
        content: String,
    },
    DocumentClose {
        path: String,
    },
    DocumentChange {
        path: String,
        content: String,
        version: usize,
    },
    GotoDefinition(Pos),
    Hover(Pos),
}

#[derive(Debug)]
pub(crate) enum Pending {
    Initialize(String, Vec<PendingParams>),
    GotoDefinition,
    Hover,
}

fn handle_goto_definition(
    data: Option<GotoDefinitionResponse>,
    p: PositionEncoding,
) -> Option<Actions> {
    match data? {
        GotoDefinitionResponse::Scalar(loc) => {
            let (path, coords) = Coords::new(loc, p);

            Some(Actions::Multi(vec![
                Action::OpenFile { path },
                Action::DotSetFromCoords { coords },
                Action::SetViewPort(ViewPort::Center),
            ]))
        }

        GotoDefinitionResponse::Array(mut locs) => {
            let (path, coords) = Coords::new(locs.remove(0), p);

            Some(Actions::Multi(vec![
                Action::OpenFile { path },
                Action::DotSetFromCoords { coords },
                Action::SetViewPort(ViewPort::Center),
            ]))
        }

        GotoDefinitionResponse::Link(links) => {
            error!("unhandled goto definition links response: {links:?}");
            None
        }
    }
}

fn handle_hover(data: Option<Hover>) -> Option<Actions> {
    use lsp_types::{HoverContents, MarkedString};

    let ms_to_string = |ms: MarkedString| match ms {
        MarkedString::String(s) => s,
        MarkedString::LanguageString(ls) => ls.value,
    };

    let txt = match data?.contents {
        HoverContents::Scalar(ms) => ms_to_string(ms),
        HoverContents::Markup(mc) => mc.value,
        HoverContents::Array(mss) => {
            let strs: Vec<_> = mss.into_iter().map(ms_to_string).collect();
            strs.join("\n")
        }
    };

    Some(Actions::Single(Action::OpenVirtualFile {
        name: LSP_FILE.to_string(),
        txt,
    }))
}

fn handle_progress(
    params: ProgressParams,
    tokens: &mut HashMap<NumberOrString, String>,
) -> Option<Actions> {
    use lsp_types::{
        ProgressParamsValue, WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressEnd,
        WorkDoneProgressReport,
    };
    use ProgressParamsValue::*;
    use WorkDoneProgress::*;

    let actions = |title: &str, message: Option<String>, perc: Option<u32>| {
        let message = message.unwrap_or_default();
        let message = if let Some(perc) = perc {
            format!("{title}: {message} ({perc}/100)")
        } else {
            format!("{title}: {message}")
        };

        Some(Actions::Single(Action::SetStatusMessage { message }))
    };

    match params.value {
        WorkDone(Begin(WorkDoneProgressBegin {
            title,
            message,
            percentage,
            ..
        })) => {
            let actions = actions(&title, message, percentage);
            tokens.insert(params.token, title);

            actions
        }

        WorkDone(Report(WorkDoneProgressReport {
            message,
            percentage,
            ..
        })) => {
            let title: &str = tokens.get(&params.token).map_or("", |s| s);
            actions(title, message, percentage)
        }

        WorkDone(End(WorkDoneProgressEnd { .. })) => {
            tokens.remove(&params.token);

            // Clear the status message when progress is done
            Some(Actions::Single(Action::SetStatusMessage {
                message: "".to_owned(),
            }))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diagnostic {
    path: String,
    content: String,
    coords: Coords,
}

impl Diagnostic {
    fn new(uri: Uri, d: lsp_types::Diagnostic, encoding: PositionEncoding) -> Self {
        let loc = lsp_types::Location {
            uri: uri.clone(),
            range: d.range,
        };
        let (path, coords) = Coords::new(loc, encoding);
        let fname = path.split("/").last().unwrap();
        let source = d.source.map(|s| format!("({s}) ")).unwrap_or_default();
        let content = format!("{source}{fname}:{} {}", coords.line(), d.message);

        Diagnostic {
            path,
            content,
            coords,
        }
    }

    pub fn as_actions(&self) -> Actions {
        Actions::Multi(vec![
            Action::OpenFile {
                path: self.path.clone(),
            },
            Action::DotSetFromCoords {
                coords: self.coords,
            },
            Action::SetViewPort(ViewPort::Center),
        ])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostics(Vec<Diagnostic>);

impl MbSelect for Diagnostics {
    fn clone_selector(&self) -> MbSelector {
        self.clone().into_selector()
    }

    fn prompt_and_options(&self) -> (String, Vec<String>) {
        (
            "Diagnostics> ".to_owned(),
            self.0.iter().map(|d| d.content.clone()).collect(),
        )
    }

    fn selected_actions(&self, sel: MiniBufferSelection) -> Option<Actions> {
        match sel {
            MiniBufferSelection::Line { cy, .. } => self.0.get(cy).map(|d| d.as_actions()),
            _ => None,
        }
    }
}
