//! Built-in minimal LSP support for ad
//!
//! This is not a general purpose client and it is not aiming to support all LSP features.
use crate::lsp::{
    msg::{Message, Notification, Request, RequestId},
    Req,
};
use lsp_types::{TextDocumentIdentifier, TextDocumentPositionParams};
use std::{
    io::{self, BufRead, BufReader},
    process::{self, ChildStdin, Command, Stdio},
    str::FromStr,
    sync::mpsc::Sender,
    thread::{spawn, JoinHandle},
};
use tracing::error;

/// A tagged message from an LSP
#[derive(Debug)]
pub struct LspMessage {
    pub lsp_id: usize,
    pub msg: Message,
}

#[derive(Debug, Clone, Copy)]
pub enum Status {
    Initializing,
    Running,
}

// TODO: probably need to track capabilities in case the server we're talking to doesn't support
//       some of the actions we want to make use of.
/// A simple LSP client that talks to a single lsp server subprocess over stdin / stdout
#[derive(Debug)]
pub struct LspClient {
    pub(super) status: Status,
    pub(super) id: usize,
    pub(super) cmd: String,
    stdin: ChildStdin,
    read_thread: JoinHandle<io::Result<()>>,
    err_thread: JoinHandle<io::Result<()>>,
    next_id: i32,
}

impl LspClient {
    /// Spawn a new lsp server running the given command.
    ///
    /// Stdin for the server is held within the client and can be used via the [LspClient::write]
    /// method to communicate with the server. Messages coming from the server are sent over `tx`
    /// for centeral processing in the main editor event loop and errors are logged.
    pub fn new(lsp_id: usize, cmd: &str, tx: Sender<Req>) -> io::Result<Self> {
        let mut proc = Command::new(cmd)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdin = proc.stdin.take().unwrap();
        let mut stdout = BufReader::new(proc.stdout.take().unwrap());
        let stderr = BufReader::new(proc.stderr.take().unwrap());

        let read_thread = spawn(move || {
            while let Some(msg) = Message::read(&mut stdout)? {
                if let Err(e) = tx.send(Req::Message(LspMessage { lsp_id, msg })) {
                    return Err(io::Error::new(io::ErrorKind::Other, e));
                }
            }
            Ok(())
        });

        let cmd_ = cmd.to_string();
        let err_thread = spawn(move || {
            for line in stderr.lines() {
                println!("lsp error ({cmd_}): {}", line?);
            }

            Ok(())
        });

        Ok(Self {
            status: Status::Initializing,
            id: lsp_id,
            cmd: cmd.to_string(),
            stdin,
            read_thread,
            err_thread,
            next_id: 0,
        })
    }

    fn next_id(&mut self) -> RequestId {
        let id = self.next_id;
        self.next_id += 1;

        id.into()
    }

    fn write(&mut self, msg: Message) -> io::Result<()> {
        msg.write(&mut self.stdin)
    }

    pub fn initialize(&mut self, root: &str) -> io::Result<RequestId> {
        use lsp_types::{
            request::Initialize, ClientCapabilities, DiagnosticClientCapabilities,
            DiagnosticWorkspaceClientCapabilities, HoverClientCapabilities, InitializeParams,
            MarkupKind, NumberOrString, TextDocumentClientCapabilities, Uri,
            WindowClientCapabilities, WorkDoneProgressParams, WorkspaceClientCapabilities,
            WorkspaceFolder,
        };

        #[allow(deprecated)] // root_uri
        let params = InitializeParams {
            process_id: Some(process::id()),
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: Some(NumberOrString::String("abc123".to_string())),
            },
            root_uri: Some(Uri::from_str(&format!("file://{root}")).unwrap()),
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Uri::from_str(&format!("file://{root}")).unwrap(),
                name: "ad".to_string(),
            }]),
            capabilities: ClientCapabilities {
                workspace: Some(WorkspaceClientCapabilities {
                    // https://docs.rs/lsp-types/0.97.0/lsp_types/struct.WorkspaceClientCapabilities.html
                    workspace_folders: Some(true),
                    diagnostic: Some(DiagnosticWorkspaceClientCapabilities {
                        refresh_support: Some(true),
                    }),
                    ..Default::default()
                }),
                text_document: Some(TextDocumentClientCapabilities {
                    diagnostic: Some(DiagnosticClientCapabilities {
                        dynamic_registration: Some(true),
                        related_document_support: Some(true),
                    }),
                    hover: Some(HoverClientCapabilities {
                        dynamic_registration: Some(true),
                        content_format: Some(vec![MarkupKind::PlainText]),
                    }),
                    // https://docs.rs/lsp-types/0.97.0/lsp_types/struct.TextDocumentClientCapabilities.html
                    ..Default::default()
                }),
                // This is what we need for getting rust-analyzer (and presumably other LSPs?) to
                // report things like their current state and progress during init
                // -> results in us getting "window/workDoneProgress/create" requests
                window: Some(WindowClientCapabilities {
                    work_done_progress: Some(true),
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        };

        let id = self.next_id();
        self.write(Request::new::<Initialize>(id.clone(), params).into())?;

        Ok(id)
    }

    pub fn ack_initialized(&mut self) -> io::Result<()> {
        use lsp_types::{notification::Initialized, InitializedParams};

        self.write(Notification::new::<Initialized>(InitializedParams {}).into())
    }

    pub fn goto_definition(
        &mut self,
        file: &str,
        line: u32,
        character: u32,
    ) -> io::Result<RequestId> {
        use lsp_types::{request::GotoDefinition, GotoDefinitionParams};

        let params = GotoDefinitionParams {
            text_document_position_params: txtdoc_pos(file, line, character),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let id = self.next_id();
        self.write(Request::new::<GotoDefinition>(id.clone(), params).into())?;

        Ok(id)
    }

    pub fn hover(&mut self, file: &str, line: u32, character: u32) -> io::Result<RequestId> {
        use lsp_types::{request::HoverRequest, HoverParams};

        let params = HoverParams {
            text_document_position_params: txtdoc_pos(file, line, character),
            work_done_progress_params: Default::default(),
        };

        let id = self.next_id();
        self.write(Request::new::<HoverRequest>(id.clone(), params).into())?;

        Ok(id)
    }

    pub fn shutdown_and_exit(mut self) -> io::Result<()> {
        use lsp_types::{notification::Exit, request::Shutdown};

        let id = self.next_id();
        self.write(Request::new::<Shutdown>(id, ()).into())?;
        self.write(Notification::new::<Exit>(()).into())?;
        self.join();

        Ok(())
    }

    fn join(self) {
        match self.read_thread.join() {
            Ok(res) => {
                if let Err(e) = res {
                    error!("LSP({}): stdout thread error: {e}", self.cmd);
                }
            }
            Err(_) => error!(
                "LSP({}): stdout thread failed to shut down cleanly",
                self.cmd
            ),
        }
        match self.err_thread.join() {
            Ok(res) => {
                if let Err(e) = res {
                    error!("LSP({}): stderr thread error: {e}", self.cmd);
                }
            }
            Err(_) => error!(
                "LSP({}): stderr thread failed to shut down cleanly",
                self.cmd
            ),
        }
    }
}

fn txt_doc_id(file: &str) -> TextDocumentIdentifier {
    use lsp_types::{TextDocumentIdentifier, Uri};

    TextDocumentIdentifier {
        uri: Uri::from_str(&format!("file://{file}")).unwrap(),
    }
}

fn txtdoc_pos(file: &str, line: u32, character: u32) -> TextDocumentPositionParams {
    use lsp_types::Position;

    TextDocumentPositionParams {
        text_document: txt_doc_id(file),
        position: Position { line, character },
    }
}
