//! Built-in minimal LSP support for ad
//!
//! This is not a general purpose client and it is not aiming to support all LSP features.
use crate::msg::{Message, Notification, Request};
use std::{
    io::{self, BufRead, BufReader},
    process::{self, ChildStdin, Command, Stdio},
    str::FromStr,
    sync::mpsc::Sender,
    thread::{spawn, JoinHandle},
};
// use tracing::error;

/// A tagged message from an LSP
#[derive(Debug)]
pub struct LspMessage {
    pub lsp_id: usize,
    pub msg: Message,
}

// TODO: probably need to track capabilities in case the server we're talking to doesn't support
//       some of the actions we want to make use of.
/// A simple LSP client that talks to a single lsp server subprocess over stdin / stdout
#[derive(Debug)]
pub struct LspClient {
    // cmd: String,
    stdin: ChildStdin,
    read_thread: JoinHandle<io::Result<()>>,
    err_thread: JoinHandle<io::Result<()>>,
}

impl LspClient {
    /// Spawn a new lsp server running the given command.
    ///
    /// Stdin for the server is held within the client and can be used via the [LspClient::write]
    /// method to communicate with the server. Messages coming from the server are sent over `tx`
    /// for centeral processing in the main editor event loop and errors are logged.
    pub fn new(lsp_id: usize, cmd: &str, tx: Sender<LspMessage>) -> io::Result<Self> {
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
                if let Err(e) = tx.send(LspMessage { lsp_id, msg }) {
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
            // cmd,
            stdin,
            read_thread,
            err_thread,
        })
    }

    fn write(&mut self, msg: Message) -> io::Result<()> {
        msg.write(&mut self.stdin)
    }

    pub fn initialize(&mut self, workspace_folders: &[&str]) -> io::Result<()> {
        use lsp_types::{
            request::Initialize, ClientCapabilities, InitializeParams,
            TextDocumentClientCapabilities, Uri, WorkspaceClientCapabilities, WorkspaceFolder,
        };

        let params = InitializeParams {
            process_id: Some(process::id()),
            workspace_folders: Some(
                workspace_folders
                    .iter()
                    .map(|f| WorkspaceFolder {
                        uri: Uri::from_str(&format!("file://{f}")).unwrap(),
                        name: "ad".into(),
                    })
                    .collect(),
            ),
            capabilities: ClientCapabilities {
                workspace: Some(WorkspaceClientCapabilities {
                    // https://docs.rs/lsp-types/0.97.0/lsp_types/struct.WorkspaceClientCapabilities.html
                    workspace_folders: Some(true),
                    ..Default::default()
                }),
                text_document: Some(TextDocumentClientCapabilities {
                    // https://docs.rs/lsp-types/0.97.0/lsp_types/struct.TextDocumentClientCapabilities.html
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        };

        self.write(Request::new::<Initialize>(params).into())
    }

    pub fn ack_initialized(&mut self) -> io::Result<()> {
        use lsp_types::{notification::Initialized, InitializedParams};

        self.write(Notification::new::<Initialized>(InitializedParams {}).into())
    }

    pub fn workspace_folders(&mut self) -> io::Result<()> {
        use lsp_types::request::WorkspaceFoldersRequest;

        self.write(Request::new::<WorkspaceFoldersRequest>(()).into())
    }

    pub fn goto_definition(&mut self, file: &str, line: u32, character: u32) -> io::Result<()> {
        use lsp_types::{
            request::GotoDefinition, GotoDefinitionParams, PartialResultParams, Position,
            TextDocumentIdentifier, TextDocumentPositionParams, Uri, WorkDoneProgressParams,
        };

        let params = GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str(&format!("file:///home/sminez/repos/personal/ad/{file}"))
                        .unwrap(),
                },
                position: Position { line, character },
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
        };

        self.write(Request::new::<GotoDefinition>(params).into())
    }

    pub fn shutdown_and_exit(mut self) -> io::Result<()> {
        use lsp_types::{notification::Exit, request::Shutdown};

        self.write(Request::new::<Shutdown>(()).into())?;
        self.write(Notification::new::<Exit>(()).into())?;

        self.join()
    }

    // TODO: this shouldn't panic, just log that shutdown wasn't clean
    fn join(self) -> io::Result<()> {
        match self.read_thread.join() {
            Ok(r) => r?,
            Err(err) => std::panic::panic_any(err),
        }
        match self.err_thread.join() {
            Ok(r) => r,
            Err(err) => std::panic::panic_any(err),
        }
    }
}
