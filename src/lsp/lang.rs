//! Language config for LSP clients
//! TODO:
//!   - parse this from a config file
//!   - provide a mechanism for mapping the root (see neovim's support for rust-analyzer)
use crate::{buffer::Buffer, util::parent_dir_containing};
use std::path::Path;

/// Configuration for running a given language server
#[derive(Debug, Clone)]
pub struct LspConfig {
    /// The language name that this config applies to
    pub lang: String,
    /// The command to run to start the language server
    pub cmd: String,
    /// Additional arguments to pass to the language server command
    pub args: Vec<String>,
    /// The file extensions that should be associated with this language
    pub extensions: Vec<String>,
    /// Files or directories to search for in order to determine the project root
    pub roots: Vec<String>,
}

impl LspConfig {
    pub fn root_for_buffer<'a>(&self, b: &'a Buffer) -> Option<&'a Path> {
        let d = b.dir()?;
        for root in self.roots.iter() {
            if let Some(p) = parent_dir_containing(d, root) {
                return Some(p);
            }
        }

        None
    }
}

/// The built-in configured LSP servers
pub fn built_in_configs() -> Vec<LspConfig> {
    vec![
        LspConfig {
            lang: "rust".to_owned(),
            cmd: "rust-analyzer".to_owned(),
            args: Vec::new(),
            extensions: vec!["rs".to_owned()],
            roots: vec!["Cargo.toml".to_owned()],
        },
        LspConfig {
            lang: "dart".to_owned(),
            cmd: "dart".to_owned(),
            args: vec!["language-server".to_owned(), "--protocol=lsp".to_owned()],
            extensions: vec!["dart".to_owned()],
            roots: vec!["melos.yaml".to_owned(), "pubspec.yaml".to_owned()],
        },
    ]
}
