//! A lightweight wrapper around the JSON RPC message protocol required for
//! communicating with LSP servers.
//!
//! The implementation here is heavily inspired by the lsp-server module found in
//! rust-analyzer: https://github.com/rust-lang/rust-analyzer/tree/master/lib/lsp-server
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{
    borrow::Cow,
    fmt,
    io::{self, BufRead, Write},
};
use tracing::trace;

fn invalid_data(error: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, error)
}

macro_rules! invalid_data {
    ($($tt:tt)*) => (invalid_data(format!($($tt)*)))
}

fn read_msg_text(r: &mut dyn BufRead) -> io::Result<Option<String>> {
    let mut size = None;
    let mut buf = String::new();
    loop {
        buf.clear();
        if r.read_line(&mut buf)? == 0 {
            return Ok(None);
        }
        if !buf.ends_with("\r\n") {
            return Err(invalid_data!("malformed header: {:?}", buf));
        }
        let buf = &buf[..buf.len() - 2];
        if buf.is_empty() {
            break;
        }
        let mut parts = buf.splitn(2, ": ");
        let header_name = parts.next().unwrap();
        let header_value = parts
            .next()
            .ok_or_else(|| invalid_data!("malformed header: {:?}", buf))?;
        if header_name.eq_ignore_ascii_case("Content-Length") {
            size = Some(header_value.parse::<usize>().map_err(invalid_data)?);
        }
    }

    let size: usize = size.ok_or_else(|| invalid_data!("no Content-Length"))?;
    let mut buf = buf.into_bytes();
    buf.resize(size, 0);
    r.read_exact(&mut buf)?;
    let buf = String::from_utf8(buf).map_err(invalid_data)?;
    trace!("recv LSP message: {buf}");

    Ok(Some(buf))
}

fn write_msg_text(w: &mut dyn Write, msg: &str) -> io::Result<()> {
    trace!("send LSP message: {msg}");
    write!(w, "Content-Length: {}\r\n\r\n", msg.len())?;
    w.write_all(msg.as_bytes())?;
    w.flush()?;

    Ok(())
}

/// The various message types supported in JSON rpc
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

impl Message {
    pub fn read(r: &mut impl BufRead) -> io::Result<Option<Self>> {
        Message::_read(r)
    }

    fn _read(r: &mut dyn BufRead) -> io::Result<Option<Self>> {
        let text = match read_msg_text(r)? {
            None => return Ok(None),
            Some(text) => text,
        };

        let msg = match serde_json::from_str(&text) {
            Ok(msg) => msg,
            Err(e) => {
                return Err(invalid_data!("malformed LSP payload: {:?}", e));
            }
        };

        Ok(Some(msg))
    }

    pub fn write(self, w: &mut impl Write) -> io::Result<()> {
        self._write(w)
    }

    fn _write(self, w: &mut dyn Write) -> io::Result<()> {
        let s = serde_json::to_string(&JsonRpc {
            jsonrpc: "2.0",
            msg: self,
        })?;

        return write_msg_text(w, &s);

        // Serde structs

        #[derive(Serialize)]
        struct JsonRpc {
            jsonrpc: &'static str,
            #[serde(flatten)]
            msg: Message,
        }
    }
}

#[derive(Debug)]
pub enum ExtractError<T> {
    /// The extracted message was of a different method than expected.
    MethodMismatch(T),

    /// The underlying request failed
    ResponseError(ResponseError),

    /// Failed to deserialize the message.
    JsonError {
        method: &'static str,
        error: serde_json::Error,
    },
}

impl std::error::Error for ExtractError<Response> {}
impl fmt::Display for ExtractError<Response> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractError::MethodMismatch(res) => {
                write!(f, "Method mismatch for response '{}'", res.id)
            }

            ExtractError::ResponseError(err) => {
                write!(f, "Request returned error '{:?}'", err)
            }

            ExtractError::JsonError { method, error } => {
                write!(f, "Invalid response\nMethod: {method}\n error: {error}",)
            }
        }
    }
}

impl std::error::Error for ExtractError<Notification> {}
impl fmt::Display for ExtractError<Notification> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractError::MethodMismatch(not) => {
                write!(f, "Method mismatch for notification '{}'", not.method)
            }

            ExtractError::ResponseError(_) => unreachable!("only for responses"),

            ExtractError::JsonError { method, error } => {
                write!(f, "Invalid notification\nMethod: {method}\n error: {error}")
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    pub id: RequestId,
    pub method: Cow<'static, str>,
    #[serde(default = "Value::default")]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}

impl Request {
    pub fn new<R>(id: RequestId, params: R::Params) -> Request
    where
        R: lsp_types::request::Request,
    {
        Self {
            id,
            method: Cow::Borrowed(R::METHOD),
            params: serde_json::to_value(params).unwrap(),
        }
    }

    pub fn extract<R>(self) -> Result<(RequestId, R::Params), ExtractError<Request>>
    where
        R: lsp_types::request::Request,
    {
        if self.method != R::METHOD {
            return Err(ExtractError::MethodMismatch(self));
        }
        match serde_json::from_value(self.params) {
            Ok(params) => Ok((self.id, params)),
            Err(error) => Err(ExtractError::JsonError {
                method: R::METHOD,
                error,
            }),
        }
    }
}

impl From<Request> for Message {
    fn from(r: Request) -> Self {
        Message::Request(r)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub id: RequestId,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<ResponseError>,
}

impl Response {
    pub fn extract<R>(self) -> Result<(RequestId, Option<R::Result>), ExtractError<Response>>
    where
        R: lsp_types::request::Request,
    {
        if let Some(err) = self.error {
            return Err(ExtractError::ResponseError(err));
        }

        match self.result {
            Some(val) => match serde_json::from_value(val) {
                Ok(res) => Ok((self.id, res)),
                Err(error) => Err(ExtractError::JsonError {
                    method: R::METHOD,
                    error,
                }),
            },
            None => Ok((self.id, None)),
        }
    }
}

impl From<Response> for Message {
    fn from(r: Response) -> Self {
        Message::Response(r)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Notification {
    pub method: Cow<'static, str>,
    #[serde(default = "Value::default")]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}

impl From<Notification> for Message {
    fn from(n: Notification) -> Self {
        Message::Notification(n)
    }
}

impl Notification {
    pub fn new<N>(params: N::Params) -> Notification
    where
        N: lsp_types::notification::Notification,
    {
        Self {
            method: Cow::Borrowed(N::METHOD),
            params: serde_json::to_value(params).unwrap(),
        }
    }

    pub fn extract<N>(self) -> Result<N::Params, ExtractError<Notification>>
    where
        N: lsp_types::notification::Notification,
    {
        if self.method != N::METHOD {
            return Err(ExtractError::MethodMismatch(self));
        }
        match serde_json::from_value(self.params) {
            Ok(params) => Ok(params),
            Err(error) => Err(ExtractError::JsonError {
                method: N::METHOD,
                error,
            }),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResponseError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

// #[derive(Debug, Clone, Copy)]
// #[non_exhaustive]
// pub enum ErrorCode {
//     // Defined by JSON RPC:
//     ParseError = -32700,
//     InvalidRequest = -32600,
//     MethodNotFound = -32601,
//     InvalidParams = -32602,
//     InternalError = -32603,
//     ServerErrorStart = -32099,
//     ServerErrorEnd = -32000,

//     /// Error code indicating that a server received a notification or
//     /// request before the server has received the `initialize` request.
//     ServerNotInitialized = -32002,
//     Unknown = -32001,

//     // Defined by the protocol:
//     /// The client has canceled a request and a server has detected
//     /// the cancel.
//     RequestCanceled = -32800,

//     /// The server detected that the content of a document got
//     /// modified outside normal conditions. A server should
//     /// NOT send this error code if it detects a content change
//     /// in it unprocessed messages. The result even computed
//     /// on an older state might still be useful for the client.
//     ///
//     /// If a client decides that a result is not of any use anymore
//     /// the client should cancel the request.
//     ContentModified = -32801,

//     /// The server cancelled the request. This error code should
//     /// only be used for requests that explicitly support being
//     /// server cancellable.
//     ///
//     /// @since 3.17.0
//     ServerCancelled = -32802,

//     /// A request failed but it was syntactically correct, e.g the
//     /// method name was known and the parameters were valid. The error
//     /// message should contain human readable information about why
//     /// the request failed.
//     ///
//     /// @since 3.17.0
//     RequestFailed = -32803,
// }

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RequestId(IdInner);

impl fmt::Display for RequestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            IdInner::Num(i) => fmt::Display::fmt(i, f),
            IdInner::Str(s) => fmt::Debug::fmt(s, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(untagged)]
enum IdInner {
    Num(i32),
    Str(String),
}

impl From<i32> for RequestId {
    fn from(id: i32) -> RequestId {
        RequestId(IdInner::Num(id))
    }
}

impl From<String> for RequestId {
    fn from(id: String) -> RequestId {
        RequestId(IdInner::Str(id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{request::Initialize, InitializeParams};

    #[test]
    fn new_req_works() {
        let r = Request::new::<Initialize>(0.into(), InitializeParams::default());
        let s = serde_json::to_string(&r).unwrap();
        assert_eq!(s, "");
    }
}
