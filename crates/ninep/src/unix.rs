//! Unix specific handling of 9p connections.
//! See [intro(3)](https://9fans.github.io/plan9port/man/man3/intro.html)
//! and [intro(4)](https://9fans.github.io/plan9port/man/man4/intro.html)
//! of  Plan 9 from User Space (`plan9port`).

use crate::Result;

use std::{env, ffi::OsString, path::PathBuf};

/// Determine the current namespace (set by the user via the `NAMESPACE`
/// environment variable) or fallback to the default location
/// (based on user name and `DISPLAY`).
/// On unix, the namespace is a directory which 9p unix type sockets are
/// placed by different 9p servers.
///
/// The implementation follows `getns.c` from `plan9port`.
/// See also [intro(4)](https://9fans.github.io/plan9port/man/man4/intro.html)
/// and [namespace(1)](https://9fans.github.io/plan9port/man/man1/namespace.html).
pub fn namespace() -> Result<PathBuf> {
    if let Some(ns) = env::var_os("NAMESPACE") {
        Ok(ns.into())
    } else {
        let uname = get_user_name()?;
        // FIXME: we should fail if DISPLAY is unset, like getns.c in p9p
        let display = env::var_os("DISPLAY").unwrap_or(OsString::from(":0"));
        // plan9port hardcodes /tmp, believe it or not
        let mut ns = OsString::from("/tmp/ns.");
        ns.push(uname);
        ns.push(".");
        ns.push(display);
        Ok(ns.into())
    }
}

// FIXME: use something more robust like getuid()
/// Determine the (unix) name of the executing user.
pub fn get_user_name() -> Result<String> {
    match env::var("USER") {
        Ok(s) => Ok(s),
        Err(env::VarError::NotPresent) => Err("USER environment variable is not set".to_string()),
        Err(env::VarError::NotUnicode(_)) => {
            Err("USER environment variable is not valid UTF-8".to_string())
        }
    }
}
