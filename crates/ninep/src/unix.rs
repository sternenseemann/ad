//! Unix specific handling of 9p connections.
//! See [intro(3)](https://9fans.github.io/plan9port/man/man3/intro.html)
//! and [intro(4)](https://9fans.github.io/plan9port/man/man4/intro.html)
//! of  Plan 9 from User Space (`plan9port`).

use crate::Result;

use std::env;

pub fn namespace() -> Result<String> {
    let uname = get_user_name()?;
    Ok(format!("/tmp/ns.{uname}.:0"))
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
