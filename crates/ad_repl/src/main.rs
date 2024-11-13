//! A simple REPL that syncs a shell session with an ad buffer.
//!
//! - "Execute" is defined to be "send as input to the shell".
//! - Hitting return at the end of the buffer will send that line to the shell.
//! - Running "clear" will clear the ad buffer
//! - Running "exit" will close the shell subprocess as well as the ad buffer
use ad_client::{Client, EventFilter, Outcome, Source};
use std::{
    fs::File,
    io::{self, copy, Write},
    process::exit,
    thread::spawn,
};
use subprocess::{Popen, PopenConfig, Redirection};

fn main() -> io::Result<()> {
    let mut client = Client::new()?;
    client.open_in_new_window("+win")?;
    let buffer_id = client.current_buffer()?;

    let mut proc = Popen::create(
        &["sh", "-i"],
        PopenConfig {
            stdin: Redirection::Pipe,
            stdout: Redirection::Pipe,
            stderr: Redirection::Merge,
            ..Default::default()
        },
    )
    .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let stdin = proc.stdin.take().unwrap();
    let mut stdout = proc.stdout.take().unwrap();
    let mut w = client.body_writer(&buffer_id)?;

    spawn(move || {
        _ = copy(&mut stdout, &mut w);
    });

    let f = Filter {
        proc,
        buffer_id: buffer_id.clone(),
        stdin,
    };

    client.run_event_filter(&buffer_id, f).unwrap();

    Ok(())
}

struct Filter {
    proc: Popen,
    buffer_id: String,
    stdin: File,
}

impl Drop for Filter {
    fn drop(&mut self) {
        _ = self.proc.kill();
    }
}

impl Filter {
    fn send_input(&mut self, input: &str, client: &mut Client) -> io::Result<Outcome> {
        match input.trim() {
            "clear" => {
                client.write_xaddr(&self.buffer_id, ",")?;
                client.write_xdot(&self.buffer_id, "$ ")?;
                client.write_addr(&self.buffer_id, "$")?;
                return Ok(Outcome::Handled);
            }

            "exit" => {
                client.ctl("db!", "")?;
                self.proc.kill()?;
                exit(0);
            }

            _ => (),
        }

        self.stdin.write_all(input.as_bytes())?;
        if !input.ends_with("\n") {
            self.stdin.write_all(b"\n")?;
        }

        Ok(Outcome::Handled)
    }
}

impl EventFilter for Filter {
    fn handle_insert(
        &mut self,
        src: Source,
        _from: usize,
        _to: usize,
        txt: &str,
        client: &mut Client,
    ) -> io::Result<Outcome> {
        if src == Source::Fsys {
            // This is us writing to the body so move dot to EOF
            client.write_addr(&self.buffer_id, "$")?;
            return Ok(Outcome::Handled);
        }

        if txt == "\n" {
            client.write_xaddr(&self.buffer_id, "$")?;
            let xaddr = client.read_xaddr(&self.buffer_id)?;
            let addr = client.read_addr(&self.buffer_id)?;

            if xaddr == addr {
                client.write_xaddr(&self.buffer_id, "$-1")?;
                let raw = client.read_xdot(&self.buffer_id)?;
                let s = match raw.strip_prefix("$ ") {
                    Some(s) => s,
                    None => &raw,
                };
                return self.send_input(s, client);
            }
        }

        Ok(Outcome::Handled)
    }

    fn handle_execute(
        &mut self,
        _src: Source,
        _from: usize,
        _to: usize,
        txt: &str,
        client: &mut Client,
    ) -> io::Result<Outcome> {
        client.append_to_body(&self.buffer_id, "\n")?;
        let outcome = self.send_input(txt, client)?;

        Ok(outcome)
    }
}
