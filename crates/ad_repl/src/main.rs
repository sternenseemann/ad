//! A simple REPL that syncs an rc shell session with an ad buffer.
//!
//! - "Execute" is defined to be "send as input to the shell".
//! - Hitting return at the end of the buffer will send that line to the shell.
//! - Running "clear" will clear the ad buffer
//! - Running "exit" will close the shell subprocess as well as the ad buffer
use ad_client::{Client, EventFilter, Outcome, Source};
use std::{
    io::{self, copy, Write},
    process::{exit, Child, ChildStdin, Command, Stdio},
    thread::{sleep, spawn},
    time::Duration,
};

fn main() -> io::Result<()> {
    let mut client = Client::new()?;
    client.open_in_new_window("+win")?;
    let buffer_id = client.current_buffer()?;

    let mut child = Command::new("rc")
        .arg("-i")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let stdin = child.stdin.take().unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();

    let mut w = client.body_writer(&buffer_id)?;
    spawn(move || {
        _ = copy(&mut stdout, &mut w);
    });

    let mut w = client.body_writer(&buffer_id)?;
    spawn(move || {
        _ = copy(&mut stderr, &mut w);
    });

    let mut f = Filter {
        child,
        buffer_id: buffer_id.clone(),
        stdin,
    };

    // Ensure that the prompt is set to '%' as the ubuntu package changes it to ';'
    sleep(Duration::from_millis(50));
    f.send_input("prompt='% '\n", &mut client)?;
    f.clear_buffer(&mut client)?;
    client.ctl("mark-clean", "")?;

    client.run_event_filter(&buffer_id, f).unwrap();

    Ok(())
}

struct Filter {
    child: Child,
    buffer_id: String,
    stdin: ChildStdin,
}

impl Drop for Filter {
    fn drop(&mut self) {
        _ = self.child.kill();
    }
}

impl Filter {
    fn clear_buffer(&mut self, client: &mut Client) -> io::Result<()> {
        client.write_xaddr(&self.buffer_id, ",")?;
        client.write_xdot(&self.buffer_id, "% ")?;
        client.write_addr(&self.buffer_id, "$")?;
        client.ctl("mark-clean", "")?;

        Ok(())
    }

    fn send_input(&mut self, input: &str, client: &mut Client) -> io::Result<Outcome> {
        match input.trim() {
            "clear" => {
                self.clear_buffer(client)?;
                return Ok(Outcome::Handled);
            }

            "exit" => {
                client.ctl("db!", "")?;
                self.child.kill()?;
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
        client.ctl("mark-clean", "")?;

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
                let s = match raw.strip_prefix("% ") {
                    Some(s) => s,
                    None => &raw,
                };
                return self.send_input(s, client);
            }
        }

        Ok(Outcome::Handled)
    }

    fn handle_delete(
        &mut self,
        _src: Source,
        _from: usize,
        _to: usize,
        client: &mut Client,
    ) -> io::Result<Outcome> {
        client.ctl("mark-clean", "")?;

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
        let s = match txt.strip_prefix("% ") {
            Some(s) => s,
            None => &txt,
        };

        client.append_to_body(&self.buffer_id, &format!("\n% {s}\n"))?;
        let outcome = self.send_input(s, client)?;

        Ok(outcome)
    }
}
