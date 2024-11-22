use ad_lsp::client::{LspClient, LspMessage};
use std::{
    io,
    sync::mpsc::channel,
    thread::{sleep, spawn},
    time::Duration,
};

fn main() -> io::Result<()> {
    let (tx, rx) = channel::<LspMessage>();

    spawn(move || {
        for msg in rx.into_iter() {
            println!("got msg: {}", serde_json::to_string(&msg.msg).unwrap());
        }
    });

    let mut client = LspClient::new(0, "rust-analyzer", tx)?;

    println!(">> initializing");
    client.initialize(&["/home/sminez/repos/personal/ad"])?;
    client.ack_initialized()?;

    sleep(Duration::from_secs(5));

    println!(">> requesting goto definition result");
    client.goto_definition("src/main.rs", 55, 19)?;

    sleep(Duration::from_secs(1));

    println!(">> shutting down");
    client.shutdown_and_exit()?;

    sleep(Duration::from_secs(5));

    Ok(())
}
