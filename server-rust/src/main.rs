use futures_util::{StreamExt, SinkExt};
use std::env;
use std::sync::{Arc, Mutex};
use tokio::net::TcpListener;
use tokio::sync::mpsc::{self, UnboundedSender};
use tokio_tungstenite::accept_async;
use tokio_tungstenite::tungstenite::protocol::Message;

type Clients = Arc<Mutex<Vec<UnboundedSender<String>>>>;

async fn handle_connection(raw_stream: tokio::net::TcpStream, clients: Clients) {
    let ws_stream = accept_async(raw_stream).await.expect("Error during websocket handshake");
    let (mut ws_sender, mut ws_receiver) = ws_stream.split();

    let (tx, mut rx) = mpsc::unbounded_channel::<String>();
    clients.lock().unwrap().push(tx.clone());

    tokio::spawn(async move {
        while let Some(msg) = rx.recv().await {
            if ws_sender.send(Message::Text(msg)).await.is_err() {
                break;
            }
        }
    });

    while let Some(Ok(msg)) = ws_receiver.next().await {
        if let Message::Text(text) = msg {
            let clients_guard = clients.lock().unwrap();
            for client in clients_guard.iter() {
                let _ = client.send(text.clone());
            }
        }
    }

    // Clean up client on disconnect
    let index = clients.lock().unwrap().iter().position(|x| x.same_channel(&tx)).unwrap();
    clients.lock().unwrap().remove(index);
}

async fn send_ready_message(clients: Clients) {
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    println!("All clients connected");

    let clients_guard = clients.lock().unwrap();
    for client in clients_guard.iter() {
        let _ = client.send("ready".to_string());
    }
}

#[tokio::main]
async fn main() {
    let port: u16 = env::var("PORT").unwrap_or_else(|_| "3011".to_string()).parse().unwrap();
    let clients_to_wait_for: usize = env::var("CLIENTS_COUNT").unwrap_or_else(|_| "32".to_string()).parse().unwrap();

    let clients = Arc::new(Mutex::new(Vec::new()));
    let listener = TcpListener::bind(format!("0.0.0.0:{}", port)).await.unwrap();

    println!("Waiting for {} clients to connect..", clients_to_wait_for);

    while let Ok((stream, _)) = listener.accept().await {
        let clients = Arc::clone(&clients);

        tokio::spawn(async move {
            handle_connection(stream, clients.clone()).await;

            if clients.lock().unwrap().len() == clients_to_wait_for {
                send_ready_message(clients.clone()).await;
            }
        });
    }
}
