use futures_util::{StreamExt, SinkExt};
use std::env;
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio::sync::mpsc;
use tokio_tungstenite::accept_async;
use tokio_tungstenite::tungstenite::protocol::Message;
use dashmap::DashMap;

type Clients = Arc<DashMap<usize, mpsc::UnboundedSender<String>>>;

async fn handle_connection(raw_stream: tokio::net::TcpStream, clients: Clients, client_id: usize) {
    let ws_stream = accept_async(raw_stream).await.expect("Error during websocket handshake");
    let (mut ws_sender, mut ws_receiver) = ws_stream.split();

    let (tx, mut rx) = mpsc::unbounded_channel::<String>();
    clients.insert(client_id, tx.clone());

    tokio::spawn(async move {
        while let Some(msg) = rx.recv().await {
            if ws_sender.send(Message::Text(msg)).await.is_err() {
                break;
            }
        }
    });

    while let Some(Ok(msg)) = ws_receiver.next().await {
        if let Message::Text(text) = msg {
            clients.iter().for_each(|client| {
                let _ = client.value().send(text.clone());
            });
        }
    }

    clients.remove(&client_id);
}

async fn send_ready_message(clients: Clients) {
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    println!("All clients connected");

    clients.iter().for_each(|client| {
        let _ = client.value().send("ready".to_string());
    });
}

#[tokio::main]
async fn main() {
    let port: u16 = env::var("PORT").unwrap_or_else(|_| "3011".to_string()).parse().unwrap();
    let clients_to_wait_for: usize = env::var("CLIENTS_COUNT").unwrap_or_else(|_| "32".to_string()).parse().unwrap();

    let clients: Clients = Arc::new(DashMap::new());
    let listener = TcpListener::bind(format!("0.0.0.0:{}", port)).await.unwrap();

    println!("Waiting for {} clients to connect..", clients_to_wait_for);

    let mut client_id_counter = 0;

    while let Ok((stream, _)) = listener.accept().await {
        let clients = Arc::clone(&clients);
        let client_id = client_id_counter;
        client_id_counter += 1;

        tokio::spawn(async move {
            handle_connection(stream, clients.clone(), client_id).await;

            if clients.len() == clients_to_wait_for {
                send_ready_message(clients.clone()).await;
            }
        });
    }
}
