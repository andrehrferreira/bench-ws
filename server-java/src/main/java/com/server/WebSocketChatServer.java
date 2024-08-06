package com.server;

import org.java_websocket.server.WebSocketServer;
import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;

import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class WebSocketChatServer extends WebSocketServer {
    private static final int PORT = 3012;
    private static final int CLIENTS_TO_WAIT_FOR = 100;

    private final Set<WebSocket> clients = Collections.synchronizedSet(new HashSet<>());

    public WebSocketChatServer() {
        super(new InetSocketAddress(PORT));
    }

    @Override
    public void onOpen(WebSocket conn, ClientHandshake handshake) {
        clients.add(conn);
        String name = conn.getResourceDescriptor();
        System.out.println(name + " connected (" + (CLIENTS_TO_WAIT_FOR - clients.size()) + " remain)");

        if (clients.size() == CLIENTS_TO_WAIT_FOR) {
            sendReadyMessage();
        }
    }

    @Override
    public void onClose(WebSocket conn, int code, String reason, boolean remote) {
        clients.remove(conn);
        System.out.println("Connection closed: " + reason);
    }

    @Override
    public void onMessage(WebSocket conn, String message) {
        String name = conn.getResourceDescriptor();
        String msg = name + ": " + message;
        synchronized (clients) {
            for (WebSocket client : clients) {
                if (client != conn) {
                    client.send(msg);
                }
            }
        }
    }

    @Override
    public void onError(WebSocket conn, Exception ex) {
        System.err.println("Error: " + ex.getMessage());
    }

    @Override
    public void onStart() {
        System.out.println("Server started on port " + PORT);
    }

    private void sendReadyMessage() {
        System.out.println("All clients connected");
        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        System.out.println("Starting benchmark");
        synchronized (clients) {
            for (WebSocket client : clients) {
                client.send("ready");
            }
        }
    }
}
