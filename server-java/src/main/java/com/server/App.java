package com.server;

public class App {
    public static void main(String[] args) {
        WebSocketChatServer server = new WebSocketChatServer();
        server.start();
        System.out.println("Waiting for 100 clients to connect...");

        // Para manter o servidor rodando, caso contrário, o programa será encerrado.
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            System.out.println("Stopping server...");
            try {
                server.stop();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }));
    }
}
