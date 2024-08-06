#include "crow.h"
#include <tbb/concurrent_unordered_set.h>
#include <iostream>
#include <string>
#include <shared_mutex>

const int CLIENTS_TO_WAIT_FOR = 100;
tbb::concurrent_unordered_set<crow::websocket::connection*> clients;

void sendReadyMessage() {
    std::cout << "Todos os clientes conectados" << std::endl;
    for (auto& client : clients) {
        client->send_text("ready");
    }
}

int main() {
    crow::SimpleApp app;

    CROW_ROUTE(app, "/")
        .websocket()
        .onopen([](crow::websocket::connection& conn) {
        clients.insert(&conn);
        std::string name = "Client" + std::to_string(clients.size());
        std::cout << name << " conectado (" << CLIENTS_TO_WAIT_FOR - clients.size() << " restantes)" << std::endl;

        if (clients.size() == CLIENTS_TO_WAIT_FOR) {
            sendReadyMessage();
        }
            })
        .onclose([](crow::websocket::connection& conn, const std::string& reason) {
                auto it = clients.find(&conn);
                if (it != clients.end()) {
                    clients.unsafe_erase(it); // Usando unsafe_erase para remover o elemento
                }
                std::cout << "Conexão WebSocket fechada: " << reason << std::endl;
            })
                .onmessage([](crow::websocket::connection& conn, const std::string& data, bool is_binary) {
                for (auto& client : clients) {
                    if (client != &conn) {
                        client->send_text(data);
                    }
                }
                    });

            app.port(3010).run();
}
