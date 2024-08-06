#include <websocketpp/config/asio_no_tls.hpp>
#include <websocketpp/server.hpp>
#include <iostream>
#include <unordered_set>
#include <mutex>
#include <thread>

typedef websocketpp::server<websocketpp::config::asio> server;
typedef websocketpp::connection_hdl connection_hdl;

// A função hash para usar connection_hdl em um unordered_set
struct connection_hdl_hash {
    std::size_t operator()(const connection_hdl& hdl) const {
        return std::hash<std::uintptr_t>()(
            reinterpret_cast<std::uintptr_t>(hdl.lock().get()));
    }
};

// Comparador para connection_hdl
struct connection_hdl_equal {
    bool operator()(const connection_hdl& lhs, const connection_hdl& rhs) const {
        return !std::owner_less<connection_hdl>()(lhs, rhs) &&
            !std::owner_less<connection_hdl>()(rhs, lhs);
    }
};

// Um conjunto de conexões usando connection_hdl com hash e comparador personalizados
typedef std::unordered_set<connection_hdl, connection_hdl_hash, connection_hdl_equal> con_list;
con_list clients;
std::mutex clients_mutex;

const int CLIENTS_TO_WAIT_FOR = 100;

void send_ready_message(server* s) {
    con_list local_clients;
    {
        std::lock_guard<std::mutex> lock(clients_mutex);
        local_clients = clients; // Copiar clientes para local_clients
    }

    std::cout << "All clients connected" << std::endl;
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    std::cout << "Starting benchmark" << std::endl;

    for (auto& hdl : local_clients) {
        try {
            s->send(hdl, "ready", websocketpp::frame::opcode::text);
        }
        catch (const websocketpp::exception& e) {
            std::cerr << "Send error: " << e.what() << std::endl;
        }
    }
}

void on_open(server* s, connection_hdl hdl) {
    {
        std::lock_guard<std::mutex> lock(clients_mutex);
        clients.insert(hdl);
        std::cout << "Client connected (" << CLIENTS_TO_WAIT_FOR - clients.size() << " remain)" << std::endl;
    }

    if (clients.size() == CLIENTS_TO_WAIT_FOR) {
        send_ready_message(s);
    }
}

void on_message(server* s, connection_hdl hdl, server::message_ptr msg) {
    std::string message = "Message from server: " + msg->get_payload();
    con_list local_clients;

    {
        std::lock_guard<std::mutex> lock(clients_mutex);
        local_clients = clients; // Copiar clientes para local_clients
    }

    for (auto& client : local_clients) {
        if (connection_hdl_equal()(hdl, client)) {
            continue; // Skip the sender
        }
        try {
            s->send(client, message, websocketpp::frame::opcode::text);
        }
        catch (const websocketpp::exception& e) {
            std::cerr << "Send error: " << e.what() << std::endl;
        }
    }
}

void on_close(connection_hdl hdl) {
    std::lock_guard<std::mutex> lock(clients_mutex);
    clients.erase(hdl);
    std::cout << "Client disconnected" << std::endl;
}

int main() {
    server ws_server;

    try {
        ws_server.set_access_channels(websocketpp::log::alevel::all);
        ws_server.clear_access_channels(websocketpp::log::alevel::frame_payload);

        ws_server.init_asio();

        ws_server.set_open_handler(std::bind(&on_open, &ws_server, std::placeholders::_1));
        ws_server.set_message_handler(std::bind(&on_message, &ws_server, std::placeholders::_1, std::placeholders::_2));
        ws_server.set_close_handler(std::bind(&on_close, std::placeholders::_1));

        ws_server.listen(3010);
        ws_server.start_accept();

        std::cout << "Waiting for " << CLIENTS_TO_WAIT_FOR << " clients to connect.." << std::endl;

        ws_server.run();
    }
    catch (websocketpp::exception const& e) {
        std::cerr << "WebSocket++ exception: " << e.what() << std::endl;
    }
    catch (std::system_error const& e) {
        std::cerr << "System error: " << e.what() << std::endl;
    }
    catch (std::exception const& e) {
        std::cerr << "STD exception: " << e.what() << std::endl;
    }
    catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return 0;
}
