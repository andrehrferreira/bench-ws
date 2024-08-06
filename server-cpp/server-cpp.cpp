#include <websocketpp/config/asio_no_tls.hpp>
#include <websocketpp/server.hpp>

#include <iostream>

typedef websocketpp::server<websocketpp::config::asio> server;

// Define a custom message handler
void on_message(websocketpp::connection_hdl hdl, server::message_ptr msg) {
    std::cout << "Received message: " << msg->get_payload() << std::endl;
    // Echo the received message back to the client
    msg->get_connection()->send(msg->get_payload(), msg->get_opcode());
}

int main() {
    server ws_server;

    try {
        // Set logging settings
        ws_server.set_access_channels(websocketpp::log::alevel::all);
        ws_server.clear_access_channels(websocketpp::log::alevel::frame_payload);

        // Initialize Asio
        ws_server.init_asio();

        // Register our message handler
        ws_server.set_message_handler(&on_message);

        // Listen on port 3000
        ws_server.listen(3000);

        // Start the server accept loop
        ws_server.start_accept();

        std::cout << "Server started on ws://localhost:3000" << std::endl;

        // Start the ASIO io_service run loop
        ws_server.run();
    }
    catch (websocketpp::exception const& e) {
        std::cerr << "WebSocket++ exception: " << e.what() << std::endl;
    }
    catch (std::exception const& e) {
        std::cerr << "STD exception: " << e.what() << std::endl;
    }
    catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return 0;
}
