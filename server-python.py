from gevent import monkey
monkey.patch_all()

from gevent.pywsgi import WSGIServer
from geventwebsocket.handler import WebSocketHandler
from geventwebsocket import WebSocketError

PORT = 3007
CLIENTS_TO_WAIT_FOR = 100
clients = set()

def handle_connection(environ, start_response):
    """Gerencia a conexão WebSocket."""
    if environ.get("HTTP_UPGRADE") == "websocket":
        ws = environ["wsgi.websocket"]
        name = f"Client{len(clients) + 1}"
        clients.add(ws)
        print(f"{name} connected ({CLIENTS_TO_WAIT_FOR - len(clients)} remain)")

        if len(clients) == CLIENTS_TO_WAIT_FOR:
            send_ready_message()

        try:
            while True:
                message = ws.receive()
                if message is None or message.strip() == "":
                    break
                handle_message(message, ws)
        except WebSocketError:
            print(f"Error with {name}, closing connection.")
        finally:
            handle_disconnection(ws, name)
    else:
        start_response("404 Not Found", [])
        return []

def handle_message(message, sender):
    """Processa mensagens recebidas de clientes."""
    broadcast_message(message, sender)

def broadcast_message(message, sender):
    """Envia mensagens a todos os clientes, exceto o remetente."""
    msg = f"Message from server: {message}"
    for client in clients:
        if client != sender:
            try:
                client.send(msg)
            except WebSocketError:
                handle_disconnection(client)

def send_ready_message():
    """Envia uma mensagem de 'ready' para todos os clientes."""
    print("All clients connected")
    for client in clients:
        try:
            client.send("ready")
        except WebSocketError:
            handle_disconnection(client)

def handle_disconnection(ws, name):
    """Gerencia a desconexão de um cliente."""
    clients.discard(ws)  # Remove o cliente de forma segura
    print(f"{name} disconnected")

http_server = WSGIServer(('0.0.0.0', PORT), handle_connection, handler_class=WebSocketHandler)
print(f"Waiting for {CLIENTS_TO_WAIT_FOR} clients to connect...")
http_server.serve_forever()