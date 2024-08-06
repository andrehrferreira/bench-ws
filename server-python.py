import asyncio
import websockets

PORT = 3007
CLIENTS_TO_WAIT_FOR = 100
clients = set()

async def handle_connection(websocket, path):
    name = f"Client{len(clients) + 1}"
    clients.add(websocket)
    print(f"{name} connected ({CLIENTS_TO_WAIT_FOR - len(clients)} remain)")

    if len(clients) == CLIENTS_TO_WAIT_FOR:
        await send_ready_message()

    try:
        async for message in websocket:
            #print(f"Received message: {message}")
            await broadcast_message(message, websocket)
    finally:
        clients.remove(websocket)
        print(f"{name} disconnected")

async def broadcast_message(message, sender):
    msg = f"Message from server: {message}"
    for client in clients:
        if client != sender:
            await client.send(msg)

async def send_ready_message():
    print("All clients connected")
    await asyncio.sleep(0.1)
    print("Starting benchmark")
    for client in clients:
        await client.send("ready")

start_server = websockets.serve(handle_connection, "localhost", PORT)

print(f"Waiting for {CLIENTS_TO_WAIT_FOR} clients to connect...")
asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()
