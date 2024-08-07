import asyncio
import websockets
from concurrent.futures import ThreadPoolExecutor

PORT = 3007
CLIENTS_TO_WAIT_FOR = 32
clients = set()

executor = ThreadPoolExecutor(max_workers=4)

async def handle_connection(websocket, path):
    name = f"Client{len(clients) + 1}"
    clients.add(websocket)
    print(f"{name} connected ({CLIENTS_TO_WAIT_FOR - len(clients)} remain)")

    if len(clients) == CLIENTS_TO_WAIT_FOR:
        await send_ready_message()

    try:
        async for message in websocket:
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

async def start_server():
    server = await websockets.serve(handle_connection, "localhost", PORT)
    print(f"Waiting for {CLIENTS_TO_WAIT_FOR} clients to connect...")
    await server.wait_closed()

loop = asyncio.get_event_loop()
loop.set_default_executor(executor)
loop.run_until_complete(start_server())
loop.run_forever()