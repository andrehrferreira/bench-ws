import asyncio
import websockets

port = 3007
CLIENTS_TO_WAIT_FOR = 32
clients = set()

async def handle_connection(websocket, path):
    # Usar um identificador simples para cada cliente
    name = f"Client{len(clients) + 1}"
    clients.add(websocket)
    
    print(f"{name} connected ({CLIENTS_TO_WAIT_FOR - len(clients)} remain)")

    try:
        async for message in websocket:
            msg = f"{name}: {message}"
            await broadcast_message(msg, websocket)
    except websockets.exceptions.ConnectionClosed:
        pass
    finally:
        clients.remove(websocket)

    if len(clients) == CLIENTS_TO_WAIT_FOR:
        await send_ready_message()

async def broadcast_message(message, sender_socket):
    tasks = []
    for client in clients:
        if client != sender_socket and client.open:
            tasks.append(client.send(message))
    if tasks:
        await asyncio.gather(*tasks)

async def send_ready_message():
    print("All clients connected")
    #await asyncio.sleep(0.1)
    print("Starting benchmark")
    tasks = [client.send('ready') for client in clients if client.open]
    if tasks:
        await asyncio.gather(*tasks)

async def main():
    print(f"Waiting for {CLIENTS_TO_WAIT_FOR} clients to connect..")
    async with websockets.serve(handle_connection, 'localhost', port):
        await asyncio.Future()  # Run forever

asyncio.run(main())
