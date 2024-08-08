import asyncio
from aiohttp import web

CLIENTS_TO_WAIT_FOR = 32
clients = set()

async def websocket_handler(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)

    name = f"Client{len(clients) + 1}"
    clients.add(ws)
    print(f"{name} connected ({CLIENTS_TO_WAIT_FOR - len(clients)} remain)")

    if len(clients) == CLIENTS_TO_WAIT_FOR:
        await send_ready_message()

    try:
        async for msg in ws:
            if msg.type == web.WSMsgType.TEXT:
                await broadcast_message(msg.data, ws)
    finally:
        clients.remove(ws)
        print(f"{name} disconnected")

    return ws

async def broadcast_message(message, sender):
    msg = f"Message from server: {message}"
    for client in clients:
        if client != sender:
            await client.send_str(msg)

async def send_ready_message():
    print("All clients connected")
    await asyncio.sleep(0.1)
    print("Starting benchmark")
    for client in clients:
        await client.send_str("ready")

app = web.Application()
app.add_routes([web.get('/ws', websocket_handler)])

if __name__ == "__main__":
    print(f"Waiting for {CLIENTS_TO_WAIT_FOR} clients to connect...")
    web.run_app(app, port=3007)
