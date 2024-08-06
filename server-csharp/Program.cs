using System;
using System.Collections.Concurrent;
using System.Net;
using System.Net.WebSockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

class WebSocketServer
{
    private static ConcurrentDictionary<WebSocket, string> clients = new ConcurrentDictionary<WebSocket, string>();
    private static readonly int Port = 3006; // Porta onde o servidor vai escutar
    private static readonly int ClientsToWaitFor = 100;

    static async Task Main(string[] args)
    {
        HttpListener listener = new HttpListener();
        listener.Prefixes.Add($"http://*:{Port}/");
        listener.Start();
        Console.WriteLine($"WebSocket server is running on ws://localhost:{Port}");

        while (true)
        {
            try
            {
                var context = await listener.GetContextAsync();

                if (context.Request.IsWebSocketRequest)
                {
                    _ = Task.Run(() => HandleWebSocketAsync(context));
                }
                else
                {
                    context.Response.StatusCode = 400;
                    context.Response.Close();
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Listener error: {ex.Message}");
            }
        }
    }

    private static async Task HandleWebSocketAsync(HttpListenerContext context)
    {
        WebSocketContext webSocketContext;
        WebSocket webSocket;

        try
        {
            webSocketContext = await context.AcceptWebSocketAsync(null);
            webSocket = webSocketContext.WebSocket;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Failed to accept WebSocket: {ex.Message}");
            return;
        }

        string name = $"Client{clients.Count + 1}";
        clients.TryAdd(webSocket, name);
        Console.WriteLine($"{name} connected ({ClientsToWaitFor - clients.Count} remain)");

        if (clients.Count == ClientsToWaitFor)
        {
            SendReadyMessage();
        }

        try
        {
            while (webSocket.State == WebSocketState.Open)
            {
                ArraySegment<byte> buffer = new ArraySegment<byte>(new byte[1024]);
                WebSocketReceiveResult result = await webSocket.ReceiveAsync(buffer, CancellationToken.None);

                if (result.MessageType == WebSocketMessageType.Close)
                {
                    break;
                }

                string message = Encoding.UTF8.GetString(buffer.Array, 0, result.Count);
                //Console.WriteLine($"{name}: {message}");
                await BroadcastMessage(name, message);
            }
        }
        catch (WebSocketException ex)
        {
            Console.WriteLine($"WebSocket error: {ex.Message}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
        finally
        {
            if (webSocket.State == WebSocketState.Open || webSocket.State == WebSocketState.CloseReceived)
            {
                try
                {
                    await webSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "Closed by server", CancellationToken.None);
                }
                catch (WebSocketException ex)
                {
                    Console.WriteLine($"WebSocket close error: {ex.Message}");
                }
            }

            webSocket.Dispose();
            clients.TryRemove(webSocket, out _);
        }
    }

    private static async Task BroadcastMessage(string name, string message)
    {
        string msg = $"{name}: {message}";
        byte[] msgBuffer = Encoding.UTF8.GetBytes(msg);
        ArraySegment<byte> segment = new ArraySegment<byte>(msgBuffer);

        foreach (var client in clients.Keys)
        {
            if (client.State == WebSocketState.Open)
            {
                try
                {
                    await client.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None);
                }
                catch (WebSocketException ex)
                {
                    Console.WriteLine($"Error sending message: {ex.Message}");
                }
            }
        }
    }

    private static void SendReadyMessage()
    {
        Console.WriteLine("All clients connected");
        Task.Delay(100).ContinueWith(async _ =>
        {
            //Console.WriteLine("Starting benchmark");
            string readyMessage = "ready";
            byte[] msgBuffer = Encoding.UTF8.GetBytes(readyMessage);
            ArraySegment<byte> segment = new ArraySegment<byte>(msgBuffer);

            foreach (var client in clients.Keys)
            {
                if (client.State == WebSocketState.Open)
                {
                    try
                    {
                        await client.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None);
                    }
                    catch (WebSocketException ex)
                    {
                        Console.WriteLine($"Error sending ready message: {ex.Message}");
                    }
                }
            }
        });
    }
}
