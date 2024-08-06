using System.Collections.Concurrent;
using System.Net;
using System.Net.WebSockets;
using System.Text;

class WebSocketServer
{
    private static readonly ConcurrentDictionary<WebSocket, (string, SemaphoreSlim)> clients = new();
    private static readonly int Port = 3006;
    private static readonly int ClientsToWaitFor = 100;

    static async Task Main(string[] args)
    {
        HttpListener listener = new();
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
                    _ = HandleWebSocketAsync(context);
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
        WebSocket webSocket;

        try
        {
            var webSocketContext = await context.AcceptWebSocketAsync(null);
            webSocket = webSocketContext.WebSocket;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Failed to accept WebSocket: {ex.Message}");
            return;
        }

        string name = $"Client{clients.Count + 1}";
        SemaphoreSlim semaphore = new(1, 1);
        clients.TryAdd(webSocket, (name, semaphore));

        Console.WriteLine($"{name} connected ({ClientsToWaitFor - clients.Count} remain)");

        if (clients.Count == ClientsToWaitFor)
        {
            SendReadyMessage();
            await Task.Delay(230);
        }

        try
        {
            var buffer = new ArraySegment<byte>(new byte[1024]);

            while (webSocket.State == WebSocketState.Open)
            {
                WebSocketReceiveResult result = await webSocket.ReceiveAsync(buffer, CancellationToken.None).ConfigureAwait(false);

                if (result.MessageType == WebSocketMessageType.Close)
                    break;

                string message = Encoding.UTF8.GetString(buffer.Array, 0, result.Count);
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
                    await webSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "Closed by server", CancellationToken.None).ConfigureAwait(false);
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
        var segment = new ArraySegment<byte>(msgBuffer);

        var clientsCopy = clients.ToArray();

        var tasks = clientsCopy
            .Where(c => c.Key.State == WebSocketState.Open)
            .Select(async client =>
            {
                var semaphore = client.Value.Item2;
                await semaphore.WaitAsync();
                try
                {
                    await client.Key.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None).ConfigureAwait(false);
                }
                catch (WebSocketException ex)
                {
                    Console.WriteLine($"Error sending message: {ex.Message}");
                }
                finally
                {
                    semaphore.Release();
                }
            });

        await Task.WhenAll(tasks);
    }

    private static void SendReadyMessage()
    {
        string readyMessage = "ready";
        byte[] msgBuffer = Encoding.UTF8.GetBytes(readyMessage);
        ArraySegment<byte> segment = new ArraySegment<byte>(msgBuffer);

        Task.Delay(100).ContinueWith(async _ =>
        {
            var clientsCopy = clients.ToArray();

            var tasks = clientsCopy
                .Where(c => c.Key.State == WebSocketState.Open)
                .Select(async client =>
                {
                    var semaphore = client.Value.Item2;
                    await semaphore.WaitAsync();

                    try
                    {
                        await client.Key.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None);
                    }
                    catch (WebSocketException ex)
                    {
                        Console.WriteLine($"Error sending ready message: {ex.Message}");
                    }
                    finally
                    {
                        semaphore.Release();
                    }
                });

            await Task.WhenAll(tasks);
        });
    }
}
