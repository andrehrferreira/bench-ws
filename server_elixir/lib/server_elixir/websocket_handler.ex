defmodule ServerElixir.WebSocketHandler do
  @behaviour :cowboy_websocket

  def init(req, state) do
    {:cowboy_websocket, req, state}
  end

  def websocket_init(state) do
    ServerElixir.Clients.add(self())
    clients_size = ServerElixir.Clients.size()
    IO.puts("WebSocket connection established, #{clients_size} clients connected")

    if clients_size == 32 do
      ServerElixir.Clients.send_ready_message()
    end

    {:ok, state}
  end

  def websocket_handle({:text, message}, state) do
    ServerElixir.Clients.broadcast(self(), message)
    {:ok, state}
  end

  def websocket_info(:send_ready, state) do
    IO.puts("Sending ready message to clients")
    {:reply, {:text, "ready"}, state}
  end

  def websocket_info({:send_message, message}, state) do
    {:reply, {:text, message}, state}
  end

  def terminate(_reason, _req, _state) do
    ServerElixir.Clients.remove(self())
    IO.puts("WebSocket connection terminated")
    :ok
  end
end
