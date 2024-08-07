defmodule ServerElixir.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  match "/" do
    IO.puts("Received WebSocket connection request at root")
    conn
    |> fetch_query_params()
    |> upgrade_adapter(:websocket, {ServerElixir.WebSocketHandler, %{}, %{timeout: 60_000}})
  end

  match _ do
    send_resp(conn, 404, "Not Found")
  end
end
