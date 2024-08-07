defmodule ServerElixir do
  @moduledoc """
  Documentation for ServerElixir.
  """

  use Application

  def start(_type, _args) do
    IO.puts("Starting ServerElixir application...")

    children = [
      ServerElixir.Clients,
      {Plug.Cowboy, scheme: :http, plug: ServerElixir.Router, options: [port: 3009]}
    ]

    opts = [strategy: :one_for_one, name: ServerElixir.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
