defmodule ServerElixir.Clients do
  use GenServer

  @name __MODULE__
  @clients_to_wait_for 32

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{}, name: @name)
  end

  def init(state) do
    {:ok, state}
  end

  def add(pid) do
    GenServer.call(@name, {:add, pid})
  end

  def remove(pid) do
    GenServer.call(@name, {:remove, pid})
  end

  def size do
    GenServer.call(@name, :size)
  end

  def send_ready_message do
    GenServer.cast(@name, :send_ready_message)
  end

  def broadcast(sender, message) do
    GenServer.cast(@name, {:broadcast, sender, message})
  end

  def handle_call({:add, pid}, _from, state) do
    new_state = Map.put(state, pid, true)
    IO.puts("Client#{map_size(new_state)} connected (#{@clients_to_wait_for - map_size(new_state)} remain)")
    {:reply, :ok, new_state}
  end

  def handle_call({:remove, pid}, _from, state) do
    new_state = Map.delete(state, pid)
    {:reply, :ok, new_state}
  end

  def handle_call(:size, _from, state) do
    {:reply, map_size(state), state}
  end

  def handle_cast(:send_ready_message, state) do
    IO.puts("All clients connected")
    Task.start(fn ->
      Process.sleep(100)
      Enum.each(state, fn {pid, _} ->
        send(pid, :send_ready)
      end)
    end)
    {:noreply, state}
  end

  def handle_cast({:broadcast, sender, message}, state) do
    Enum.each(state, fn {pid, _} ->
      unless pid == sender do
        send(pid, {:send_message, "Message from server: #{message}"})
      end
    end)
    {:noreply, state}
  end
end
