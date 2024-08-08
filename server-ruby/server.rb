require 'eventmachine'
require 'em-websocket'
require 'json'
require 'set'

CLIENTS = Set.new
CLIENTS_TO_WAIT_FOR = (ENV['CLIENTS_COUNT'] || '32').to_i
PORT = (ENV['PORT'] || '3015').to_i
BUFFER_SIZE = 1000
FLUSH_INTERVAL = 0.1 # segundos

class BroadcastBuffer
  def initialize
    @buffer = []
    @mutex = Mutex.new
  end

  def <<(message)
    should_flush = false
    @mutex.synchronize do
      @buffer << message
      should_flush = @buffer.size >= BUFFER_SIZE
    end
    flush if should_flush
  end

  def flush
    messages = nil
    @mutex.synchronize do
      messages = @buffer.dup
      @buffer.clear
    end
    return if messages.empty?
    EM.defer do
      batch = messages.join("\n")
      CLIENTS.each { |client| client.send(batch) }
    end
  end
end

$broadcast_buffer = BroadcastBuffer.new

def send_ready_message
  puts "All clients connected"
  EM.add_timer(0.1) do
    CLIENTS.each { |client| client.send("ready") }
  end
end

EM.run do
  puts "Waiting for #{CLIENTS_TO_WAIT_FOR} clients to connect.."

  EM::WebSocket.start(host: '0.0.0.0', port: PORT) do |ws|
    ws.onopen do
      CLIENTS << ws
      puts "Client connected. Total clients: #{CLIENTS.size}"

      if CLIENTS.size == CLIENTS_TO_WAIT_FOR
        send_ready_message
      end
    end

    ws.onmessage do |msg|
      $broadcast_buffer << msg
    end

    ws.onclose do
      CLIENTS.delete(ws)
      puts "Client disconnected. Total clients: #{CLIENTS.size}"
    end
  end

  EM.add_periodic_timer(FLUSH_INTERVAL) do
    $broadcast_buffer.flush
  end
end
