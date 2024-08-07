const WebSocket = require('ws');

const port = 3001;
const CLIENTS_TO_WAIT_FOR = 32;
const clients = new Set();

const server = new WebSocket.Server({ port });

server.on('connection', (socket) => {
  // Usar um identificador simples para cada cliente
  const name = `Client${clients.size + 1}`;
  clients.add(socket);

  console.log(`${name} connected (${CLIENTS_TO_WAIT_FOR - clients.size} remain)`);

  socket.on('message', (message) => {
    const msg = `${name}: ${message}`;
    for (const client of clients) {
      if (client !== socket && client.readyState === WebSocket.OPEN) {
        client.send(msg);
      }
    }
  });

  socket.on('close', () => {
    clients.delete(socket);
  });

  if (clients.size === CLIENTS_TO_WAIT_FOR) {
    sendReadyMessage();
  }
});

function sendReadyMessage() {
  console.log("All clients connected");
  setTimeout(() => {
    console.log("Starting benchmark");
    for (const client of clients) {
      client.send(`ready`);
    }
  }, 100);
}

console.log(`Waiting for ${CLIENTS_TO_WAIT_FOR} clients to connect..`);
