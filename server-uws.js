const uWS = require('uWebSockets.js');

const port = 3002;
const CLIENTS_TO_WAIT_FOR = 100;
const clients = new Set();

const app = uWS.App().ws('/*', {
  open: (ws) => {
    const name = `Client${clients.size + 1}`;
    clients.add(ws);
    console.log(`${name} connected (${CLIENTS_TO_WAIT_FOR - clients.size} remain)`);

    if (clients.size === CLIENTS_TO_WAIT_FOR) {
      sendReadyMessage();
    }
  },
  message: (ws, message, isBinary) => {
    const msg = `Message from client: ${Buffer.from(message).toString()}`;
    for (const client of clients) {
      if (client !== ws) {
        client.send(msg, isBinary);
      }
    }
  },
  close: (ws) => {
    clients.delete(ws);
  }
}).listen(port, (token) => {
  if (token) {
    console.log(`WebSocket server is running on ws://localhost:${port}`);
  } else {
    console.log('Failed to listen to port ' + port);
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
