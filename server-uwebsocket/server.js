const uWS = require('uWebSockets.js');

const port = 3002;
const CLIENTS_TO_WAIT_FOR = 32;

const clients = new Set();

const app = uWS.App().ws('/*', {
  open: (ws) => {
    const clientId = `${clients.size + 1}`;
    ws.clientId = clientId;
    clients.add(ws);
    console.log(`Client ${clientId} connected (${CLIENTS_TO_WAIT_FOR - clients.size} remain)`);

    if (clients.size === CLIENTS_TO_WAIT_FOR) {
      sendReadyMessage();
    }
  },
  message: (ws, message, isBinary) => {
    const msg = `Message from client ${ws.clientId}: ${Buffer.from(message).toString()}`;
    broadcastMessage(ws.clientId, msg);
  },
  close: (ws) => {
    clients.delete(ws);
    console.log(`Client ${ws.clientId} disconnected, ${CLIENTS_TO_WAIT_FOR - clients.size} remain`);
  }
}).listen(port, (token) => {
  if (token) {
    console.log(`Server is running on ws://localhost:${port}`);
  } else {
    console.log('Failed to listen to port ' + port);
  }
});

function broadcastMessage(senderId, message) {
  for (const client of clients) {
    if (client.clientId !== senderId && client.readyState === uWS.OPEN) {
      client.send(message);
    }
  }
}

function sendReadyMessage() {
  console.log("All clients connected");
  setTimeout(() => {
    console.log("Starting benchmark");
    for (const client of clients) {
      client.send('ready');
    }
  }, 100);
}
