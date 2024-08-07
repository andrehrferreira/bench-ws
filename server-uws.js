const cluster = require('cluster');
const numCPUs = require('os').cpus().length;
const uWS = require('uWebSockets.js');

const port = 3002;
const CLIENTS_TO_WAIT_FOR = 32;

if (cluster.isMaster) {
  const clients = new Map();

  // Cria um worker para cada CPU
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`Worker ${worker.process.pid} died. Forking a new worker.`);
    cluster.fork();
  });

  cluster.on('message', (worker, message) => {
    switch (message.type) {
      case 'register':
        clients.set(message.clientId, worker);
        break;
      case 'unregister':
        clients.delete(message.clientId);
        break;
      case 'broadcast':
        for (const [clientId, clientWorker] of clients.entries()) {
          if (clientId !== message.clientId) {
            clientWorker.send({ type: 'message', msg: message.msg });
          }
        }
        break;
    }
  });
} else {
  const clients = new Set();

  const app = uWS.App().ws('/*', {
    open: (ws) => {
      const clientId = `${process.pid}-${clients.size + 1}`;
      ws.clientId = clientId;
      clients.add(ws);
      process.send({ type: 'register', clientId });
      console.log(`Client ${clientId} connected (${CLIENTS_TO_WAIT_FOR - clients.size} remain)`);

      if (clients.size === CLIENTS_TO_WAIT_FOR) {
        sendReadyMessage();
      }
    },
    message: (ws, message, isBinary) => {
      const msg = `Message from client ${ws.clientId}: ${Buffer.from(message).toString()}`;
      process.send({ type: 'broadcast', clientId: ws.clientId, msg });
    },
    close: (ws) => {
      clients.delete(ws);
      process.send({ type: 'unregister', clientId: ws.clientId });
      console.log(`Client ${ws.clientId} disconnected, ${CLIENTS_TO_WAIT_FOR - clients.size} remain`);
    }
  }).listen(port, (token) => {
    if (token) {
      console.log(`Worker ${process.pid} is running on ws://localhost:${port}`);
    } else {
      console.log('Failed to listen to port ' + port);
    }
  });

  process.on('message', (message) => {
    if (message.type === 'message') {
      for (const client of clients) {
        if (client.readyState === uWS.OPEN) {
          client.send(message.msg);
        }
      }
    }
  });

  function sendReadyMessage() {
    console.log("All clients connected");
    setTimeout(() => {
      console.log("Starting benchmark");
      for (const client of clients) {
        client.send('ready');
      }
    }, 100);
  }
}
