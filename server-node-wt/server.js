const { Worker, isMainThread, MessageChannel, parentPort, workerData } = require('worker_threads');
const WebSocket = require('ws');

const port = 3014;
const CLIENTS_TO_WAIT_FOR = 32;
const NUM_WORKERS = 4; // Número de threads (workers) que você deseja criar
const clients = new Set();

if (isMainThread) {
  const workers = [];

  // Cria os workers
  for (let i = 0; i < NUM_WORKERS; i++) {
    const { port1, port2 } = new MessageChannel();
    const worker = new Worker(__filename, { workerData: { port: port2 }, transferList: [port2] });
    workers.push(worker);

    worker.on('error', (err) => {
      console.error(`Worker ${i} error:`, err);
    });

    worker.on('exit', (code) => {
      if (code !== 0) {
        console.error(`Worker ${i} stopped with exit code ${code}`);
      }
    });

    port1.on('message', (msg) => {
      for (const client of clients) {
        if (client.readyState === WebSocket.OPEN) {
          client.send(msg);
        }
      }
    });
  }

  const server = new WebSocket.Server({ port });

  server.on('connection', (socket) => {
    const name = `Client${clients.size + 1}`;
    clients.add(socket);

    console.log(`${name} connected (${CLIENTS_TO_WAIT_FOR - clients.size} remain)`);

    socket.on('message', (message) => {
      const msg = `${name}: ${message}`;
      // Envia a mensagem para um worker disponível
      const worker = workers[Math.floor(Math.random() * workers.length)];
      worker.postMessage({ msg });
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

  console.log(`Waiting for ${CLIENTS_TO_WAIT_FOR} clients to connect on port ${port}...`);
} else {
  const port = workerData.port;

  parentPort.on('message', (message) => {
    port.postMessage(message.msg);
  });
}
