import { serve } from "bun";

const port = 3004;
const CLIENTS_TO_WAIT_FOR = 100;
const clients = new Set();

const server = serve({
  fetch(req, server) {
    if (server.upgrade(req)) {
      return;
    }
    return new Response("Upgrade failed", { status: 426 });
  },
  websocket: {
    open(ws) {
      const name = `Client${clients.size + 1}`;
      clients.add(ws);
      console.log(`${name} connected (${CLIENTS_TO_WAIT_FOR - clients.size} remain)`);

      if (clients.size === CLIENTS_TO_WAIT_FOR) {
        sendReadyMessage();
      }
    },
    message(ws, message) {
      const msg = `Message from server: ${message}`;
      for (const client of clients) {
        if (client !== ws) {
          client.send(msg);
        }
      }
    },
    close(ws) {
      clients.delete(ws);
    }
  },
  port
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
