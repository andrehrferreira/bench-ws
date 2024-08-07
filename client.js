const env = typeof process !== 'undefined' ? process.env : (typeof Deno !== 'undefined' ? Deno.env.toObject() : {});

const SERVERS = [  
  { name: "Node", url: "ws://0.0.0.0:3001" },
  { name: "uWebsocket.js", url: "ws://0.0.0.0:3002" },  
  { name: "Bun", url: "ws://0.0.0.0:3004" },
  { name: "Go", url: "ws://0.0.0.0:3005" },
  { name: "C#", url: "ws://0.0.0.0:3006" },
  { name: "Phyton3", url: "ws://0.0.0.0:3007" },  
  { name: "Erlang / Elixir", url: "ws://0.0.0.0:3009" },
  { name: "C++ (Crow + TBB)", url: "ws://0.0.0.0:3010" },
  { name: "Rust", url: "ws://0.0.0.0:3011" },
  { name: "Java", url: "ws://0.0.0.0:3012" },
  { name: "PHP / Swoole", url: "ws://0.0.0.0:3013" },
  //{ name: "Node (Worker Thread)", url: "ws://0.0.0.0:3014" },
  //{ name: "Zig", url: "ws://0.0.0.0:3008" }, // In implementation
];

//{ name: "Deno", url: "ws://0.0.0.0:3003" },

const WebSocket = typeof globalThis.WebSocket !== 'undefined' ? globalThis.WebSocket : (await import("bun")).WebSocket;
const LOG_MESSAGES = env.LOG_MESSAGES === "1";
const CLIENTS_TO_WAIT_FOR = 100;
const DELAY = 64;
const WAIT_TIME_BETWEEN_TESTS = 5000;
const MESSAGES_TO_SEND = [
  "Hello World!",
  "Hello World! 1",
  "Hello World! 2",
  "Hello World! 3",
  "Hello World! 4",
  "Hello World! 5",
  "Hello World! 6",
  "Hello World! 7",
  "Hello World! 8",
  "Hello World! 9",
  "What is the meaning of life?",
  "where is the bathroom?",
  "zoo",
  "kangaroo",
  "erlang",
  "elixir",
  "bun",
  "mochi",
  "typescript",
  "javascript"
];

const NAMES = Array.from({ length: 100 }, (a, i) => [
  "Alice" + i,
  "Bob" + i,
  "Charlie" + i,
  "David" + i,
  "Eve" + i,
  "Frank" + i,
  "Grace" + i,
  "Heidi" + i,
  "Ivan" + i,
  "Judy" + i,
  "Karl" + i,
  "Linda" + i,
  "Mike" + i,
  "Nancy" + i,
  "Oscar" + i,
  "Peggy" + i,
  "Quentin" + i,
  "Ruth" + i,
  "Steve" + i,
  "Trudy" + i,
  "Ursula" + i,
  "Victor" + i,
  "Wendy" + i,
  "Xavier" + i,
  "Yvonne" + i,
  "Zach" + i,
]).flat().slice(0, CLIENTS_TO_WAIT_FOR);

const results = [];

async function testServer(server) {
  console.log(`Connecting to ${server.name} at ${server.url}`);
  console.time(`All clients connected to ${server.name}`);
  const promises = [];
  let received = 0;
  let lostPackets = 0;
  let more = false;

  const clients = new Array(CLIENTS_TO_WAIT_FOR);
  for (let i = 0; i < CLIENTS_TO_WAIT_FOR; i++) {
    clients[i] = new WebSocket(`${server.url}?name=${NAMES[i]}`);
    promises.push(
      new Promise((resolve, reject) => {
        clients[i].onopen = () => {
          resolve();
        };
        clients[i].onerror = (err) => {
          reject(err);
        };
      })
    );
  }

  await Promise.all(promises);
  console.timeEnd(`All clients connected to ${server.name}`);

  for (let i = 0; i < CLIENTS_TO_WAIT_FOR; i++) {
    clients[i].onmessage = (event) => {
      if (LOG_MESSAGES) console.log(event.data);
      received++;
    };
  }

  function sendMessagesContinuously() {
    for (let i = 0; i < CLIENTS_TO_WAIT_FOR; i++) {
      for (let j = 0; j < MESSAGES_TO_SEND.length; j++) {
        clients[i].send(MESSAGES_TO_SEND[j]);
      }
    }
  }

  const runs = [];
  await new Promise((resolve) => {
    const interval = setInterval(() => {
      const last = received;
      if (last === 0) {
        lostPackets++;
      } else if (last > 0) {
        runs.push(last);
      }
      received = 0;
      console.log(
        `${server.name}: ${last} messages per second (${CLIENTS_TO_WAIT_FOR} clients x ${MESSAGES_TO_SEND.length} msg, min delay: ${DELAY}ms)`
      );

      if (runs.length >= 5) {
        console.log(`${server.name}: 5 runs completed`);
        clearInterval(interval);
        resolve();
      }
    }, 1000);

    sendMessagesContinuously();
    setInterval(sendMessagesContinuously, DELAY);
  });

  const sum = runs.reduce((acc, val) => acc + val, 0);
  const average = sum / runs.length;
  console.log(`Average messages per second for ${server.name}: ${average}`);
  results.push({ name: server.name, average, lostPackets });

  // Fechar todas as conexões
  for (let i = 0; i < CLIENTS_TO_WAIT_FOR; i++) {
    clients[i].close();
  }
}

async function runTests() {
  for (const server of SERVERS) {
    await testServer(server);
    await new Promise((resolve) => setTimeout(resolve, WAIT_TIME_BETWEEN_TESTS));
  }

  // Calculate the average of all servers
  const overallAverage = results.reduce((acc, { average }) => acc + average, 0) / results.length;

  // Add percentage difference to each result
  results.forEach(result => {
    result.percentage = ((result.average - overallAverage) / overallAverage) * 100;
  });

  // Sort results by average messages per second
  results.sort((a, b) => b.average - a.average);

  // Display results
  console.table(results.map(result => ({
    Server: result.name,
    "Avg Messages/sec": result.average,
    "Lost Packets": result.lostPackets,
    "% Difference": `${result.percentage.toFixed(2)}%`
  })));
}

runTests();
