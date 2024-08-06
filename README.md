Benchmark Websocket
=========================

The result so far is on an Intel Core i9 10980XE desktop, 256GB of DDR RAM,

|   | Server        | Avg Messages/sec | % Difference |
|---|---------------|------------------|--------------|
| 0 | Rust          | 3,007,707.8      | 337.82%      |
| 1 | C#            | 1,819,930        | 164.92%      |
| 2 | Python3       | 185,285.2        | -73.03%      |
| 3 | Bun           | 136,835.2        | -80.08%      |
| 4 | Go            | 118,835.8        | -82.70%      |
| 5 | uWebsocket.js | 86,810.9         | -87.36%      |
| 6 | Deno          | 72,728.4         | -89.41%      |
| 7 | Node          | 67,626.6         | -90.16%      |

Soon implementations in C++, Erlang, Zig and Java

Run in Node
-------------

```bash
node server-node.js
```

Run in uWebsocket.js
-------------

```bash
node server-uws.js
```

Run in Deno
-------------

```bash
deno run -A server-deno.mjs
```

Run in Bun
-------------

```bash
bun server-bun.js
```

Run in C#
-------------

Build
```bash
cd server-csharp && dotnet build
```

Run
```bash
cd server-csharp && dotnet run
```

Run in Erlang
-------------

Build
```bash
cd server-erlang && rebar3 compile
```

Run in Phyton
-------------

```bash
python3 server-python.py
```

Run in Rust
-------------

Build
```bash
cd server-rust && cargo build
```

Run
```bash
cd server-rust && cargo run
```

## Client 

To start the test it will be necessary to start all the servers, each one is configured on a port

```bash
bun ./client.js
```

The test script will perform local tests and send and broadcast messages and will store the total number of messages received per second. It will also generate a comparison between all tested servers, summarizing the average number of messages per second and the comparative performance to other servers.