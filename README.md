Benchmark Websocket
=========================

The result so far is on an Intel Core i9 10980XE desktop, 256GB of DDR RAM,

┌───┬───────────────┬──────────────────┬──────────────┐
│   │ Server        │ Avg Messages/sec │ % Difference │
├───┼───────────────┼──────────────────┼──────────────┤
│ 0 │ C#            │ 2038129          │ 427.07%      │
│ 1 │ Phyton3       │ 169332           │ -56.21%      │
│ 2 │ Go            │ 141941.3         │ -63.29%      │
│ 3 │ Bun           │ 131950.4         │ -65.88%      │
│ 4 │ uWebsocket.js │ 86801.6          │ -77.55%      │
│ 5 │ Deno          │ 70829.9          │ -81.68%      │
│ 6 │ Node          │ 67855.9          │ -82.45%      │
└───┴───────────────┴──────────────────┴──────────────┘

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

Build
```bash
python3 server-python.py
```

## Client 

To start the test it will be necessary to start all the servers, each one is configured on a port

```bash
bun ./client.js
```

The test script will perform local tests and send and broadcast messages and will store the total number of messages received per second. It will also generate a comparison between all tested servers, summarizing the average number of messages per second and the comparative performance to other servers.