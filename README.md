# Benchmark Websocket

## Run in Node:

```bash
node server-node.js
```

## Run in uWebsocket.js:

```bash
node server-uws.js
```

## Run in Deno:

```bash
deno run -A server-deno.mjs
```

## Run in Bun:

```bash
bun server-bun.js
```

## Run in C#:

Build
```bash
cd server-csharp && dotnet build
```

Run
```bash
cd server-csharp && dotnet run
```

## Run in Erlang

Build
```bash
cd server-erlang && rebar3 compile
```
## Run in python

Run
```bash
python server-python.py
```

## Client 

To start the test it will be necessary to start all the servers, each one is configured on a port

```bash
bun ./client.js
```

The test script will perform local tests and send and broadcast messages and will store the total number of messages received per second. It will also generate a comparison between all tested servers, summarizing the average number of messages per second and the comparative performance to other servers.