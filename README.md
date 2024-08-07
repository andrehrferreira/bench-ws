Benchmark Websocket
=========================

The result so far is on an Intel Core i9 10980XE desktop, 256GB of DDR RAM.

|   | Server           | Avg Messages/sec | Lost Packets | % Difference |
|---|------------------|------------------|--------------|--------------|
| 0 | Rust             | 947184.2         | 0            | 162.88%      |
| 1 | C#               | 912115.6         | 0            | 153.15%      |
| 2 | Erlang / Elixir  | 662187.4         | 0            | 83.78%       |
| 3 | C++ (Crow + TBB) | 544027.4         | 0            | 50.99%       |
| 4 | Java             | 133235.6         | 0            | -63.02%      |
| 5 | Go               | 90075.2          | 0            | -75.00%      |
| 6 | Bun              | 88235.2          | 0            | -75.51%      |
| 7 | uWebsocket.js    | 86597.2          | 0            | -75.97%      |
| 8 | Python3          | 74816.8          | 0            | -79.24%      |
| 9 | Node             | 64610.2          | 0            | -82.07%      |
| - | * Deno           | 61,819           | 47           | -80.32%      | 

* Deno has been removed from testing due to memory leek and poor performance issues

* Soon implementations in Zig 

Run in Node
-------------

```bash
$ node server-node.js
```

Run in uWebsocket.js
-------------

```bash
$ node server-uws.js
```

Run in Deno
-------------

```bash
$ deno run -A server-deno.mjs
```

Run in Bun
-------------

```bash
$ bun server-bun.js
```

Run in C#
-------------

Build
```bash
$ cd server-csharp && dotnet build
```

Run
```bash
$ cd server-csharp && dotnet run
```

Run in Elixir
-------------

Depedences 
```bash
$ mix deps.get 

```

Build
```bash
$ mix deps.compile
```

Run
```bash
$ mix run --no-halt
```

Run in Phyton
-------------

Depedences 
```bash
$ pip install gevent
$ pip install gevent-websocket
```

Run
```bash
$ python server-python.py
```

Run in Rust
-------------

Build
```bash
$ cargo build
```

Run
```bash
$ cargo run
```

Run in Java
-------------

Build
```bash
$ mvn compile
```

Run
```bash
$ java -jar server-java/target/server-1.0-SNAPSHOT-jar-with-dependencies.jar
```

Run in C++ (Crow + TBB)
-------------

Dependeces 
```bash
$ vcpkg install
```

Run
```bash
$ server-cpp/x64/Release/server-cpp.exe
```

## Client 

To start the test it will be necessary to start all the servers, each one is configured on a port

```bash
$ bun ./client.js
```

The test script will perform local tests and send and broadcast messages and will store the total number of messages received per second. It will also generate a comparison between all tested servers, summarizing the average number of messages per second and the comparative performance to other servers.