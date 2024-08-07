Benchmark Websocket
=========================

The result so far is on an Intel Core i9 10980XE desktop, 256GB of DDR RAM.

|    | Server           | Avg Messages/sec | Lost Packets | % Difference |
|----|------------------|------------------|--------------|--------------|
|  0 | Rust             | 991099.6         | 0            | 197.91%      |
|  1 | C#               | 910258.6         | 0            | 173.61%      |
|  2 | Erlang / Elixir  | 626898.4         | 0            | 88.44%       |
|  3 | C++ (Crow + TBB) | 527998.4         | 0            | 58.71%       |
|  4 | Java             | 169504.6         | 0            | -49.05%      |
|  5 | Bun              | 88640            | 0            | -73.36%      |
|  6 | Go               | 87936.2          | 0            | -73.57%      |
|  7 | uWebsocket.js    | 85037.8          | 0            | -74.44%      |
|  8 | PHP / Swoole     | 77878.2          | 0            | -76.59%      |
|  9 | Node             | 62807.2          | 0            | -81.12%      |
| 10 | Python3          | 31420.6          | 0            | -90.56%      |
| -  | * Deno           | 61,819           | 47           | -80.32%      | 

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
$ cd server-csharp 
$ dotnet build
```

Run
```bash
$ dotnet run
```

Run in Erlang / Elixir
-------------

Depedences 
```bash
$ mix deps.get 
```

Build
```bash
$ cd server_elixir
$ mix deps.compile
```

Run
```bash
$ mix run --no-halt
```

Run in Phyton 3
-------------

Depedences 
```bash
$ pip install gevent
$ pip install gevent-websocket
```

Run
```bash
$ cd server-python
$ python server-python.py
```

Run in Rust
-------------

Build
```bash
$ cd server-rust
$ cargo build --release
```

Run
```bash
$ ./target/release/server-rust
```

Run in Java
-------------

Build
```bash
$ cd server-java
$ mvn compile
```

Run
```bash
$ java -jar server-java/target/server-1.0-SNAPSHOT-jar-with-dependencies.jar
```

Run in Go
-------------

Build
```bash
$ cd server-go
$ go build
```

Run
```bash
$ ./server
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

Run in PHP / Swoole
-------------

Run
```bash
$ php ./server-swoole/server.php
```

## Client 

To start the test it will be necessary to start all the servers, each one is configured on a port

```bash
$ bun ./client.js
```

The test script will perform local tests and send and broadcast messages and will store the total number of messages received per second. It will also generate a comparison between all tested servers, summarizing the average number of messages per second and the comparative performance to other servers.