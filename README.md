Benchmark Websocket
=========================

The result so far is an Intel Core i9 10980XE desktop, 256GB DDR RAM, individual Docker configuration for each server


The result so far is an Intel Core i9 10980XE desktop, 256GB of DDR RAM, using WSL and manual configuration.

|    | Server           | Avg Messages/sec | Lost Packets | % Difference |
|----|------------------|------------------|--------------|--------------|
|  0 | Rust             | 990420.4         | 0            | 197.95%      |
|  1 | C#               | 871702.6         | 0            | 162.24%      |
|  2 | Erlang / Elixir  | 654033.6         | 0            | 96.75%       |
|  3 | C++ (Crow + TBB) | 518418.2         | 0            | 55.96%       |
|  4 | Java             | 146882.6         | 0            | -55.81%      |
|  5 | Bun              | 88682.2          | 0            | -73.32%      |
|  6 | Go               | 87612.8          | 0            | -73.64%      |
|  7 | uWebsocket.js    | 82604.8          | 0            | -75.15%      |
|  8 | PHP / Swoole     | 80273.8          | 0            | -75.85%      |
|  9 | Python3          | 72891            | 0            | -78.07%      |
| 10 | Node             | 63003.6          | 0            | -81.05%      |
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