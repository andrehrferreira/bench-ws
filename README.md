Benchmark Websocket
=========================

The result so far is an Intel Core i9 10980XE desktop, 256GB DDR RAM, individual Docker configuration for each server

| (index) | Server           | Avg Messages/sec | % Difference |
|---------|------------------|------------------|--------------|
| 0       | Rust             | 1,232,041.4      | 541.60%      |
| 1       | Java             | 1,175,892        | 516.41%      |
| 2       | C#               | 1,132,847.8      | 496.48%      |
| 3       | C++ (Crow + TBB) | 504,620.8        | 157.50%      |
| 4       | PHP / Swoole     | 485,236.6        | 149.58%      |
| 5       | Erlang / Elixir  | 296,681.2        | 66.95%       |
| 6       | Bun              | 266,875.2        | 53.20%       |
| 7       | Go               | 263,391.2        | 51.51%       |
| 8       | Python3          | 191,937          | 16.15%       |
| 9       | Node             | 154,831.2        | -4.33%       |
| 10      | uWebsocket.js    | 100,140.4        | -33.47%      |
| 11      | Dart             | 6,936.8          | -95.12%      |
| 12      | Ruby             | 3,456.4          | -97.51%      |

The result so far is an Intel Core i9 10980XE desktop, 256GB of DDR RAM, using WSL and manual configuration.

|    | Server           | Avg Messages/sec | % Difference |
|----|------------------|------------------|--------------|
|  0 | Rust             | 990,420.4        | 315.79%      |
|  1 | C#               | 871,702.6        | 265.95%      |
|  2 | Erlang / Elixir  | 654,033.6        | 156.37%      |
|  3 | C++ (Crow + TBB) | 518,418.2        | 90.88%       |
|  4 | Java             | 146,882.6        | -57.73%      |
|  5 | Bun              | 88,682.2         | -76.19%      |
|  6 | Go               | 87,612.8         | -76.53%      |
|  7 | uWebsocket.js    | 82,604.8         | -78.24%      |
|  8 | PHP / Swoole     | 80,273.8         | -79.00%      |
|  9 | Python3          | 72,891.0         | -81.36%      |
| 10 | Node             | 63,003.6         | -84.76%      |
| 11 | * Deno           | 61,819.0         | -85.18%      |
 

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