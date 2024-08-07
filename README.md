Benchmark Websocket
=========================

The result so far is on an Intel Core i9 10980XE desktop, 256GB of DDR RAM,

| #  | Server           | Avg Messages/sec | Lost Packets | % Difference |
|----|------------------|------------------|--------------|--------------|
| 0  | Rust             | 1,027,930.4      | 0            | 227.24%      |
| 1  | C#               | 910,847.2        | 0            | 189.97%      |
| 2  | C++ (Crow + TBB) | 546,900.8        | 0            | 74.10%       |
| 3  | Java             | 201,114.4        | 0            | -35.98%      |
| 4  | Bun              | 89,689           | 0            | -71.45%      |
| 5  | Go               | 89,075.6         | 0            | -71.64%      |
| 6  | uWebsocket.js    | 87,573.6         | 0            | -72.12%      |
| 7  | Node             | 63,247.4         | 0            | -79.87%      |
| 8  | Phyton3          | 63,018.4         | 0            | -79.94%      |
| 9  | Deno             | 61,819           | 47           | -80.32%      |

Soon implementations in Erlang, Elixir and Zig 

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

Run in Erlang
-------------

Build
```bash
$ cd server-erlang && rebar3 compile
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
$ cd server-rust && cargo build
```

Run
```bash
$ cd server-rust && cargo run
```

Run in Java
-------------

Build
```bash
$ cd server-java && mvn compile
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