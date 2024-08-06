Benchmark Websocket
=========================

The result so far is on an Intel Core i9 10980XE desktop, 256GB of DDR RAM,

|   | Server        | Avg Messages/sec | % Difference |
|---|---------------|------------------|--------------|
| 0 | Rust          | 3,072,247.2      | 322.48%      |
| 1 | C#            | 2,040,620        | 180.62%      |
| 2 | Java          | 829,580.6        | 14.08%       |
| 3 | Python3       | 176,374.1        | -75.75%      |
| 4 | Go            | 107,940.8        | -85.16%      |
| 5 | Bun           | 98,558.4         | -86.45%      |
| 6 | uWebsocket.js | 86,886.5         | -88.05%      |
| 7 | Node          | 67,055.6         | -90.78%      |
| 8 | Deno          | 65,420           | -91.00%      |

Soon implementations in C++, Erlang and Zig 

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
## Run in python

Run
```bash
python server-python.py
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

Run in Java
-------------

Build
```bash
cd server-java && mvn compile
```

Run
```bash
java -jar server-java/target/server-1.0-SNAPSHOT-jar-with-dependencies.jar
```

## Client 

To start the test it will be necessary to start all the servers, each one is configured on a port

```bash
bun ./client.js
```

The test script will perform local tests and send and broadcast messages and will store the total number of messages received per second. It will also generate a comparison between all tested servers, summarizing the average number of messages per second and the comparative performance to other servers.