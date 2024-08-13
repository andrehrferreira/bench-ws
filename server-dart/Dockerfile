FROM dart:stable AS build

WORKDIR /app

COPY . .

RUN mkdir bin
RUN dart pub get
RUN dart compile exe server.dart -o bin/server

FROM debian:buster-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/bin/server /app/server

EXPOSE 3016

CMD ["./server"]
