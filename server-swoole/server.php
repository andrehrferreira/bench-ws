<?php

use Swoole\WebSocket\Server;

$port = 3013;
$clientsToWaitFor = 32;
$clients = [];

$server = new Server("0.0.0.0", $port);

$server->on("start", function (Server $server) use ($port) {
    echo "Swoole WebSocket server started on port {$port}\n";
});

$server->on("open", function (Server $server, $request) use (&$clients, $clientsToWaitFor) {
    $clients[$request->fd] = $request->fd;
    echo "Client {$request->fd} connected (" . ($clientsToWaitFor - count($clients)) . " remain)\n";

    if (count($clients) === $clientsToWaitFor) {
        sendReadyMessage($server, $clients);
    }
});

$server->on("message", function (Server $server, $frame) use (&$clients) {
    foreach ($clients as $fd) {
        if ($frame->fd !== $fd) {
            $server->push($fd, "Message from server: {$frame->data}");
        }
    }
});

$server->on("close", function (Server $server, $fd) use (&$clients) {
    unset($clients[$fd]);
    echo "Client {$fd} disconnected\n";
});

function sendReadyMessage(Server $server, array $clients) {
    echo "All clients connected\n";
    Swoole\Timer::after(100, function () use ($server, $clients) {
        echo "Starting benchmark\n";
        foreach ($clients as $fd) {
            $server->push($fd, "ready");
        }
    });
}

$server->start();
