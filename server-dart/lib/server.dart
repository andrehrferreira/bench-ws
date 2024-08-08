import 'dart:async';
import 'dart:io';

class Clients {
  final StreamController<String> _broadcastController =
      StreamController<String>.broadcast();
  final Set<WebSocket> _clients = {};

  void addClient(WebSocket client) {
    _clients.add(client);

    // Forward messages from the broadcast stream to the client
    _broadcastController.stream.listen((message) {
      if (_clients.isNotEmpty) {
        for (var c in _clients) {
          c.add(message);
        }
      }
    });
  }

  void removeClient(WebSocket client) {
    _clients.remove(client);
    client.close();
  }

  void broadcast(String message) {
    _broadcastController.add(message);
  }

  int get count => _clients.length;
}

Future<void> handleConnection(WebSocket socket, Clients clients) async {
  clients.addClient(socket);

  // Forward messages from the WebSocket to the broadcast stream
  socket.listen((message) {
    if (message is String) {
      clients.broadcast(message);
    }
  }, onDone: () {
    clients.removeClient(socket);
  });
}

Future<void> sendReadyMessage(Clients clients) async {
  await Future.delayed(Duration(milliseconds: 100));
  print('All clients connected');
  clients.broadcast('ready');
}

Future<void> main() async {
  var port = int.parse(Platform.environment['PORT'] ?? '3016');
  var clientsToWaitFor =
      int.parse(Platform.environment['CLIENTS_COUNT'] ?? '32');

  var clients = Clients();
  var server = await HttpServer.bind(InternetAddress.anyIPv4, port);

  print('Waiting for $clientsToWaitFor clients to connect..');
  print('Running server on port $port');

  await for (var request in server) {
    if (request.uri.path == '/') {
      var socket = await WebSocketTransformer.upgrade(request);
      handleConnection(socket, clients);

      if (clients.count == clientsToWaitFor) {
        sendReadyMessage(clients);
      }
    } else {
      request.response
        ..statusCode = HttpStatus.forbidden
        ..write('WebSocket connections only')
        ..close();
    }
  }
}
