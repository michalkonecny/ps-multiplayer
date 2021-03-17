// based on code from https://github.com/websockets/ws#server-broadcast
const WebSocket = require('ws');

const WEB_SOCKET_PORT = 3000;

const wss = new WebSocket.Server({ port: WEB_SOCKET_PORT });

wss.on('connection', function connection(ws) {
  ws.on('message', function incoming(data) {
    console.log("Broadcasting: " + data)
    wss.clients.forEach(function each(client) {
      if (client !== ws && client.readyState === WebSocket.OPEN) {
      // if (client.readyState === WebSocket.OPEN) {
          client.send(data);
      }
    });
  });
});

console.log("WebSocket broadcast server started at ws://locahost:"+ WEB_SOCKET_PORT);

// a mini test:

// const client1 = new WebSocket( "ws://localhost:" + WEB_SOCKET_PORT );
// const client2 = new WebSocket( "ws://localhost:" + WEB_SOCKET_PORT );

// client1.on('message', function (data) { console.log("client1 received: " + data) });
// client2.on('message', function (data) { console.log("client2 received: " + data) });

// client1.on('open', function() {client1.send("zprava 1")});
// client2.on('open', function() {client2.send("zprava 2")});
