'use strict';

const express = require('express');
const { Server } = require('ws');

const PORT = process.env.PORT || 3000;

const server = express()
.use(express.static('public'))
.use((req, res) => res.sendFile('public/index.html', { root: __dirname + '/public' }))
.listen(PORT, () => console.log(`Express listening on ${PORT}`));

const wss = new Server({ server });

wss.on('connection', function connection(ws) {
  ws.on('message', function incoming(data) {
    // console.log("Broadcasting: " + data)
    wss.clients.forEach(function each(client) {
      if (client !== ws && client.readyState === WebSocket.OPEN) {
      // if (client.readyState === WebSocket.OPEN) {
          client.send(data);
      }
    });
  });
});

console.log("WebSocket broadcast server configured on " + wss);

