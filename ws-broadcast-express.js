'use strict';

import express from 'express';
import { Server } from 'ws';

const PORT = process.env.PORT || 3000;
const INDEX = '/tigGame.html';

const server = express()
  .use((req, res) => res.sendFile(INDEX, { root: __dirname }))
  .listen(PORT, () => console.log(`Express listening on ${PORT}`));

const wss = new Server({ server });

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
  console.log("WebSocket broadcast server started.");
});
