# PureScript web-based multiplayer game

This project is intended to be a template for creating simple PureScript web-based real-time multiplayer games.  The game is deliberately very simple: In a game of Tag, each player controls a character on a grid. "It" chases the others to "tag" them.

Players' browser instances communicate using broadcast messages via a web-socket broadcast server.  (A simple broadcast server is provided.)

## Set-up

  1. Copy file ```ws-broadcast.js``` to a server reachable by all players and start it using node.js:

    > npm install ws
    > node ws-broadcast.js

  2. Install PureScript tools (if needed):

    > npm install purescript@0.13.8
    > npm install spago

  3. Build index.js:

    > spago bundle-app

  4. Each player opens the provided HTML file tigGame.html in a browser and play.