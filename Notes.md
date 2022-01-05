# Notes

## TODO

* [ ] Turn TigGame into an instance of a generic engine
  * Extract as much generic functionality as possible
    * [x] MovingPoint
    * [x] GameObject state
    * [x] GameCanvas
    * [x] Coordinator
    * [X] reactive GameObject with a private state type using continuations
      * types:
        * date GameObject action output = GameObject { handleAction :: HandleAction action output, draw :: Effect _ }
        * newtype HandleAction action output = HandleAction (action -> (GameObject action output, Maybe output))
      * root component holds 
        * collection of GameObjects
    * [ ] separate generic functionality from TigGame as much as possible
      * Where are the states of GameObjects kept?
      * option 1: generic rootComponent + injection of specific functionality via types and other parameters
      * option 2: small TigGame component including generic functionality
        * standard elements mixed with bespoke elements for slots, state, render, actions and action components
  * [X] GameObjectStore
    * [X] holds a map of GameObjects
    * [X] can send an object action to one of more objects
      * automatically updating the map if the object(s) changed
      * also taking a process hook for each object change
    * [X] can do collision detection
      * focusing on a subset of objects, detecting and handling their collisions with all objects
        * some pairs of objects are ignored, determined by a given binary predicate
      * also taking a process hook for each collided object-pair
  * [ ] safe and reusable collision handling using GameObjectStore
    * [ ] leader does all collision handling
* Create other simple games
  * [ ] Add balls to tig game
  * [ ] Coin ball

## Main ideas

* main ideas
* P2P with a leader
  * non-leaders send relevant keyboard / mouse events to the leader
    * the leader calculates all state changes
    * the leader broadcasts state changes to peers
      * efficiently communicate state changes
        * only sprites or global variables that changed
        * for each sprite, communicate only aspects that changed, eg only position or scaling
    * leader assigned and re-assigned automatically when players join and leave
      * joining protocol:
        * listen on the channel and note other players' ids, who is the leader
        * randomly chose an id number, check not coinciding
        * if noone else present, assume leadership
        * broadcast own id and leader status
        * keep pinging others at least every 200 ms (leader) or 1000 ms (non-leader)
      * leader changes:
        * if leader receives leadership claim from another peer (eg if two start at the same time)
          * if their id is larger, stop being the leader
          * if their id is smaller, re-broadcast own id and leader status
        * if the leader falls silent, the largest id becomes the leader
* inspired by scratch concepts
  * main controller
    * state
      * a dictionary of sprites / containers
        * each has its internal state
      * a dictionary of global variables, eg
        * scores
        * elapsed / remaining time
      * leader status (transparent to game programmers)
  * sprites and containers
    * internal state, a combination of:
      * shape = tree of circles / rectangles joined by "union" or "difference" operations
      * size scaling factor
      * xy_state: position, velocity, accelleration
      * angle_state: angle, spin, spin accelleration
      * consistency: phantom, solid or container (possibly with a set of rectrangular holes)
      * mass, "springiness", friction
      * logical state (other state components can depends on this)
    * canvas draw
      * can include drawing images, rendering text
    * reacting to events
      * keyboard/mouse input from player n
      * collision
        * movement response handled automatically
  * backdrops
  * sounds
