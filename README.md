# gbnet-hs-demo

Visual P2P demo for [gbnet-hs](https://github.com/Gondola-Bros-Entertainment/gbnet-hs).

## Building

```bash
cabal build
```

## Running

```bash
# Terminal 1 - binds to port 7777
cabal run gbnet-demo

# Terminal 2 - binds to 7778, connects to 7777
cabal run gbnet-demo -- 7778 7777
```

WASD to move. Each peer broadcasts its position over the network.

## Architecture

Demonstrates the polymorphic `peerTick` API in gbnet-hs:

```haskell
networkLoop :: IORef PlayerState -> IORef SharedNetState -> IORef NetPeer -> NetT IO ()
networkLoop localStateRef sharedNetRef peerRef = go Map.empty
  where
    go peers = do
      peer <- liftIO $ readIORef peerRef
      localState <- liftIO $ readIORef localStateRef

      -- Encode local state to broadcast
      let encoded = toBytes (bitSerialize localState empty)

      -- Single call: receive, process, broadcast, send
      (events, peer') <- peerTick [(stateChannel, encoded)] peer

      -- Handle events and update peer map
      peers' <- liftIO $ handleEvents peers events
      liftIO $ threadDelay netTickUs
      go peers'
```

The `peerTick` function handles receive, process, broadcast, and send in one call
within the `NetT` monad, which abstracts over real sockets or pure test networks.
