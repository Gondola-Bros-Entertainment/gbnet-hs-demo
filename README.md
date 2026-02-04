# gbnet-hs-demo

Visual P2P networking demo for [gbnet-hs](https://github.com/Gondola-Bros-Entertainment/gbnet-hs).

Each instance is a peer that broadcasts its position over UDP. Peers
auto-discover each other via mesh introduction -- connect two nodes and
a third will learn about both.

## Quick Start

```bash
# Terminal 1 -- binds to port 7777 (default)
cabal run gbnet-demo

# Terminal 2 -- binds to 7778, connects to 7777
cabal run gbnet-demo -- 7778 7777

# Terminal 3 -- binds to 7779, connects to 7777 (discovers 7778 via mesh)
cabal run gbnet-demo -- 7779 7777
```

WASD to move. Green square is you; coloured squares are remote peers.

## Building

Requires GHC 9.6+ and a local checkout of `gbnet-hs` (referenced via
`cabal.project`).

```bash
cabal build
```

## Architecture

Two modules:

| Module | Purpose |
|--------|---------|
| `Game` | Shared types (`PlayerState`, `PeerAddr`), movement physics, TH-derived serialization |
| `Main` | Gloss window, network thread, mesh introduction, event handling |

The network thread runs inside `NetT IO` and calls `peerTick` once per
frame -- a single function that receives, processes, broadcasts, and
sends in one step:

```haskell
networkLoop :: IORef PlayerState -> ... -> NetT IO ()
networkLoop localStateRef ... = go Map.empty
  where
    go peers = do
      peer <- liftIO $ readIORef peerRef
      localState <- liftIO $ readIORef localStateRef
      let encoded = toBytes (bitSerialize localState empty)

      -- Single call: receive, process, broadcast, send
      (events, peer') <- peerTick [(stateChannel, encoded)] peer

      -- Handle events and update peer map
      (peers', peer'') <- liftIO $ handleEvents peers peer' now events
      ...
```

### Channels

| Channel | Mode | Use |
|---------|------|-----|
| 0 | Unreliable | Position state broadcast (~60 Hz) |
| 1 | Reliable ordered | Mesh peer introduction |

### Mesh Peer Introduction

When peer A connects to peer B, B sends A the addresses of all its
other connections over the reliable channel. A then connects to each
new address, forming a full mesh without a central server.

## License

MIT -- see [LICENSE](LICENSE).
