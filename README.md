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

Demonstrates the pure/IO separation in gbnet-hs:

```haskell
go peer peers = do
  -- 1. Queue outgoing messages (pure)
  let withBroadcast = peerBroadcast channel state Nothing now peer

  -- 2. Receive packets (IO)
  (packets, sock') <- peerRecvAll (npSocket withBroadcast) now

  -- 3. Process packets (pure)
  let PeerResult processed events outgoing = peerProcess now packets withBroadcast { npSocket = sock' }

  -- 4. Send packets (IO)
  sock'' <- peerSendAll outgoing (npSocket processed) now

  go processed { npSocket = sock'' } peers'
```

The networking logic is deterministic and testable - only socket operations are IO.
