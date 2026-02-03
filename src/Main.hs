{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Main
-- Description : P2P demo for gbnet-hs with Gloss rendering
--
-- Run multiple instances of this demo to see them connect.
-- Each instance is a peer that broadcasts its position.
--
-- Usage:
--   gbnet-demo              -- Binds to port 7777
--   gbnet-demo 7778         -- Binds to port 7778
--   gbnet-demo 7778 7777    -- Binds to 7778, connects to peer on 7777
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (foldM)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16, Word8)
import GBNet.Channel (ChannelConfig (..), DeliveryMode (..), defaultChannelConfig)
import GBNet.Config (NetworkConfig (..), defaultNetworkConfig)
import GBNet.Peer
import GBNet.Reliability (MonoTime)
import GBNet.Serialize.BitBuffer (ReadResult (..), empty, fromBytes, toBytes)
import GBNet.Serialize.Class (BitDeserialize (..), BitSerialize (..))
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Network.Socket (SockAddr (..), tupleToHostAddress)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- | Get current monotonic time in milliseconds.
getMonoTime :: IO MonoTime
getMonoTime = do
  t <- getPOSIXTime
  pure $ round (t * 1000)

-- | Window dimensions (integer version for gloss).
windowWidthInt, windowHeightInt :: Int
windowWidthInt = round windowWidth
windowHeightInt = round windowHeight

-- | Demo state (render side).
data DemoState = DemoState
  { dsLocalState :: !PlayerState,
    dsLocalInput :: !PlayerInput,
    dsPeers :: !(Map PeerId PlayerState),
    dsLocalPort :: !Word16
  }
  deriving (Show)

-- | Network state shared between threads.
data NetState = NetState
  { nsLocalState :: !PlayerState,
    nsPeers :: !(Map PeerId PlayerState),
    nsPeerCount :: !Int
  }

-- | Initial demo state.
initialDemoState :: Word16 -> DemoState
initialDemoState port =
  DemoState
    { dsLocalState = defaultPlayerState,
      dsLocalInput = PlayerInput False False False False,
      dsPeers = Map.empty,
      dsLocalPort = port
    }

-- | Colors for players (local is first).
playerColors :: [Color]
playerColors =
  [ makeColorI 50 200 50 255, -- Green (local player)
    makeColorI 255 50 50 255, -- Red
    makeColorI 50 50 255 255, -- Blue
    makeColorI 255 255 50 255, -- Yellow
    makeColorI 255 50 255 255, -- Magenta
    makeColorI 50 255 255 255, -- Cyan
    makeColorI 255 128 50 255, -- Orange
    makeColorI 128 50 255 255 -- Purple
  ]

-- | Player render size (2x radius for full square).
playerRenderSize :: Float
playerRenderSize = playerRadius * 2

-- | Center offset for coordinate conversion (game coords -> gloss coords).
centerX, centerY :: Float
centerX = windowWidth / 2
centerY = windowHeight / 2

-- | HUD positioning.
hudLeftMargin :: Float
hudLeftMargin = -(centerX - 20)

hudTopOffset :: Float
hudTopOffset = centerY - 30

hudLineSpacing :: Float
hudLineSpacing = 20

-- | Channel for state broadcasts.
stateChannel :: Word8
stateChannel = 0

-- | Network tick rate in microseconds.
netTickUs :: Int
netTickUs = 16667 -- ~60Hz

main :: IO ()
main = do
  args <- getArgs
  let (localPort, connectTo) = parseArgs args

  putStrLn "gbnet-hs P2P demo"
  putStrLn $ "Binding to port: " ++ show localPort
  case connectTo of
    Nothing -> putStrLn "Waiting for connections..."
    Just p -> putStrLn $ "Connecting to port: " ++ show p
  putStrLn "WASD to move"

  -- Initialize gbnet peer with unreliable channel for position updates
  let bindAddr = SockAddrInet (fromIntegral localPort) 0
      -- Channel 0: Unreliable for position updates (fire and forget)
      unreliableChannel = defaultChannelConfig {ccDeliveryMode = Unreliable}
      -- High send rate, no congestion window
      config =
        defaultNetworkConfig
          { ncChannelConfigs = [unreliableChannel],
            ncSendRate = 1000.0, -- Allow high send rate
            ncMaxPacketRate = 1000.0
          }
  now <- getMonoTime

  result <- newPeer bindAddr config now
  case result of
    Left err -> error $ "Failed to create peer: " ++ show err
    Right peer -> do
      -- Connect to target if specified (peerConnect is pure)
      let peer' = case connectTo of
            Nothing -> peer
            Just targetPort ->
              let targetAddr = SockAddrInet (fromIntegral targetPort) (tupleToHostAddress (127, 0, 0, 1))
                  targetPid = peerIdFromAddr targetAddr
               in peerConnect targetPid now peer

      -- Shared state between network thread and render thread
      localStateRef <- newIORef defaultPlayerState
      netStateRef <- newIORef (NetState defaultPlayerState Map.empty 0)

      -- Start network thread
      _ <- forkIO $ networkLoop peer' localStateRef netStateRef

      -- Run gloss on main thread
      playIO
        (InWindow ("gbnet-demo :" ++ show localPort) (windowWidthInt, windowHeightInt) (100, 100))
        (makeColorI 25 25 38 255)
        60
        (initialDemoState localPort)
        (render netStateRef)
        handleInput
        (update localStateRef netStateRef)

-- | Network loop running in separate thread.
--
-- Demonstrates pure/IO separation:
--   1. Receive packets (IO)
--   2. Queue outgoing messages (pure)
--   3. Process packets and update state (pure)
--   4. Send packets (IO)
networkLoop :: NetPeer -> IORef PlayerState -> IORef NetState -> IO ()
networkLoop initialPeer localStateRef netStateRef = go initialPeer Map.empty `catch` handleEx
  where
    handleEx :: SomeException -> IO ()
    handleEx e = putStrLn $ "NETWORK THREAD CRASHED: " ++ show e

    go peer peers = do
      now <- getMonoTime
      localState <- readIORef localStateRef

      -- 1. Receive packets (IO)
      (packets, sock') <- peerRecvAll (npSocket peer) now

      -- 2. Process packets first (establishes connections)
      let result = peerProcess now packets peer {npSocket = sock'}
          peer1 = prPeer result
          events = prEvents result
          outgoing1 = prOutgoing result

      -- 3. Queue broadcast on UPDATED peer (now has connections)
      let encoded = encodeState localState
          peer2 = peerBroadcast stateChannel encoded Nothing now peer1

      -- 4. Drain send queue to get broadcast packets
      let (broadcastPackets, peer') = drainPeerSendQueue peer2
          outgoing = outgoing1 ++ broadcastPackets

      -- 5. Send all packets (IO)
      sock'' <- peerSendAll outgoing (npSocket peer') now

      -- Update peer map and shared state
      peers' <- foldM handleEvent peers events
      writeIORef netStateRef $ NetState localState peers' (Map.size peers')

      threadDelay netTickUs
      go peer' {npSocket = sock''} peers'

    encodeState = toBytes . (`bitSerialize` empty)

    handleEvent peerMap = \case
      PeerConnected pid dir -> do
        putStrLn $ "Connected: " ++ show pid ++ " (" ++ show dir ++ ")"
        pure $ Map.insert pid defaultPlayerState peerMap
      PeerDisconnected pid reason -> do
        putStrLn $ "Disconnected: " ++ show pid ++ " (" ++ show reason ++ ")"
        pure $ Map.delete pid peerMap
      PeerMessage pid _ch dat ->
        pure $ case bitDeserialize (fromBytes dat) of
          Left _ -> peerMap
          Right result -> Map.insert pid (readValue result) peerMap
      PeerMigrated oldPid newPid -> do
        putStrLn $ "Migrated: " ++ show oldPid ++ " -> " ++ show newPid
        pure $ maybe peerMap (\ps -> Map.insert newPid ps $ Map.delete oldPid peerMap) (Map.lookup oldPid peerMap)

-- | Parse command line args: [localPort] [connectToPort]
parseArgs :: [String] -> (Word16, Maybe Word16)
parseArgs [] = (defaultPort, Nothing)
parseArgs [p] = (fromMaybe defaultPort (readMaybe p), Nothing)
parseArgs (p : t : _) = (fromMaybe defaultPort (readMaybe p), readMaybe t)

-- | Render the demo.
render :: IORef NetState -> DemoState -> IO Picture
render netStateRef state = do
  netState <- readIORef netStateRef
  pure $
    Pictures $
      -- Draw local player (green)
      [ drawPlayer (head playerColors) (dsLocalState state) True
      ]
        ++
        -- Draw peer players
        [ drawPlayer (playerColors !! (i `mod` length playerColors)) ps False
        | (i, (_, ps)) <- zip [1 ..] (Map.toList (nsPeers netState))
        ]
        ++
        -- Draw HUD
        [ Translate hudLeftMargin hudTopOffset $
            Scale 0.12 0.12 $
              Color white $
                Text $
                  "Port: " ++ show (dsLocalPort state),
          Translate hudLeftMargin (hudTopOffset - hudLineSpacing) $
            Scale 0.10 0.10 $
              Color (greyN 0.5) $
                Text $
                  "Peers: " ++ show (nsPeerCount netState),
          Translate hudLeftMargin (hudTopOffset - hudLineSpacing * 2) $
            Scale 0.10 0.10 $
              Color (greyN 0.5) $
                Text "WASD to move"
        ]

-- | Selection outline thickness.
selectionOutlineThickness :: Float
selectionOutlineThickness = 4

-- | Draw a player.
drawPlayer :: Color -> PlayerState -> Bool -> Picture
drawPlayer col ps isLocal =
  Translate (psX ps - centerX) (centerY - psY ps) $
    Pictures
      [ Color col $ rectangleSolid playerRenderSize playerRenderSize,
        if isLocal
          then
            Color white $
              rectangleWire
                (playerRenderSize + selectionOutlineThickness)
                (playerRenderSize + selectionOutlineThickness)
          else Blank
      ]

-- | Handle input events.
handleInput :: Event -> DemoState -> IO DemoState
handleInput event state =
  let input = dsLocalInput state
      input' = case event of
        EventKey (Char 'w') Down _ _ -> input {piUp = True}
        EventKey (Char 'w') Up _ _ -> input {piUp = False}
        EventKey (Char 's') Down _ _ -> input {piDown = True}
        EventKey (Char 's') Up _ _ -> input {piDown = False}
        EventKey (Char 'a') Down _ _ -> input {piLeft = True}
        EventKey (Char 'a') Up _ _ -> input {piLeft = False}
        EventKey (Char 'd') Down _ _ -> input {piRight = True}
        EventKey (Char 'd') Up _ _ -> input {piRight = False}
        _ -> input
   in pure state {dsLocalInput = input'}

-- | Update the demo state (render thread).
update :: IORef PlayerState -> IORef NetState -> Float -> DemoState -> IO DemoState
update localStateRef _netStateRef dt state = do
  -- Update local physics
  let localState' = applyInput dt (dsLocalInput state) (dsLocalState state)

  -- Share with network thread
  writeIORef localStateRef localState'

  pure state {dsLocalState = localState'}
