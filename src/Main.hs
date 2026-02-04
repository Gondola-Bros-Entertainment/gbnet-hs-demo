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
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import GBNet
import GBNet.Serialize.BitBuffer (ReadResult (..))
import Game hiding (defaultPort)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Network.Socket (tupleToHostAddress)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

-- | Window dimensions (integer version for gloss).
windowWidthInt, windowHeightInt :: Int
windowWidthInt = round windowWidth
windowHeightInt = round windowHeight

-- | Demo state (render side).
data DemoState = DemoState
  { dsLocalState :: !PlayerState,
    dsLocalInput :: !PlayerInput,
    dsLocalPort :: !Word16
  }
  deriving (Show)

-- | Network state shared between threads.
data SharedNetState = SharedNetState
  { snsPeers :: !(Map PeerId PlayerState),
    snsPeerCount :: !Int
  }

-- | Initial demo state.
initialDemoState :: Word16 -> DemoState
initialDemoState port =
  DemoState
    { dsLocalState = defaultPlayerState,
      dsLocalInput = PlayerInput False False False False,
      dsLocalPort = port
    }

-- | Color for the local player.
localPlayerColor :: Color
localPlayerColor = makeColorI 50 200 50 255

-- | Colors for remote peers (cycles for >7 peers).
remotePlayerColors :: [Color]
remotePlayerColors =
  [ makeColorI 255 50 50 255, -- Red
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

-- | Default port.
defaultPort :: Word16
defaultPort = 7777

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

  -- Initialize network state
  let bindAddr = SockAddrInet (fromIntegral localPort) 0
      -- Channel 0: Unreliable for position updates (fire and forget)
      unreliableChannel = defaultChannelConfig {ccDeliveryMode = Unreliable}
      config =
        defaultNetworkConfig
          { ncChannelConfigs = [unreliableChannel],
            ncSendRate = 1000.0,
            ncMaxPacketRate = 1000.0
          }

  -- Create peer (newPeer creates the socket)
  now <- getMonoTimeIO
  peerResult <- newPeer bindAddr config now
  case peerResult of
    Left err -> do
      putStrLn $ "Failed to create peer: " ++ show err
      exitFailure
    Right (peer, sock) -> do
      -- Create NetState from the peer's socket (spawns receive thread)
      netState <- newNetState sock (npLocalAddr peer)

      -- Connect to target if specified
      let peer' = case connectTo of
            Nothing -> peer
            Just targetPort ->
              let targetAddr = SockAddrInet (fromIntegral targetPort) (tupleToHostAddress (127, 0, 0, 1))
                  targetPid = peerIdFromAddr targetAddr
               in peerConnect targetPid now peer

      -- Shared state between network thread and render thread
      localStateRef <- newIORef defaultPlayerState
      sharedNetRef <- newIORef (SharedNetState Map.empty 0)
      peerRef <- newIORef peer'

      -- Start network thread with new polymorphic API
      _ <- forkIO $ networkThread localStateRef sharedNetRef peerRef netState

      -- Run gloss on main thread
      playIO
        (InWindow ("gbnet-demo :" ++ show localPort) (windowWidthInt, windowHeightInt) (100, 100))
        (makeColorI 25 25 38 255)
        60
        (initialDemoState localPort)
        (render sharedNetRef)
        handleInput
        (update localStateRef sharedNetRef)

-- | Network thread wrapper that handles exceptions.
networkThread :: IORef PlayerState -> IORef SharedNetState -> IORef NetPeer -> NetState -> IO ()
networkThread localStateRef sharedNetRef peerRef netState =
  (evalNetT (networkLoop localStateRef sharedNetRef peerRef) netState) `catch` handleEx
  where
    handleEx :: SomeException -> IO ()
    handleEx e = putStrLn $ "NETWORK THREAD CRASHED: " ++ show e

-- | Network loop using the new polymorphic API.
--
-- Runs inside NetT IO, using peerTick for clean receive/process/send.
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

      -- Update peer ref
      liftIO $ writeIORef peerRef peer'

      -- Handle events and update peer map
      peers' <- liftIO $ handleEvents peers events

      -- Update shared state for render thread
      liftIO $ writeIORef sharedNetRef $ SharedNetState peers' (Map.size peers')

      liftIO $ threadDelay netTickUs
      go peers'

    handleEvents peerMap evts = foldM handleEvent peerMap evts

    handleEvent peerMap = \case
      PeerConnected pid dir -> do
        putStrLn $ "Connected: " ++ show pid ++ " (" ++ show dir ++ ")"
        pure $ Map.insert pid defaultPlayerState peerMap
      PeerDisconnected pid reason -> do
        putStrLn $ "Disconnected: " ++ show pid ++ " (" ++ show reason ++ ")"
        pure $ Map.delete pid peerMap
      PeerMessage pid _ch dat ->
        case bitDeserialize (fromBytes dat) of
          Left err -> do
            putStrLn $ "Deserialize error from " ++ show pid ++ ": " ++ err
            pure peerMap
          Right (ReadResult ps _) -> pure $ Map.insert pid ps peerMap
      PeerMigrated oldPid newPid -> do
        putStrLn $ "Migrated: " ++ show oldPid ++ " -> " ++ show newPid
        pure $ maybe peerMap (\ps -> Map.insert newPid ps $ Map.delete oldPid peerMap) (Map.lookup oldPid peerMap)

-- | Parse command line args: [localPort] [connectToPort]
parseArgs :: [String] -> (Word16, Maybe Word16)
parseArgs [] = (defaultPort, Nothing)
parseArgs [p] = (fromMaybe defaultPort (readMaybe p), Nothing)
parseArgs (p : t : _) = (fromMaybe defaultPort (readMaybe p), readMaybe t)

-- | Render the demo.
render :: IORef SharedNetState -> DemoState -> IO Picture
render sharedNetRef state = do
  netState <- readIORef sharedNetRef
  pure $
    Pictures $
      -- Draw local player (green)
      [ drawPlayer localPlayerColor (dsLocalState state) True
      ]
        ++
        -- Draw peer players
        [ drawPlayer col ps False
        | (col, (_, ps)) <- zip (cycle remotePlayerColors) (Map.toList (snsPeers netState))
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
                  "Peers: " ++ show (snsPeerCount netState),
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
update :: IORef PlayerState -> IORef SharedNetState -> Float -> DemoState -> IO DemoState
update localStateRef _sharedNetRef dt state = do
  -- Update local physics
  let localState' = applyInput dt (dsLocalInput state) (dsLocalState state)

  -- Share with network thread
  writeIORef localStateRef localState'

  pure state {dsLocalState = localState'}
