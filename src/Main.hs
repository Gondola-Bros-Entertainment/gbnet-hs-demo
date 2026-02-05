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
import Control.Exception (SomeException, catch, finally)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word16)
import qualified Data.ByteString as BS
import GBNet
import GBNet.Peer (npLocalAddr)
import Game hiding (defaultPort)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
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
stateChannel :: ChannelId
stateChannel = ChannelId 0

-- | Channel for mesh peer introduction (reliable).
meshChannel :: ChannelId
meshChannel = ChannelId 1

-- | Network tick rate in microseconds.
netTickUs :: Int
netTickUs = 16667 -- ~60Hz

-- | Shutdown grace period in microseconds (lets disconnect packets send).
shutdownDelayUs :: Int
shutdownDelayUs = 100000 -- 100ms

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
  let bindAddr = anyAddr localPort
      -- Channel 0: Unreliable for position updates (fire and forget)
      -- Channel 1: Reliable for mesh peer introduction
      unreliableChannel = defaultChannelConfig {ccDeliveryMode = Unreliable}
      reliableChannel = defaultChannelConfig {ccDeliveryMode = ReliableOrdered}
      config =
        defaultNetworkConfig
          { ncChannelConfigs = [unreliableChannel, reliableChannel],
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
              let targetAddr = localhost targetPort
                  targetPid = peerIdFromAddr targetAddr
               in peerConnect targetPid now peer

      -- Shared state between network thread and render thread
      localStateRef <- newIORef defaultPlayerState
      sharedNetRef <- newIORef (SharedNetState Map.empty 0)
      peerRef <- newIORef peer'
      shutdownRef <- newIORef False

      -- Start network thread
      _ <- forkIO $ networkThread localStateRef sharedNetRef peerRef netState shutdownRef

      -- Run gloss on main thread; signal shutdown when window closes
      playIO
        (InWindow ("gbnet-demo :" ++ show localPort) (windowWidthInt, windowHeightInt) (100, 100))
        (makeColorI 25 25 38 255)
        60
        (initialDemoState localPort)
        (render sharedNetRef)
        handleInput
        (update localStateRef sharedNetRef)
        `finally` do
          writeIORef shutdownRef True
          threadDelay shutdownDelayUs
          putStrLn "Shutdown complete."

-- | Network thread wrapper that handles exceptions.
networkThread :: IORef PlayerState -> IORef SharedNetState -> IORef NetPeer -> NetState -> IORef Bool -> IO ()
networkThread localStateRef sharedNetRef peerRef netState shutdownRef =
  evalNetT (networkLoop localStateRef sharedNetRef peerRef shutdownRef) netState `catch` handleEx
  where
    handleEx :: SomeException -> IO ()
    handleEx e = putStrLn $ "NETWORK THREAD CRASHED: " ++ show e

-- | Network loop using the new polymorphic API.
--
-- Runs inside NetT IO, using peerTick for clean receive/process/send.
-- Checks the shutdown ref each tick and sends disconnect packets before exiting.
networkLoop :: IORef PlayerState -> IORef SharedNetState -> IORef NetPeer -> IORef Bool -> NetT IO ()
networkLoop localStateRef sharedNetRef peerRef shutdownRef = go Map.empty
  where
    go peers = do
      shutdown <- liftIO $ readIORef shutdownRef
      if shutdown
        then do
          peer <- liftIO $ readIORef peerRef
          peerShutdownM peer
          liftIO $ putStrLn "Network shutdown: disconnect packets sent."
        else do
          peer <- liftIO $ readIORef peerRef
          localState <- liftIO $ readIORef localStateRef
          now <- liftIO getMonoTimeIO

          -- Encode local state to broadcast
          let encoded = serialize localState

          -- Single call: receive, process, broadcast, send
          (events, peer') <- peerTick [(stateChannel, encoded)] peer

          -- Handle events, updating both peer map and peer state (for mesh sends)
          (peers', peer'') <- liftIO $ handleEvents peers peer' now events

          -- Update peer ref
          liftIO $ writeIORef peerRef peer''

          -- Update shared state for render thread
          liftIO $ writeIORef sharedNetRef $ SharedNetState peers' (Map.size peers')

          liftIO $ threadDelay netTickUs
          go peers'

    handleEvents peerMap peer now = foldM (handleEvent now) (peerMap, peer)

    handleEvent now (peerMap, peer) = \case
      PeerConnected pid dir -> do
        putStrLn $ "Connected: " ++ show pid ++ " (" ++ show dir ++ ")"
        -- Send existing peer list to the new peer for mesh introduction
        let existingPeers = filter (/= pid) (peerConnectedIds peer)
            peerAddrs = mapMaybe peerIdToPeerAddr existingPeers
            encoded = encodePeerAddrs peerAddrs
            peer' = case peerSend pid meshChannel encoded now peer of
              Left _ -> peer
              Right p -> p
        pure (Map.insert pid defaultPlayerState peerMap, peer')
      PeerDisconnected pid reason -> do
        putStrLn $ "Disconnected: " ++ show pid ++ " (" ++ show reason ++ ")"
        pure (Map.delete pid peerMap, peer)
      PeerMessage pid ch dat
        | ch == meshChannel ->
            -- Mesh peer introduction: deserialize peer addresses and connect
            case decodePeerAddrs dat of
              Left err -> do
                putStrLn $ "Mesh deserialize error from " ++ show pid ++ ": " ++ err
                pure (peerMap, peer)
              Right addrs -> do
                let known = peerConnectedIds peer
                    localAddr = npLocalAddr peer
                    newPeers =
                      [ peerIdFromAddr sa
                      | addr <- addrs,
                        let sa = peerAddrToSockAddr addr,
                        peerIdFromAddr sa `notElem` known,
                        sa /= localAddr
                      ]
                    peer' = foldl' (\p newPid -> peerConnect newPid now p) peer newPeers
                if null newPeers
                  then pure ()
                  else putStrLn $ "Mesh: connecting to " ++ show (length newPeers) ++ " new peer(s)"
                pure (peerMap, peer')
      PeerMessage pid _ch dat ->
        case deserialize dat of
          Left err -> do
            putStrLn $ "Deserialize error from " ++ show pid ++ ": " ++ err
            pure (peerMap, peer)
          Right ps -> pure (Map.insert pid ps peerMap, peer)
      PeerMigrated oldPid newPid -> do
        putStrLn $ "Migrated: " ++ show oldPid ++ " -> " ++ show newPid
        pure (maybe (peerMap, peer) (\ps -> (Map.insert newPid ps $ Map.delete oldPid peerMap, peer)) (Map.lookup oldPid peerMap))

-- | Encode a list of PeerAddr for mesh introduction.
encodePeerAddrs :: [PeerAddr] -> BS.ByteString
encodePeerAddrs addrs =
  let count = fromIntegral (length addrs) :: Word16
   in serialize count <> mconcat (map serialize addrs)

-- | Decode a list of PeerAddr from mesh introduction message.
decodePeerAddrs :: BS.ByteString -> Either String [PeerAddr]
decodePeerAddrs bs
  | BS.length bs < 2 = Left "buffer too short for count"
  | otherwise = do
      let countBs = BS.take 2 bs
          addrsBs = BS.drop 2 bs
      count <- deserialize countBs :: Either String Word16
      decodeN (fromIntegral count :: Int) addrsBs []
  where
    addrSize = sizeOf (undefined :: PeerAddr)
    decodeN 0 _ acc = Right (reverse acc)
    decodeN n remaining acc
      | BS.length remaining < addrSize = Left "buffer too short for PeerAddr"
      | otherwise = do
          addr <- deserialize (BS.take addrSize remaining)
          decodeN (n - 1) (BS.drop addrSize remaining) (addr : acc)

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
