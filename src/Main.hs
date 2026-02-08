{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Main
-- Description : P2P demo for gbnet-hs with Gloss rendering
--
-- Single-threaded architecture: networking runs inside Gloss's update
-- callback via 'runNetT'. Pure event processing via 'processEvents'.
--
-- Usage:
--   gbnet-demo              -- Binds to port 7777
--   gbnet-demo 7778         -- Binds to port 7778
--   gbnet-demo 7778 7777    -- Binds to 7778, connects to peer on 7777
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.IORef
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word16)
import qualified Data.ByteString as BS
import Foreign.Storable (sizeOf)
import GBNet
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

-- | Demo state (unified, single-threaded).
data DemoState = DemoState
  { dsLocalState :: !PlayerState,
    dsLocalInput :: !PlayerInput,
    dsLocalPort :: !Word16,
    dsPeer :: !NetPeer,
    dsNet :: !NetState,
    dsPeers :: !(Map PeerId PlayerState)
  }

-- | Initial demo state.
initialDemoState :: Word16 -> NetPeer -> NetState -> DemoState
initialDemoState port peer netSt =
  DemoState
    { dsLocalState = defaultPlayerState,
      dsLocalInput = PlayerInput False False False False,
      dsLocalPort = port,
      dsPeer = peer,
      dsNet = netSt,
      dsPeers = Map.empty
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

  let bindAddr = anyAddr localPort
      -- Channel 0: Unreliable for position updates (fire and forget)
      -- Channel 1: Reliable for mesh peer introduction
      config =
        defaultNetworkConfig
          { ncChannelConfigs = [unreliableConfig, reliableOrderedConfig],
            ncSendRate = 1000.0,
            ncMaxPacketRate = 1000.0
          }

  now <- getMonoTimeIO
  peerResult <- newPeer bindAddr config now
  case peerResult of
    Left err -> do
      putStrLn $ "Failed to create peer: " ++ show err
      exitFailure
    Right (peer, sock) -> do
      netSt <- newNetState sock (peerLocalAddr peer)

      let peer' = case connectTo of
            Nothing -> peer
            Just targetPort ->
              let targetAddr = localhost targetPort
                  targetPid = peerIdFromAddr targetAddr
               in peerConnect targetPid now peer

      -- Single IORef for shutdown cleanup (playIO doesn't return final state)
      shutdownRef <- newIORef (peer', netSt)

      playIO
        (InWindow ("gbnet-demo :" ++ show localPort) (windowWidthInt, windowHeightInt) (100, 100))
        (makeColorI 25 25 38 255)
        tickRateHz
        (initialDemoState localPort peer' netSt)
        render
        handleInput
        (update shutdownRef)
        `finally` shutdown shutdownRef

-- | Update: physics, network tick, pure event processing, logging.
update :: IORef (NetPeer, NetState) -> Float -> DemoState -> IO DemoState
update shutdownRef dt state = do
  -- Apply local physics
  let localState' = applyInput dt (dsLocalInput state) (dsLocalState state)
      encoded = serialize localState'

  -- Network tick: receive, process, broadcast, send
  ((events, peer'), netSt') <-
    runNetT (peerTick [(stateChannel, encoded)] (dsPeer state)) (dsNet state)

  -- Pure event processing
  now <- getMonoTimeIO
  let (peers', peer'') = processEvents now events (dsPeers state) peer'

  -- Log notable events
  mapM_ logEvent events

  -- Update shutdown ref for cleanup
  writeIORef shutdownRef (peer'', netSt')

  pure
    state
      { dsLocalState = localState',
        dsPeer = peer'',
        dsNet = netSt',
        dsPeers = peers'
      }

-- | Pure fold over peer events, updating the peers map and peer state.
processEvents ::
  MonoTime ->
  [PeerEvent] ->
  Map PeerId PlayerState ->
  NetPeer ->
  (Map PeerId PlayerState, NetPeer)
processEvents now events peers peer =
  foldl' (processEvent now) (peers, peer) events

-- | Handle a single peer event (pure).
processEvent ::
  MonoTime ->
  (Map PeerId PlayerState, NetPeer) ->
  PeerEvent ->
  (Map PeerId PlayerState, NetPeer)
processEvent now (peers, peer) = \case
  PeerConnected pid _dir ->
    -- Send existing peer list to new peer for mesh introduction
    let existingPeers = filter (/= pid) (peerConnectedIds peer)
        peerAddrs = mapMaybe peerIdToPeerAddr existingPeers
        encoded = encodePeerAddrs peerAddrs
        peer' = case peerSend pid meshChannel encoded now peer of
          Left _ -> peer
          Right p -> p
     in (Map.insert pid defaultPlayerState peers, peer')
  PeerDisconnected pid _reason ->
    (Map.delete pid peers, peer)
  PeerMessage _pid ch dat
    | ch == meshChannel ->
        case decodePeerAddrs dat of
          Left _ -> (peers, peer)
          Right addrs ->
            let known = peerConnectedIds peer
                localAddr = peerLocalAddr peer
                newPeerIds =
                  [ peerIdFromAddr sa
                  | addr <- addrs,
                    let sa = peerAddrToSockAddr addr,
                    peerIdFromAddr sa `notElem` known,
                    sa /= localAddr
                  ]
                peer' = foldl' (\p newPid -> peerConnect newPid now p) peer newPeerIds
             in (peers, peer')
  PeerMessage pid _ch dat ->
    case deserialize dat of
      Left _ -> (peers, peer)
      Right ps -> (Map.insert pid ps peers, peer)
  PeerMigrated oldPid newPid ->
    case Map.lookup oldPid peers of
      Nothing -> (peers, peer)
      Just ps -> (Map.insert newPid ps $ Map.delete oldPid peers, peer)

-- | Log notable peer events.
logEvent :: PeerEvent -> IO ()
logEvent = \case
  PeerConnected pid dir ->
    putStrLn $ "Connected: " ++ show pid ++ " (" ++ show dir ++ ")"
  PeerDisconnected pid reason ->
    putStrLn $ "Disconnected: " ++ show pid ++ " (" ++ show reason ++ ")"
  PeerMigrated oldPid newPid ->
    putStrLn $ "Migrated: " ++ show oldPid ++ " -> " ++ show newPid
  _ -> pure ()

-- | Shutdown: send disconnect packets and wait for delivery.
shutdown :: IORef (NetPeer, NetState) -> IO ()
shutdown ref = do
  (peer, netSt) <- readIORef ref
  _ <- runNetT (peerShutdownM peer) netSt
  putStrLn "Network shutdown: disconnect packets sent."
  threadDelay shutdownDelayUs
  putStrLn "Shutdown complete."

-- | Render the demo.
render :: DemoState -> IO Picture
render state =
  pure $
    Pictures $
      -- Draw local player (green)
      [ drawPlayer localPlayerColor (dsLocalState state) True
      ]
        ++
        -- Draw peer players
        [ drawPlayer col ps False
        | (col, (_, ps)) <- zip (cycle remotePlayerColors) (Map.toList (dsPeers state))
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
                  "Peers: " ++ show (Map.size (dsPeers state)),
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
    addrSize = sizeOf (PeerAddr 0 0)
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
