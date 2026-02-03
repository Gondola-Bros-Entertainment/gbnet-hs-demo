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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word16)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Game

-- | Window dimensions (integer version for gloss).
windowWidthInt, windowHeightInt :: Int
windowWidthInt = round windowWidth
windowHeightInt = round windowHeight

-- | Demo state.
data DemoState = DemoState
  { dsLocalState :: !PlayerState,
    dsLocalInput :: !PlayerInput,
    dsPeers :: !(Map Word16 PlayerState),  -- port -> state
    dsLocalPort :: !Word16
  }
  deriving (Show)

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
  [ makeColorI 50 200 50 255,   -- Green (local player)
    makeColorI 255 50 50 255,   -- Red
    makeColorI 50 50 255 255,   -- Blue
    makeColorI 255 255 50 255,  -- Yellow
    makeColorI 255 50 255 255,  -- Magenta
    makeColorI 50 255 255 255,  -- Cyan
    makeColorI 255 128 50 255,  -- Orange
    makeColorI 128 50 255 255   -- Purple
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

main :: IO ()
main = do
  args <- getArgs

  let (localPort, _connectTo) = parseArgs args

  putStrLn $ "gbnet-hs P2P demo"
  putStrLn $ "Binding to port: " ++ show localPort
  putStrLn "WASD to move"
  putStrLn "[Placeholder: gbnet peer not yet integrated]"

  -- TODO: Initialize gbnet peer
  -- let addr = SockAddrInet (fromIntegral localPort) 0
  -- result <- newPeer addr defaultNetworkConfig =<< getMonoTime
  -- case result of
  --   Left err -> error $ "Failed to create peer: " ++ show err
  --   Right peer -> do
  --     peer' <- case connectTo of
  --       Nothing -> pure peer
  --       Just targetPort -> do
  --         let targetAddr = SockAddrInet (fromIntegral targetPort) (tupleToHostAddress (127,0,0,1))
  --         peerConnect (peerIdFromAddr targetAddr) =<< getMonoTime $ peer
  --     runDemo peer' localPort

  playIO
    (InWindow ("gbnet-demo :" ++ show localPort) (windowWidthInt, windowHeightInt) (100, 100))
    (makeColorI 25 25 38 255)
    60
    (initialDemoState localPort)
    render
    handleInput
    update

-- | Parse command line args: [localPort] [connectToPort]
parseArgs :: [String] -> (Word16, Maybe Word16)
parseArgs [] = (defaultPort, Nothing)
parseArgs [p] = (maybe defaultPort id (readMaybe p), Nothing)
parseArgs (p:t:_) = (maybe defaultPort id (readMaybe p), readMaybe t)

-- | Render the demo.
render :: DemoState -> IO Picture
render state = pure $ Pictures $
  -- Draw local player (green)
  [ drawPlayer (head playerColors) (dsLocalState state) True
  ] ++
  -- Draw peer players
  [ drawPlayer (playerColors !! (i `mod` length playerColors)) ps False
  | (i, (_, ps)) <- zip [1..] (Map.toList (dsPeers state))
  ] ++
  -- Draw HUD
  [ Translate hudLeftMargin hudTopOffset $ Scale 0.12 0.12 $ Color white $
      Text $ "Port: " ++ show (dsLocalPort state)
  , Translate hudLeftMargin (hudTopOffset - hudLineSpacing) $ Scale 0.10 0.10 $ Color (greyN 0.5) $
      Text $ "Peers: " ++ show (Map.size (dsPeers state))
  , Translate hudLeftMargin (hudTopOffset - hudLineSpacing * 2) $ Scale 0.10 0.10 $ Color (greyN 0.5) $
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
      [ Color col $ rectangleSolid playerRenderSize playerRenderSize
      , if isLocal
          then Color white $ rectangleWire
                 (playerRenderSize + selectionOutlineThickness)
                 (playerRenderSize + selectionOutlineThickness)
          else Blank
      ]

-- | Handle input events.
handleInput :: Event -> DemoState -> IO DemoState
handleInput event state =
  let input = dsLocalInput state
      input' = case event of
        EventKey (Char 'w') Down _ _ -> input { piUp = True }
        EventKey (Char 'w') Up _ _   -> input { piUp = False }
        EventKey (Char 's') Down _ _ -> input { piDown = True }
        EventKey (Char 's') Up _ _   -> input { piDown = False }
        EventKey (Char 'a') Down _ _ -> input { piLeft = True }
        EventKey (Char 'a') Up _ _   -> input { piLeft = False }
        EventKey (Char 'd') Down _ _ -> input { piRight = True }
        EventKey (Char 'd') Up _ _   -> input { piRight = False }
        _ -> input
   in pure state { dsLocalInput = input' }

-- | Update the demo state.
update :: Float -> DemoState -> IO DemoState
update dt state = do
  -- TODO: Network update
  -- (events, peer') <- peerUpdate now peer
  -- forM_ events $ \case
  --   PeerConnected pid _ -> putStrLn $ "Peer connected: " ++ show pid
  --   PeerDisconnected pid _ -> putStrLn $ "Peer disconnected: " ++ show pid
  --   PeerMessage pid _ dat -> updatePeerState pid dat
  --   _ -> pure ()
  --
  -- Broadcast local state
  -- let encoded = serialize (dsLocalState state)
  -- peerBroadcast 0 encoded Nothing now peer'

  -- Local physics
  let localState' = applyInput dt (dsLocalInput state) (dsLocalState state)
  pure state { dsLocalState = localState' }
