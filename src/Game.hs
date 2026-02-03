{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Game
-- Description : Shared game state types for P2P demo
--
-- Common types used by the demo.
module Game
  ( -- * Game constants
    tickRateHz,
    defaultPort,
    maxPeers,
    windowWidth,
    windowHeight,
    playerRadius,
    moveSpeed,

    -- * Player state
    PlayerState (..),
    PlayerInput (..),
    defaultPlayerState,

    -- * Movement
    applyInput,
    clamp,
  )
where

import Data.Word (Word16)
import GBNet.Serialize.TH (deriveNetworkSerialize)

-- | Tick rate in Hz.
tickRateHz :: Int
tickRateHz = 60

-- | Default port to bind.
defaultPort :: Word16
defaultPort = 7777

-- | Maximum peers in demo.
maxPeers :: Int
maxPeers = 8

-- | Window dimensions.
windowWidth, windowHeight :: Float
windowWidth = 800.0
windowHeight = 600.0

-- | Player size for collision bounds.
playerRadius :: Float
playerRadius = 16.0

-- | Movement speed in units per second.
moveSpeed :: Float
moveSpeed = 200.0

-- | Player state synchronized over the network.
data PlayerState = PlayerState
  { psX :: !Float,
    psY :: !Float,
    psVelX :: !Float,
    psVelY :: !Float
  }
  deriving (Eq, Show)

-- | Default spawn state.
defaultPlayerState :: PlayerState
defaultPlayerState =
  PlayerState
    { psX = 400.0,
      psY = 300.0,
      psVelX = 0.0,
      psVelY = 0.0
    }

-- | Input from local player.
data PlayerInput = PlayerInput
  { piLeft :: !Bool,
    piRight :: !Bool,
    piUp :: !Bool,
    piDown :: !Bool
  }
  deriving (Eq, Show)

-- | Apply player input to state.
applyInput :: Float -> PlayerInput -> PlayerState -> PlayerState
applyInput dt input ps =
  let dx =
        (if piRight input then moveSpeed else 0)
          - (if piLeft input then moveSpeed else 0)
      dy =
        (if piDown input then moveSpeed else 0)
          - (if piUp input then moveSpeed else 0)
      newX = clamp playerRadius (windowWidth - playerRadius) (psX ps + dx * dt)
      newY = clamp playerRadius (windowHeight - playerRadius) (psY ps + dy * dt)
   in ps {psX = newX, psY = newY, psVelX = dx, psVelY = dy}

-- | Clamp a value to a range.
clamp :: Float -> Float -> Float -> Float
clamp lo hi x = max lo (min hi x)

-- Generate BitSerialize instances
deriveNetworkSerialize ''PlayerState
deriveNetworkSerialize ''PlayerInput
