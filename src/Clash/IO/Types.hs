{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module Clash.IO.Types
  ( Input (..)
  , Output (..)
  , Pos (..)
  , SevenSegment (..)
  , sevenSegmentsHex
  ) where

import           Clash.Prelude   (Bit, Index, Unsigned, Vec)
import           Control.DeepSeq (NFData)
import           Data.Word       (Word8)
import           GHC.Generics    (Generic)

data Input width height =
  Input
    { up       :: Bool
      -- ^ Up arrow key pressed
    , down     :: Bool
      -- ^ Down arrow key pressed
    , left     :: Bool
      -- ^ Left arrow key pressed
    , right    :: Bool
      -- ^ Right arrow key pressed
    , space    :: Bool
      -- ^ Space key pressed
    , pixelPos :: Pos width height
      -- ^ Current pixel position
    }
  deriving (Show, Eq, Generic, NFData)

data Output n =
  Output
    { red           :: Word8
    , green         :: Word8
    , blue          :: Word8
    , sevenSegments :: Vec n SevenSegment
    }
  deriving (Show, Eq, Generic, NFData)

data Pos width height =
  Pos
    { x :: Index width
    , y :: Index height
    }
  deriving (Show, Eq, Generic, NFData)

data SevenSegment = SevenSegment
  { top         :: Bit
  , topRight    :: Bit
  , topLeft     :: Bit
  , middle      :: Bit
  , bottomRight :: Bit
  , bottomLeft  :: Bit
  , bottom      :: Bit
  } deriving (Show, Eq, Generic, NFData)

sevenSegmentsHex :: Unsigned 4 -> SevenSegment
sevenSegmentsHex = \case
  0x0 -> SevenSegment 1 1 1 0 1 1 1
  0x1 -> SevenSegment 0 1 0 0 0 1 0
  0x2 -> SevenSegment 1 1 0 1 0 1 1
  0x3 -> SevenSegment 1 1 0 1 1 0 1
  0x4 -> SevenSegment 0 1 1 1 1 0 0
  0x5 -> SevenSegment 1 0 1 1 1 0 1
  0x6 -> SevenSegment 1 1 0 1 1 1 1
  0x7 -> SevenSegment 1 1 0 0 1 0 0
  0x8 -> SevenSegment 1 1 1 1 1 1 1
  0x9 -> SevenSegment 1 1 1 1 1 0 1
  0xA -> SevenSegment 1 1 1 1 1 1 0
  0xB -> SevenSegment 0 0 1 1 1 1 1
  0xC -> SevenSegment 1 0 1 0 0 1 1
  0xD -> SevenSegment 0 1 0 1 1 1 1
  0xE -> SevenSegment 1 0 1 1 0 1 1
  0xF -> SevenSegment 1 0 1 1 0 1 0
