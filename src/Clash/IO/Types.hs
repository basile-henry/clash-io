{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module Clash.IO.Types
  ( Pos (..)
  , VGAInput (..)
  , VGAOutput (..)
  , Keyboard (..)
  , defaultKeyboard
  , SevenSegment (..)
  , hexToSevenSegments
  ) where

import           Clash.Prelude   (Bit, Index, Unsigned)
import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

data Pos width height = Pos
  { x :: Index width
  , y :: Index height
  } deriving (Show, Eq, Generic, NFData)

newtype VGAInput width height = VGAInput
  { pixelPos :: Maybe (Pos width height)
  } deriving (Show, Eq, Generic, NFData)

data VGAOutput n = VGAOutput
  { red           :: Unsigned n
  , green         :: Unsigned n
  , blue          :: Unsigned n
  } deriving (Show, Eq, Generic, NFData)

data Keyboard = Keyboard
  { keyUp    :: Bool
  , keyDown  :: Bool
  , keyLeft  :: Bool
  , keyRight :: Bool
  , keySpace :: Bool
  , keyA     :: Bool
  , keyB     :: Bool
  , keyC     :: Bool
  , keyD     :: Bool
  , keyE     :: Bool
  , keyF     :: Bool
  , keyG     :: Bool
  , keyH     :: Bool
  , keyI     :: Bool
  , keyJ     :: Bool
  , keyK     :: Bool
  , keyL     :: Bool
  , keyM     :: Bool
  , keyN     :: Bool
  , keyO     :: Bool
  , keyP     :: Bool
  , keyQ     :: Bool
  , keyR     :: Bool
  , keyS     :: Bool
  , keyT     :: Bool
  , keyU     :: Bool
  , keyV     :: Bool
  , keyW     :: Bool
  , keyX     :: Bool
  , keyY     :: Bool
  , keyZ     :: Bool
  , key0     :: Bool
  , key1     :: Bool
  , key2     :: Bool
  , key3     :: Bool
  , key4     :: Bool
  , key5     :: Bool
  , key6     :: Bool
  , key7     :: Bool
  , key8     :: Bool
  , key9     :: Bool
  } deriving (Show, Eq, Generic, NFData)

defaultKeyboard :: Keyboard
defaultKeyboard =
  Keyboard
    False False False False False False False False False False False False
    False False False False False False False False False False False False
    False False False False False False False False False False False False
    False False False False False

data SevenSegment = SevenSegment
  { top         :: Bit
  , topRight    :: Bit
  , topLeft     :: Bit
  , middle      :: Bit
  , bottomRight :: Bit
  , bottomLeft  :: Bit
  , bottom      :: Bit
  } deriving (Show, Eq, Generic, NFData)

hexToSevenSegments :: Unsigned 4 -> SevenSegment
hexToSevenSegments = \case
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
  _   -> error "Impossible case"
