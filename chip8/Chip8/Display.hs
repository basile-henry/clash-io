{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chip8.Display where

import           Clash.Prelude

import           Chip8.Types

type PixelClock = 'Dom "pixel" 1000

draw
  :: SystemClockReset
  => Signal System (Maybe (Position, Nibble))
  -> Signal PixelClock (Maybe Position)
  -> (Signal System Bool) -- System PixelClock VGAOut
draw = undefined
