{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chip8
  ( topEntity
  ) where

import           Clash.IO
import           Clash.Prelude

import           Chip8.Types

topEntity
  :: SystemClockReset
  => Signal System (Keyboard, VGAInput Width Height)
  -> Signal System (VGAOutput 1)
topEntity = undefined
