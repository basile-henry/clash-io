{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chip8
  ( topEntity
  ) where

import           Clash.IO
import           Clash.Prelude

type Width  = 64
type Height = 32

topEntity
  :: SystemClockReset
  => Signal System (Input Width Height)
  -> Signal System (Output 0)
topEntity = undefined
