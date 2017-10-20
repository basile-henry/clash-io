{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Snake
  ( topEntity
  ) where

import           Clash.IO
import           Clash.Prelude

topEntity
  :: SystemClockReset
  => Signal System (Input 64 32)
  -> Signal System Output
topEntity = fmap stripes
  where
    stripes Input{..}
      | testX
      , testY     = Output 255   0   0
      | testX     = Output   0 255   0
      | testY     = Output   0   0 255
      | otherwise = Output 255 255 255
      where
        testX = testBit (pack $ x pixelPos) 4
        testY = testBit (pack $ y pixelPos) 4
