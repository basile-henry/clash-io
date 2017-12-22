{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}

module Clash.IO.Util
  ( binaryToBCD
  ) where

import           Clash.Prelude
  (type (*), Bit, Signal, System, SystemClockReset, Unsigned, Vec, mealy)

data ToBCD n m = ToBCD
  { state    :: Vec (m * 4) Bit
  , incoming :: Unsigned n
  }

newtype ToBCDPipeline n m =
  ToBCDPipeline { unPipeline :: Vec n (ToBCD n m) }

binaryToBCD
  :: SystemClockReset
  => Signal System (Unsigned n)
  -> Signal System (Vec m (Unsigned 4))
binaryToBCD = mealy initialState update
  where
    initialState = undefined
    update = undefined


