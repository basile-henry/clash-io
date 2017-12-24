{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- TODO remove
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

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
  :: forall n m
   . SystemClockReset
  => Signal System (Unsigned n)
  -> Signal System (Vec m (Unsigned 4))
binaryToBCD = mealy update initialState
  where
    initialState :: ToBCDPipeline n m
    initialState = undefined

    update = undefined
