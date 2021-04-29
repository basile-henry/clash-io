{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoStarIsType        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.IO.Util
  ( binaryToBCD
  ) where

import           Clash.Prelude

data ToBCD n m = ToBCD
  { state    :: Vec (m * 4) Bit
  , incoming :: Unsigned n
  } deriving (Generic, NFDataX)

newtype ToBCDPipeline n m =
  ToBCDPipeline { unPipeline :: Vec n (ToBCD n m) }
  deriving (Generic, NFDataX)

binaryToBCD
  :: forall n m
   . (KnownNat n, KnownNat m, SystemClockResetEnable)
  => Signal System (Unsigned n)
  -> Signal System (Vec m (Unsigned 4))
binaryToBCD = mealy update initialState
  where
    initialState :: ToBCDPipeline n m
    initialState = undefined

    update = undefined
