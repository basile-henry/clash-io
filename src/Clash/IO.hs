{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Clash.IO
  ( Input (..)
  , Output (..)
  , Pos (..)
  , run
  ) where

import           Clash.Prelude
    (Index, KnownNat, SNat (..), Signal, System, SystemClockReset, simulate,
    snatToNum)
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.DeepSeq         (NFData)
import           Control.Monad           (forM_, unless)
import           Data.Word               (Word8)
import           Foreign.C.Types         (CInt)
import           GHC.Generics            (Generic)
import           SDL

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

data Output =
  Output
    { red   :: Word8
    , green :: Word8
    , blue  :: Word8
    }
  deriving (Show, Eq, Generic, NFData)

data Pos width height =
  Pos
    { x :: Index width
    , y :: Index height
    }
  deriving (Show, Eq, Generic, NFData)

coords
  :: forall w h a
   . (Num a, Enum a)
  => SNat w
  -> SNat h
  -> [(a, a)]
coords SNat SNat = do
  x <- [0..(snatToNum $ SNat @w) - 1]
  y <- [0..(snatToNum $ SNat @h) - 1]
  return (x, y)

run
  :: (KnownNat w, KnownNat h)
  => (SystemClockReset => Signal System (Input w h) -> Signal System Output)
  -> IO ()
run circuit = do
  initializeAll
  window <- createWindow "Clash IO"
          $ defaultWindow { windowInitialSize = V2 512 256 }
  getWindowPixelFormat window >>= \case
    RGB888 -> runSdl window circuit
    _      -> error "Window pixel format unsupported"

runSdl
  :: forall w h
   . (KnownNat w, KnownNat h)
  => Window
  -> (SystemClockReset => Signal System (Input w h)-> Signal System Output)
  -> IO ()
runSdl window circuit = mdo
  let inputs' = (inputFrame $ const False) ++ inputs
      outputs = simulate circuit inputs'
  inputs <- sdlPump outputs
  pure ()
  where
    size :: Int
    size = snatToNum (SNat @w) * snatToNum (SNat @h)

    sdlPump :: [Output] -> IO [Input w h]
    sdlPump outputs = do
      let (currentO, futureO) = splitAt size outputs
      drawFrame currentO
      threadDelay 16000 -- 60 fps
      pumpEvents
      keyboardState <- getKeyboardState
      -- Quite on 'Esc'
      if (keyboardState ScancodeEscape)
        then do
          let inputs = inputFrame keyboardState
          (inputs ++) <$> sdlPump futureO
        else pure []

    drawFrame :: [Output] -> IO ()
    drawFrame outputs = do
      V2 w h <- get $ windowSize window
      let dw = div w $ snatToNum $ SNat @w
          dh = div h $ snatToNum $ SNat @h
      surface <- getWindowSurface window
      updatePixels (dw, dh) outputs surface
      updateWindowSurface window

    updatePixels :: (CInt, CInt) -> [Output] -> Surface -> IO ()
    updatePixels (dw, dh) outputs surface = do
      forM_ (zip outputs $ coords (SNat @w) (SNat @h)) $
        \(Output {..}, (x, y)) ->
          surfaceFillRect surface
            (Just $ Rectangle (P $ V2 (x * dw) (y * dh)) (V2 dw dh))
            $ V4 red blue green 0

inputFrame
  :: forall w h
   . (KnownNat w, KnownNat h)
  => (Scancode -> Bool)
  -> [Input w h]
inputFrame pressed =
  map (\(x, y) -> Input up down left right space
                $ Pos (fromIntegral x) (fromIntegral y))
      $ coords (SNat @w) (SNat @h)
  where
    up    = pressed ScancodeUp
    down  = pressed ScancodeDown
    left  = pressed ScancodeLeft
    right = pressed ScancodeRight
    space = pressed ScancodeSpace
