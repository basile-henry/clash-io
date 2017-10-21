{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
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
    (Index, KnownNat, SNat (..), Signal, System, SystemClockReset,
    simulate_lazy, snatToNum)
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad           (forM_, unless)
import           Data.Word               (Word8)
import           Foreign.C.Types         (CInt)
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
  deriving (Show, Eq)

data Output =
  Output
    { red   :: Word8
    , green :: Word8
    , blue  :: Word8
    }
  deriving (Show, Eq)

data Pos width height =
  Pos
    { x :: Index width
    , y :: Index height
    }
  deriving (Show, Eq)

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
    RGB888 -> do
      inChan  <- newChan
      outChan <- newChan
      forkIO (runCircuit circuit inChan outChan)
      runSdl window inChan outChan
    _      -> error "Window pixel format unsupported"

runCircuit
  :: forall w h
   . (KnownNat w, KnownNat h)
  => (SystemClockReset => Signal System (Input w h)-> Signal System Output)
  -> Chan (Input w h)
  -> Chan Output
  -> IO ()
runCircuit circuit inChan outChan = do
  inputs <- getChanContents inChan
  let inputs' = (inputFrame $ const False) ++ inputs
      outputs = simulate_lazy circuit inputs'
  mapM_ (writeChan outChan) outputs

runSdl
  :: forall w h
   . (KnownNat w, KnownNat h)
  => Window
  -> Chan (Input w h)
  -> Chan Output
  -> IO ()
runSdl window inChan outChan = do
  outputs <- getChanContents outChan
  sdlPump outputs
  where
    size :: Int
    size = snatToNum (SNat @w) * snatToNum (SNat @h)

    sdlPump :: [Output] -> IO ()
    sdlPump outputs = do
      let (currentO, futureO) = splitAt size outputs
      drawFrame currentO
      threadDelay 16000 -- 60 fps
      pumpEvents
      keyboardState <- getKeyboardState
      -- Quite on 'Esc'
      unless (keyboardState ScancodeEscape) $ do
        let inputs = inputFrame keyboardState
        mapM_ (writeChan inChan) inputs
        sdlPump futureO

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
