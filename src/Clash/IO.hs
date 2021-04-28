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
  ( runVGA
  , module Clash.IO.Types
  , module Clash.IO.Util
  ) where

import           Clash.Prelude
  (KnownNat, SNat (..), Signal, System, SystemClockResetEnable, simulate,
  snatToNum)
import           Control.Concurrent
import           Control.Monad      (forM_, unless)
import           Foreign.C.Types    (CInt)
import           SDL

import           Clash.IO.Types
import           Clash.IO.Util

coords
  :: forall w h a b
   . (Num a, Enum a, Num b, Enum b)
  => SNat w
  -> SNat h
  -> [(a, b)]
coords SNat SNat = do
  x <- [0..(snatToNum $ SNat @w) - 1]
  y <- [0..(snatToNum $ SNat @h) - 1]
  return (fromInteger x, fromInteger y)

runVGA
  :: (KnownNat w, KnownNat h, KnownNat n)
  => ( SystemClockResetEnable
    => Signal System (Keyboard, VGAInput w h)
    -> Signal System (VGAOutput n))
  -> IO ()
runVGA circuit = do
  initializeAll
  window <- createWindow "Clash IO"
          $ defaultWindow { windowInitialSize = V2 512 256 }
  getWindowPixelFormat window >>= \case
    RGB888 -> do
      inChan  <- newChan
      outChan <- newChan
      _ <- forkIO (runVGACircuit circuit inChan outChan)
      runVGASdl window inChan outChan
    _      -> error "Window pixel format unsupported"

runVGACircuit
  :: forall w h n
   . (KnownNat w, KnownNat h, KnownNat n)
  => ( SystemClockResetEnable
    => Signal System (Keyboard, VGAInput w h)
    -> Signal System (VGAOutput n))
  -> Chan (Keyboard, VGAInput w h)
  -> Chan (VGAOutput n)
  -> IO ()
runVGACircuit circuit inChan outChan = do
  inputs <- getChanContents inChan
  let inputs' = (inputFrame $ const False) ++ inputs
      outputs = simulate circuit inputs'
  mapM_ (writeChan outChan) outputs

runVGASdl
  :: forall w h n
   . (KnownNat w, KnownNat h, KnownNat n)
  => Window
  -> Chan (Keyboard, VGAInput w h)
  -> Chan (VGAOutput n)
  -> IO ()
runVGASdl window inChan outChan = do
  outputs <- getChanContents outChan
  sdlPump outputs
  where
    size :: Int
    size = snatToNum (SNat @w) * snatToNum (SNat @h)

    sdlPump :: [VGAOutput n] -> IO ()
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

    drawFrame :: [VGAOutput n] -> IO ()
    drawFrame outputs = do
      V2 w h <- get $ windowSize window
      let dw = div w $ snatToNum $ SNat @w
          dh = div h $ snatToNum $ SNat @h
      surface <- getWindowSurface window
      updatePixels (dw, dh) outputs surface
      updateWindowSurface window

    updatePixels :: (CInt, CInt) -> [VGAOutput n] -> Surface -> IO ()
    updatePixels (dw, dh) outputs surface = do
      forM_ (zip outputs $ coords (SNat @w) (SNat @h)) $
        \(VGAOutput {..}, (x, y)) ->
          let unsignedToWord8 n = round $
                (255 * fromIntegral n / fromIntegral (maxBound `asTypeOf` n)
                  :: Double)
              r = unsignedToWord8 red
              g = unsignedToWord8 green
              b = unsignedToWord8 blue
          in
            surfaceFillRect surface
              (Just $ Rectangle (P $ V2 (x * dw) (y * dh)) (V2 dw dh))
              $ V4 r b g 0

inputFrame
  :: forall w h
   . (KnownNat w, KnownNat h)
  => (Scancode -> Bool)
  -> [(Keyboard, VGAInput w h)]
inputFrame pressed =
  map ((,) keyboard . VGAInput . Just . uncurry Pos)
      (coords (SNat @w) (SNat @h))
  where
    keyboard =
      Keyboard
        { keyUp    = pressed ScancodeUp
        , keyDown  = pressed ScancodeDown
        , keyLeft  = pressed ScancodeLeft
        , keyRight = pressed ScancodeRight
        , keySpace = pressed ScancodeSpace
        , keyA     = pressed ScancodeA
        , keyB     = pressed ScancodeB
        , keyC     = pressed ScancodeC
        , keyD     = pressed ScancodeD
        , keyE     = pressed ScancodeE
        , keyF     = pressed ScancodeF
        , keyG     = pressed ScancodeG
        , keyH     = pressed ScancodeH
        , keyI     = pressed ScancodeI
        , keyJ     = pressed ScancodeJ
        , keyK     = pressed ScancodeK
        , keyL     = pressed ScancodeL
        , keyM     = pressed ScancodeM
        , keyN     = pressed ScancodeN
        , keyO     = pressed ScancodeO
        , keyP     = pressed ScancodeP
        , keyQ     = pressed ScancodeQ
        , keyR     = pressed ScancodeR
        , keyS     = pressed ScancodeS
        , keyT     = pressed ScancodeT
        , keyU     = pressed ScancodeU
        , keyV     = pressed ScancodeV
        , keyW     = pressed ScancodeW
        , keyX     = pressed ScancodeX
        , keyY     = pressed ScancodeY
        , keyZ     = pressed ScancodeZ
        , key0     = pressed Scancode0
        , key1     = pressed Scancode1
        , key2     = pressed Scancode2
        , key3     = pressed Scancode3
        , key4     = pressed Scancode4
        , key5     = pressed Scancode5
        , key6     = pressed Scancode6
        , key7     = pressed Scancode7
        , key8     = pressed Scancode8
        , key9     = pressed Scancode9
        }
