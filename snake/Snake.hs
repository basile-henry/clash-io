{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Snake
  ( topEntity
  ) where

import           Clash.Prelude hiding (Either (..))
import           Data.Bool     (bool)

import           Clash.IO

type Width    = 64
type Height   = 32
type Position = Pos Width Height

data State
  = Alive
  | Dead
  deriving (Show, Eq)

data Direction
  = Up
  | Right
  | Down
  | Left
  deriving (Show, Eq)

data Model =
  Model
    { snake        :: Vec 100 Position
      -- ^ Where the snake is located
    , mask         :: Vec 100 Bit
      -- ^ Size of the snake
    , state        :: State
      -- ^ Game state
    , direction    :: Direction
      -- ^ Current direction
    , food         :: Position
      -- ^ Food position
    , speed        :: Unsigned 4
      -- ^ Speed of movement of the snake
      -- Used to reset wait time so high number means slow speed
    , wait         :: Unsigned 4
      -- ^ Wait time until next movement
    , points       :: Unsigned 6
      -- ^ Number of points (TODO actually use this)
    , lastKeyboard :: Keyboard
      -- ^ A way to accumulate inputs while in the waiting for the next move
    }
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    (Pos 16 16 :> repeat (Pos 0 0))
    (1 :> repeat 0)
    Alive Right (Pos 10 5) 10 10 0
    defaultKeyboard

topEntity
  :: SystemClockReset
  => Signal System (Keyboard, VGAInput Width Height)
  -> Signal System (VGAOutput 1)
topEntity input = view <$> model <*> position <*> snakeMask
  where
    (keyboard, vga) = unbundle input
    -- The model is only updated once per frame
    (model, snakeMask)
      = unbundle
      $ regEn (initialModel, repeat $ repeat False) frameStart
              (update <$> keyboard <*> randomPosition frameStart <*> model)

    position = pixelPos <$> vga

    frameStart = (Just (Pos 0 0) ==) <$> position

update
  :: (KnownNat (Width * Height))
  => Keyboard
  -> Position
  -> Model
  -> (Model, Vec Width (Vec Height Bool))
update inKeyboard newFood model@Model{..}
  | keyR kb         = ( initialModel, repeat $ repeat False )
  | state  == Dead  = ( model, snakeMask )
  | wait   /= 0     = ( model { wait = pred wait, lastKeyboard = kb }
                      , snakeMask
                      )
  | newPos == food  = ( model
                          { food      = newFood
                          , snake     = newSnake
                          , mask      = 1 +>> mask
                          , direction = newDirection
                          , wait      = newSpeed
                          , speed     = newSpeed
                            -- Reset the accumulated last input
                          , lastKeyboard = defaultKeyboard
                          }
                      , snakeMask
                      )
  | isBorder newPos = ( model { state = Dead }, snakeMask )
  | touchingItself  = ( model { state = Dead }, snakeMask )
  | otherwise       = ( model
                          { snake     = newSnake
                          , direction = newDirection
                          , wait      = speed
                            -- Reset the accumulated last input
                          , lastKeyboard = defaultKeyboard
                          }
                      , snakeMask
                      )
  where
    -- Same as 'any' but for Vec
    anyVec = (/= 0) . v2bv . map (bool 0 1)

    -- Accumulate keyboard input when waiting
    kb
      | anyVec
          ( keyUp    inKeyboard
         :> keyDown  inKeyboard
         :> keyLeft  inKeyboard
         :> keyRight inKeyboard
         :> keyR     inKeyboard
         :> Nil
          )       = inKeyboard
      | otherwise = lastKeyboard

    newSpeed
      | speed > 1 = pred speed
      | otherwise = speed

    newDirection
      | keyUp kb
      , direction /= Down  = Up
      | keyRight kb
      , direction /= Left  = Right
      | keyDown kb
      , direction /= Up    = Down
      | keyLeft kb
      , direction /= Right = Left
      | otherwise          = direction


    newPos@Pos{..} =
      let (Pos a b) = head snake
      in case newDirection of
          Up    -> Pos a       (b - 1)
          Right -> Pos (a + 1) b
          Down  -> Pos a       (b + 1)
          Left  -> Pos (a - 1) b

    newSnake = newPos +>> snake

    touchingItself = snakeMask !! x !! y

    -- This snakeMask is used for better CPU perfomance but is kind of a hack
    -- when it comes to a hardware implementation
    snakeMask = unconcatI $ foldl step (repeat False) $ zip mask snake

    step s (m, Pos i j)
      | m == 1    =
          let (i', j') = (resize $ pack i, resize $ pack j)
          in replace ((i' `shiftL` 5) + j' :: BitVector 11) True s
      | otherwise = s

view
  :: Model
  -> Maybe Position
  -> Vec Width (Vec Height Bool)
  -> VGAOutput 1
view _ Nothing _ = VGAOutput 0 0 0
view Model{..} (Just pos@Pos{..}) snakeMask
  | isBorder pos  = VGAOutput 0 1 0
  | isFood        = VGAOutput 0 0 1
  | isSnake
  , state == Dead = VGAOutput 1 0 0
  | isSnake       = VGAOutput 1 1 1
  | otherwise     = VGAOutput 0 0 0
  where
    isFood  = pos == food
    isSnake = snakeMask !! x !! y

isBorder
  :: Position
  -> Bool
isBorder Pos{..}
  = x == minBound || x == maxBound
 || y == minBound || y == maxBound

randomPosition
  :: SystemClockReset
  => Signal System Bool
  -> Signal System Position
randomPosition enable =
  Pos <$> fmap insideWalls x <*> fmap insideWalls y
  where
    -- LFSR from Clash examples
    lfsrF :: BitVector 16 -> BitVector 16
    lfsrF s = fb ++# slice d15 d1 s
      where
        fb = s!5 `xor` s!3 `xor` s!2 `xor` s!0

    random = regEn 0x1234 enable (lfsrF <$> random)

    x :: Signal System (Index (Width - 2))
    x = toIndex . slice d5  d0 <$> random

    y :: Signal System (Index (Height - 2))
    y = toIndex . slice d10 d6 <$> random

    insideWalls a = 1 + resize a

    toIndex
      :: forall n
       . KnownNat n
      => BitVector (CLog 2 n)
      -> Index n
    toIndex b
      | b >= pack (maxBound :: Index n) = maxBound
      | otherwise = unpack b
