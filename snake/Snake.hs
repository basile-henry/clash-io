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

module Snake
  ( topEntity
  ) where

import           Clash.IO
import           Clash.Prelude       hiding (Either (..))
import           Clash.Prelude.Moore (medvedevB)

import           Debug.Trace

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
    { snake     :: Vec 100 Position
      -- ^ Where the snake is located
    , mask      :: Vec 100 Bit
      -- ^ Size of the snake
    , state     :: State
      -- ^ Game state
    , direction :: Direction
      -- ^ Current direction
    , food      :: Position
      -- ^ Food position
    , speed     :: Unsigned 6
      -- ^ Speed of movement of the snake
      -- Used to reset wait time so high number means slow speed
    , wait      :: Unsigned 6
      -- ^ Wait time until next movement
    }
  deriving (Show, Eq)

topEntity
  :: SystemClockReset
  => Signal System (Input Width Height)
  -> Signal System Output
topEntity input = view <$> model <*> position <*> snakeMask
  where
    -- The model is only updated once per frame
    (model, snakeMask)
      = unbundle
      $ regEn (initialModel, repeat $ repeat False) frameStart
              (update <$> input <*> randomPosition frameStart <*> model)

    initialModel =
      Model
        (Pos 16 16 :> repeat (Pos 0 0))
        (1 :> repeat 0)
        Alive Right (Pos 10 5) 20 0

    position = pixelPos <$> input

    frameStart = (Pos 0 0 ==) <$> position

update
  :: (KnownNat (Width * Height))
  => Input Width Height
  -> Position
  -> Model
  -> (Model, Vec Width (Vec Height Bool))
update Input{..} newFood model@Model{..}
  | state  == Dead  = ( model, snakeMask )
  | wait   /= 0     = ( model { wait = pred wait }, snakeMask )
  | newPos == food  = ( model
                          { food      = newFood
                          , snake     = newSnake
                          , mask      = 1 +>> mask
                          , direction = newDirection
                          , wait      = newSpeed
                          , speed     = newSpeed
                          }
                      , snakeMask
                      )
  | isBorder newPos = ( model { state = Dead }, snakeMask )
  | touchingItself  = ( model { state = Dead }, snakeMask )
  | otherwise       = ( model
                        { snake     = newSnake
                        , direction = newDirection
                        , wait      = speed
                        }
                      , snakeMask
                      )
  where
    newSpeed
      | speed > 2 = pred speed
      | otherwise = speed

    newDirection
      | up
      , direction /= Down  = Up
      | right
      , direction /= Left  = Right
      | down
      , direction /= Up    = Down
      | left
      , direction /= Right = Left
      | otherwise          = direction


    newPos@Pos{..} =
      let (Pos x y) = head snake
      in case newDirection of
          Up    -> Pos x       (y - 1)
          Right -> Pos (x + 1)       y
          Down  -> Pos x       (y + 1)
          Left  -> Pos (x - 1)       y

    newSnake = newPos +>> snake

    touchingItself = snakeMask !! x !! y

    -- This snakeMask is used for better CPU perfomance but is kind of a hack
    -- when it comes to a hardware implementation
    snakeMask = unconcatI $ foldl step (repeat False) $ zip mask snake

    step x (m, Pos i j)
      | m == 1    =
          let (i', j') = (resize $ pack i, resize $ pack j)
          in replace ((i' `shiftL` 5) + j' :: BitVector 11) True x
      | otherwise = x

view
  :: Model
  -> Position
  -> Vec Width (Vec Height Bool)
  -> Output
view Model{..} pos@Pos{..} snakeMask
  | isBorder pos  = Output   0 255   0
  | isFood        = Output   0   0 255
  | isSnake
  , state == Dead = Output 255   0   0
  | isSnake       = Output 255 255 255
  | otherwise     = Output  10  10  10
  where
    isFood  = pos == food
    isSnake = snakeMask !! x !! y

isBorder
  :: Position
  -> Bool
isBorder (Pos x y)
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
    lfsrF s = feedback ++# slice d15 d1 s
      where
        feedback = s!5 `xor` s!3 `xor` s!2 `xor` s!0

    random = regEn 0x1234 enable (lfsrF <$> random)

    x :: Signal System (Index (Width - 2))
    x = unpack . slice d5  d0 <$> random

    y :: Signal System (Index (Height - 2))
    y = unpack . slice d10 d6 <$> random

    insideWalls a = 1 + resize a

    toIndex
      :: forall n
       . KnownNat n
      => BitVector (CLog 2 n)
      -> Index n
    toIndex b
      | b >= pack (maxBound :: Index n) = maxBound
      | otherwise = unpack b
