module Main where

import           Chip8    (topEntity)
import           Clash.IO (runVGA)

main :: IO ()
main = runVGA topEntity
