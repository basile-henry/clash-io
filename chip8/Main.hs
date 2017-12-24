module Main where

import           Chip8    (topEntity)
import           Clash.IO (run)

main :: IO ()
main = run topEntity
