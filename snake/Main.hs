module Main where

import           Clash.IO (runVGA)
import           Snake    (topEntity)

main :: IO ()
main = runVGA topEntity
