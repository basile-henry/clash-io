module Main where

import           Clash.IO (run)
import           Snake    (topEntity)

main :: IO ()
main = run topEntity
