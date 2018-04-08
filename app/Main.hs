module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.Display

main :: IO ()
main = display (InWindow "Nice Window" (400, 400) (10, 10)) white (cpuToPicture defaultCpu)
