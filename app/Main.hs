module Main where

import Lib
import Control.Lens

main :: IO Gameboy
main = debugMode (runGameboyNSteps 0)

