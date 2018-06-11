module Main where

import Lib
import Control.Lens

main :: IO ()
main = prettyPrintGb (runGameboyNSteps 24627)

