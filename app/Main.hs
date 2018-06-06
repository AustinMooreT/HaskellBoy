module Main where

import Lib
import Control.Lens

main :: IO ()
main = putStrLn . show $ ((runGameboyNSteps 24611) ^. cpu)

--getDebugView :: Gameboy -> IO ()
