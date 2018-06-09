module Main where

import Lib
import Control.Lens

main :: IO ()
main = do { gb <- runGameboyNSteps 24611
          ; let cpu_ = gb ^. cpu
            in putStrLn . show $ cpu_ }
