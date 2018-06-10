module Main where

import Lib
import Control.Lens

main :: IO Gameboy
main = do { gb <- runGameboyNSteps 0
          ; foo gb 0
            }

foo :: Gameboy -> Int -> IO Gameboy
foo gb n = do { gbn <- stepNGameboy n gb
              ; _ <- prettyPrintGb (return gbn)
              ; _ <- getLine
              ; foo gbn (n + 1)}
