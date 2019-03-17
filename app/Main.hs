module Main where

import Lcd
import Memory

main :: IO ()
main = do { tetris <- defaultMemory >>= \x -> loadMemorySnapshot "/home/maurice/test.txt" x
          ; db     <- mainBuffer
          ; foo tetris db }

