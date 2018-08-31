{-# LANGUAGE TemplateHaskell #-}

module Memory () where

import Control.Lens
import Data.Array.IO
import Data.Word

--MEMORY
-- | Represents gameboy memory.
data Memory =
  Memory
  {
    _bytes :: IOUArray Word16 Word8
  }
makeLenses ''Memory

--MEMORY
-- | Default gameboy memory on startup.
defaultMemory :: IO Memory
defaultMemory = do { mem <- (newArray (0, 0xFFFF) 0)
                   ; return $ Memory mem }
