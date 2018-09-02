--{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE BinaryLiterals #-}
--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RankNTypes #-}

module Lib (module Lib) where


import Data.Word
--import Control.Lens
--import Data.Array.IO as IOA
import Data.Bits
--import Data.Binary.Get
--import Graphics.Gloss
--import Control.Monad
--import System.Console.ANSI as A
--import Numeric (showHex)

--LIB
-- | Cute little operator to make my life easier.
(.|) :: (Monad m) => (a -> m a) -> (a -> m a) -> (a -> m a)
(.|) f2 f1 = \a -> do { evalF1 <- f1 a
                      ; f2 evalF1}

--LIB
-- | Converts an byte to 16 bit value.
toWord16 :: Word8 -> Word16
toWord16 w = fromIntegral w

--LIB
-- | Converts a 16 bit value into a byte.
toWord8 :: Word16 -> Word8
toWord8 w = fromIntegral w

--LIB
-- | Combine two bytes into a 16 bit value.
combineData :: Word8 -> Word8 -> Word16
combineData d1 d2 = shiftL (toWord16 d1) 8 .|. toWord16 d2

--LIB
-- | Returns the hi byte from a 16 bit value.
breakHi :: Word16 -> Word8
breakHi d = fromIntegral $ shiftR d 8

--LIB
-- | Returns the lo byte from a 16 bit value.
breakLo :: Word16 -> Word8
breakLo d = fromIntegral d

maybeSetBit8 :: Bool -> Int -> Word8 -> Word8
maybeSetBit8 b n w = if b then setBit w n else clearBit w n
