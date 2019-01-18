{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE BinaryLiterals #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib (module Lib) where


import Data.Word
import Control.Lens
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

-- | Sets the n'th bit if b is true; otherwise resets it.
maybeSetBit8 :: Bool -> Int -> Word8 -> Word8
maybeSetBit8 b n w = if b then setBit w n else clearBit w n

-- | Converts a 8 bit word to a signed integer type.
wordToSignedInt :: Word8 -> Int
wordToSignedInt w
  | testBit w 7 == True = - (fromIntegral $ (complement w) + 1)
  | otherwise = fromIntegral w

-- | performs signed arithmetic if bool is true
  -- otherwise it's unsigned.
signedAddIf :: Bool -> Word8 -> Word16 -> Word16
signedAddIf True  w8 w16 = fromIntegral $ (wordToSignedInt w8) + (fromIntegral w16)
signedAddIf False w8 w16 = (fromIntegral w8) + (fromIntegral w16)

(\*/) :: a -> b -> (a,b)
(\*/) a b = (a, b)

newtype ScopedLens a b = ScopedLens (Lens' a b)

scopeLens :: Lens' a b -> ScopedLens a b
scopeLens = ScopedLens

unscopeLens :: ScopedLens a b -> Lens' a b
unscopeLens (ScopedLens l) = l
