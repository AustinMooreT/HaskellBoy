{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    () where


import Data.Word
import Control.Lens
import Data.Array.IO as IOA
import Data.Bits
import Data.Binary.Get
import Graphics.Gloss
import Control.Monad
import System.Console.ANSI as A
import Numeric (showHex)

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





evalInstruction :: Gameboy -> Instruction -> IO Gameboy
evalInstruction gb inst = gb & inst ^. operation

fetchNextInstr :: Gameboy -> IO Instruction
fetchNextInstr gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb) gb
                       ; return $ decodeOp mem }

stepGameboy :: Gameboy -> IO Gameboy
stepGameboy gb = do { instr <- fetchNextInstr gb
                    ; evGB  <- evalInstruction gb instr
                    ; return $ gb1 evGB }
  where gb1 = incrementRegistersWithoutFlags (PHI, CLO)

stepNGameboy :: Int -> (Gameboy -> IO Gameboy)
stepNGameboy n = Prelude.foldl (.|) (fixGB id) $ Prelude.replicate n stepGameboy

runGameboyNSteps :: Int -> IO Gameboy
runGameboyNSteps n = do { gb   <- defaultGameboy
                        ; boot <- loadBootRom gb
                        ; stepNGameboy n boot }


