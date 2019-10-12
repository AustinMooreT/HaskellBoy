{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module Memory (module Memory) where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Control.Lens
import Control.Monad
import Data.Array.IO
import Data.Array
import Data.Word
import Data.Bits
import Numeric (showHex)
import BootRom
import Lib
{-
These are some notes on the layout of gameboy memory.
0x0000 - 0x3FFF is the Non-switchable ROM bank. (ROM0)
0x4000 - 0x9FFH is the Swithchable Rom bank (ROMX)
0x8000 - 0x9FFF is the video ram (VRAM) [This is switchable on GBC]
0xA000 - 0xBFFF is memory mapped cartridge ram. (SRAM)
0xC000 - 0xCFFF is genereal memory (WRAM0)
0xD000 - 0xDFFF is more general memory (WRAMX) [This is switchable on GBC]
0xE000 - 0xFDFF See notes on ECHO memory. (ECHO)
0xFEA0 - 0xFEFF See notes on UNUSED memory. (UNUSED)
0xFF00 - 0xFF7F is the IO
registers (IO REGISTERS)
0xFF80 - 0xFFFE is the internal CPU ram (HRAM)
0xFFFF is the interrupt enable flags (IE REGISTER)
-}

-- | Represents gameboy memory.
data Memory =
  Memory
  {
    _bytes          :: IOUArray Word16 Word8
  }
makeLenses ''Memory

-- | Default gameboy memory on startup.
defaultMemory :: IO Memory
defaultMemory = do { mem <- (newArray (0, 0xFFFF) 0)
                   ; return $ Memory mem }

-- | Handles the special case when address 0xFF0F is read.
  -- bits 5-7 always are 1.
get0xFF0F :: Memory -> IO Word8
get0xFF0F mem = do { val <- readArray (mem ^. bytes) 0xFF0F
                   ; return $ val .|. 0b11100000 }

-- | Handles the special case when address 0xFF0F is written.
  -- Cannot write bits 5-7.
set0xFF0F :: Word8 -> Memory -> IO Memory
set0xFF0F d mem = writeArray (mem ^. bytes) 0xFF0F (0b11100000 .|. d) >>=
                  \_ -> return mem

-- | Handles the special case when address 0xFF41 is read.
  -- Bit 7 always returns 1.
get0xFF41 :: Memory -> IO Word8
get0xFF41 mem = readArray (mem ^. bytes) 0xFF41 >>=
                \x -> return $ x .|. 0b10000000

-- | Handles the special case when address 0xFF41 is written.
  -- Cannot write bit 7.
set0xFF41 :: Word8 -> Memory -> IO Memory
set0xFF41 d mem = writeArray (mem ^. bytes) 0xFF41 (0b10000000 .|. d) >>=
                  \_ -> return mem

-- | Handles the special case when address 0xFF44 is written.
  -- address is set to 0. NOTE think about this more.
set0xFF44 :: Word8 -> Memory -> IO Memory
set0xFF44 _ mem = writeArray (mem ^. bytes) 0xFF41 0b00000000 >>=
                  \_ -> return mem

-- | Uses 16 bit value addr to index and return an 8 bit value in memory.
getMemory :: Word16 -> Memory -> IO Word8
getMemory 0xFF0F mem = get0xFF0F mem
getMemory 0xFF41 mem = get0xFF41 mem
getMemory addr   mem = do { bootRomFlag <- readArray (mem ^. bytes) 0xFF50
                          ; if (bootRomFlag == 0x00) && addr <= 0x00FF then
                              return $ getBootRom (toWord8 addr)
                            else readArray (mem ^. bytes) addr }

-- | Uses 16 bit value addr as an index to set the element there to 8 bit value d.
setMemory :: Word16 -> Word8 -> (Memory -> IO Memory)
setMemory 0xFF0F d mem = set0xFF0F d mem
setMemory 0xFF41 d mem = set0xFF41 d mem
setMemory addr   d mem = writeArray (mem ^. bytes) addr d >> return mem

memBegin :: Word16
memBegin = 0x0000

memEnd :: Word16
memEnd = 0xFFFF

loadMemorySnapshot :: FilePath -> Memory -> IO Memory
loadMemorySnapshot fp mem = do { fileData <- BL.readFile fp
                               ; memList  <- mapM (\(byte, addr) -> setMemory addr byte mem) $ zip (BL.unpack fileData) [memBegin .. memEnd]
                               ; return $ head memList }
