{-# LANGUAGE BinaryLiterals #-}

module Interrupts (module Interrupts) where

import Memory
import Data.Word
import Data.Bits
import Data.List

data Interrupt = Joypad | Serial | Timer | LCDStat | VBlank
  deriving (Eq)

-- | Interrupts are ordered by who has the highest priority when dispatching interrupts.
instance Ord Interrupt where
  compare Joypad  Joypad  = EQ
  compare Joypad  Serial  = GT
  compare Joypad  Timer   = GT
  compare Joypad  LCDStat = GT
  compare Joypad  VBlank  = GT
  compare Serial  Joypad  = LT
  compare Serial  Serial  = EQ
  compare Serial  Timer   = GT
  compare Serial  LCDStat = GT
  compare Serial  VBlank  = GT
  compare Timer   Joypad  = LT
  compare Timer   Serial  = LT
  compare Timer   Timer   = EQ
  compare Timer   LCDStat = GT
  compare Timer   VBlank  = GT
  compare LCDStat Joypad  = LT
  compare LCDStat Serial  = LT
  compare LCDStat Timer   = LT
  compare LCDStat LCDStat = EQ
  compare LCDStat VBlank  = GT
  compare VBlank  Joypad  = LT
  compare VBlank  Serial  = LT
  compare VBlank  Timer   = LT
  compare VBlank  LCDStat = LT
  compare VBlank  VBlank  = EQ

-- | Converts an Interrupt into an 8 bit mask.
interruptToMask :: Interrupt -> Word8
interruptToMask Joypad  = 0b000010000
interruptToMask Serial  = 0b000001000
interruptToMask Timer   = 0b000000100
interruptToMask LCDStat = 0b000000010
interruptToMask VBlank  = 0b000000001

-- | Given a byte it determines what interupts are set by the byte using their masks.
decodeInterruptByte :: Word8 -> [Interrupt]
decodeInterruptByte b = map (\(x, y) -> x) $
                        filter (\(x, y) -> y /= 0b00000000) $
                        map (\x -> (x, (interruptToMask x) .&. b)) [Joypad, Serial, Timer, LCDStat, VBlank]

-- | Gets the list of interrupts that are currently enabled.
getEnabledInterrupts :: Memory -> IO [Interrupt]
getEnabledInterrupts mem = getMemory 0xFFFF mem >>= \b -> return $ decodeInterruptByte b

-- | Gets the list of interrupts that are currently triggered.
getTriggeredInterrupts :: Memory -> IO [Interrupt]
getTriggeredInterrupts mem = getMemory 0xFF0F mem >>= \b -> return $ decodeInterruptByte b

-- | Gets the list of interrupts that need to be handled.
getDispatchableInterrupts :: Memory -> IO [Interrupt]
getDispatchableInterrupts mem = do { enabled   <- getEnabledInterrupts mem
                                   ; triggered <- getTriggeredInterrupts mem
                                   ; return $ intersect enabled triggered }

-- | Returns the interrupt with thi highest priority.
  -- NOTE/TODO maybe make this [Interrupt] -> Maybe Interrupt,
  -- since [] will make the program crash.
getHighestPriorityInterrupt :: [Interrupt] -> Interrupt
getHighestPriorityInterrupt is = minimum is

-- | Converts a given Interrupt to its appropriate address.
interruptToAddress :: Interrupt -> Word16
interruptToAddress Joypad  = 0x0060
interruptToAddress Serial  = 0x0058
interruptToAddress Timer   = 0x0050
interruptToAddress LCDStat = 0x0048
interruptToAddress VBlank  = 0x0040
