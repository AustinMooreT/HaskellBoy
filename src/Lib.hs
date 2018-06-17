{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    (defaultCpu,
     runGameboyNSteps,
     stepNGameboy,
     cpu,
     Gameboy,
     prettyPrintGb,
     debugMode) where


import Data.Word
import Control.Lens
import Data.Array.IO as IOA
import Data.Bits
import Data.Binary.Get
import Graphics.Gloss
import Control.Monad
import System.Console.ANSI as A
import Numeric (showHex)

--ETC
-- | Cute little operator to make my life easier.
(.|) :: (Monad m) => (a -> m a) -> (a -> m a) -> (a -> m a)
(.|) f2 f1 = \a -> do { evalF1 <- f1 a
                      ; f2 evalF1}

--DATA
-- | Converts an byte to 16 bit value.
toWord16 :: Word8 -> Word16
toWord16 w = fromIntegral w

--DATA
-- | Converts a 16 bit value into a byte.
toWord8 :: Word16 -> Word8
toWord8 w = fromIntegral w

--DATA
-- | Combine two bytes into a 16 bit value.
combineData :: Word8 -> Word8 -> Word16
combineData d1 d2 = shiftL (toWord16 d1) 8 .|. toWord16 d2

--DATA
-- | Returns the hi byte from a 16 bit value.
breakHi :: Word16 -> Word8
breakHi d = fromIntegral $ shiftR d 8

--DATA
-- | Returns the lo byte from a 16 bit value.
breakLo :: Word16 -> Word8
breakLo d = fromIntegral d

--CPU
-- | Represents the CPU.
data Cpu =
  Cpu
  {
    _registerA    :: Word8,
    _registerB    :: Word8,
    _registerC    :: Word8,
    _registerD    :: Word8,
    _registerE    :: Word8,
    _registerF    :: Word8,
    _registerH    :: Word8,
    _registerL    :: Word8,
    _registerS_hi :: Word8,
    _registerP_lo :: Word8,
    _registerP_hi :: Word8,
    _registerC_lo :: Word8
  }
makeLenses ''Cpu

--CPU
-- | Instance for converting a CPU to string for easy debug.
instance Show Cpu where
  show cpu = "A:[" Prelude.++ (show $ cpu ^. registerA) Prelude.++ "]\n" Prelude.++
             "B:[" Prelude.++ (show $ cpu ^. registerB) Prelude.++ "]\n" Prelude.++
             "C:[" Prelude.++ (show $ cpu ^. registerC) Prelude.++ "]\n" Prelude.++
             "D:[" Prelude.++ (show $ cpu ^. registerD) Prelude.++ "]\n" Prelude.++
             "E:[" Prelude.++ (show $ cpu ^. registerE) Prelude.++ "]\n" Prelude.++
             "F:[" Prelude.++ (show $ cpu ^. registerF) Prelude.++ "]\n" Prelude.++
             "H:[" Prelude.++ (show $ cpu ^. registerH) Prelude.++ "]\n" Prelude.++
             "L:[" Prelude.++ (show $ cpu ^. registerL) Prelude.++ "]\n" Prelude.++
             "SP:[" Prelude.++ (show $ cpu ^. composeRegisterLenses (SHI, PLO)) Prelude.++ "]\n" Prelude.++
             "PC:[" Prelude.++ (show $ cpu ^. composeRegisterLenses (PHI, CLO)) Prelude.++ "]\n"

--CPU
-- | Default cpu on startup.
defaultCpu :: Cpu
defaultCpu = Cpu 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00

--CPU
-- | Represents given registers in the CPU.
data Register = A | B | C | D | E | F | H | L | SHI | PLO | PHI | CLO

--CPU
-- | CPU flag constant for zero.
zeroFlag :: Word8
zeroFlag = 128

--CPU
-- | CPU flag constant for suborrupt) with the following:
subtractFlag :: Word8
subtractFlag = 64

--CPU
-- | CPU flag constant for half carry.
halfCarryFlag :: Word8
halfCarryFlag = 32

--CPU
-- | CPU flag constant for carry.
carryFlag :: Word8
carryFlag = 16

--CPU
-- | Converts flag constants to corresponding flag bit.
flagToInt :: Word8 -> Int
flagToInt 128 = 7
flagToInt 64  = 6
flagToInt 32  = 5
flagToInt 16  = 4

--CPU
-- | Converts a register datum to an record accesor
registerToFunc :: Register -> (Cpu -> Word8)
registerToFunc A    = _registerA
registerToFunc B    = _registerB
registerToFunc C    = _registerC
registerToFunc D    = _registerD
registerToFunc E    = _registerE
registerToFunc F    = _registerF
registerToFunc H    = _registerH
registerToFunc L    = _registerL
registerToFunc SHI  = _registerS_hi
registerToFunc PLO  = _registerP_lo
registerToFunc PHI  = _registerP_hi
registerToFunc CLO  = _registerC_lo

--CPU
-- | Converts a register datum to a Lens (functor).
registerToLens :: Functor f => Register -> (Word8 -> f Word8) -> Cpu -> f Cpu
registerToLens A    = registerA
registerToLens B    = registerB

registerToLens C    = registerC
registerToLens D    = registerD
registerToLens E    = registerE
registerToLens F    = registerF
registerToLens H    = registerH
registerToLens L    = registerL
registerToLens SHI  = registerS_hi
registerToLens PLO  = registerP_lo
registerToLens PHI  = registerP_hi
registerToLens CLO  = registerC_lo

--CPU
-- | Takes two registers and creates a Lens (functor) for getting and setting them as a 16 bit unit.
composeRegisterLenses :: Functor f => (Register, Register) -> (Word16 -> f Word16) -> Cpu -> f Cpu
composeRegisterLenses (reg1, reg2) = lens getter setter
  where
    getter __cpu = combineData (registerToFunc reg1 __cpu) (registerToFunc reg2 __cpu)
    setter __cpu d = __cpu & registerToLens reg1 .~ breakHi d & registerToLens reg2 .~ breakLo d

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
--GAMEBOY
-- | Represents a gameboy.
data Gameboy =
  Gameboy
  {
    _cpu    :: Cpu,
    _memory :: Memory
  }
makeLenses ''Gameboy

--GAMEBOY
-- | Default gameboy used on startup.
defaultGameboy :: IO Gameboy
defaultGameboy = do { mem <- defaultMemory
                    ; return $ Gameboy defaultCpu mem }

--GAMEBOY
-- | Represents an instruction to the Gameboy's processor.
data Instruction =
  Instruction
  {
    _opcode    :: Word8,
    _name      :: String,
    _operation :: (Gameboy -> IO Gameboy)
  }
makeLenses ''Instruction

--GAMEBOY
-- | Instance of show for converting Instructions to a String.
instance Show Instruction where
  show instr = (show $ instr ^. opcode) Prelude.++ (show $ instr ^. name)

--GAMEBOY
-- | Uses 16 bit value addr to index and return an 8 bit value in memory.
getMemory :: Word16 -> Gameboy -> IO Word8
getMemory addr gb = let mem = view (memory . bytes) gb
                    in readArray mem addr

--GAMEBOY
-- | Uses 16 bit value addr as an index to set the element there to 8 bit value d.
setMemory :: Word16 -> Word8 -> (Gameboy -> IO Gameboy)
setMemory addr d gb = do { written <- (writeArray mem addr d)
                         ; return $ gb & memory .~ (Memory mem)}
  where
    mem = view (memory . bytes) gb

--GAMEBOY
-- | Sets the value in register r to some 8 bit value d.
setRegister :: Register -> Word8 -> Gameboy -> Gameboy
setRegister r d gb = gb & cpu . registerToLens r .~ d

--GAMEBOY
-- | Sets the value in the combined registers rs to a 16 bit value d.
setRegisters :: (Register, Register) -> Word16 -> Gameboy -> Gameboy
setRegisters rs d gb = gb & cpu . composeRegisterLenses rs .~ d

--GAMEBOY
-- | Fetches 8 bit value from register r.
getRegister :: Register -> Gameboy -> Word8
getRegister r gb = gb ^. cpu . registerToLens r

--GAMEBOY
-- | Fetches 16 bit combined value from the registers rs.
getRegisters :: (Register, Register) -> Gameboy -> Word16
getRegisters rs gb = gb ^. cpu . composeRegisterLenses rs

--GAMEBOY
-- | Loads data from src into dest.
ldRegWithReg :: Register -> Register -> (Gameboy -> Gameboy)
ldRegWithReg dest src gb = setRegister dest (getRegister src gb) gb

--GAMEBOY
-- | Using addr as an index grabs an 8 bit value from memory and loads it into reg.
ldRegWithMem :: Register -> Word16 -> (Gameboy -> IO Gameboy)
ldRegWithMem reg addr gb = do { mem <- getMemory addr gb
                              ; return $ setRegister reg mem gb }


--GAMEBOY
-- | Using the 16 bit value from the combined registers rs as an index
  -- grabs an 8 bit value from memory and loads it into r.
ldRegWithRegRegMem :: Register -> (Register, Register) -> (Gameboy -> IO Gameboy)
ldRegWithRegRegMem r rs gb = do { mem <- getMemory (getRegisters rs gb) gb
                                ; return $ setRegister r mem gb}

--GAMEBOY
-- | Using the combined register rs as an index set that location in memory to the value stored in r.
ldMemRegRegWithReg :: (Register, Register) -> Register -> (Gameboy -> IO Gameboy)
ldMemRegRegWithReg rs r gb = setMemory (getRegisters rs gb) (getRegister r gb) gb

--GAMEBOY
-- | Increments a register r's value by 1 while ignoring all flags.
incrementRegisterWithoutFlags :: Register -> (Gameboy -> Gameboy)
incrementRegisterWithoutFlags r gb = setRegister r (getRegister r gb + 1) gb

--GAMEBOY
-- | Increments the combined registers rs's value by 1.
incrementRegistersWithoutFlags :: (Register, Register) -> (Gameboy -> Gameboy)
incrementRegistersWithoutFlags rs gb = setRegisters rs (getRegisters rs gb + 1) gb

--GAMEBOY
-- | Using the 16 bit value stored in rs as an index set memory to the next byte from the program counter.
ldMemRegRegWithData :: (Register, Register) -> (Gameboy -> IO Gameboy)
ldMemRegRegWithData rs gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                               ; setMemory (getRegisters rs gb) mem gb1}
  where gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

--GAMEBOY
-- | Loads two registers r1 and r2 with data fetched from memory using the program counter.
ldRegRegWithData :: (Register, Register) -> (Gameboy -> IO Gameboy)
ldRegRegWithData rs gb = do { mem1 <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                            ; mem2 <- getMemory (getRegisters (PHI, CLO) gb2) gb2
                            ; return $ setRegisters rs (combineData mem2 mem1) gb2}
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb
    gb2 = incrementRegistersWithoutFlags (PHI, CLO) gb1



--GAMEBOY
-- | Loads a register r with data fetched from memory using the program counter.
ldRegWithData :: Register -> (Gameboy -> IO Gameboy)
ldRegWithData r gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                        ; return $ setRegister r mem gb1 }
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

--GAMEBOY
-- | Set's a flag using one of the flag constants.
setFlag :: Word8 -> Bool -> Gameboy -> Gameboy
setFlag w True  gb = gb & cpu . registerToLens F %~ \w8 -> w8 .|. w
setFlag w False gb = gb & cpu . registerToLens F %~ \w8 -> w8 .&. complement w

--GAMEBOY
-- | If the byte passed in is 0 return a function setting the 0 flag appropriately.
setZero8 :: Word8 -> (Gameboy -> Gameboy)
setZero8 0 = setFlag zeroFlag True
setZero8 _ = setFlag zeroFlag False

--GAMEBOY
-- | If 16 bit value passed in is 0 return a function setting the 0 flag appropriately.
setZero16 :: Word16 -> (Gameboy -> Gameboy)
setZero16 0 = setFlag zeroFlag True
setZero16 _ = setFlag zeroFlag False

--GAMEBOY
-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowCarry8 :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setOverflowCarry8 sum addend = setFlag carryFlag (sum < addend)

--GAMEBOY
-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowCarry16 :: Word16 -> Word16 -> (Gameboy -> Gameboy)
setOverflowCarry16 sum addend = setFlag carryFlag (sum < addend)

--GAMEBOY
-- | Check wether or not the sum overflows a the least significant nibble from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowHalfCarry8 :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setOverflowHalfCarry8 sum addend = setFlag halfCarryFlag ((sum .&. 0xf) < (addend .&. 0xf))

--GAMEBOY
-- | Check wether or not the sum overflows a the least significant nibble from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowHalfCarry16 :: Word16 -> Word16 -> (Gameboy -> Gameboy)
setOverflowHalfCarry16 sum addend = setFlag halfCarryFlag ((sum .&. 0xfff ) < (addend .&. 0xfff))

--GAMEBOY
-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags8 :: Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
incrementWithFlags8 byte store = store increment .
                                 halfCarry .
                                 subtractf .
                                 zero
  where
    increment = byte + 1
    zero      = \gb -> setZero8 increment gb
    halfCarry = \gb -> setOverflowHalfCarry8 increment byte gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY
-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags8IO :: Word8 -> (Word8 -> (Gameboy -> IO Gameboy)) -> (Gameboy -> IO Gameboy)
incrementWithFlags8IO byte store = store increment .
                                 halfCarry .
                                 subtractf .
                                 zero
  where
    increment = byte + 1
    zero      = \gb -> setZero8 increment gb
    halfCarry = \gb -> setOverflowHalfCarry8 increment byte gb
    subtractf = \gb -> setFlag subtractFlag False gb


--GAMEBOY
-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags16 :: Word16 -> (Word16 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
incrementWithFlags16 byte store = store increment .
                                  halfCarry .
                                  subtractf .
                                  zero
  where
    increment = byte + 1
    zero      = \gb -> setZero16 increment gb
    halfCarry = \gb -> setOverflowHalfCarry16 increment byte gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY
-- | Increments the value stored in a given register and sets the associated flags.
incrementRegisterWithFlags :: Register -> (Gameboy -> Gameboy)
incrementRegisterWithFlags r gb = incrementWithFlags8 (getRegister r gb) (\d -> (\gb1 -> (setRegister r d gb1))) gb

--GAMEBOY
-- | Increments the value stored in a register pair.
incrementRegistersWithFlags :: (Register, Register) -> (Gameboy -> Gameboy)
incrementRegistersWithFlags rs gb = incrementWithFlags16 (getRegisters rs gb) (\d -> (\gb1 -> (setRegisters rs d gb1))) gb

--GAMEBOY
-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setUnderflowCarry8 :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setUnderflowCarry8 difference subtrahend = setFlag carryFlag (difference > subtrahend)

--GAMEBOY
-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setUnderflowCarry16 :: Word16 -> Word16 -> (Gameboy -> Gameboy)
setUnderflowCarry16 difference subtrahend = setFlag carryFlag (difference > subtrahend)

--GAMEBOY
-- | Checks wether the difference underflows from a subtraction opperation by looking at one of the subtrahends.
setUnderflowHalfCarry8 :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setUnderflowHalfCarry8 difference subtrahend = setFlag halfCarryFlag ((difference .&. 0xf) > (subtrahend .&. 0xf))

--GAMEBOY
-- | Checks wether the difference underflows from a subtraction opperation by looking at one of the subtrahends.
setUnderflowHalfCarry16 :: Word16 -> Word16 -> (Gameboy -> Gameboy)
setUnderflowHalfCarry16 difference subtrahend = setFlag halfCarryFlag ((difference .&. 0xfff) > (subtrahend .&. 0xfff))

--GAMEBOY
-- | Decrement byte with flags
decrementWithFlags8 :: Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
decrementWithFlags8 byte store = store decrement .
                                 zero .
                                 halfCarry .
                                 subtractf
  where
    decrement = byte - 1
    zero      = \gb -> setZero8 decrement gb
    halfCarry = \gb -> setUnderflowHalfCarry8 decrement byte gb
    subtractf = \gb -> setFlag subtractFlag True gb

--GAMEBOY
-- | Decrement byte with flags
decrementWithFlags8IO :: Word8 -> (Word8 -> (Gameboy -> IO Gameboy)) -> (Gameboy -> IO Gameboy)
decrementWithFlags8IO byte store = store decrement .
                                 zero .
                                 halfCarry .
                                 subtractf
  where
    decrement = byte - 1
    zero      = \gb -> setZero8 decrement gb
    halfCarry = \gb -> setUnderflowHalfCarry8 decrement byte gb
    subtractf = \gb -> setFlag subtractFlag True gb

--GAMEBOY
-- | Decrement byte with flags
decrementWithFlags16 :: Word16 -> (Word16 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
decrementWithFlags16 byte store = store decrement .
                                  zero .
                                  halfCarry .
                                  subtractf
  where
    decrement = byte - 1
    zero      = \gb -> setZero16 decrement gb
    halfCarry = \gb -> setUnderflowHalfCarry16 decrement byte gb
    subtractf = \gb -> setFlag subtractFlag True gb

--GAMEBOY
-- | decrement a register and set the associated flags.
decrementRegisterWithFlags :: Register -> (Gameboy -> Gameboy)
decrementRegisterWithFlags r gb= decrementWithFlags8 (getRegister r gb) (\d -> (\gb1 -> setRegister r d gb1)) gb

--GAMEBOY
-- | decrement a set of registers and set the associated flags.
decrementRegisters :: (Register, Register) -> (Gameboy -> Gameboy)
decrementRegisters rs gb = decrementWithFlags16 (getRegisters rs gb) (\d -> (\gb1 -> setRegisters rs d gb1)) gb

--GAMEBOY
-- | decrement a register and don't set the associated flags.
decrementRegisterWithoutFlags :: Register -> (Gameboy -> Gameboy)
decrementRegisterWithoutFlags r gb = setRegister r ((getRegister r gb) - 1) gb

--GAMEBOY
-- | decrement a set of registers and don't set the associated flags.
decrementRegistersWithoutFlags :: (Register, Register) -> (Gameboy -> Gameboy)
decrementRegistersWithoutFlags rs gb = setRegisters rs ((getRegisters rs gb) - 1) gb

--GAMEBOY
-- | Increments a value in memory by indexing with a register pair.
incrementMemoryRegReg :: (Register, Register) -> (Gameboy -> IO Gameboy)
incrementMemoryRegReg rs gb  = do { mem <- getMemory (getRegisters rs gb) gb
                                  ; let incMem = incrementWithFlags8IO mem
                                        storeMem = \d -> \gb_ -> setMemory (getRegisters rs gb_) d gb
                                    in incMem storeMem gb }

--GAMEBOY
-- | decrement a value in memory by indexing with a register pair.
decrementMemoryRegReg :: (Register, Register) -> (Gameboy -> IO Gameboy)
decrementMemoryRegReg rs gb = do { mem <- getMemory (getRegisters rs gb) gb
                                 ; let decMem = decrementWithFlags8IO mem
                                       storeMem = \d -> \gb_ -> setMemory (getRegisters rs gb_) d gb_
                                   in decMem storeMem gb }

--GAMEBOY
-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags8 :: Word8 -> Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
addWithFlags8 e1 e2 f = f addition .
                       carry .
                       halfCarry .
                       subtractf .
                       zero
  where
    addition  = e1 + e2
    zero      = \gb -> setZero8 addition gb
    carry     = \gb -> setOverflowCarry8 addition e2 gb
    halfCarry = \gb -> setOverflowHalfCarry8 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag False gb


boolToWord :: Bool -> Word8
boolToWord True = 1
boolToWord _ = 0

--GAMEBOY
-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags8PlusC :: Word8 -> Word8 -> Bool -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
addWithFlags8PlusC e1 e2 b f = f addition .
                       carry .
                       halfCarry .
                       subtractf .
                       zero
  where
    addition  = e1 + e2 + (boolToWord b)
    zero      = \gb -> setZero8 addition gb
    carry     = \gb -> setOverflowCarry8 addition e2 gb
    halfCarry = \gb -> setOverflowHalfCarry8 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY
-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags16 :: Word16 -> Word16 -> (Word16 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
addWithFlags16 e1 e2 f = f addition .
                       carry .
                       halfCarry .
                       subtractf
  where
    addition  = e1 + e2
    carry     = \gb -> setOverflowCarry16 addition e2 gb
    halfCarry = \gb -> setOverflowHalfCarry16 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY
-- | subtracts e2 from e1 and uses f to store the result and all of the state changes back in a gb.
subWithFlags8 :: Word8 -> Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
subWithFlags8 e1 e2 f = f addition .
                       carry .
                       halfCarry .
                       subtractf .
                       zero
  where
    addition  = e1 - e2
    zero      = \gb -> setZero8 addition gb
    carry     = \gb -> setUnderflowCarry8 addition e2 gb
    halfCarry = \gb -> setUnderflowHalfCarry8 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag True gb


--TODO comment this.
subWithFlags8PlusC :: Word8 -> Word8 -> Bool -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
subWithFlags8PlusC e1 e2 b f = f addition .
                       carry .
                       halfCarry .
                       subtractf .
                       zero
  where
    addition  = (e1 + (boolToWord b)) - e2
    zero      = \gb -> setZero8 addition gb
    carry     = \gb -> setUnderflowCarry8 addition e2 gb
    halfCarry = \gb -> setUnderflowHalfCarry8 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag True gb


--GAMEBOOY
-- | adds two registers r1 and r2 together and stores the value in r1 and sets the appropriate flags.
addRegWithRegWithFlags :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithFlags r1 r2 gb = addWithFlags8 (getRegister r1 gb) (getRegister r2 gb) (\d -> (\gb1 -> setRegister r1 d gb)) gb

--GAMEBOY
-- | adds two registers r1 and r2 together and stores the value in r1 and sets the appropriate flags.
addRegWithRegWithFlagsPlusC :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithFlagsPlusC r1 r2 gb = addWithFlags8PlusC (getRegister r1 gb) (getRegister r2 gb) (getFlag gb carryFlag)
                                       (\d -> (\gb1 -> setRegister r1 d gb)) gb

--GAMEBOY
-- | adds two register together r1 and r2 and stores the value in r1 and sets no flags.
addRegWithRegWithoutFlags :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithoutFlags r1 r2 gb = setRegister r1 ((getRegister r1 gb) + (getRegister r2 gb)) gb

--GAMEBOY
-- | adds two register pairs together rs1 and rs2 and stores the result in rs1 and sets the appropriate flags.
addRegRegWithRegRegWithFlags :: (Register, Register) -> (Register, Register) -> (Gameboy -> Gameboy)
addRegRegWithRegRegWithFlags rs1 rs2 gb = addWithFlags16 (getRegisters rs1 gb) (getRegisters rs2 gb) (\d -> (\gb1 -> setRegisters rs1 d gb1)) gb

--GAMEBOY
-- | adds two register pairs together rs1 and rs2 and stores the result in rs1 and sets no flags.
addRegRegWithRegRegWithoutFlags :: (Register, Register) -> (Register, Register) -> (Gameboy -> Gameboy)
addRegRegWithRegRegWithoutFlags rs1 rs2 gb = setRegisters rs1 ((getRegisters rs1 gb) + (getRegisters rs2 gb)) gb

--GAMEBOY
-- | subs r2 from r1 and stores the result back in r1
subRegWithRegWithFlags :: Register -> Register -> (Gameboy -> Gameboy)
subRegWithRegWithFlags r1 r2 gb = subWithFlags8 (getRegister r1 gb) (getRegister r2 gb) (\d -> (\gb1 -> setRegister r1 d gb)) gb

--GAMEBOY
-- | subs r2 from r1 and stores the result back in r1
subRegWithRegWithFlagsPlusC :: Register -> Register -> (Gameboy -> Gameboy)
subRegWithRegWithFlagsPlusC r1 r2 gb = subWithFlags8PlusC (getRegister r1 gb) (getRegister r2 gb) (getFlag gb carryFlag)
                                       (\d -> (\gb1 -> setRegister r1 d gb)) gb

--GAMEBOY
-- | given a gameboy and a flag constant return wether or not it is set.
-- TODO change the api to Word8 -> Bool -> Gameboy pls.
getFlag :: Gameboy -> Word8 -> Bool
getFlag gb w = testBit (getRegister F gb) (flagToInt w)

--GAMEBOY
-- | Given a gameboy rotate it's accumulator to the left and store the 7th bit in the carry.
rotateLeftACarry :: (Gameboy -> Gameboy)
rotateLeftACarry = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) .
                   (\gb -> setFlag carryFlag (testBit (getRegister A gb) 0) gb) .
                   (\gb -> setRegister A (rotateL (getRegister A gb) 1) gb)

--GAMEBOY
-- | Rotate a register to the left and set the carry appropriately TODO check logic.
rotateLeft :: Register -> (Gameboy -> Gameboy)
rotateLeft r gb = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) $
                 (\gb1 -> (setRegister r (((getRegister r gb1) .&. 0b11111110) .|. carryBeforeMask) gb1)) $
                 setFlag carryFlag carryAfter $
                 setRegister r (rotateL (getRegister r gb) 1) gb
  where
    carryBefore = getFlag gb carryFlag
    carryAfter  = testBit (getRegister r gb) 7
    carryBeforeMask = if carryBefore then 0b00000001 else 0b00000000

--GAMEBOY
-- | Rotate a register to the left and set the carry appropriately TODO check logic.
rotateRight :: Register -> (Gameboy -> Gameboy)
rotateRight r gb = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) $
                  (\gb1 -> (setRegister r (((getRegister r gb1) .&. 0b11111110) .|. carryBeforeMask) gb1)) $
                  setFlag carryFlag carryAfter $
                  setRegister r (rotateR (getRegister r gb) 1) gb
  where
    carryBefore = getFlag gb carryFlag
    carryAfter  = testBit (getRegister r gb) 0
    carryBeforeMask = if carryBefore then 0b10000000 else 0b00000000

--GAMEBOY
-- | Rotate left but for the accumulator.
rotateLeftA :: Gameboy -> Gameboy
rotateLeftA = rotateLeft A

--GAMEBOY
-- | Rotate right but for the accumulator.
rotateRightA :: Gameboy -> Gameboy
rotateRightA = rotateRight A

--GAMEBOY
-- | Rotate accumlator right and set the carry flag tot he value in the 0th bit.
rotateRightACarry :: (Gameboy -> Gameboy)
rotateRightACarry = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) .
                    (\gb -> setFlag carryFlag (testBit (getRegister A gb) 7) gb) .
                    (\gb -> setRegister A (rotateR (getRegister A gb) 1) gb)

-- TODO everything below this comment needs documentation.

xorReg :: Register -> (Gameboy -> Gameboy)
xorReg r gb = setRegister A (xor (getRegister A gb) (getRegister r gb)) gb

andReg :: Register -> (Gameboy -> Gameboy)
andReg r gb = setRegister A ((.&.) (getRegister A gb) (getRegister r gb)) gb

orReg :: Register -> (Gameboy -> Gameboy)
orReg r gb = setRegister A ((.|.) (getRegister A gb) (getRegister r gb)) gb

wordToSignedInt :: Word8 -> Int
wordToSignedInt w
  | testBit w 7 == True = - (fromIntegral $ (complement w) + 1)
  | otherwise = fromIntegral w

ldFFRegAddrReg :: Register -> Register -> (Gameboy -> IO Gameboy)
ldFFRegAddrReg d s gb = setMemory (0xFF00 + (toWord16 $ getRegister d gb)) (getRegister s gb) gb

ldhA :: Gameboy -> IO Gameboy
ldhA gb = join (\mem -> return $ setMemory (0xFF00 + (toWord16 mem)) (getRegister A gb1) gb1) =<< getMemory (getRegisters (PHI,CLO) gb1) gb1
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

(.:) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c
f .: g = liftM f . g

push :: Word16 -> (Gameboy -> IO Gameboy)
push d gb = join (\gb_ -> return $ pushByte dhi gb_) =<< (pushByte dlo gb)
  where
    dhi = toWord8 $ shiftR d 8
    dlo = toWord8 $ 0x00FF .&. d
    pushByte = \b  -> decrementRegistersWithoutFlags (SHI, PLO) .:
               (\gb -> setMemory (getRegisters (SHI, PLO) gb) b gb)

pop :: (Register, Register) -> (Gameboy -> IO Gameboy)
pop rs gb = do { mem1 <- getMemory (getRegisters (SHI, PLO) gb1) gb
               ; mem2 <- getMemory (getRegisters (SHI, PLO) gb2) gb
               ; let combinedMem = combineData mem1 mem2
                 in return $ setRegisters rs combinedMem gb2}
  where gb1 = incrementRegistersWithoutFlags (SHI, PLO) gb
        gb2 = incrementRegistersWithoutFlags (SHI, PLO) gb1


--TODO Combine emu data makes me very uncomfortable here. I'm doing something wrong.
call :: Gameboy -> IO Gameboy
call gb = do { mem1 <- getMemory (getRegisters (PHI, CLO) gb1) gb1
             ; mem2 <- getMemory (getRegisters (PHI, CLO) gb2) gb2
             ; let combinedMem = combineData mem2 mem1
               in do { pushedGb <- push (getRegisters (PHI, CLO) gb3) gb
                     ; let setGb = setRegisters (PHI, CLO) combinedMem pushedGb
                       in return $ decrementRegistersWithoutFlags (PHI, CLO) setGb }}
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb
    gb2 = incrementRegistersWithoutFlags (PHI, CLO) gb1
    gb3 = incrementRegistersWithoutFlags (PHI, CLO) gb2

ret :: Gameboy -> IO Gameboy
ret gb = do { mem1 <- getMemory (getRegisters (SHI, PLO) gb1) gb
            ; mem2 <- getMemory (getRegisters (SHI, PLO) gb2) gb
            ; let combinedMem = combineData mem1 mem2
              in return $ setRegisters (PHI, CLO) (combinedMem - 1) gb2 }
   where
     gb1 = incrementRegistersWithoutFlags (SHI, PLO) gb
     gb2 = incrementRegistersWithoutFlags (SHI, PLO) gb1

--TODO double check logic
ldMemDataWithRegReg :: (Register, Register) -> (Gameboy -> IO Gameboy)
ldMemDataWithRegReg (r1, r2) gb = do { mem1 <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                                     ; mem2 <- getMemory (getRegisters (PHI, CLO) gb2) gb2
                                     ; let combinedMem = combineData mem1 mem2
                                       in  do { gbMem1 <- setMemory combinedMem (getRegister r2 gb2) gb2
                                              ; setMemory (combinedMem + 1) (getRegister r1 gbMem1) gbMem1 }}
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb
    gb2 = incrementRegistersWithoutFlags (PHI, CLO) gb1

jumpRelative :: (Gameboy -> IO Gameboy)
jumpRelative gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                     ; let signedJmp = wordToSignedInt mem
                       in return $ setRegisters (PHI, CLO)
                       (fromIntegral ((fromIntegral $ getRegisters (PHI, CLO) gb1) + signedJmp)) gb1 }
  where
    gb1 = incrementRegistersWithoutFlags (CLO, PHI) gb

jumpIfRelative :: Bool -> (Gameboy -> IO Gameboy)
jumpIfRelative False gb = return gb
jumpIfRelative True  gb = jumpRelative gb



--TODO daa sets flags apparently.
daa :: (Gameboy -> Gameboy)
daa gb = setRegister A (regA + calculateHigh + calculateLow) gb
    where
      regA          = getRegister A gb
      nibbleHigh    = regA .&. 0xF0
      nibbleLow     = regA .&. 0x0F
      calculateHigh = if (nibbleHigh > 144) || (getFlag gb carryFlag)
                      then 0x60
                      else 0x00
      calculateLow  = if (nibbleLow > 9) || (getFlag gb halfCarryFlag)
                      then 0x06
                      else 0x00

scf :: (Gameboy -> Gameboy)
scf = (setFlag carryFlag True) . (setFlag (halfCarryFlag .&. subtractFlag) False)

cpl :: (Gameboy -> Gameboy)
cpl gb = setRegister A (xor (getRegister A gb) 0xFF) gb

ccf :: (Gameboy -> Gameboy)
ccf gb = (setFlag carryFlag (not $ getFlag gb carryFlag)) . (setFlag (halfCarryFlag .&. subtractFlag) False) $ gb

addRegWithRegRegMemWithFlags :: Register -> (Register, Register) -> (Gameboy -> IO Gameboy)
addRegWithRegRegMemWithFlags r rs gb = do { mem <- getMemory (getRegisters rs gb) gb
                                          ; return $ addWithFlags8 (getRegister A gb) mem (\w -> setRegister A w) gb }

addRegWithRegRegMemWithFlagsPlusC :: Register -> (Register, Register) -> (Gameboy -> IO Gameboy)
addRegWithRegRegMemWithFlagsPlusC r rs gb = do { mem <- getMemory (getRegisters rs gb) gb
                                               ; return $ addWithFlags8PlusC (getRegister A gb) mem (getFlag gb carryFlag)
                                                 (\w -> setRegister A w) gb }

cpReg :: Register -> (Gameboy -> Gameboy)
cpReg r2 gb = subWithFlags8 (getRegister A gb) (getRegister r2 gb) (\_ -> \gb1 -> gb1) gb

fixGB :: (Gameboy -> Gameboy) -> (Gameboy -> IO Gameboy)
fixGB gb = (\gb_ -> return gb_ >>= (\gb__ -> return $ gb gb__))

decodeOp :: Word8 -> Instruction
decodeOp 0x00 = Instruction 0x00 "NOP" $ \gb -> return gb
decodeOp 0x01 = Instruction 0x01 "LD BC, d16" $ ldRegRegWithData (B, C)
decodeOp 0x02 = Instruction 0x02 "LD (BC), A" $ ldMemRegRegWithReg (B, C) A
decodeOp 0x03 = Instruction 0x03 "INC BC" $ fixGB $ incrementRegistersWithoutFlags (B, C)
decodeOp 0x04 = Instruction 0x04 "INC B" $ fixGB $ incrementRegisterWithFlags B
decodeOp 0x05 = Instruction 0x05 "DEC B" $ fixGB $ decrementRegisterWithFlags B
decodeOp 0x06 = Instruction 0x06 "LD B, d8" $ ldRegWithData B
decodeOp 0x07 = Instruction 0x07 "RLCA" $ fixGB rotateLeftACarry
decodeOp 0x08 = Instruction 0x08 "LD (a16), SP" $ ldMemDataWithRegReg (SHI, PLO)
decodeOp 0x09 = Instruction 0x09 "ADD HL, BC" $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (B, C)
decodeOp 0x0A = Instruction 0x0A "LD A, (BC)" $ ldRegWithRegRegMem A (B, C)
decodeOp 0x0B = Instruction 0x0B "DEC BC" $ fixGB $ decrementRegistersWithoutFlags (B, C)
decodeOp 0x0C = Instruction 0x0C "INC C" $ fixGB $ incrementRegisterWithFlags C
decodeOp 0x0D = Instruction 0x0D "DEC C" $ fixGB $ decrementRegisterWithFlags C
decodeOp 0x0E = Instruction 0x0E "LD C, d8" $ ldRegWithData C
decodeOp 0x0F = Instruction 0x0F "RRCA" $ fixGB rotateRightACarry
--TODO 0x10 "STOP 0"
decodeOp 0x11 = Instruction 0x11 "LD DE, d16" $ ldRegRegWithData (D, E)
decodeOp 0x12 = Instruction 0x12 "LD (DE), A" $ ldMemRegRegWithReg (D, E) A
decodeOp 0x13 = Instruction 0x13 "INC DE" $ fixGB$ incrementRegistersWithoutFlags (D, E)
decodeOp 0x14 = Instruction 0x14 "INC D" $ fixGB $ incrementRegisterWithFlags D
decodeOp 0x15 = Instruction 0x15 "DEC D" $ fixGB $ decrementRegisterWithFlags D
decodeOp 0x16 = Instruction 0x16 "LD D, d8" $ ldRegWithData D
decodeOp 0x17 = Instruction 0x17 "RLA" $ fixGB rotateLeftA
decodeOp 0x18 = Instruction 0x18 "JR r8" jumpRelative
decodeOp 0x19 = Instruction 0x19 "ADD HL, DE" $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (D, E)
decodeOp 0x1A = Instruction 0x1A "LD A, (DE)" $ ldRegWithRegRegMem A (D, E)
decodeOp 0x1B = Instruction 0x1B "DEC BC" $ fixGB $ decrementRegistersWithoutFlags (D, E)
decodeOp 0x1C = Instruction 0x1C "INC E" $ fixGB $ incrementRegisterWithFlags E
decodeOp 0x1D = Instruction 0x1D "DEC E" $ fixGB $ decrementRegisterWithFlags E
decodeOp 0x1E = Instruction 0x1E "LD C, d8" $ ldRegWithData C
decodeOp 0x1F = Instruction 0x1F "RRA" $ fixGB rotateRightA
decodeOp 0x20 = Instruction 0x20 "JR NZ, r8" $ \gb -> jumpIfRelative (not $ getFlag gb zeroFlag) gb
decodeOp 0x21 = Instruction 0x21 "LD HL, d16" $ ldRegRegWithData (H, L)
decodeOp 0x22 = Instruction 0x22 "LD (HL+), A" $ \gb -> do { ldedGB <- ldMemRegRegWithReg (H, L) A gb
                                                           ; return $ incrementRegistersWithoutFlags (H, L) ldedGB }
decodeOp 0x23 = Instruction 0x23 "INC HL" $ fixGB $ incrementRegistersWithoutFlags (H, L)
decodeOp 0x24 = Instruction 0x24 "INC H" $ fixGB $ incrementRegisterWithFlags H
decodeOp 0x25 = Instruction 0x25 "DEC H" $ fixGB $ decrementRegisterWithFlags H
decodeOp 0x26 = Instruction 0x26 "LD H, d8" $ ldRegWithData H
decodeOp 0x27 = Instruction 0x27 "DAA" $ fixGB daa
decodeOp 0x28 = Instruction 0x28 "JR Z,r8" $ \gb -> jumpIfRelative (getFlag gb zeroFlag) gb
decodeOp 0x29 = Instruction 0x29 "ADD HL, HL" $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (H, L)
decodeOp 0x2A = Instruction 0x2A "LD A, (HL+)" $ \gb -> do { ldedGB <- ldRegWithRegRegMem A (H,L) gb
                                                           ; return $ incrementRegistersWithoutFlags (H, L) ldedGB }
decodeOp 0x2B = Instruction 0x2B "DEC HL" $ fixGB $ decrementRegistersWithoutFlags (H, L)
decodeOp 0x2C = Instruction 0x2C "INC L" $ fixGB $ incrementRegisterWithFlags L
decodeOp 0x2D = Instruction 0x2D "DEC L" $ fixGB $ decrementRegisterWithFlags L
decodeOp 0x2E = Instruction 0x2E "LD L, d8" $ ldRegWithData L
decodeOp 0x2F = Instruction 0x2F "CPL" $ fixGB cpl
decodeOp 0x30 = Instruction 0x30 "JR NC, r8" $ \gb -> jumpIfRelative (not $ getFlag gb carryFlag) gb
decodeOp 0x31 = Instruction 0x31 "LD SP, d16" $ ldRegRegWithData (SHI, PLO)
decodeOp 0x32 = Instruction 0x32 "LD (HL-), A" $ \gb -> do { ldedGB <- ldMemRegRegWithReg (H, L) A gb
                                                           ; return $ decrementRegistersWithoutFlags (H, L) ldedGB }
decodeOp 0x33 = Instruction 0x33 "INC SP" $ fixGB $ incrementRegistersWithoutFlags (SHI, PLO)
decodeOp 0x34 = Instruction 0x34 "INC (HL)" $ incrementMemoryRegReg (H, L)
decodeOp 0x35 = Instruction 0x35 "DEC (HL)" $ decrementMemoryRegReg (H, L)
decodeOp 0x36 = Instruction 0x36 "LD (HL), d8" $ ldMemRegRegWithData (H, L)
decodeOp 0x37 = Instruction 0x37 "SCF" $ fixGB scf
decodeOp 0x38 = Instruction 0x38 "JR C, r8" $ \gb -> jumpIfRelative (getFlag gb carryFlag) gb
decodeOp 0x39 = Instruction 0x39 "ADD HL, SP" $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (SHI, PLO)
decodeOp 0x3A = Instruction 0x3A "LD A, (HL-)" $ \gb -> do { ldedGB <- ldRegWithRegRegMem A (H, L) gb
                                                           ; return $ decrementRegistersWithoutFlags (H, L) ldedGB }
decodeOp 0x3B = Instruction 0x3B "DEC SP" $ fixGB $ decrementRegistersWithoutFlags (SHI, PLO)
decodeOp 0x3C = Instruction 0x3C "INC A" $ fixGB $ incrementRegisterWithFlags A
decodeOp 0x3D = Instruction 0x3D "DEC A" $ fixGB $ decrementRegisterWithFlags A
decodeOp 0x3E = Instruction 0x3E "LD A, d8" $ ldRegWithData A
decodeOp 0x3F = Instruction 0x3F "CCF" $ fixGB ccf
decodeOp 0x40 = Instruction 0x40 "LD B, B" $ fixGB id
decodeOp 0x41 = Instruction 0x41 "LD B, C" $ fixGB $ ldRegWithReg B C
decodeOp 0x42 = Instruction 0x42 "LD B, D" $ fixGB $ ldRegWithReg B D
decodeOp 0x43 = Instruction 0x43 "LD B, E" $ fixGB $ ldRegWithReg B E
decodeOp 0x44 = Instruction 0x44 "LD B, H" $ fixGB $ ldRegWithReg B H
decodeOp 0x45 = Instruction 0x45 "LD B, L" $ fixGB $ ldRegWithReg B L
decodeOp 0x46 = Instruction 0x46 "LD B, (HL)" $ ldRegWithRegRegMem B (H, L)
decodeOp 0x47 = Instruction 0x46 "LD B, A" $ fixGB $ ldRegWithReg B A
decodeOp 0x48 = Instruction 0x48 "LD C, B" $ fixGB $ ldRegWithReg C B
decodeOp 0x49 = Instruction 0x49 "LD C, C" $ fixGB id
decodeOp 0x4A = Instruction 0x4A "LD C, D" $ fixGB $ ldRegWithReg C D
decodeOp 0x4B = Instruction 0x4B "LD C, E" $ fixGB $ ldRegWithReg C E
decodeOp 0x4C = Instruction 0x4C "LD C, H" $ fixGB $ ldRegWithReg C H
decodeOp 0x4D = Instruction 0x4D "LD C, L" $ fixGB $ ldRegWithReg C L
decodeOp 0x4E = Instruction 0x4E "LD C, (HL)" $ ldRegWithRegRegMem C (H, L)
decodeOp 0x4F = Instruction 0x4F "LD C, A" $ fixGB $ ldRegWithReg C A
decodeOp 0x50 = Instruction 0x50 "LD D, B" $ fixGB $ ldRegWithReg D B
decodeOp 0x51 = Instruction 0x51 "LD D, C" $ fixGB $ ldRegWithReg D C
decodeOp 0x52 = Instruction 0x52 "LD D, D" $ fixGB id
decodeOp 0x53 = Instruction 0x53 "LD D, E" $ fixGB $ ldRegWithReg D E
decodeOp 0x54 = Instruction 0x54 "LD D, H" $ fixGB $ ldRegWithReg D H
decodeOp 0x55 = Instruction 0x55 "LD D, L" $ fixGB $ ldRegWithReg D L
decodeOp 0x56 = Instruction 0x56 "LD D, (HL)" $ ldRegWithRegRegMem D (H, L)
decodeOp 0x57 = Instruction 0x57 "LD D, A" $ fixGB $ ldRegWithReg D A
decodeOp 0x58 = Instruction 0x58 "LD E, B" $ fixGB $ ldRegWithReg E B
decodeOp 0x59 = Instruction 0x59 "LD E, C" $ fixGB $ ldRegWithReg E C
decodeOp 0x5A = Instruction 0x5A "LD E, D" $ fixGB $ ldRegWithReg E D
decodeOp 0x5B = Instruction 0x5B "LD E, E" $ fixGB id
decodeOp 0x5C = Instruction 0x5C "LD E, H" $ fixGB $ ldRegWithReg E H
decodeOp 0x5D = Instruction 0x5D "LD E, L" $ fixGB $ ldRegWithReg E L
decodeOp 0x5E = Instruction 0x5E "LD E, (HL)" $ ldRegWithRegRegMem E (H, L)
decodeOp 0x5F = Instruction 0x5F "LD E, A" $ fixGB $ ldRegWithReg E A
decodeOp 0x60 = Instruction 0x60 "LD H, B" $ fixGB $ ldRegWithReg H B
decodeOp 0x61 = Instruction 0x61 "LD H, C" $ fixGB $ ldRegWithReg H C
decodeOp 0x62 = Instruction 0x62 "LD H, D" $ fixGB $ ldRegWithReg H D
decodeOp 0x63 = Instruction 0x63 "LD H, E" $ fixGB $ ldRegWithReg H E
decodeOp 0x64 = Instruction 0x64 "LD H, H" $ fixGB id
decodeOp 0x65 = Instruction 0x65 "LD H, L" $ fixGB $ ldRegWithReg H L
decodeOp 0x66 = Instruction 0x66 "LD H, (HL)" $ ldRegWithRegRegMem H (H, L)
decodeOp 0x67 = Instruction 0x67 "LD H, A" $ fixGB $ ldRegWithReg H A
decodeOp 0x68 = Instruction 0x68 "LD L, B" $ fixGB $ ldRegWithReg L B
decodeOp 0x69 = Instruction 0x69 "LD L, C" $ fixGB $ ldRegWithReg L C
decodeOp 0x6A = Instruction 0x6A "LD L, D" $ fixGB $ ldRegWithReg L D
decodeOp 0x6B = Instruction 0x6B "LD L, E" $ fixGB $ ldRegWithReg L E
decodeOp 0x6C = Instruction 0x6C "LD L, H" $ fixGB $ ldRegWithReg L H
decodeOp 0x6D = Instruction 0x6D "LD L, L" $ fixGB id
decodeOp 0x6E = Instruction 0x6E "LD L, (HL)" $ ldRegWithRegRegMem L (H, L)
decodeOp 0x6F = Instruction 0x6F "LD L, A" $ fixGB $ ldRegWithReg L A
decodeOp 0x70 = Instruction 0x70 "LD (HL) B" $ ldMemRegRegWithReg (H, L) B
decodeOp 0x71 = Instruction 0x71 "LD (HL) C" $ ldMemRegRegWithReg (H, L) C
decodeOp 0x72 = Instruction 0x72 "LD (HL) D" $ ldMemRegRegWithReg (H, L) D
decodeOp 0x73 = Instruction 0x73 "LD (HL) E" $ ldMemRegRegWithReg (H, L) E
decodeOp 0x74 = Instruction 0x74 "LD (HL) H" $ ldMemRegRegWithReg (H, L) H
decodeOp 0x75 = Instruction 0x75 "LD (HL) L" $ ldMemRegRegWithReg (H, L) L
--TODO 0x76 "HALT"
decodeOp 0x77 = Instruction 0x77 "LD (HL) A" $ ldMemRegRegWithReg (H, L) A
decodeOp 0x78 = Instruction 0x78 "LD A B" $ fixGB $ ldRegWithReg A B
decodeOp 0x79 = Instruction 0x79 "LD A C" $ fixGB $ ldRegWithReg A C
decodeOp 0x7A = Instruction 0x7A "LD A D" $ fixGB $ ldRegWithReg A D
decodeOp 0x7B = Instruction 0x7B "LD A E" $ fixGB $ ldRegWithReg A E
decodeOp 0x7C = Instruction 0x7C "LD A H" $ fixGB $ ldRegWithReg A H
decodeOp 0x7D = Instruction 0x7D "LD A L" $ fixGB $ ldRegWithReg A L
decodeOp 0x7E = Instruction 0x7E "LD A (HL)" $ ldRegWithRegRegMem A (H, L)
decodeOp 0x7F = Instruction 0x7F "LD A A" $ fixGB id
decodeOp 0x80 = Instruction 0x80 "ADD A, B" $ fixGB $ addRegWithRegWithFlags A B
decodeOp 0x81 = Instruction 0x81 "ADD A, C" $ fixGB $ addRegWithRegWithFlags A C
decodeOp 0x82 = Instruction 0x82 "ADD A, D" $ fixGB $ addRegWithRegWithFlags A D
decodeOp 0x83 = Instruction 0x83 "ADD A, E" $ fixGB $ addRegWithRegWithFlags A E
decodeOp 0x84 = Instruction 0x84 "ADD A, H" $ fixGB $ addRegWithRegWithFlags A H
decodeOp 0x85 = Instruction 0x85 "ADD A, L" $ fixGB $ addRegWithRegWithFlags A L
decodeOp 0x86 = Instruction 0x86 "ADD A, (HL)" $ addRegWithRegRegMemWithFlags A (H, L)
decodeOp 0x87 = Instruction 0x87 "ADD A, A" $ fixGB $ addRegWithRegWithFlags A A
decodeOp 0x88 = Instruction 0x88 "ADC A, B" $ fixGB $ addRegWithRegWithFlagsPlusC A B
decodeOp 0x89 = Instruction 0x89 "ADC A, C" $ fixGB $ addRegWithRegWithFlagsPlusC A C
decodeOp 0x8A = Instruction 0x8A "ADC A, D" $ fixGB $ addRegWithRegWithFlagsPlusC A D
decodeOp 0x8B = Instruction 0x8B "ADC A, E" $ fixGB $ addRegWithRegWithFlagsPlusC A E
decodeOp 0x8C = Instruction 0x8C "ADC A, H" $ fixGB $ addRegWithRegWithFlagsPlusC A H
decodeOp 0x8D = Instruction 0x8D "ADC A, L" $ fixGB $ addRegWithRegWithFlagsPlusC A B
decodeOp 0x8E = Instruction 0x8E "ADC A, (HL)" $ addRegWithRegRegMemWithFlagsPlusC A (H, L)
decodeOp 0x8F = Instruction 0x8F "ADC A, A" $ fixGB $ addRegWithRegWithFlagsPlusC A A
decodeOp 0x90 = Instruction 0x90 "SUB B" $ fixGB $ subRegWithRegWithFlags A B
decodeOp 0x91 = Instruction 0x91 "SUB C" $ fixGB $ subRegWithRegWithFlags A C
decodeOp 0x92 = Instruction 0x92 "SUB D" $ fixGB $ subRegWithRegWithFlags A D
decodeOp 0x93 = Instruction 0x93 "SUB E" $ fixGB $ subRegWithRegWithFlags A E
decodeOp 0x94 = Instruction 0x94 "SUB H" $ fixGB $ subRegWithRegWithFlags A H
decodeOp 0x95 = Instruction 0x95 "SUB L" $ fixGB $ subRegWithRegWithFlags A L
--TODO 0x96
decodeOp 0x97 = Instruction 0x97 "SUB A" $ fixGB $ subRegWithRegWithFlags A A
decodeOp 0x98 = Instruction 0x98 "SBC A, B" $ fixGB $ subRegWithRegWithFlagsPlusC A B
decodeOp 0x99 = Instruction 0x99 "SBC A, C" $ fixGB $ subRegWithRegWithFlagsPlusC A C
decodeOp 0x9A = Instruction 0x9A "SBC A, D" $ fixGB $ subRegWithRegWithFlagsPlusC A D
decodeOp 0x9B = Instruction 0x9B "SBC A, E" $ fixGB $ subRegWithRegWithFlagsPlusC A E
decodeOp 0x9C = Instruction 0x9C "SBC A, H" $ fixGB $ subRegWithRegWithFlagsPlusC A H
decodeOp 0x9D = Instruction 0x9D "SBC A, L" $ fixGB $ subRegWithRegWithFlagsPlusC A L
--TODO 0x9E
decodeOp 0x9F = Instruction 0x9F "SBC A, A" $ fixGB $ subRegWithRegWithFlagsPlusC A A
decodeOp 0xA0 = Instruction 0xA0 "AND B" $ fixGB $ andReg B
decodeOp 0xA1 = Instruction 0xA1 "AND C" $ fixGB $ andReg C
decodeOp 0xA2 = Instruction 0xA2 "AND D" $ fixGB $ andReg D
decodeOp 0xA3 = Instruction 0xA3 "AND E" $ fixGB $ andReg E
decodeOp 0xA4 = Instruction 0xA4 "AND H" $ fixGB $ andReg H
decodeOp 0xA5 = Instruction 0xA5 "AND L" $ fixGB $ andReg L
--TODO 0xA6
decodeOp 0xA7 = Instruction 0xA7 "AND A" $ fixGB $ andReg A
decodeOp 0xA8 = Instruction 0xA8 "XOR B" $ fixGB $ xorReg B
decodeOp 0xA9 = Instruction 0xA9 "XOR C" $ fixGB $ xorReg C
decodeOp 0xAA = Instruction 0xAA "XOR D" $ fixGB $ xorReg D
decodeOp 0xAB = Instruction 0xAB "XOR E" $ fixGB $ xorReg E
decodeOp 0xAC = Instruction 0xAC "XOR H" $ fixGB $ xorReg H
decodeOp 0xAD = Instruction 0xAD "XOR L" $ fixGB $ xorReg L
--TODO 0xAE
decodeOp 0xAF = Instruction 0xAF "XOR A" $ fixGB $ xorReg A
decodeOp 0xB0 = Instruction 0xB0 "OR B" $ fixGB $ orReg B
decodeOp 0xB1 = Instruction 0xB1 "OR C" $ fixGB $ orReg C
decodeOp 0xB2 = Instruction 0xB2 "OR D" $ fixGB $ orReg D
decodeOp 0xB3 = Instruction 0xB3 "OR E" $ fixGB $ orReg E
decodeOp 0xB4 = Instruction 0xB4 "OR H" $ fixGB $ orReg H
decodeOp 0xB5 = Instruction 0xB5 "OR L" $ fixGB $ orReg L
--TODO 0xB6 - 0xC0
decodeOp 0xB7 = Instruction 0xB7 "OR A" $ fixGB $ orReg A
decodeOp 0xB8 = Instruction 0xB8 "CP B" $ fixGB $ cpReg B
decodeOp 0xB9 = Instruction 0xB9 "CP C" $ fixGB $ cpReg C
decodeOp 0xBA = Instruction 0xBA "CP D" $ fixGB $ cpReg D
decodeOp 0xBB = Instruction 0xBB "CP E" $ fixGB $ cpReg E
decodeOp 0xBC = Instruction 0xBC "CP H" $ fixGB $ cpReg H
decodeOp 0xBD = Instruction 0xBD "CP L" $ fixGB $ cpReg L
--TODO 0xBE
decodeOp 0xBF = Instruction 0xBF "CP A" $ fixGB $ cpReg A
decodeOp 0xC1 = Instruction 0xC1 "POP BC" $ pop (B, C)
--TODO 0xC2 - 0xC4
decodeOp 0xC5 = Instruction 0xC5 "PUSH BC" $ \gb -> push (getRegisters (B, C) gb) gb
--TODO 0xC6 - 0xC8
decodeOp 0xC9 = Instruction 0xC9 "RET" $ ret
--TODO 0xCA
decodeOp 0xCB = Instruction 0xCB "[CB Instruction]" $ \gb -> do { cb <- fetchCb $ incrementRegistersWithoutFlags (PHI, CLO) gb
                                                                ; return $ decodeCb cb (incrementRegistersWithoutFlags (PHI,CLO) gb) }
--TODO 0xCC
decodeOp 0xCD = Instruction 0xCD "CALL a8" $ call
--TODO 0xCE - 0xDF
decodeOp 0xE0 = Instruction 0xE0 "LD (a8), A" $ ldhA
--TODO 0xE1
decodeOp 0xE2 = Instruction 0xE2 "LD (C), A" $ ldFFRegAddrReg C A
--TODO 0xE3 - 0xFF


fetchCb :: Gameboy -> IO Word8
fetchCb gb = getMemory (getRegisters (PHI, CLO) gb) gb

testBitReg :: Register -> Int -> (Gameboy -> Gameboy)
testBitReg r i = \gb -> zero . sub . half $ gb
  where
    isSet = \gb -> testBit (getRegister r gb) i
    zero  = \gb -> (setFlag zeroFlag $ isSet gb) $ gb
    sub   = setFlag subtractFlag  False
    half  = setFlag halfCarryFlag True

decodeCb :: Word8 -> (Gameboy -> Gameboy)
decodeCb 0x7C = testBitReg H 7
decodeCb 0x11 = rotateLeft C

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

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)


stepNGameboy :: Int -> (Gameboy -> IO Gameboy)
stepNGameboy n = Prelude.foldl (.|) (fixGB id) $ Prelude.replicate n stepGameboy

loadBootRom :: Gameboy -> IO Gameboy
loadBootRom gb = (\gb_ -> setMemory 0x00FF 0x50 gb_) .|
                 (\gb_ -> setMemory 0x00FE 0xE0 gb_) .|
                 (\gb_ -> setMemory 0x00FD 0x01 gb_) .|
                 (\gb_ -> setMemory 0x00FC 0x3E gb_) .|
                 (\gb_ -> setMemory 0x00FB 0xFE gb_) .|
                 (\gb_ -> setMemory 0x00FA 0x20 gb_) .|
                 (\gb_ -> setMemory 0x00F9 0x86 gb_) .|
                 (\gb_ -> setMemory 0x00F8 0xFB gb_) .|
                 (\gb_ -> setMemory 0x00F7 0x20 gb_) .|
                 (\gb_ -> setMemory 0x00F6 0x05 gb_) .|
                 (\gb_ -> setMemory 0x00F5 0x23 gb_) .|
                 (\gb_ -> setMemory 0x00F4 0x86 gb_) .|
                 (\gb_ -> setMemory 0x00F3 0x78 gb_) .|
                 (\gb_ -> setMemory 0x00F2 0x19 gb_) .|
                 (\gb_ -> setMemory 0x00F1 0x06 gb_) .|
                 (\gb_ -> setMemory 0x00F0 0xF5 gb_) .|
                 (\gb_ -> setMemory 0x00EF 0x20 gb_) .|
                 (\gb_ -> setMemory 0x00EE 0x34 gb_) .|
                 (\gb_ -> setMemory 0x00ED 0xFE gb_) .|
                 (\gb_ -> setMemory 0x00EC 0x7D gb_) .|
                 (\gb_ -> setMemory 0x00EB 0x23 gb_) .|
                 (\gb_ -> setMemory 0x00EA 0xFE gb_) .|
                 (\gb_ -> setMemory 0x00E9 0x20 gb_) .|
                 (\gb_ -> setMemory 0x00E8 0xBE gb_) .|
                 (\gb_ -> setMemory 0x00E7 0x13 gb_) .|
                 (\gb_ -> setMemory 0x00E6 0x1A gb_) .|
                 (\gb_ -> setMemory 0x00E5 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00E4 0xA8 gb_) .|
                 (\gb_ -> setMemory 0x00E3 0x11 gb_) .|
                 (\gb_ -> setMemory 0x00E2 0x01 gb_) .|
                 (\gb_ -> setMemory 0x00E1 0x04 gb_) .|
                 (\gb_ -> setMemory 0x00E0 0x21 gb_) .|
                 (\gb_ -> setMemory 0x00DF 0x3C gb_) .|
                 (\gb_ -> setMemory 0x00DE 0x42 gb_) .|
                 (\gb_ -> setMemory 0x00DD 0xA5 gb_) .|
                 (\gb_ -> setMemory 0x00DC 0xB9 gb_) .|
                 (\gb_ -> setMemory 0x00DB 0xA5 gb_) .|
                 (\gb_ -> setMemory 0x00DA 0xB9 gb_) .|
                 (\gb_ -> setMemory 0x00D9 0x42 gb_) .|
                 (\gb_ -> setMemory 0x00D8 0x3C gb_) .|
                 (\gb_ -> setMemory 0x00D7 0x3E gb_) .|
                 (\gb_ -> setMemory 0x00D6 0x33 gb_) .|
                 (\gb_ -> setMemory 0x00D5 0xB9 gb_) .|
                 (\gb_ -> setMemory 0x00D4 0xBB gb_) .|
                 (\gb_ -> setMemory 0x00D3 0x9F gb_) .|
                 (\gb_ -> setMemory 0x00D2 0x99 gb_) .|
                 (\gb_ -> setMemory 0x00D1 0xDC gb_) .|
                 (\gb_ -> setMemory 0x00D0 0xDD gb_) .|
                 (\gb_ -> setMemory 0x00CF 0xCC gb_) .|
                 (\gb_ -> setMemory 0x00CE 0xEC gb_) .|
                 (\gb_ -> setMemory 0x00CD 0x0E gb_) .|
                 (\gb_ -> setMemory 0x00CC 0x6E gb_) .|
                 (\gb_ -> setMemory 0x00CB 0x63 gb_) .|
                 (\gb_ -> setMemory 0x00CA 0x67 gb_) .|
                 (\gb_ -> setMemory 0x00C9 0xBB gb_) .|
                 (\gb_ -> setMemory 0x00C8 0xBB gb_) .|
                 (\gb_ -> setMemory 0x00C7 0x99 gb_) .|
                 (\gb_ -> setMemory 0x00C6 0xD9 gb_) .|
                 (\gb_ -> setMemory 0x00C5 0xDD gb_) .|
                 (\gb_ -> setMemory 0x00C4 0xDD gb_) .|
                 (\gb_ -> setMemory 0x00C3 0xE6 gb_) .|
                 (\gb_ -> setMemory 0x00C2 0x6E gb_) .|
                 (\gb_ -> setMemory 0x00C1 0xCC gb_) .|
                 (\gb_ -> setMemory 0x00C0 0xDC gb_) .|
                 (\gb_ -> setMemory 0x00BF 0x0E gb_) .|
                 (\gb_ -> setMemory 0x00BE 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00BD 0x89 gb_) .|
                 (\gb_ -> setMemory 0x00BC 0x88 gb_) .|
                 (\gb_ -> setMemory 0x00BB 0x1F gb_) .|
                 (\gb_ -> setMemory 0x00BA 0x11 gb_) .|
                 (\gb_ -> setMemory 0x00B9 0x08 gb_) .|
                 (\gb_ -> setMemory 0x00B8 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00B7 0x0D gb_) .|
                 (\gb_ -> setMemory 0x00B6 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00B5 0x0C gb_) .|
                 (\gb_ -> setMemory 0x00B4 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00B3 0x83 gb_) .|
                 (\gb_ -> setMemory 0x00B2 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00B1 0x73 gb_) .|
                 (\gb_ -> setMemory 0x00B0 0x03 gb_) .|
                 (\gb_ -> setMemory 0x00AF 0x0B gb_) .|
                 (\gb_ -> setMemory 0x00AE 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00AD 0x0D gb_) .|
                 (\gb_ -> setMemory 0x00AC 0xCC gb_) .|
                 (\gb_ -> setMemory 0x00AB 0x66 gb_) .|
                 (\gb_ -> setMemory 0x00AA 0x66 gb_) .|
                 (\gb_ -> setMemory 0x00A9 0xED gb_) .|
                 (\gb_ -> setMemory 0x00A8 0xCE gb_) .|
                 (\gb_ -> setMemory 0x00A7 0xC9 gb_) .|
                 (\gb_ -> setMemory 0x00A6 0x23 gb_) .| --Good
                 (\gb_ -> setMemory 0x00A5 0x22 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x00A4 0x23 gb_) .| --Good
                 (\gb_ -> setMemory 0x00A3 0x22 gb_) .| --24607 Maybe Good
                 (\gb_ -> setMemory 0x00A2 0xF5 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x00A1 0x20 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x00A0 0x05 gb_) .| --Good
                 (\gb_ -> setMemory 0x009F 0x17 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x009E 0x11 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x009D 0xCB gb_) .| --Good
                 (\gb_ -> setMemory 0x009C 0xC1 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x009B 0x17 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x009A 0x11 gb_) .| --24600 Maybe Good
                 (\gb_ -> setMemory 0x0099 0xCB gb_) .| --Good
                 (\gb_ -> setMemory 0x0098 0xC5 gb_) .| --Good
                 (\gb_ -> setMemory 0x0097 0x04 gb_) .| --Good
                 (\gb_ -> setMemory 0x0096 0x06 gb_) .| --Good
                 (\gb_ -> setMemory 0x0095 0x4F gb_) .| --Good
                 (\gb_ -> setMemory 0x0094 0xCB gb_) .|
                 (\gb_ -> setMemory 0x0093 0x18 gb_) .|
                 (\gb_ -> setMemory 0x0092 0x20 gb_) .|
                 (\gb_ -> setMemory 0x0091 0x16 gb_) .|
                 (\gb_ -> setMemory 0x0090 0x4F gb_) .|
                 (\gb_ -> setMemory 0x008F 0x20 gb_) .|
                 (\gb_ -> setMemory 0x008E 0x05 gb_) .|
                 (\gb_ -> setMemory 0x008D 0xD2 gb_) .|
                 (\gb_ -> setMemory 0x008C 0x20 gb_) .|
                 (\gb_ -> setMemory 0x008B 0x15 gb_) .|
                 (\gb_ -> setMemory 0x008A 0x42 gb_) .|
                 (\gb_ -> setMemory 0x0089 0xE0 gb_) .|
                 (\gb_ -> setMemory 0x0088 0x90 gb_) .|
                 (\gb_ -> setMemory 0x0087 0x42 gb_) .|
                 (\gb_ -> setMemory 0x0086 0xF0 gb_) .|
                 (\gb_ -> setMemory 0x0085 0xE2 gb_) .|
                 (\gb_ -> setMemory 0x0084 0x87 gb_) .|
                 (\gb_ -> setMemory 0x0083 0x3E gb_) .|
                 (\gb_ -> setMemory 0x0082 0x0C gb_) .|
                 (\gb_ -> setMemory 0x0081 0xE2 gb_) .|
                 (\gb_ -> setMemory 0x0080 0x7B gb_) .|
                 (\gb_ -> setMemory 0x007F 0x06 gb_) .|
                 (\gb_ -> setMemory 0x007E 0x20 gb_) .|
                 (\gb_ -> setMemory 0x007D 0x64 gb_) .|
                 (\gb_ -> setMemory 0x007C 0xFE gb_) .|
                 (\gb_ -> setMemory 0x007B 0xC1 gb_) .|
                 (\gb_ -> setMemory 0x007A 0x1E gb_) .|
                 (\gb_ -> setMemory 0x0079 0x06 gb_) .|
                 (\gb_ -> setMemory 0x0078 0x28 gb_) .|
                 (\gb_ -> setMemory 0x0077 0x62 gb_) .|
                 (\gb_ -> setMemory 0x0076 0xFE gb_) .|
                 (\gb_ -> setMemory 0x0075 0x83 gb_) .|
                 (\gb_ -> setMemory 0x0074 0x1E gb_) .|
                 (\gb_ -> setMemory 0x0073 0x7C gb_) .|
                 (\gb_ -> setMemory 0x0072 0x24 gb_) .|
                 (\gb_ -> setMemory 0x0071 0x13 gb_) .|
                 (\gb_ -> setMemory 0x0070 0x0E gb_) .|
                 (\gb_ -> setMemory 0x006F 0xF2 gb_) .|
                 (\gb_ -> setMemory 0x006E 0x20 gb_) .|
                 (\gb_ -> setMemory 0x006D 0x1D gb_) .|
                 (\gb_ -> setMemory 0x006C 0xF7 gb_) .|
                 (\gb_ -> setMemory 0x006B 0x20 gb_) .|
                 (\gb_ -> setMemory 0x006A 0x0D gb_) .|
                 (\gb_ -> setMemory 0x0069 0xFA gb_) .|
                 (\gb_ -> setMemory 0x0068 0x20 gb_) .|
                 (\gb_ -> setMemory 0x0067 0x90 gb_) .|
                 (\gb_ -> setMemory 0x0066 0xFE gb_) .|
                 (\gb_ -> setMemory 0x0065 0x44 gb_) .|
                 (\gb_ -> setMemory 0x0064 0xF0 gb_) .|
                 (\gb_ -> setMemory 0x0063 0x0C gb_) .|
                 (\gb_ -> setMemory 0x0062 0x0E gb_) .|
                 (\gb_ -> setMemory 0x0061 0x02 gb_) .|
                 (\gb_ -> setMemory 0x0060 0x1E gb_) .|
                 (\gb_ -> setMemory 0x005F 0x04 gb_) .|
                 (\gb_ -> setMemory 0x005E 0x40 gb_) .|
                 (\gb_ -> setMemory 0x005D 0xE0 gb_) .|
                 (\gb_ -> setMemory 0x005C 0x91 gb_) .|
                 (\gb_ -> setMemory 0x005B 0x3E gb_) .|
                 (\gb_ -> setMemory 0x005A 0x42 gb_) .|
                 (\gb_ -> setMemory 0x0059 0xE0 gb_) .|
                 (\gb_ -> setMemory 0x0058 0x57 gb_) .|
                 (\gb_ -> setMemory 0x0057 0x64 gb_) .|
                 (\gb_ -> setMemory 0x0056 0x3E gb_) .|
                 (\gb_ -> setMemory 0x0055 0x67 gb_) .|
                 (\gb_ -> setMemory 0x0054 0xF3 gb_) .|
                 (\gb_ -> setMemory 0x0053 0x18 gb_) .|
                 (\gb_ -> setMemory 0x0052 0x0F gb_) .|
                 (\gb_ -> setMemory 0x0051 0x2E gb_) .|
                 (\gb_ -> setMemory 0x0050 0xF9 gb_) .|
                 (\gb_ -> setMemory 0x004F 0x20 gb_) .|
                 (\gb_ -> setMemory 0x004E 0x0D gb_) .|
                 (\gb_ -> setMemory 0x004D 0x32 gb_) .|
                 (\gb_ -> setMemory 0x004C 0x08 gb_) .|
                 (\gb_ -> setMemory 0x004B 0x28 gb_) .|
                 (\gb_ -> setMemory 0x004A 0x3D gb_) .|
                 (\gb_ -> setMemory 0x0049 0x0C gb_) .|
                 (\gb_ -> setMemory 0x0048 0x0E gb_) .|
                 (\gb_ -> setMemory 0x0047 0x99 gb_) .|
                 (\gb_ -> setMemory 0x0046 0x2F gb_) .|
                 (\gb_ -> setMemory 0x0045 0x21 gb_) .|
                 (\gb_ -> setMemory 0x0044 0x99 gb_) .|
                 (\gb_ -> setMemory 0x0043 0x10 gb_) .|
                 (\gb_ -> setMemory 0x0042 0xEA gb_) .|
                 (\gb_ -> setMemory 0x0041 0x19 gb_) .|
                 (\gb_ -> setMemory 0x0040 0x3E gb_) .|
                 (\gb_ -> setMemory 0x003F 0xF9 gb_) .|
                 (\gb_ -> setMemory 0x003E 0x20 gb_) .|
                 (\gb_ -> setMemory 0x003D 0x05 gb_) .|
                 (\gb_ -> setMemory 0x003C 0x23 gb_) .|
                 (\gb_ -> setMemory 0x003B 0x22 gb_) .|
                 (\gb_ -> setMemory 0x003A 0x13 gb_) .|
                 (\gb_ -> setMemory 0x0039 0x1A gb_) .|
                 (\gb_ -> setMemory 0x0038 0x08 gb_) .|
                 (\gb_ -> setMemory 0x0037 0x06 gb_) .|
                 (\gb_ -> setMemory 0x0036 0x00 gb_) .|
                 (\gb_ -> setMemory 0x0035 0xD8 gb_) .|
                 (\gb_ -> setMemory 0x0034 0x11 gb_) .|
                 (\gb_ -> setMemory 0x0033 0xF3 gb_) .|
                 (\gb_ -> setMemory 0x0032 0x20 gb_) .|
                 (\gb_ -> setMemory 0x0031 0x34 gb_) .|
                 (\gb_ -> setMemory 0x0030 0xFE gb_) .|
                 (\gb_ -> setMemory 0x002F 0x7B gb_) .| --Good 24628
                 (\gb_ -> setMemory 0x002E 0x13 gb_) .| --Good
                 (\gb_ -> setMemory 0x002D 0x00 gb_) .| --Good
                 (\gb_ -> setMemory 0x002C 0x96 gb_) .| --Good
                 (\gb_ -> setMemory 0x002B 0xCD gb_) .| --24611 should put PC here. : Maybe Good
                 (\gb_ -> setMemory 0x002A 0x00 gb_) .| --Good
                 (\gb_ -> setMemory 0x0029 0x95 gb_) .| --Good
                 (\gb_ -> setMemory 0x0028 0xCD gb_) .| --24596 Maybe Good
                 (\gb_ -> setMemory 0x0027 0x1A gb_) .| --Good
                 (\gb_ -> setMemory 0x0026 0x80 gb_) .| --Good
                 (\gb_ -> setMemory 0x0025 0x10 gb_) .| --Good
                 (\gb_ -> setMemory 0x0024 0x21 gb_) .| --Good
                 (\gb_ -> setMemory 0x0023 0x01 gb_) .| --Good
                 (\gb_ -> setMemory 0x0022 0x04 gb_) .| --Good
                 (\gb_ -> setMemory 0x0021 0x11 gb_) .| --Good
                 (\gb_ -> setMemory 0x0020 0x47 gb_) .| --Good
                 (\gb_ -> setMemory 0x001F 0xE0 gb_) .| --Good
                 (\gb_ -> setMemory 0x001E 0xFC gb_) .| --Good
                 (\gb_ -> setMemory 0x001D 0x3E gb_) .| --Good
                 (\gb_ -> setMemory 0x001C 0x77 gb_) .| --Maybe Good 24590 steps
                 (\gb_ -> setMemory 0x001B 0x77 gb_) .| --Good
                 (\gb_ -> setMemory 0x001A 0x3E gb_) .| --Good
                 (\gb_ -> setMemory 0x0019 0x32 gb_) .| --Good
                 (\gb_ -> setMemory 0x0018 0xE2 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x0017 0xF3 gb_) .| --Good
                 (\gb_ -> setMemory 0x0016 0x3E gb_) .| --Good
                 (\gb_ -> setMemory 0x0015 0x0C gb_) .| --Good
                 (\gb_ -> setMemory 0x0014 0xE2 gb_) .| -- 24584 steps : Maybe Good
                 (\gb_ -> setMemory 0x0013 0x32 gb_) .| --Good
                 (\gb_ -> setMemory 0x0012 0x80 gb_) .| --Good
                 (\gb_ -> setMemory 0x0011 0x3E gb_) .| --Good
                 (\gb_ -> setMemory 0x0010 0x11 gb_) .| --Good
                 (\gb_ -> setMemory 0x000F 0x0E gb_) .| --Good
                 (\gb_ -> setMemory 0x000E 0xFF gb_) .| --Good
                 (\gb_ -> setMemory 0x000D 0x26 gb_) .| --Good
                 (\gb_ -> setMemory 0x000C 0x21 gb_) .| --Good
                 (\gb_ -> setMemory 0x000B 0xFB gb_) .| --Good
                 (\gb_ -> setMemory 0x000A 0x20 gb_) .| --Must run 24578 steps to get to this opcode : Maybe Good
                 (\gb_ -> setMemory 0x0009 0x7C gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x0008 0xCB gb_) .| --Good
                 (\gb_ -> setMemory 0x0007 0x32 gb_) .| --Good
                 (\gb_ -> setMemory 0x0006 0x9F gb_) .| --Good
                 (\gb_ -> setMemory 0x0005 0xFF gb_) .| --Good
                 (\gb_ -> setMemory 0x0004 0x21 gb_) .| --Good
                 (\gb_ -> setMemory 0x0003 0xAF gb_) .| --Good
                 (\gb_ -> setMemory 0x0002 0xFF gb_) .| --Good
                 (\gb_ -> setMemory 0x0001 0xFE gb_) .| --Good
                 (\gb_ -> setMemory 0x0000 0x31 gb_) $ gb --I may have a mistake here.

runGameboyNSteps :: Int -> IO Gameboy
runGameboyNSteps n = do { gb   <- defaultGameboy
                        ; boot <- loadBootRom gb
                        ; stepNGameboy n boot }



regToDoc :: Register -> String
regToDoc A   = "A"
regToDoc B   = "B"
regToDoc C   = "C"
regToDoc D   = "D"
regToDoc E   = "E"
regToDoc F   = "F"
regToDoc H   = "H"
regToDoc L   = "L"
regToDoc SHI = "S"
regToDoc PLO = "P"
regToDoc PHI = "P"
regToDoc CLO = "C"

regRegToDoc :: (Register, Register) -> String
regRegToDoc (r1, r2) = regToDoc r1 ++ regToDoc r2

data PrintRegister =
  PrintRegister
  {
    _printRegName  :: String,
    _printRegValue :: String,
    _printRegColor :: A.Color
  } | PrintRegNone
makeLenses ''PrintRegister

prettyPrintReg :: Register -> Cpu -> PrintRegister
prettyPrintReg reg cpu_ = PrintRegister (regToDoc reg) (valStr ++ (replicate (4 - (length valStr)) ' ')) A.White
  where valStr = showHex (cpu_ ^. (registerToLens reg)) ""
prettyPrintRegReg :: (Register, Register) -> Cpu -> PrintRegister
prettyPrintRegReg rs cpu_ = PrintRegister (regRegToDoc rs) (valStr ++ (replicate (5 - (length valStr)) ' ')) A.White
  where valStr = showHex (cpu_ ^. (composeRegisterLenses rs)) ""

evalPrintRegisters :: [PrintRegister] -> IO ()
evalPrintRegisters xs = putStrLn (fst str) >> putStrLn (snd str)
  where
    str = foldl (\(n1, v1) -> \(n2, v2) ->
                    (n1 ++ "   |" ++ n2, v1 ++ "|" ++ v2))
          (head mapped) (tail mapped)
    mapped = (map (\x -> (x ^. printRegName, x ^. printRegValue)) xs)

prettyPrintCpu :: Cpu -> IO ()
prettyPrintCpu cpu_ = evalPrintRegisters [(prettyPrintReg A cpu_),
                                          (prettyPrintReg B cpu_),
                                          (prettyPrintReg C cpu_),
                                          (prettyPrintReg D cpu_),
                                          (prettyPrintReg E cpu_),
                                          (prettyPrintReg F cpu_),
                                          (prettyPrintReg H cpu_),
                                          (prettyPrintReg L cpu_),
                                          (prettyPrintRegReg (H,L) cpu_),
                                          (prettyPrintRegReg (SHI,PLO) cpu_),
                                          (prettyPrintRegReg (PHI,CLO) cpu_)]

prettyPrintLineOfMemory :: Word16 -> Gameboy -> IO String
prettyPrintLineOfMemory addr gb = do { mem <- getMemory addr gb
                                     ; return ((replicate (4 - (length (showHex addr ""))) ' ') ++
                                                (showHex addr "") ++
                                                " : " ++ (showHex mem "")) }

prettyPrintMem :: Word16 -> Gameboy -> [IO String]
prettyPrintMem addr gb = let printaddr = \addr_ -> prettyPrintLineOfMemory addr_ gb
                         in map printaddr $ lower ++ [addr] ++ higher
  where addrs  = [1 .. 5]
        lower  = map (addr-) (reverse addrs)
        higher = map (addr+) addrs

attachTagsPC :: Gameboy -> [IO String] -> [IO String]
attachTagsPC gb strs =  let tagSurround = \istr -> do { str <- istr
                                                   ; return $ str ++ (replicate 25 ' ') }
                            tagMain     = \istr -> do { str <- istr
                                                      ; instr <- fetchNextInstr gb
                                                      ; let instrText = instr ^. name
                                                        in return $ str ++
                                                           " <-- " ++
                                                           instrText ++
                                                           (replicate (21 - (length instrText)) ' ') }
                        in (map tagSurround (take 5 strs)) ++
                           (map tagMain (take 1 . drop 5 $ strs)) ++
                           (map tagSurround (drop 6 strs))

testMem :: Gameboy -> [IO String] -> IO ()
testMem gb x = do { strs <- sequence (attachTagsPC gb x)
                  ; print_ <- foldl (>>) (return ())
                              (map (\x -> putStrLn x) strs)
                  ; return print_ }

prettyPrintGb :: IO Gameboy -> IO ()
prettyPrintGb igb = do { gb <- igb
                       ; let pCpu = prettyPrintCpu (gb ^. cpu)
                             pMem = testMem gb (prettyPrintMem (getRegisters (PHI, CLO) gb) gb)
                         in pCpu >> putStrLn "" >> pMem }

debugMode :: IO Gameboy -> IO Gameboy
debugMode igb = do { gb    <- igb
                   ; _     <- putStrLn "step: "
                   ; steps <- getLine
                   ; sgb   <- stepNGameboy (read steps) gb
                   ; _     <- prettyPrintGb (return sgb)
                   ; fgb   <- debugMode (return sgb)
                   ; return fgb }

