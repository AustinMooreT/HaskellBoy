{-# LANGUAGE TemplateHaskell #-}

module Cpu (module Cpu) where

import Lib
import Memory

import Control.Lens
import Data.Word
import Data.Bits

{- BEGIN CPU DATA STRUCTURE -}

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

-- | Default cpu on startup.
defaultCpu :: Cpu
defaultCpu = Cpu 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00

{- END CPU DATA STRUCTURE -}

{- BEGIN CPU FLAGS -}

-- | CPU flag constant for zero.
zeroFlag :: Word8
zeroFlag = 128

-- | CPU flag constant for suborrupt) with the following:
subtractFlag :: Word8
subtractFlag = 64

-- | CPU flag constant for half carry.
halfCarryFlag :: Word8
halfCarryFlag = 32

-- | CPU flag constant for carry.
carryFlag :: Word8
carryFlag = 16

-- | Converts flag constants to corresponding flag bit.
flagToInt :: Word8 -> Int
flagToInt 128 = 7
flagToInt 64  = 6
flagToInt 32  = 5
flagToInt 16  = 4
flagToInt _   = -1

-- | Set's a flag using one of the flag constants.
setFlag :: Word8 -> Bool -> Cpu -> Cpu
setFlag w True  cpu = cpu & registerToLens F %~ \w8 -> w8 .|. w
setFlag w False cpu = cpu & registerToLens F %~ \w8 -> w8 .&. complement w

-- | If the byte passed in is 0 return a function setting the 0 flag appropriately.
setZero8 :: Word8 -> Cpu -> Cpu
setZero8 0 = setFlag zeroFlag True
setZero8 _ = setFlag zeroFlag False

-- | If 16 bit value passed in is 0 return a function setting the 0 flag appropriately.
setZero16 :: Word16 -> Cpu -> Cpu
setZero16 0 = setFlag zeroFlag True
setZero16 _ = setFlag zeroFlag False

-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowCarry8 :: Word8 -> Word8 -> Cpu -> Cpu
setOverflowCarry8 sum addend = setFlag carryFlag (sum < addend)

-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowCarry16 :: Word16 -> Word16 -> Cpu -> Cpu
setOverflowCarry16 sum addend = setFlag carryFlag (sum < addend)

-- | Check wether or not the sum overflows a the least significant nibble from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowHalfCarry8 :: Word8 -> Word8 -> Cpu -> Cpu
setOverflowHalfCarry8 sum addend = setFlag halfCarryFlag ((sum .&. 0xf) < (addend .&. 0xf))

-- | Check wether or not the sum overflows a the least significant nibble from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowHalfCarry16 :: Word16 -> Word16 -> Cpu -> Cpu
setOverflowHalfCarry16 sum addend = setFlag halfCarryFlag ((sum .&. 0xfff ) < (addend .&. 0xfff))

-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags8 :: Word8 -> (Word8 -> (Cpu -> Cpu)) -> Cpu -> Cpu
incrementWithFlags8 byte store = store increment .
                                 halfCarry .
                                 subtractf .
                                 zero
  where
    increment = byte + 1
    zero      = \cpu -> setZero8 increment cpu
    halfCarry = \cpu -> setOverflowHalfCarry8 increment byte cpu
    subtractf = \cpu -> setFlag subtractFlag False cpu

-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags8IO :: Word8 -> (Word8 -> (Cpu -> IO Cpu)) -> Cpu -> IO Cpu
incrementWithFlags8IO byte store = store increment .
                                 halfCarry .
                                 subtractf .
                                 zero
  where
    increment = byte + 1
    zero      = \cpu -> setZero8 increment cpu
    halfCarry = \cpu -> setOverflowHalfCarry8 increment byte cpu
    subtractf = \cpu -> setFlag subtractFlag False cpu

-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags16 :: Word16 -> (Word16 -> (Cpu -> Cpu)) -> Cpu -> Cpu
incrementWithFlags16 byte store = store increment .
                                  halfCarry .
                                  subtractf .
                                  zero
  where
    increment = byte + 1
    zero      = \cpu -> setZero16 increment cpu
    halfCarry = \cpu -> setOverflowHalfCarry16 increment byte cpu
    subtractf = \cpu -> setFlag subtractFlag False cpu

-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setUnderflowCarry8 :: Word8 -> Word8 -> Cpu -> Cpu
setUnderflowCarry8 difference subtrahend = setFlag carryFlag (difference > subtrahend)

-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setUnderflowCarry16 :: Word16 -> Word16 -> Cpu -> Cpu
setUnderflowCarry16 difference subtrahend = setFlag carryFlag (difference > subtrahend)

-- | Checks wether the difference underflows from a subtraction opperation by looking at one of the subtrahends.
setUnderflowHalfCarry8 :: Word8 -> Word8 -> Cpu -> Cpu
setUnderflowHalfCarry8 difference subtrahend = setFlag halfCarryFlag ((difference .&. 0xf) > (subtrahend .&. 0xf))

-- | Checks wether the difference underflows from a subtraction opperation by looking at one of the subtrahends.
setUnderflowHalfCarry16 :: Word16 -> Word16 -> Cpu -> Cpu
setUnderflowHalfCarry16 difference subtrahend = setFlag halfCarryFlag ((difference .&. 0xfff) > (subtrahend .&. 0xfff))

-- | Decrement byte with flags
decrementWithFlags8 :: Word8 -> (Word8 -> (Cpu -> Cpu)) -> Cpu -> Cpu
decrementWithFlags8 byte store = store decrement .
                                 zero .
                                 halfCarry .
                                 subtractf
  where
    decrement = byte - 1
    zero      = \cpu -> setZero8 decrement cpu
    halfCarry = \cpu -> setUnderflowHalfCarry8 decrement byte cpu
    subtractf = \cpu -> setFlag subtractFlag True cpu

-- | Decrement byte with flags
decrementWithFlags8IO :: Word8 -> (Word8 -> (Cpu -> IO Cpu)) -> Cpu -> IO Cpu
decrementWithFlags8IO byte store = store decrement .
                                   zero .
                                   halfCarry .
                                   subtractf
  where
    decrement = byte - 1
    zero      = \cpu -> setZero8 decrement cpu
    halfCarry = \cpu -> setUnderflowHalfCarry8 decrement byte cpu
    subtractf = \cpu -> setFlag subtractFlag True cpu

-- | Decrement byte with flags
decrementWithFlags16 :: Word16 -> (Word16 -> (Cpu -> Cpu)) -> Cpu -> Cpu
decrementWithFlags16 byte store = store decrement .
                                  zero .
                                  halfCarry .
                                  subtractf
  where
    decrement = byte - 1
    zero      = \cpu -> setZero16 decrement cpu
    halfCarry = \cpu -> setUnderflowHalfCarry16 decrement byte cpu
    subtractf = \cpu -> setFlag subtractFlag True cpu

-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags8 :: Word8 -> Word8 -> (Word8 -> (Cpu -> Cpu)) -> Cpu -> Cpu
addWithFlags8 e1 e2 f = f addition .
                        carry .
                        halfCarry .
                        subtractf .
                        zero
  where
    addition  = e1 + e2
    zero      = \cpu -> setZero8 addition cpu
    carry     = \cpu -> setOverflowCarry8 addition e2 cpu
    halfCarry = \cpu -> setOverflowHalfCarry8 addition e2 cpu
    subtractf = \cpu -> setFlag subtractFlag False cpu


boolToWord :: Bool -> Word8
boolToWord True = 1
boolToWord _ = 0


-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags8PlusC :: Word8 -> Word8 -> Bool -> (Word8 -> (Cpu -> Cpu)) -> Cpu -> Cpu
addWithFlags8PlusC e1 e2 b f = f addition .
                               carry .
                               halfCarry .
                               subtractf .
                               zero
  where
    addition  = e1 + e2 + (boolToWord b)
    zero      = \cpu -> setZero8 addition cpu
    carry     = \cpu -> setOverflowCarry8 addition e2 cpu
    halfCarry = \cpu -> setOverflowHalfCarry8 addition e2 cpu
    subtractf = \cpu -> setFlag subtractFlag False cpu


-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags16 :: Word16 -> Word16 -> (Word16 -> (Cpu -> Cpu)) -> Cpu -> Cpu
addWithFlags16 e1 e2 f = f addition .
                         carry .
                         halfCarry .
                         subtractf
  where
    addition  = e1 + e2
    carry     = \cpu -> setOverflowCarry16 addition e2 cpu
    halfCarry = \cpu -> setOverflowHalfCarry16 addition e2 cpu
    subtractf = \cpu -> setFlag subtractFlag False cpu


-- | subtracts e2 from e1 and uses f to store the result and all of the state changes back in a cpu.
subWithFlags8 :: Word8 -> Word8 -> (Word8 -> (Cpu -> Cpu)) -> Cpu -> Cpu
subWithFlags8 e1 e2 f = f addition .
                        carry .
                        halfCarry .
                        subtractf .
                        zero
  where
    addition  = e1 - e2
    zero      = \cpu -> setZero8 addition cpu
    carry     = \cpu -> setUnderflowCarry8 addition e2 cpu
    halfCarry = \cpu -> setUnderflowHalfCarry8 addition e2 cpu
    subtractf = \cpu -> setFlag subtractFlag True cpu


-- | it sebtracts e2 from e1 and uses f to store the results back into cpu and sets all the flags.
  -- NOTE I don't know what plus C is. I have it on other functions as well, but don't document it.
subWithFlags8PlusC :: Word8 -> Word8 -> Bool -> (Word8 -> (Cpu -> Cpu)) -> Cpu -> Cpu
subWithFlags8PlusC e1 e2 b f = f addition .
                               carry .
                               halfCarry .
                               subtractf .
                               zero
  where
    addition  = (e1 + (boolToWord b)) - e2
    zero      = \cpu -> setZero8 addition cpu
    carry     = \cpu -> setUnderflowCarry8 addition e2 cpu
    halfCarry = \cpu -> setUnderflowHalfCarry8 addition e2 cpu
    subtractf = \cpu -> setFlag subtractFlag True cpu

{- END CPU FLAGS -}

{- BEGIN CPU REGISTERS-}

-- | Represents given registers in the CPU.
data Register = A | B | C | D | E | F | H | L | SHI | PLO | PHI | CLO

-- | Converts a register datum to a record accesor
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

-- | Takes two registers and creates a Lens (functor) for getting and setting them as a 16 bit unit.
composeRegisterLenses :: Functor f => (Register, Register) -> (Word16 -> f Word16) -> Cpu -> f Cpu
composeRegisterLenses (reg1, reg2) = lens getter setter
  where
    getter __cpu = combineData (registerToFunc reg1 __cpu) (registerToFunc reg2 __cpu)
    setter __cpu d = __cpu & registerToLens reg1 .~ breakHi d & registerToLens reg2 .~ breakLo d

-- | Sets the value in register r to some 8 bit value d.
setRegister :: Register -> Word8 -> Cpu -> Cpu
setRegister r d cpu = cpu & registerToLens r .~ d

-- | Sets the value in the combined registers rs to a 16 bit value d.
setRegisters :: (Register, Register) -> Word16 -> Cpu -> Cpu
setRegisters rs d cpu = cpu & composeRegisterLenses rs .~ d

-- | Fetches 8 bit value from register r.
getRegister :: Register -> Cpu -> Word8
getRegister r cpu = cpu ^. registerToLens r

-- | Fetches 16 bit combined value from the registers rs.
getRegisters :: (Register, Register) -> Cpu -> Word16
getRegisters rs cpu = cpu ^. composeRegisterLenses rs

-- | Loads data from src into dest.
ldRegWithReg :: Register -> Register -> Cpu -> Cpu
ldRegWithReg dest src cpu = setRegister dest (getRegister src cpu) cpu

-- | Using addr as an index grabs an 8 bit value from memory and loads it into reg.
ldRegWithMem :: Register -> Word16 -> Memory -> Cpu -> IO Cpu
ldRegWithMem reg addr mem cpu = do { d <- getMemory addr mem
                                   ; return $ setRegister reg d cpu }

-- | Using the 16 bit value from the combined registers rs as an index
  -- grabs an 8 bit value from memory and loads it into r.
ldRegWithRegRegMem :: Register -> (Register, Register) -> Memory -> Cpu -> IO Cpu
ldRegWithRegRegMem r rs mem cpu = do { d <- getMemory (getRegisters rs cpu) mem
                                     ; return $ setRegister r d cpu}

-- | Using the combined register rs as an index set that location in memory to the value stored in r.
ldMemRegRegWithReg :: (Register, Register) -> Register -> Memory -> Cpu -> IO Memory
ldMemRegRegWithReg rs r mem cpu = setMemory (getRegisters rs cpu) (getRegister r cpu) mem

-- | Increments a register r's value by 1 while ignoring all flags.
incrementRegisterWithoutFlags :: Register -> Cpu -> Cpu
incrementRegisterWithoutFlags r cpu = setRegister r (getRegister r cpu + 1) cpu

-- | Increments the combined registers rs's value by 1.
incrementRegistersWithoutFlags :: (Register, Register) -> Cpu -> Cpu
incrementRegistersWithoutFlags rs cpu = setRegisters rs (getRegisters rs cpu + 1) cpu

-- | Using the 16 bit value stored in rs as an index set memory to the next byte from the program counter.
ldMemRegRegWithData :: (Register, Register) -> Memory -> Cpu -> IO (Cpu, Memory)
ldMemRegRegWithData rs mem cpu = do { d      <- getMemory (getRegisters (PHI, CLO) cpu1) mem
                                    ; newMem <- setMemory (getRegisters rs cpu) d mem
                                    ; return (cpu1, newMem) }
  where cpu1 = incrementRegistersWithoutFlags (PHI, CLO) cpu

-- | Loads two registers r1 and r2 with data fetched from memory using the program counter.
ldRegRegWithData :: (Register, Register) -> Memory -> Cpu -> IO Cpu
ldRegRegWithData rs mem cpu = do { d1 <- getMemory (getRegisters (PHI, CLO) cpu1) mem
                                 ; d2 <- getMemory (getRegisters (PHI, CLO) cpu2) mem
                                 ; return $ setRegisters rs (combineData d2 d1) cpu2 }
  where
    cpu1 = incrementRegistersWithoutFlags (PHI, CLO) cpu
    cpu2 = incrementRegistersWithoutFlags (PHI, CLO) cpu1

-- | Loads a register r with data fetched from memory using the program counter.
ldRegWithData :: Register -> Memory -> Cpu -> IO Cpu
ldRegWithData r mem cpu = do { d <- getMemory (getRegisters (PHI, CLO) cpu1) mem
                             ; return $ setRegister r d cpu1 }
  where
    cpu1 = incrementRegistersWithoutFlags (PHI, CLO) cpu

-- | Increments the value stored in a given register and sets the associated flags.
incrementRegisterWithFlags :: Register -> Cpu -> Cpu
incrementRegisterWithFlags r cpu = incrementWithFlags8 (getRegister r cpu) (\d -> (\cpu1 -> (setRegister r d cpu1))) cpu

-- | Increments the value stored in a register pair.
incrementRegistersWithFlags :: (Register, Register) -> Cpu -> Cpu
incrementRegistersWithFlags rs cpu = incrementWithFlags16 (getRegisters rs cpu) (\d -> (\cpu1 -> (setRegisters rs d cpu1))) cpu

-- | decrement a register and set the associated flags.
decrementRegisterWithFlags :: Register -> Cpu -> Cpu
decrementRegisterWithFlags r cpu = decrementWithFlags8 (getRegister r cpu) (\d -> (\cpu1 -> setRegister r d cpu1)) cpu

-- | decrement a set of registers and set the associated flags.
decrementRegisters :: (Register, Register) -> Cpu -> Cpu
decrementRegisters rs cpu = decrementWithFlags16 (getRegisters rs cpu) (\d -> (\cpu1 -> setRegisters rs d cpu1)) cpu

-- | decrement a register and don't set the associated flags.
decrementRegisterWithoutFlags :: Register -> Cpu -> Cpu
decrementRegisterWithoutFlags r cpu = setRegister r ((getRegister r cpu) - 1) cpu

-- | decrement a set of registers and don't set the associated flags.
decrementRegistersWithoutFlags :: (Register, Register) -> Cpu -> Cpu
decrementRegistersWithoutFlags rs cpu = setRegisters rs ((getRegisters rs cpu) - 1) cpu

-- | Increments a value in memory by indexing with a register pair.
incrementMemoryRegReg :: (Register, Register) -> Memory -> Cpu -> IO (Cpu, Memory)
incrementMemoryRegReg rs mem cpu = do { d <- getMemory (getRegisters rs cpu) mem
                                      ; let incMem = incrementWithFlags8IO d
                                            storeMem = \d1 -> \cpu_ -> (setMemory (getRegisters rs cpu_) d1 mem >> return cpu_)
                                        in (incMem storeMem cpu) >>= \x -> return $ (x, mem) }

-- | decrement a value in memory by indexing with a register pair.
decrementMemoryRegReg :: (Register, Register) -> Memory -> Cpu -> IO (Cpu, Memory)
decrementMemoryRegReg rs mem cpu = do { d <- getMemory (getRegisters rs cpu) mem
                                      ; let decMem = decrementWithFlags8IO d
                                            storeMem = \d1 -> \cpu_ -> setMemory (getRegisters rs cpu_) d1 mem >> return cpu_
                                        in (decMem storeMem cpu) >>= \x -> return $ (x, mem) }

{- END CPU REGISTERS -}

-- | Represents an instruction to the Cpu's processor.
data Instruction =
  Instruction
  {
    _opcode    :: Word8,
    _name      :: String,
    _time      :: (Cpu -> Integer),
    _operation :: (Cpu -> IO Cpu)
  }
makeLenses ''Instruction

-- | Instance of show for converting Instructions to a String.
instance Show Instruction where
  show instr = (show $ instr ^. opcode) Prelude.++ (show $ instr ^. name)
