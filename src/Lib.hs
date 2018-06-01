{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module Lib
    (defaultCpu) where


import Data.Word
import Control.Lens
import Data.Vector as V
import Data.Bits
import Data.Binary.Get
import Graphics.Gloss
import Control.Monad

--DATA
-- | Converts an byte to 16 bit value.
toWord16 :: Word8 -> Word16
toWord16 w = fromIntegral w

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
-- | CPU flag constant for subtract.
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
    _bytes :: V.Vector Word8
  }
makeLenses ''Memory

--MEMORY
-- | Default gameboy memory on startup.
defaultMemory :: Memory
defaultMemory = Memory $ V.replicate 0xFFFF 0x00

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
defaultGameboy :: Gameboy
defaultGameboy = Gameboy defaultCpu defaultMemory

--GAMEBOY
-- | Represents an instruction to the Gameboy's processor.
data Instruction =
  Instruction
  {
    _opcode    :: Word8,
    _name      :: String,
    _operation :: (Gameboy -> Gameboy)
  }
makeLenses ''Instruction

--GAMEBOY
-- | Instance of show for converting Instructions to a String.
instance Show Instruction where
  show instr = (show $ instr ^. opcode) Prelude.++ (show $ instr ^. name)

--GAMEBOY
-- | Uses 16 bit value addr to index and return an 8 bit value in memory.
getMemory :: Word16 -> Gameboy -> Word8
getMemory addr gb = (view (memory . bytes) gb) ! (fromIntegral addr)

--GAMEBOY
-- | Uses 16 bit value addr as an index to set the element there to 8 bit value d.
setMemory :: Word16 -> Word8 -> Gameboy -> Gameboy
setMemory addr d gb = over (memory . bytes) (\y -> y // [(fromIntegral addr, d)]) gb

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
ldRegWithMem :: Register -> Word16 -> (Gameboy -> Gameboy)
ldRegWithMem reg addr gb = setRegister reg (getMemory addr gb) gb

--GAMEBOY
-- | Using the 16 bit value from the combined registers rs as an index
  -- grabs an 8 bit value from memory and loads it into r.
ldRegWithRegRegMem :: Register -> (Register, Register) -> (Gameboy -> Gameboy)
ldRegWithRegRegMem r rs gb = setRegister r (getMemory (getRegisters rs gb) gb) gb

--GAMEBOY
-- | Using the combined register rs as an index set that location in memory to the value stored in r.
ldMemRegRegWithReg :: (Register, Register) -> Register -> (Gameboy -> Gameboy)
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
ldMemRegRegWithData :: (Register, Register) -> (Gameboy -> Gameboy)
ldMemRegRegWithData rs gb = setMemory (getRegisters rs gb) (getMemory (getRegisters (PHI, CLO) gb1) gb1) gb1
  where gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

--GAMEBOY
-- | Loads two registers r1 and r2 with data fetched from memory using the program counter.
ldRegRegWithData :: (Register, Register) -> (Gameboy -> Gameboy)
ldRegRegWithData (r1, r2) = (\gb -> setRegister r1 (getMemory (getRegisters (PHI, CLO) gb) gb) gb) .
                            incrementRegistersWithoutFlags (PHI, CLO) .
                            (\gb -> setRegister r2 (getMemory (getRegisters (PHI, CLO) gb) gb) gb)  .
                            incrementRegistersWithoutFlags (PHI, CLO)

--GAMEBOY
-- | Loads a register r with data fetched from memory using the program counter.
ldRegWithData :: Register -> (Gameboy -> Gameboy)
ldRegWithData r gb = (setRegister r (getMemory (getRegisters (PHI, CLO) gb1) gb1) gb1)
    where gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

--GAMEBOY
-- | Set's a flag using one of the flag constants.
setFlag :: Word8 -> Bool -> Gameboy -> Gameboy
setFlag w True  gb = gb & cpu . registerToLens F %~ \w8 -> w8 .|. w
setFlag w False gb = gb & cpu . registerToLens F %~ \w8 -> w8 .&. complement w

--GAMEBOY
-- | If the byte passed in is 0 return a function setting the 0 flag appropriately.
setZero :: Word8 -> (Gameboy -> Gameboy)
setZero 0 = setFlag zeroFlag True
setZero _ = setFlag zeroFlag False

--GAMEBOY
--TODO check logic
setCarry :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setCarry e1 e2 = setFlag carryFlag (e1 < e2)

--GAMEBOY
--TODO check logic
setHalfCarry :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setHalfCarry e1 e2 =  setFlag halfCarryFlag ((e1 .&. 0b00001111) < (e2 .&. 0b00001111))


--GAMEBOY TODO
incrementWithFlags :: Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
incrementWithFlags d f = f increment .
                         halfCarry .
                         subtractf .
                         zero
    where
    increment = incrementEmuData d
    zero      = \gb -> setZero increment gb
    halfCarry = \gb -> setHalfCarry increment d gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY TODO
incrementRegister :: Register -> (Gameboy -> Gameboy)
incrementRegister r = \gb -> incrementWithFlags (getRegister gb r) (\d -> (\gb1 -> (setRegister gb1 r d))) gb

--GAMEBOY TODO
incrementRegisters :: (Register, Register) -> (Gameboy -> Gameboy)
incrementRegisters (r1, r2) = \gb -> incrementWithFlags (getRegisters gb r1 r2) (\d -> (\gb1 -> (setRegisters gb1 r1 r2 d))) gb

--GAMEBOY TODO
decrementWithFlags :: Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
decrementWithFlags d f = f decrement .
                         zero .
                         halfCarry .
                         subtractf
  where
    decrement = decrementEmuData d
    zero      = \gb -> setZero decrement gb
    halfCarry = \gb -> setHalfCarry decrement d gb
    subtractf = \gb -> setFlag subtractFlag True gb
{-
decrementRegister :: Register -> (Gameboy -> Gameboy)
decrementRegister r = \gb -> decrementWithFlags (getRegister gb r) (\d -> (\gb1 -> setRegister gb1 r d)) gb

decrementRegisters :: (Register, Register) -> (Gameboy -> Gameboy)
decrementRegisters (r1, r2) = \gb -> decrementWithFlags (getRegisters gb r1 r2) (\d -> (\gb1 -> setRegisters gb1 r1 r2 d)) gb



decrementRegisterWithoutFlags :: Register -> (Gameboy -> Gameboy)
decrementRegisterWithoutFlags r = \gb -> setRegister gb r $ decrementEmuData $ getRegister gb r

decrementRegistersWithoutFlags :: (Register, Register) -> (Gameboy -> Gameboy)
decrementRegistersWithoutFlags (r1, r2) = \gb -> setRegisters gb r1 r2 $ decrementEmuData $ getRegisters gb r1 r2


incrementMemoryReg :: Register -> (Gameboy -> Gameboy)
incrementMemoryReg r = \gb -> incrementWithFlags (getMemory gb $ getRegister gb r)
                              (\d -> (\gb1 -> setMemory gb1 (getRegister gb1 r) d)) gb

incrementMemoryRegReg :: (Register, Register) -> (Gameboy -> Gameboy)
incrementMemoryRegReg (r1, r2) = \gb -> incrementWithFlags (getMemory gb $ getRegisters gb r1 r2)
                                        (\d -> (\gb1 -> setMemory gb1 (getRegisters gb1 r1 r2) d)) gb

decrementMemoryReg :: Register -> (Gameboy -> Gameboy)
decrementMemoryReg r = \gb -> decrementWithFlags (getMemory gb $ getRegister gb r)
                              (\d -> (\gb1 -> setMemory gb1 (getRegister gb1 r) d)) gb

decrementMemoryRegReg :: (Register, Register) -> (Gameboy -> Gameboy)
decrementMemoryRegReg (r1, r2) = \gb -> decrementWithFlags (getMemory gb $ getRegisters gb r1 r2)
                                        (\d -> (\gb1 -> setMemory gb1 (getRegisters gb1 r1 r2) d)) gb

addWithFlags :: EmuData -> EmuData -> (EmuData -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
addWithFlags e1 e2 f = f addition .
                       carry .
                       halfCarry .
                       subtractf
  where
    addition  = addEmuData e1 e2
    carry     = \gb -> setCarry addition e2 gb
    halfCarry = \gb -> setHalfCarry addition e2 gb
    subtractf = \gb -> setFlag subtractFlag False gb

addRegReg :: Register -> Register -> (Gameboy -> Gameboy)
addRegReg r1 r2 = \gb -> addWithFlags (getRegister gb r1) (getRegister gb r2) (\d -> (\gb1 -> setRegister gb1 r1 d)) gb

addRegRegWithoutFlags :: Register -> Register -> (Gameboy -> Gameboy)
addRegRegWithoutFlags r1 r2 = \gb -> setRegister gb r1 $ addEmuData (getRegister gb r1) (getRegister gb r2)

addRegsRegs :: (Register, Register) -> (Register, Register) -> (Gameboy -> Gameboy)
addRegsRegs (r1,r2) (r3, r4) = \gb -> addWithFlags (getRegisters gb r1 r2) (getRegisters gb r3 r4) (\d -> (\gb1 -> setRegisters gb1 r1 r2 d)) gb

addRegsRegsWithoutFlags :: (Register, Register) -> (Register, Register) -> (Gameboy -> Gameboy)
addRegsRegsWithoutFlags (r1, r2) (r3, r4) = \gb -> setRegisters gb r1 r2 $ addEmuData (getRegisters gb r1 r2) (getRegisters gb r3 r4)





getFlag :: Gameboy -> Word8 -> Bool
getFlag gb w = testBit (_eight $ getRegister gb F) (flagToInt w)



--NOTE pretty much just adds EmuData to A.
accumAdd :: EmuData -> (Gameboy -> Gameboy)
accumAdd (Eight byte) = (\gb -> setRegister gb A $ addition gb) .
                   zero .
                   carry .
                   subtractf .
                   halfCarry
  where
    addition  = \gb -> Eight (xor byte . _eight $ getRegister gb A)
    zero      = \gb -> (setZero $ addition gb) gb
    carry     = \gb -> setCarry (addition gb) (Eight byte) gb
    halfCarry = \gb -> setHalfCarry (addition  gb) (getRegister gb A) gb
    subtractf = \gb -> setFlag subtractFlag False gb

accumAddCarry :: EmuData -> (Gameboy -> Gameboy)
accumAddCarry (Eight byte) = (\gb -> setRegister gb A $ addition gb) .
                             zero .
                             carry .
                             subtractf .
                             halfCarry
  where
    addition = \gb -> Eight $ (xor byte . _eight $ getRegister gb A) + ((_eight $ getRegister gb F) .&. 0b0000001)
    zero      = \gb -> (setZero $ addition gb) gb
    carry     = \gb -> setCarry (addition gb) (Eight byte) gb
    halfCarry = \gb -> setHalfCarry (addition  gb) (getRegister gb A) gb
    subtractf = \gb -> setFlag subtractFlag False gb

addReg :: Register -> (Gameboy -> Gameboy)
addReg r = \gb -> accumAdd (getRegister gb r) gb

rotateLeftACarry :: (Gameboy -> Gameboy)
rotateLeftACarry = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) .
                   (\gb -> setFlag carryFlag (testBit (_eight $ getRegister gb A) 0) gb) .
                   (\gb -> setRegister gb A $ Eight $ rotateL (_eight $ getRegister gb A) 1)

rotateLeft :: Register -> (Gameboy -> Gameboy)
rotateLeft r gb = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) $
                 (\gb1 -> setRegister gb1 r $ Eight $ ((_eight $ getRegister gb1 r) .&. 0b11111110) .|. carryBeforeMask) $
                 setFlag carryFlag carryAfter $
                 setRegister gb r $ Eight $ rotateL (_eight $ getRegister gb r) 1
  where
    carryBefore = getFlag gb carryFlag
    carryAfter  = testBit (_eight $ getRegister gb r) 7
    carryBeforeMask = if carryBefore then 0b00000001 else 0b00000000

rotateRight :: Register -> (Gameboy -> Gameboy)
rotateRight r gb = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) $
                  (\gb1 -> setRegister gb1 r $ Eight $ ((_eight $ getRegister gb1 r) .&. 0b11111110) .|. carryBeforeMask) $
                  setFlag carryFlag carryAfter $
                  setRegister gb r $ Eight $ rotateR (_eight $ getRegister gb r) 1
  where
    carryBefore = getFlag gb carryFlag
    carryAfter  = testBit (_eight $ getRegister gb r) 0
    carryBeforeMask = if carryBefore then 0b10000000 else 0b00000000

rotateLeftA :: Gameboy -> Gameboy
rotateLeftA = rotateLeft A

rotateRightA :: Gameboy -> Gameboy
rotateRightA = rotateRight A

rotateRightACarry :: (Gameboy -> Gameboy)
rotateRightACarry = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) .
                    (\gb -> setFlag carryFlag (testBit (_eight $ getRegister gb A) 7) gb) .
                    (\gb -> setRegister gb A $ Eight $ rotateR (_eight $ getRegister gb A) 1)

xorReg :: Register -> (Gameboy -> Gameboy)
xorReg r = (\gb -> setRegister gb A $ Eight $ xor (_eight $ getRegister gb A) (_eight $ getRegister gb r))

wordToSignedInt :: Word8 -> Int
wordToSignedInt w
  | testBit w 7 == True = - (fromIntegral $ (complement w) + 1)
  | otherwise = fromIntegral w

jumpRNZ :: Gameboy -> Gameboy
jumpRNZ gb = if getFlag gb1 zeroFlag
             then setRegister gb1 PC (Sixteen . fromIntegral $ (fromIntegral $ _sixteen $ getRegister gb1 PC) + (wordToSignedInt d))
             else gb1
  where
    d = _eight $ getMemory gb1 (getRegister gb1 PC)
    gb1 = incrementRegisterWithoutFlags PC gb

ldFFRegAddrReg :: Register -> Register -> (Gameboy -> Gameboy)
ldFFRegAddrReg d s = \gb -> setMemory gb (Sixteen (0xFF00 + (toWord16 . _eight $ getRegister gb d))) $ getRegister gb s

ldhA :: Gameboy -> Gameboy
ldhA gb = setMemory gb1 (Sixteen (0xFF00 +
                                  (toWord16 . _eight $ (getMemory gb1 $ getRegister gb1 PC))))
          (getRegister gb1 A)
  where
    gb1 = incrementRegisterWithoutFlags PC gb


toWord8 :: Word16 -> Word8
toWord8 w = fromIntegral w

push :: EmuData -> (Gameboy -> Gameboy)
push (Sixteen d) = pushByte dhi . pushByte dlo
  where
    dhi = Eight . toWord8 $ shiftR d 8
    dlo = Eight . toWord8 $ 0x00FF .&. d
    pushByte = \b  -> decrementRegisterWithoutFlags SP .
               \gb -> setMemory gb (getRegister gb SP) b

pop :: (Register, Register) -> (Gameboy -> Gameboy)
pop (r1, r2) = \gb -> setRegisters (incrementRegisterWithoutFlags SP (incrementRegisterWithoutFlags SP gb)) r1 r2
                      (combineEmuData
                        (getMemory gb $ getRegister (incrementRegisterWithoutFlags SP gb) SP)
                        (getMemory gb $ getRegister (incrementRegisterWithoutFlags SP (incrementRegisterWithoutFlags SP gb)) SP))


call :: Gameboy -> Gameboy
call gb = decrementRegisterWithoutFlags PC
  ((\gb_ -> (setRegister gb_ PC $ combineEmuData
              (getMemory gb2 $ getRegister gb2 PC)
              (getMemory gb1 $ getRegister gb1 PC))) .
    (push (getRegister gb3 PC)) $ gb)

  where
    gb1 = incrementRegisterWithoutFlags PC gb
    gb2 = incrementRegisterWithoutFlags PC gb1
    gb3 = incrementRegisterWithoutFlags PC gb2

ret :: Gameboy -> Gameboy
ret gb = decrementRegisterWithoutFlags PC
         (setRegister (incrementRegisterWithoutFlags SP (incrementRegisterWithoutFlags SP gb)) PC
          (combineEmuData
           (getMemory gb $ getRegister (incrementRegisterWithoutFlags SP gb) SP)
           (getMemory gb $ getRegister (incrementRegisterWithoutFlags SP (incrementRegisterWithoutFlags SP gb)) SP)))

--TODO This may be horribly incorrect.
--cp :: EmuData -> (Gameboy -> Gameboy)
--cp d = zero . halfCarry . subtractf
--  where
--   subtraction = \gb -> (_eight $ getRegister A gb) - d
--    zero        = \gb -> setZero subtraction gb
--    halfCarry   = \gb -> setHalfCarry subtraction d gb
--    subtractf   = \gb -> setFlag subtractFlag True gb

decodeOp :: EmuData -> Instruction
decodeOp (Eight 0x00) = Instruction (Eight 0x00) "NOP" id
decodeOp (Eight 0x01) = Instruction (Eight 0x01) "LD BC, d16" $ ldRegRegData (B, C)
decodeOp (Eight 0x02) = Instruction (Eight 0x02) "LD (BC), A" $ ldRegAddrReg (B, C) A
decodeOp (Eight 0x03) = Instruction (Eight 0x03) "INC BC" $ incrementRegistersWithoutFlags (B, C)
decodeOp (Eight 0x04) = Instruction (Eight 0x04) "INC B" $ incrementRegister B
decodeOp (Eight 0x05) = Instruction (Eight 0x05) "DEC B" $ decrementRegister B
decodeOp (Eight 0x06) = Instruction (Eight 0x06) "LD B, d8" $ ldRegData B
decodeOp (Eight 0x07) = Instruction (Eight 0x07) "RLCA" rotateLeftACarry
--TODO 0x08 "LD (a16) SP"
decodeOp (Eight 0x09) = Instruction (Eight 0x09) "ADD HL, BC" (addRegsRegs (H, L) (B, C))
decodeOp (Eight 0x0A) = Instruction (Eight 0x0A) "LD A, (BC)" (ldRegRegAddr A (B, C))
decodeOp (Eight 0x0B) = Instruction (Eight 0x0B) "DEC BC" (decrementRegistersWithoutFlags (B, C))
decodeOp (Eight 0x0C) = Instruction (Eight 0x0C) "INC C" (incrementRegister C)
decodeOp (Eight 0x0D) = Instruction (Eight 0x0D) "DEC C" (decrementRegister C)
decodeOp (Eight 0x0E) = Instruction (Eight 0x0E) "LD C, d8" (ldRegData C)
decodeOp (Eight 0x0F) = Instruction (Eight 0x0F) "RRCA" rotateRightACarry
--TODO 0x10 "STOP 0"
decodeOp (Eight 0x11) = Instruction (Eight 0x11) "LD DE, d16" $ ldRegRegData (D, E)
decodeOp (Eight 0x12) = Instruction (Eight 0x12) "LD (DE), A" $ ldRegAddrReg (D, E) A
decodeOp (Eight 0x13) = Instruction (Eight 0x13) "INC DE" $ incrementRegistersWithoutFlags (D, E)
decodeOp (Eight 0x14) = Instruction (Eight 0x14) "INC D" $ incrementRegister D
decodeOp (Eight 0x15) = Instruction (Eight 0x15) "DEC D" $ decrementRegister D
decodeOp (Eight 0x16) = Instruction (Eight 0x16) "LD D, d8" $ ldRegData D
decodeOp (Eight 0x17) = Instruction (Eight 0x17) "RLA" rotateLeftA
--TODO 0x18 "JR r8"
decodeOp (Eight 0x19) = Instruction (Eight 0x19) "ADD HL, DE" $ addRegsRegs (H, L) (D, E)
decodeOp (Eight 0x1A) = Instruction (Eight 0x1A) "LD A, (DE)" $ ldRegRegAddr A (D, E)
decodeOp (Eight 0x1B) = Instruction (Eight 0x1B) "DEC BC" $ decrementRegistersWithoutFlags (D, E)
decodeOp (Eight 0x1C) = Instruction (Eight 0x1C) "INC E" $ incrementRegister E
decodeOp (Eight 0x1D) = Instruction (Eight 0x1D) "DEC E" $ decrementRegister E
decodeOp (Eight 0x1E) = Instruction (Eight 0x1E) "LD C, d8" $ ldRegData C
decodeOp (Eight 0x1F) = Instruction (Eight 0x1F) "RRA" rotateRightA
--TODO 0x20 "JR NZ, r8"
decodeOp (Eight 0x20) = Instruction (Eight 0x20) "JR NZ, r8" $ jumpRNZ
decodeOp (Eight 0x21) = Instruction (Eight 0x21) "LD HL, d16" $ ldRegRegData (H, L)
decodeOp (Eight 0x22) = Instruction (Eight 0x22) "LD (HL+), A" $ incrementRegisters (H, L) . ldRegAddrReg (H, L) A
decodeOp (Eight 0x23) = Instruction (Eight 0x23) "INC HL" $ incrementRegistersWithoutFlags (H, L)
decodeOp (Eight 0x24) = Instruction (Eight 0x24) "INC H" $ incrementRegister H
decodeOp (Eight 0x25) = Instruction (Eight 0x25) "DEC H" $ decrementRegister H
decodeOp (Eight 0x28) = Instruction (Eight 0x26) "LD H, d8" $ ldRegData H
--TODO 0x27 "DAA"
--TODO 0x28 "JR Z, r8"
decodeOp (Eight 0x29) = Instruction (Eight 0x29) "ADD HL, HL" $ addRegsRegs (H, L) (H, L)
--TODO 0x2A "LD A, (HL+)"
decodeOp (Eight 0x2B) = Instruction (Eight 0x2B) "DEC HL" $ decrementRegistersWithoutFlags (H, L)
decodeOp (Eight 0x2C) = Instruction (Eight 0x2C) "INC L" $ incrementRegister L
decodeOp (Eight 0x2D) = Instruction (Eight 0x2D) "DEC L" $ decrementRegister L
decodeOp (Eight 0x2E) = Instruction (Eight 0x2E) "LD L, d8" $ ldRegData L
--TODO 0x2F "CPL"
--TODO 0x30 "JR NC, r8"
decodeOp (Eight 0x31) = Instruction (Eight 0x31) "LD SP, d16" $ ldRegData SP
--TODO 0x32 "LD (HL-), A"
decodeOp (Eight 0x32) = Instruction (Eight 0x32) "LD (HL-), A" $ decrementRegisters (H, L) . ldRegAddrReg (H, L) A
decodeOp (Eight 0x33) = Instruction (Eight 0x33) "INC SP" $ incrementRegisterWithoutFlags SP
decodeOp (Eight 0x34) = Instruction (Eight 0x34) "INC (HL)" $ incrementMemoryRegReg (H, L)
decodeOp (Eight 0x35) = Instruction (Eight 0x35) "DEC (HL)" $ decrementMemoryRegReg (H, L)
decodeOp (Eight 0x36) = Instruction (Eight 0x36) "LD (HL), d8" $ ldRegAddrData (H, L)
--TODO 0x37 "SCF"
--TODO 0x38 "JR C, r8"
--TODO 0x39 "ADD HL, SP"
--TODO 0x3A "LD A, (HL-)"
decodeOp (Eight 0x3B) = Instruction (Eight 0x3B) "DEC SP" $ decrementRegisterWithoutFlags SP
decodeOp (Eight 0x3C) = Instruction (Eight 0x3C) "INC A" $ incrementRegister A
decodeOp (Eight 0x3D) = Instruction (Eight 0x3D) "DEC A" $ decrementRegister A
decodeOp (Eight 0x3E) = Instruction (Eight 0x3E) "LD A, d8" $ ldRegData A
--TODO 0x3F "CCF"
decodeOp (Eight 0x40) = Instruction (Eight 0x40) "LD B, B" id
decodeOp (Eight 0x41) = Instruction (Eight 0x41) "LD B, C" $ ldReg B C
decodeOp (Eight 0x42) = Instruction (Eight 0x42) "LD B, D" $ ldReg B D
decodeOp (Eight 0x43) = Instruction (Eight 0x43) "LD B, E" $ ldReg B E
decodeOp (Eight 0x44) = Instruction (Eight 0x44) "LD B, H" $ ldReg B H
decodeOp (Eight 0x45) = Instruction (Eight 0x45) "LD B, L" $ ldReg B L
decodeOp (Eight 0x46) = Instruction (Eight 0x46) "LD B, (HL)" $ ldRegRegAddr B (H, L)
decodeOp (Eight 0x47) = Instruction (Eight 0x46) "LD B, A" $ ldReg B A
decodeOp (Eight 0x48) = Instruction (Eight 0x48) "LD C, B" $ ldReg C B
decodeOp (Eight 0x49) = Instruction (Eight 0x49) "LD C, C" id
decodeOp (Eight 0x4A) = Instruction (Eight 0x4A) "LD C, D" $ ldReg C D
decodeOp (Eight 0x4B) = Instruction (Eight 0x4B) "LD C, E" $ ldReg C E
decodeOp (Eight 0x4C) = Instruction (Eight 0x4C) "LD C, H" $ ldReg C H
decodeOp (Eight 0x4D) = Instruction (Eight 0x4D) "LD C, L" $ ldReg C L
decodeOp (Eight 0x4E) = Instruction (Eight 0x4E) "LD C, (HL)" $ ldRegRegAddr C (H, L)
decodeOp (Eight 0x4F) = Instruction (Eight 0x4F) "LD C, A" $ ldReg C A
decodeOp (Eight 0x50) = Instruction (Eight 0x50) "LD D, B" $ ldReg D B
decodeOp (Eight 0x51) = Instruction (Eight 0x51) "LD D, C" $ ldReg D C
decodeOp (Eight 0x52) = Instruction (Eight 0x52) "LD D, D" id
decodeOp (Eight 0x53) = Instruction (Eight 0x53) "LD D, E" $ ldReg D E
decodeOp (Eight 0x54) = Instruction (Eight 0x54) "LD D, H" $ ldReg D H
decodeOp (Eight 0x55) = Instruction (Eight 0x55) "LD D, L" $ ldReg D L
decodeOp (Eight 0x56) = Instruction (Eight 0x56) "LD D, (HL)" $ ldRegRegAddr D (H, L)
decodeOp (Eight 0x57) = Instruction (Eight 0x57) "LD D, A" $ ldReg D A
decodeOp (Eight 0x58) = Instruction (Eight 0x58) "LD E, B" $ ldReg E B
decodeOp (Eight 0x59) = Instruction (Eight 0x59) "LD E, C" $ ldReg E C
decodeOp (Eight 0x5A) = Instruction (Eight 0x5A) "LD E, D" $ ldReg E D
decodeOp (Eight 0x5B) = Instruction (Eight 0x5B) "LD E, E" id
decodeOp (Eight 0x5C) = Instruction (Eight 0x5C) "LD E, H" $ ldReg E H
decodeOp (Eight 0x5D) = Instruction (Eight 0x5D) "LD E, L" $ ldReg E L
decodeOp (Eight 0x5E) = Instruction (Eight 0x5E) "LD E, (HL)" $ ldRegRegAddr E (H, L)
decodeOp (Eight 0x5F) = Instruction (Eight 0x5F) "LD E, A" $ ldReg E A
decodeOp (Eight 0x60) = Instruction (Eight 0x60) "LD H, B" $ ldReg H B
decodeOp (Eight 0x61) = Instruction (Eight 0x61) "LD H, C" $ ldReg H C
decodeOp (Eight 0x62) = Instruction (Eight 0x62) "LD H, D" $ ldReg H D
decodeOp (Eight 0x63) = Instruction (Eight 0x63) "LD H, E" $ ldReg H E
decodeOp (Eight 0x64) = Instruction (Eight 0x64) "LD H, H" id
decodeOp (Eight 0x65) = Instruction (Eight 0x65) "LD H, L" $ ldReg H L
decodeOp (Eight 0x66) = Instruction (Eight 0x66) "LD H, (HL)" $ ldRegRegAddr H (H, L)
decodeOp (Eight 0x67) = Instruction (Eight 0x67) "LD H, A" $ ldReg H A
decodeOp (Eight 0x68) = Instruction (Eight 0x68) "LD L, B" $ ldReg L B
decodeOp (Eight 0x69) = Instruction (Eight 0x69) "LD L, C" $ ldReg L C
decodeOp (Eight 0x6A) = Instruction (Eight 0x6A) "LD L, D" $ ldReg L D
decodeOp (Eight 0x6B) = Instruction (Eight 0x6B) "LD L, E" $ ldReg L E
decodeOp (Eight 0x6C) = Instruction (Eight 0x6C) "LD L, H" $ ldReg L H
decodeOp (Eight 0x6D) = Instruction (Eight 0x6D) "LD L, L" id
decodeOp (Eight 0x6E) = Instruction (Eight 0x6E) "LD L, (HL)" $ ldRegRegAddr L (H, L)
decodeOp (Eight 0x6F) = Instruction (Eight 0x6F) "LD L, A" $ ldReg L A
decodeOp (Eight 0x70) = Instruction (Eight 0x70) "LD (HL) B" $ ldRegAddrReg (H, L) B
decodeOp (Eight 0x71) = Instruction (Eight 0x71) "LD (HL) C" $ ldRegAddrReg (H, L) C
decodeOp (Eight 0x72) = Instruction (Eight 0x72) "LD (HL) D" $ ldRegAddrReg (H, L) D
decodeOp (Eight 0x73) = Instruction (Eight 0x73) "LD (HL) E" $ ldRegAddrReg (H, L) E
decodeOp (Eight 0x74) = Instruction (Eight 0x74) "LD (HL) H" $ ldRegAddrReg (H, L) H
decodeOp (Eight 0x75) = Instruction (Eight 0x75) "LD (HL) L" $ ldRegAddrReg (H, L) L
--TODO 0x76 "HALT"
decodeOp (Eight 0x77) = Instruction (Eight 0x77) "LD (HL) A" $ ldRegAddrReg (H, L) A
decodeOp (Eight 0x78) = Instruction (Eight 0x78) "LD A B" $ ldReg A B
decodeOp (Eight 0x79) = Instruction (Eight 0x79) "LD A C" $ ldReg A C
decodeOp (Eight 0x7A) = Instruction (Eight 0x7A) "LD A D" $ ldReg A D
decodeOp (Eight 0x7B) = Instruction (Eight 0x7B) "LD A E" $ ldReg A E
decodeOp (Eight 0x7C) = Instruction (Eight 0x7C) "LD A H" $ ldReg A H
decodeOp (Eight 0x7D) = Instruction (Eight 0x7D) "LD A L" $ ldReg A L
decodeOp (Eight 0x7E) = Instruction (Eight 0x7E) "LD A (HL)" $ ldRegRegAddr A (H, L)
decodeOp (Eight 0x7F) = Instruction (Eight 0x7F) "LD A A" id
decodeOp (Eight 0x80) = Instruction (Eight 0x80) "ADD A, B" $ addReg B
decodeOp (Eight 0x81) = Instruction (Eight 0x81) "ADD A, C" $ addReg C
decodeOp (Eight 0x82) = Instruction (Eight 0x82) "ADD A, D" $ addReg D
decodeOp (Eight 0x83) = Instruction (Eight 0x83) "ADD A, E" $ addReg E
decodeOp (Eight 0x84) = Instruction (Eight 0x84) "ADD A, H" $ addReg H
decodeOp (Eight 0x85) = Instruction (Eight 0x85) "ADD A, L" $ addReg L
--TODO 0x86 "ADD A, (HL)"
decodeOp (Eight 0x87) = Instruction (Eight 0x87) "ADD A, A" $ addReg A
--TODO 0x88 - 0xAE
decodeOp (Eight 0xAF) = Instruction (Eight 0xAF) "XOR A" $ xorReg A
--TODO 0xB0 - 0xCA
decodeOp (Eight 0xC1) = Instruction (Eight 0xC1) "POP BC" $ pop (B, C)
decodeOp (Eight 0xC5) = Instruction (Eight 0xC5) "PUSH BC" $ \gb -> push (getRegisters gb B C) gb
decodeOp (Eight 0xC9) = Instruction (Eight 0xC9) "RET" $ ret
decodeOp (Eight 0xCB) = Instruction (Eight 0xCB) "[CB Instruction]" $ \gb -> (decodeCb $ fetchCb $ incrementRegisterWithoutFlags PC gb)
                                                                             $ incrementRegisterWithoutFlags PC gb
--TODO 0xCC
decodeOp (Eight 0xCD) = Instruction (Eight 0xCD) "CALL a8" $ call
decodeOp (Eight 0xE0) = Instruction (Eight 0xE0) "LD (a8), A" $ ldhA
--TODO 0xE1
decodeOp (Eight 0xE2) = Instruction (Eight 0xE2) "LD (C), A" $ ldFFRegAddrReg C A
--TODO 0xE3 - 0xFF


fetchCb :: Gameboy -> EmuData
fetchCb gb = getMemory gb $ getRegister gb PC

bit_ :: EmuData -> Int -> Bool
bit_ (Sixteen d) b = testBit d $ b
bit_ (Eight d) b = testBit d $ b

testBitReg :: Register -> Int -> (Gameboy -> Gameboy)
testBitReg r i
  | isReg16 r == False = \gb -> zero . sub . half $ gb
      where
        isSet = \gb -> bit_ (getRegister gb r) i
        zero  = \gb -> (setFlag zeroFlag $ isSet gb) $ gb
        sub   = setFlag subtractFlag  False
        half  = setFlag halfCarryFlag True

decodeCb :: EmuData -> (Gameboy -> Gameboy)
decodeCb (Eight 0x7C) = testBitReg H 7
decodeCb (Eight 0x11) = rotateLeft C

evalInstruction :: Gameboy -> Instruction -> Gameboy
evalInstruction gb inst = gb & inst ^. operation

fetchNextInstr :: Gameboy -> Instruction
fetchNextInstr gb = decodeOp $ getMemory gb $ getRegister gb PC

stepGameboy :: Gameboy -> Gameboy
stepGameboy gb = incrementRegisterWithoutFlags PC . evalInstruction gb $ fetchNextInstr gb

stepNGameboy :: Int -> (Gameboy -> Gameboy)
stepNGameboy n = Prelude.foldl (.) id $ Prelude.replicate n stepGameboy

loadBootRom :: Gameboy -> Gameboy
loadBootRom gb = (\gb_ -> setMemory gb_ (Sixteen 0x00FF) (Eight 0x50)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00FE) (Eight 0xE0)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00FD) (Eight 0x01)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00FC) (Eight 0x3E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00FB) (Eight 0xFE)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00FA) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F9) (Eight 0x86)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F8) (Eight 0xFB)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F7) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F6) (Eight 0x05)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F5) (Eight 0x23)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F4) (Eight 0x86)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F3) (Eight 0x78)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F2) (Eight 0x19)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F1) (Eight 0x06)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00F0) (Eight 0xF5)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00EF) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00EE) (Eight 0x34)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00ED) (Eight 0xFE)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00EC) (Eight 0x7D)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00EB) (Eight 0x23)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00EA) (Eight 0xFE)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E9) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E8) (Eight 0xBE)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E7) (Eight 0x13)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E6) (Eight 0x1A)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E5) (Eight 0x00)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E4) (Eight 0xA8)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E3) (Eight 0x11)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E2) (Eight 0x01)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E1) (Eight 0x04)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00E0) (Eight 0x21)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00DF) (Eight 0x3C)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00DE) (Eight 0x42)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00DD) (Eight 0xA5)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00DC) (Eight 0xB9)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00DB) (Eight 0xA5)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00DA) (Eight 0xB9)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D9) (Eight 0x42)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D8) (Eight 0x3C)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D7) (Eight 0x3E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D6) (Eight 0x33)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D5) (Eight 0xB9)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D4) (Eight 0xBB)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D3) (Eight 0x9F)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D2) (Eight 0x99)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D1) (Eight 0xDC)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00D0) (Eight 0xDD)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00CF) (Eight 0xCC)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00CE) (Eight 0xEC)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00CD) (Eight 0x0E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00CC) (Eight 0x6E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00CB) (Eight 0x63)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00CA) (Eight 0x67)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C9) (Eight 0xBB)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C8) (Eight 0xBB)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C7) (Eight 0x99)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C6) (Eight 0xD9)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C5) (Eight 0xDD)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C4) (Eight 0xDD)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C3) (Eight 0xE6)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C2) (Eight 0x6E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C1) (Eight 0xCC)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00C0) (Eight 0xDC)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00BF) (Eight 0x0E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00BE) (Eight 0x00)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00BD) (Eight 0x89)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00BC) (Eight 0x88)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00BB) (Eight 0x1F)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00BA) (Eight 0x11)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B9) (Eight 0x08)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B8) (Eight 0x00)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B7) (Eight 0x0D)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B6) (Eight 0x00)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B5) (Eight 0x0C)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B4) (Eight 0x00)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B3) (Eight 0x83)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B2) (Eight 0x00)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B1) (Eight 0x73)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00B0) (Eight 0x03)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00AF) (Eight 0x0B)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00AE) (Eight 0x00)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00AD) (Eight 0x0D)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00AC) (Eight 0xCC)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00AB) (Eight 0x66)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00AA) (Eight 0x66)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A9) (Eight 0xED)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A8) (Eight 0xCE)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A7) (Eight 0xC9)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A6) (Eight 0x23)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A5) (Eight 0x22)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A4) (Eight 0x23)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A3) (Eight 0x22)) . --24607 Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A2) (Eight 0xF5)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A1) (Eight 0x20)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x00A0) (Eight 0x05)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x009F) (Eight 0x17)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x009E) (Eight 0x11)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x009D) (Eight 0xCB)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x009C) (Eight 0xC1)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x009B) (Eight 0x17)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x009A) (Eight 0x11)) . --24600 Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0099) (Eight 0xCB)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0098) (Eight 0xC5)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0097) (Eight 0x04)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0096) (Eight 0x06)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0095) (Eight 0x4F)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0094) (Eight 0xCB)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0093) (Eight 0x18)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0092) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0091) (Eight 0x16)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0090) (Eight 0x4F)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x008F) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x008E) (Eight 0x05)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x008D) (Eight 0xD2)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x008C) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x008B) (Eight 0x15)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x008A) (Eight 0x42)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0089) (Eight 0xE0)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0088) (Eight 0x90)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0087) (Eight 0x42)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0086) (Eight 0xF0)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0085) (Eight 0xE2)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0084) (Eight 0x87)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0083) (Eight 0x3E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0082) (Eight 0x0C)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0081) (Eight 0xE2)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0080) (Eight 0x7B)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x007F) (Eight 0x06)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x007E) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x007D) (Eight 0x64)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x007C) (Eight 0xFE)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x007B) (Eight 0xC1)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x007A) (Eight 0x1E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0079) (Eight 0x06)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0078) (Eight 0x28)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0077) (Eight 0x62)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0076) (Eight 0xFE)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0075) (Eight 0x83)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0074) (Eight 0x1E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0073) (Eight 0x7C)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0072) (Eight 0x24)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0071) (Eight 0x13)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0070) (Eight 0x0E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x006F) (Eight 0xF2)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x006E) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x006D) (Eight 0x1D)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x006C) (Eight 0xF7)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x006B) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x006A) (Eight 0x0D)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0069) (Eight 0xFA)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0068) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0067) (Eight 0x90)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0066) (Eight 0xFE)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0065) (Eight 0x44)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0064) (Eight 0xF0)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0063) (Eight 0x0C)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0062) (Eight 0x0E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0061) (Eight 0x02)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0060) (Eight 0x1E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x005F) (Eight 0x04)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x005E) (Eight 0x40)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x005D) (Eight 0xE0)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x005C) (Eight 0x91)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x005B) (Eight 0x3E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x005A) (Eight 0x42)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0059) (Eight 0xE0)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0058) (Eight 0x57)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0057) (Eight 0x64)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0056) (Eight 0x3E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0055) (Eight 0x67)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0054) (Eight 0xF3)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0053) (Eight 0x18)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0052) (Eight 0x0F)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0051) (Eight 0x2E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0050) (Eight 0xF9)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x004F) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x004E) (Eight 0x0D)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x004D) (Eight 0x32)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x004C) (Eight 0x08)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x004B) (Eight 0x28)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x004A) (Eight 0x3D)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0049) (Eight 0x0C)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0048) (Eight 0x0E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0047) (Eight 0x99)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0046) (Eight 0x2F)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0045) (Eight 0x21)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0044) (Eight 0x99)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0043) (Eight 0x10)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0042) (Eight 0xEA)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0041) (Eight 0x19)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0040) (Eight 0x3E)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x003F) (Eight 0xF9)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x003E) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x003D) (Eight 0x05)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x003C) (Eight 0x23)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x003B) (Eight 0x22)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x003A) (Eight 0x13)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0039) (Eight 0x1A)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0038) (Eight 0x08)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0037) (Eight 0x06)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0036) (Eight 0x00)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0035) (Eight 0xD8)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0034) (Eight 0x11)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0033) (Eight 0xF3)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0032) (Eight 0x20)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0031) (Eight 0x34)) .
                 (\gb_ -> setMemory gb_ (Sixteen 0x0030) (Eight 0xFE)) . 
                 (\gb_ -> setMemory gb_ (Sixteen 0x002F) (Eight 0x7B)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x002E) (Eight 0x13)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x002D) (Eight 0x00)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x002C) (Eight 0x96)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x002B) (Eight 0xCD)) . --24611 should put PC here. : Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x002A) (Eight 0x00)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0029) (Eight 0x95)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0028) (Eight 0xCD)) . --24596 Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0027) (Eight 0x1A)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0026) (Eight 0x80)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0025) (Eight 0x10)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0024) (Eight 0x21)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0023) (Eight 0x01)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0022) (Eight 0x04)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0021) (Eight 0x11)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0020) (Eight 0x47)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x001F) (Eight 0xE0)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x001E) (Eight 0xFC)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x001D) (Eight 0x3E)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x001C) (Eight 0x77)) . --Maybe Good 24590 steps
                 (\gb_ -> setMemory gb_ (Sixteen 0x001B) (Eight 0x77)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x001A) (Eight 0x3E)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0019) (Eight 0x32)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0018) (Eight 0xE2)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0017) (Eight 0xF3)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0016) (Eight 0x3E)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0015) (Eight 0x0C)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0014) (Eight 0xE2)) . -- 24584 steps : Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0013) (Eight 0x32)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0012) (Eight 0x80)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0011) (Eight 0x3E)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0010) (Eight 0x11)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x000F) (Eight 0x0E)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x000E) (Eight 0xFF)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x000D) (Eight 0x26)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x000C) (Eight 0x21)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x000B) (Eight 0xFB)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x000A) (Eight 0x20)) . --Must run 24578 steps to get to this opcode : Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0009) (Eight 0x7C)) . --Maybe Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0008) (Eight 0xCB)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0007) (Eight 0x32)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0006) (Eight 0x9F)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0005) (Eight 0xFF)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0004) (Eight 0x21)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0003) (Eight 0xAF)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0002) (Eight 0xFF)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0001) (Eight 0xFE)) . --Good
                 (\gb_ -> setMemory gb_ (Sixteen 0x0000) (Eight 0x31)) $ gb --I may have a mistake here.

runGameboyNSteps :: Int -> Gameboy
runGameboyNSteps n = stepNGameboy n $ loadBootRom defaultGameboy

data LcdState =
  LcdState
  {
    tileMapSelect :: (EmuData, EmuData),
    windowDisplay :: Bool,
    bgTileData    :: (EmuData, EmuData),
    bgTileMap     :: (EmuData, EmuData),
    spriteSize    :: Bool
  }

-}
