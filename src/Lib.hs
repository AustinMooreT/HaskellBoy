{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module Lib
    () where

import Data.Word
import Control.Lens
import Data.Vector as V
import Data.Bits
import Data.Binary.Get
import Graphics.Gloss

data EmuData =
  Eight
  {
    _eight :: Word8
  }
  |
  Sixteen
  {
    _sixteen :: Word16
  }
makeLenses ''EmuData
makePrisms ''EmuData

instance Show EmuData where
  show (Eight w)   = show w
  show (Sixteen w) = show w

combineEmuData :: EmuData -> EmuData -> EmuData
combineEmuData (Eight d1) (Eight d2) = Sixteen . fromIntegral $ shiftL d1 8 .&. fromIntegral d2
--TODO non exhaustive

breakHiEnum :: EmuData -> EmuData
breakHiEnum (Sixteen d) = Eight . fromIntegral $ shiftR d 8
--TODO non exhaustive

breakLoEnum :: EmuData -> EmuData
breakLoEnum (Sixteen d) = Eight $ fromIntegral d
--TODO non exhaustive



data Cpu =
  Cpu
  {
    _registerA  :: EmuData,
    _registerB  :: EmuData,
    _registerC  :: EmuData,
    _registerD  :: EmuData,
    _registerE  :: EmuData,
    _registerF  :: EmuData,
    _registerH  :: EmuData,
    _registerL  :: EmuData,
    _registerSP :: EmuData,
    _registerPC :: EmuData
  }
makeLenses ''Cpu
instance Show Cpu where
  show cpu = "A:[" Prelude.++ (show $ cpu ^. registerA) Prelude.++ "]\n" Prelude.++
             "B:[" Prelude.++ (show $ cpu ^. registerB) Prelude.++ "]\n" Prelude.++
             "C:[" Prelude.++ (show $ cpu ^. registerC) Prelude.++ "]\n" Prelude.++
             "D:[" Prelude.++ (show $ cpu ^. registerD) Prelude.++ "]\n" Prelude.++
             "E:[" Prelude.++ (show $ cpu ^. registerE) Prelude.++ "]\n" Prelude.++
             "F:[" Prelude.++ (show $ cpu ^. registerF) Prelude.++ "]\n" Prelude.++
             "H:[" Prelude.++ (show $ cpu ^. registerH) Prelude.++ "]\n" Prelude.++
             "L:[" Prelude.++ (show $ cpu ^. registerL) Prelude.++ "]" Prelude.++
             "SP:[" Prelude.++ (show $ cpu ^. registerSP) Prelude.++ "]\n" Prelude.++
             "PC:[" Prelude.++ (show $ cpu ^. registerPC) Prelude.++ "]\n"



data Register = A | B | C | D | E | F | H | L | SP | PC

zeroFlag :: Word8
zeroFlag = 128

subtractFlag :: Word8
subtractFlag = 64

halfCarryFlag :: Word8
halfCarryFlag = 32

carryFlag :: Word8
carryFlag = 16

flagToInt :: Word8 -> Int
flagToInt 128 = 7
flagToInt 64  = 6
flagToInt 32  = 5
flagToInt 16  = 4

registerToFunc :: Register -> (Cpu -> EmuData)
registerToFunc A  = _registerA
registerToFunc B  = _registerB
registerToFunc C  = _registerC
registerToFunc D  = _registerD
registerToFunc E  = _registerE
registerToFunc F  = _registerF
registerToFunc H  = _registerH
registerToFunc L  = _registerL
registerToFunc SP = _registerSP
registerToFunc PC = _registerPC

registerToLens :: Functor f => Register -> (EmuData -> f EmuData) -> Cpu -> f Cpu
registerToLens A  = registerA
registerToLens B  = registerB
registerToLens C  = registerC
registerToLens D  = registerD
registerToLens E  = registerE
registerToLens F  = registerF
registerToLens H  = registerH
registerToLens L  = registerL
registerToLens SP = registerSP
registerToLens PC = registerPC

composeRegisterLenses :: Functor f => Register -> Register -> (EmuData -> f EmuData) -> Cpu -> f Cpu
composeRegisterLenses reg1 reg2 = lens getter setter
  where
    getter __cpu = combineEmuData (registerToFunc reg1 __cpu) (registerToFunc reg2 __cpu)
    setter __cpu d = __cpu & registerToLens reg1 .~ breakHiEnum d & registerToLens reg2 .~ breakLoEnum d



data Memory =
  Memory
  {
    _bytes :: V.Vector Word8
  }
makeLenses ''Memory



data Gameboy =
  Gameboy
  {
    _cpu    :: Cpu,
    _memory :: Memory
  }
makeLenses ''Gameboy



data Instruction =
  Instruction
  {
    _opcode    :: EmuData,
    _name      :: String,
    _operation :: (Gameboy -> Gameboy)
  }
makeLenses ''Instruction



getMemory :: Gameboy -> EmuData -> EmuData
getMemory gb (Sixteen addr) = Eight $ (view (memory . bytes) gb) ! (fromIntegral addr)
--TODO non exhaustive

setMemory :: Gameboy -> EmuData -> EmuData -> Gameboy
setMemory gb (Sixteen addr) (Eight d) = over (memory . bytes) (\y -> y // [(fromIntegral addr, d)]) gb
--TODO non exhaustive

data Cartridge =
  Cartridge
  {
    _data :: V.Vector Word8
  }


setRegister :: Gameboy -> Register -> EmuData -> Gameboy
setRegister gb r d = gb & cpu . registerToLens r .~ d

setRegisters :: Gameboy -> Register -> Register -> EmuData -> Gameboy
setRegisters gb r1 r2 d = gb & cpu . composeRegisterLenses r1 r2 .~ d

getRegister :: Gameboy -> Register -> EmuData
getRegister gb r = gb ^. cpu . registerToLens r

getRegisters :: Gameboy -> Register -> Register -> EmuData
getRegisters gb r1 r2 = gb ^. cpu . composeRegisterLenses r1 r2

isReg16 :: Register -> Bool
isReg16 SP = True
isReg16 PC = True
isReg16 _  = False

ldReg :: Register -> Register -> (Gameboy -> Gameboy)
ldReg d s = \gb -> setRegister gb d $ getRegister gb s

ldRegMem :: Register -> EmuData -> (Gameboy -> Gameboy)
ldRegMem reg addr = \gb -> setRegister gb reg $ getMemory gb addr

--TODO come up with a better name
ldRegRegAddr :: Register -> (Register, Register) -> (Gameboy -> Gameboy)
ldRegRegAddr r (h, l) = \gb -> setRegister gb r $ getMemory gb $ getRegisters gb h l

--TODO come up with a better name
ldRegAddrReg :: (Register, Register) -> Register -> (Gameboy -> Gameboy)
ldRegAddrReg (h, l) r = \gb -> setMemory gb (getRegisters gb h l) (getRegister gb r)

ldRegAddrData :: (Register, Register) -> (Gameboy -> Gameboy)
ldRegAddrData (r1, r2) gb = setMemory gb1 (getRegisters gb r1 r2) (getMemory gb1 $ getRegister gb1 PC)
  where
    gb1 = incrementRegister PC gb

ldRegRegData :: (Register, Register) -> (Gameboy -> Gameboy)
ldRegRegData (r1, r2) = (\gb -> (setRegister gb r2) (getMemory gb $ getRegister gb PC)) .
                        incrementRegister PC .
                        (\gb -> (setRegister gb r1) (getMemory gb $ getRegister gb PC)) .
                        incrementRegister PC

ldRegData :: Register -> (Gameboy -> Gameboy)
ldRegData r
  | isReg16 r == True = \gb -> (setRegister (gb2 gb) r)
    (combineEmuData (getMemory (gb1 gb) $
                     getRegister (gb1 gb) PC) (getMemory (gb1 gb) $
                                               getRegister (gb1 gb) PC))
  | otherwise = \gb -> (setRegister (gb1 gb) r) (getMemory (gb1 gb) $ getRegister (gb1 gb) PC)
    where
      gb1 = incrementRegister PC
      gb2 = incrementRegister PC . gb1

incrementEmuData :: EmuData -> EmuData
incrementEmuData (Eight d)   = Eight $ d + 1
incrementEmuData (Sixteen d) = Sixteen $ d + 1

decrementEmuData :: EmuData -> EmuData
decrementEmuData (Eight d)   = Eight $ d - 1
decrementEmuData (Sixteen d) = Sixteen $ d - 1

addEmuData :: EmuData -> EmuData -> EmuData
addEmuData (Eight e1) (Eight e2)     = Eight $ e1 + e2
addEmuData (Sixteen e1) (Sixteen e2) = Sixteen $ e1 + e2
--TODO non exhaustive

isZeroEmuData :: EmuData -> Bool
isZeroEmuData (Eight d)   = d == 0b00000000
isZeroEmuData (Sixteen d) = d == 0b0000000000000000

setZero :: EmuData -> (Gameboy -> Gameboy)
setZero d = setFlag zeroFlag $ isZeroEmuData d

--TODO check logic
setCarry :: EmuData -> EmuData -> (Gameboy -> Gameboy)
setCarry (Eight e1) (Eight e2) = setFlag zeroFlag (e1 < e2)
setCarry (Sixteen e1) (Sixteen e2) = setFlag zeroFlag (e1 < e2)

--TODO check logic
setHalfCarry :: EmuData -> EmuData -> (Gameboy -> Gameboy)
setHalfCarry (Eight e1) (Eight e2) =  setFlag halfCarryFlag ((e1 .&. 0b00001111) < (e2 .&. 0b00001111))
setHalfCarry (Sixteen e1) (Sixteen e2) =  setFlag halfCarryFlag
  ((e1 .&. 0b0000111111111111) < (e2 .&. 0b0000111111111111))

incrementWithFlags :: EmuData -> (EmuData -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
incrementWithFlags d f = f increment .
                         halfCarry .
                         subtractf .
                         zero
    where
    increment = incrementEmuData d
    zero      = \gb -> setZero increment gb
    halfCarry = \gb -> setHalfCarry increment d gb
    subtractf = \gb -> setFlag subtractFlag False gb

incrementRegister :: Register -> (Gameboy -> Gameboy)
incrementRegister r = \gb -> incrementWithFlags (getRegister gb r) (\d -> (\gb1 -> (setRegister gb1 r d))) gb

incrementRegisters :: (Register, Register) -> (Gameboy -> Gameboy)
incrementRegisters (r1, r2) = \gb -> incrementWithFlags (getRegisters gb r1 r2) (\d -> (\gb1 -> (setRegisters gb1 r1 r2 d))) gb

decrementWithFlags :: EmuData -> (EmuData -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
decrementWithFlags d f = f decrement .
                         zero .
                         halfCarry .
                         subtractf
  where
    decrement = decrementEmuData d
    zero      = \gb -> setZero decrement gb
    halfCarry = \gb -> setHalfCarry decrement d gb
    subtractf = \gb -> setFlag subtractFlag True gb

decrementRegister :: Register -> (Gameboy -> Gameboy)
decrementRegister r = \gb -> decrementWithFlags (getRegister gb r) (\d -> (\gb1 -> setRegister gb1 r d)) gb

decrementRegisters :: (Register, Register) -> (Gameboy -> Gameboy)
decrementRegisters (r1, r2) = \gb -> decrementWithFlags (getRegisters gb r1 r2) (\d -> (\gb1 -> setRegisters gb1 r1 r2 d)) gb

incrementRegisterWithoutFlags :: Register -> (Gameboy -> Gameboy)
incrementRegisterWithoutFlags r = \gb -> setRegister gb r $ incrementEmuData $ getRegister gb r

incrementRegistersWithoutFlags :: (Register, Register) -> (Gameboy -> Gameboy)
incrementRegistersWithoutFlags (r1, r2) = \gb -> setRegisters gb r1 r2 $ incrementEmuData $ getRegisters gb r1 r2

decrementRegisterWithoutFlags :: Register -> (Gameboy -> Gameboy)
decrementRegisterWithoutFlags r = \gb -> setRegister gb r $ incrementEmuData $ getRegister gb r

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



setFlag :: Word8 -> Bool -> Gameboy -> Gameboy
setFlag w True  gb = gb & cpu . registerToLens F %~ \(Eight w8) -> Eight $ w8 .|. w
setFlag w False gb = gb & cpu . registerToLens F %~ \(Eight w8) -> Eight $ w8 .&. complement w

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

rotateLeftA :: Gameboy -> Gameboy
rotateLeftA gb = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) $
                 (\gb1 -> setRegister gb1 A $ Eight $ ((_eight $ getRegister gb1 A) .&. 0b11111110) .|. carryBeforeMask) $
                 setFlag carryFlag carryAfter $
                 setRegister gb A $ Eight $ rotateL (_eight $ getRegister gb A) 1
  where
    carryBefore = getFlag gb carryFlag
    carryAfter  = testBit (_eight $ getRegister gb A) 7
    carryBeforeMask = if carryBefore then 0b00000001 else 0b00000000

rotateRightA :: Gameboy -> Gameboy
rotateRightA gb = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) $
                  (\gb1 -> setRegister gb1 A $ Eight $ ((_eight $ getRegister gb1 A) .&. 0b11111110) .|. carryBeforeMask) $
                  setFlag carryFlag carryAfter $
                  setRegister gb A $ Eight $ rotateR (_eight $ getRegister gb A) 1
  where
    carryBefore = getFlag gb carryFlag
    carryAfter  = testBit (_eight $ getRegister gb A) 0
    carryBeforeMask = if carryBefore then 0b10000000 else 0b00000000

rotateRightACarry :: (Gameboy -> Gameboy)
rotateRightACarry = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) .
                    (\gb -> setFlag carryFlag (testBit (_eight $ getRegister gb A) 7) gb) .
                    (\gb -> setRegister gb A $ Eight $ rotateR (_eight $ getRegister gb A) 1)

decodeOp :: EmuData-> Instruction
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
decodeOp (Eight 0x21) = Instruction (Eight 0x21) "LD HL, d16" $ ldRegRegData (H, L)
--TODO 0x22 "LD (HL+), A"
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
--TODO 0x88 - 0xFF

evalInstruction :: Gameboy -> Instruction -> Gameboy
evalInstruction gb inst = gb & inst ^. operation

fetchNextInstr :: Gameboy -> Instruction
fetchNextInstr gb = decodeOp $ getRegister gb PC

stepGameboy :: Gameboy -> Gameboy
stepGameboy gb = evalInstruction gb $ fetchNextInstr gb
