-- Gameboy emulator

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module Lib
    () where

import Data.Word
import Control.Lens
import Data.Vector
import Data.Bits



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

data Register = A | B | C | D | E | F | H | L | SP | PC

zeroFlag :: Word8
zeroFlag = 0b10000000

subtractFlag :: Word8
subtractFlag = 0b01000000

halfCarryFlag :: Word8
halfCarryFlag = 0b00100000

carryFlag :: Word8
carryFlag = 0b00010000

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
    _bytes :: Vector Word8
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

--NOTE pretty much just adds EmuData to A.
accumAdd :: EmuData -> (Gameboy -> Gameboy)
accumAdd (Eight byte) = (\gb -> (setRegister gb A (addition gb))) .
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

addReg :: Register -> (Gameboy -> Gameboy)
addReg r = \gb -> accumAdd (getRegister gb r) gb

decodeOp :: EmuData-> Instruction
decodeOp (Eight 0x00) = Instruction (Eight 0x00) "NOP" id
decodeOp (Eight 0x01) = Instruction (Eight 0x01) "LD BC, d16" (ldRegRegData (B, C))
decodeOp (Eight 0x02) = Instruction (Eight 0x02) "LD (BC), A" (ldRegAddrReg (B, C) A)
decodeOp (Eight 0x03) = Instruction (Eight 0x03) "INC BC" (incrementRegistersWithoutFlags (B, C))
decodeOp (Eight 0x04) = Instruction (Eight 0x04) "INC B" (incrementRegister B)
decodeOp (Eight 0x05) = Instruction (Eight 0x05) "DEC B" (decrementRegister B)
decodeOp (Eight 0x06) = Instruction (Eight 0x06) "LD B, d8" (ldRegData B)
--TODO 0x07 "RLCA"
--TODO 0x08 "LD (a16) SP"
decodeOp (Eight 0x09) = Instruction (Eight 0x09) "ADD HL, BC" (addRegsRegs (H, L) (B, C))
decodeOp (Eight 0x0A) = Instruction (Eight 0x0A) "LD A, (BC)" (ldRegRegAddr A (B, C))
decodeOp (Eight 0x0B) = Instruction (Eight 0x0B) "DEC BC" (decrementRegistersWithoutFlags (B, C))
decodeOp (Eight 0x0C) = Instruction (Eight 0x0C) "INC C" (incrementRegister C)
decodeOp (Eight 0x0D) = Instruction (Eight 0x0D) "DEC C" (decrementRegister C)
decodeOp (Eight 0x0E) = Instruction (Eight 0x0E) "LD C, d8" (ldRegData C)
--TODO 0x0F "RRCA"
--TODO 0x10 "STOP 0"
decodeOp (Eight 0x11) = Instruction (Eight 0x11) "LD DE, d16" (ldRegRegData (D, E))
decodeOp (Eight 0x12) = Instruction (Eight 0x12) "LD (DE), A" (ldRegAddrReg (D, E) A)
decodeOp (Eight 0x13) = Instruction (Eight 0x13) "INC DE" (incrementRegistersWithoutFlags (D, E))
decodeOp (Eight 0x14) = Instruction (Eight 0x14) "INC D" (incrementRegister D)
decodeOp (Eight 0x15) = Instruction (Eight 0x15) "DEC D" (decrementRegister D)
decodeOp (Eight 0x16) = Instruction (Eight 0x16) "LD D, d8" (ldRegData D)
--TODO 0x17 "RLA"
--TODO 0x18 "JR r8"
decodeOp (Eight 0x19) = Instruction (Eight 0x19) "ADD HL, DE" (addRegsRegs (H, L) (D, E))
decodeOp (Eight 0x1A) = Instruction (Eight 0x1A) "LD A, (DE)" (ldRegRegAddr A (D, E))
decodeOp (Eight 0x1B) = Instruction (Eight 0x1B) "DEC BC" (decrementRegistersWithoutFlags (D, E))
decodeOp (Eight 0x1C) = Instruction (Eight 0x1C) "INC E" (incrementRegister E)
decodeOp (Eight 0x1D) = Instruction (Eight 0x1D) "DEC E" (decrementRegister E)
decodeOp (Eight 0x1E) = Instruction (Eight 0x1E) "LD C, d8" (ldRegData C)
--TODO 0x1F "RRA"
--TODO 0x20 "JR NZ, r8"
decodeOp (Eight 0x21) = Instruction (Eight 0x21) "LD HL, d16" (ldRegRegData (H, L))
--TODO 0x22 "LD (HL+), A"
decodeOp (Eight 0x23) = Instruction (Eight 0x23) "INC HL" (incrementRegistersWithoutFlags (H, L))
decodeOp (Eight 0x24) = Instruction (Eight 0x24) "INC H" (incrementRegister H)
decodeOp (Eight 0x25) = Instruction (Eight 0x25) "DEC H" (decrementRegister H)
decodeOp (Eight 0x28) = Instruction (Eight 0x26) "LD H, d8" (ldRegData H)
--TODO 0x27 "DAA"
--TODO 0x28 "JR Z, r8"
decodeOp (Eight 0x29) = Instruction (Eight 0x29) "ADD HL, HL" (addRegsRegs (H, L) (H, L))
--TODO 0x2A "LD A, (HL+)"
decodeOp (Eight 0x2B) = Instruction (Eight 0x2B) "DEC HL" (decrementRegistersWithoutFlags (H, L))
decodeOp (Eight 0x2C) = Instruction (Eight 0x2C) "INC L" (incrementRegister L)
decodeOp (Eight 0x2D) = Instruction (Eight 0x2D) "DEC L" (decrementRegister L)
decodeOp (Eight 0x2E) = Instruction (Eight 0x2E) "LD L, d8" (ldRegData L)
--TODO 0x2F "CPL"
--TODO 0x30 "JR NC, r8"
decodeOp (Eight 0x31) = Instruction (Eight 0x31) "LD SP, d16" (ldRegData SP)
--TODO 0x32 "LD (HL-), A"
decodeOp (Eight 0x33) = Instruction (Eight 0x33) "INC SP" (incrementRegisterWithoutFlags SP)
--TODO 0x34 "INC (HL)"
--TODO 0x35 "DEC (HL)"
--TODO 0x36 "LD (HL), d8"
--TODO 0x37 "SCF"
--TODO 0x38 "JR C, r8"
--TODO 0x39 "ADD HL, SP"
--TODO 0x3A "LD A, (HL-)"
decodeOp (Eight 0x3B) = Instruction (Eight 0x3B) "DEC SP" (decrementRegisterWithoutFlags SP)
decodeOp (Eight 0x3C) = Instruction (Eight 0x3C) "INC A" (incrementRegister A)
decodeOp (Eight 0x3D) = Instruction (Eight 0x3D) "DEC A" (decrementRegister A)
decodeOp (Eight 0x3E) = Instruction (Eight 0x3E) "LD A, d8" (ldRegData A)
--TODO 0x3F "CCF"
decodeOp (Eight 0x40) = Instruction (Eight 0x40) "ld B, B" id
decodeOp (Eight 0x41) = Instruction (Eight 0x41) "ld B, C" (ldReg B C)
decodeOp (Eight 0x42) = Instruction (Eight 0x42) "ld B, D" (ldReg B D)
decodeOp (Eight 0x43) = Instruction (Eight 0x43) "ld B, E" (ldReg B E)
decodeOp (Eight 0x44) = Instruction (Eight 0x44) "ld B, H" (ldReg B H)
decodeOp (Eight 0x45) = Instruction (Eight 0x45) "ld B, L" (ldReg B L)
decodeOp (Eight 0x46) = Instruction (Eight 0x46) "ld B, (HL)" (ldRegRegAddr B (H, L))
decodeOp (Eight 0x47) = Instruction (Eight 0x46) "ld B, A" (ldReg B A)
decodeOp (Eight 0x48) = Instruction (Eight 0x48) "ld C, B" (ldReg C B)
decodeOp (Eight 0x49) = Instruction (Eight 0x49) "ld C, C" id
decodeOp (Eight 0x4A) = Instruction (Eight 0x4A) "ld C, D" (ldReg C D)
decodeOp (Eight 0x4B) = Instruction (Eight 0x4B) "ld C, E" (ldReg C E)
decodeOp (Eight 0x4C) = Instruction (Eight 0x4C) "ld C, H" (ldReg C H)
decodeOp (Eight 0x4D) = Instruction (Eight 0x4D) "ld C, L" (ldReg C L)
decodeOp (Eight 0x4E) = Instruction (Eight 0x4E) "ld C, (HL)" (ldRegRegAddr C (H, L))
decodeOp (Eight 0x4F) = Instruction (Eight 0x4F) "ld C, A" (ldReg C A)
decodeOp (Eight 0x50) = Instruction (Eight 0x50) "ld D, B" (ldReg D B)
decodeOp (Eight 0x51) = Instruction (Eight 0x51) "ld D, C" (ldReg D C)
decodeOp (Eight 0x52) = Instruction (Eight 0x52) "ld D, D" id
decodeOp (Eight 0x53) = Instruction (Eight 0x53) "ld D, E" (ldReg D E)
decodeOp (Eight 0x54) = Instruction (Eight 0x54) "ld D, H" (ldReg D H)
decodeOp (Eight 0x55) = Instruction (Eight 0x55) "ld D, L" (ldReg D L)
decodeOp (Eight 0x56) = Instruction (Eight 0x56) "ld D, (HL)" (ldRegRegAddr D (H, L))
decodeOp (Eight 0x57) = Instruction (Eight 0x57) "ld D, A" (ldReg D A)
decodeOp (Eight 0x58) = Instruction (Eight 0x58) "ld E, B" (ldReg E B)
decodeOp (Eight 0x59) = Instruction (Eight 0x59) "ld E, C" (ldReg E C)
decodeOp (Eight 0x5A) = Instruction (Eight 0x5A) "ld E, D" (ldReg E D)
decodeOp (Eight 0x5B) = Instruction (Eight 0x5B) "ld E, E" id
decodeOp (Eight 0x5C) = Instruction (Eight 0x5C) "ld E, H" (ldReg E H)
decodeOp (Eight 0x5D) = Instruction (Eight 0x5D) "ld E, L" (ldReg E L)
decodeOp (Eight 0x5E) = Instruction (Eight 0x5E) "ld E, (HL)" (ldRegRegAddr E (H, L))
decodeOp (Eight 0x5F) = Instruction (Eight 0x5F) "ld E, A" (ldReg E A)
decodeOp (Eight 0x60) = Instruction (Eight 0x60) "ld H, B" (ldReg H B)
decodeOp (Eight 0x61) = Instruction (Eight 0x61) "ld H, C" (ldReg H C)
decodeOp (Eight 0x62) = Instruction (Eight 0x62) "ld H, D" (ldReg H D)
decodeOp (Eight 0x63) = Instruction (Eight 0x63) "ld H, E" (ldReg H E)
decodeOp (Eight 0x64) = Instruction (Eight 0x64) "ld H, H" id
decodeOp (Eight 0x65) = Instruction (Eight 0x65) "ld H, L" (ldReg H L)
decodeOp (Eight 0x66) = Instruction (Eight 0x66) "ld H, (HL)" (ldRegRegAddr H (H, L))
decodeOp (Eight 0x67) = Instruction (Eight 0x67) "ld H, A" (ldReg H A)
decodeOp (Eight 0x68) = Instruction (Eight 0x68) "ld L, B" (ldReg L B)
decodeOp (Eight 0x69) = Instruction (Eight 0x69) "ld L, C" (ldReg L C)
decodeOp (Eight 0x6A) = Instruction (Eight 0x6A) "ld L, D" (ldReg L D)
decodeOp (Eight 0x6B) = Instruction (Eight 0x6B) "ld L, E" (ldReg L E)
decodeOp (Eight 0x6C) = Instruction (Eight 0x6C) "ld L, H" (ldReg L H)
decodeOp (Eight 0x6D) = Instruction (Eight 0x6D) "ld L, L" id
decodeOp (Eight 0x6E) = Instruction (Eight 0x6E) "ld L, (HL)" (ldRegRegAddr L (H, L))
decodeOp (Eight 0x6F) = Instruction (Eight 0x6F) "ld L, A" (ldReg L A)
decodeOp (Eight 0x70) = Instruction (Eight 0x70) "ld (HL) B" (ldRegAddrReg (H, L) B)
decodeOp (Eight 0x71) = Instruction (Eight 0x71) "ld (HL) C" (ldRegAddrReg (H, L) C)
decodeOp (Eight 0x72) = Instruction (Eight 0x72) "ld (HL) D" (ldRegAddrReg (H, L) D)
decodeOp (Eight 0x73) = Instruction (Eight 0x73) "ld (HL) E" (ldRegAddrReg (H, L) E)
decodeOp (Eight 0x74) = Instruction (Eight 0x74) "ld (HL) H" (ldRegAddrReg (H, L) H)
decodeOp (Eight 0x75) = Instruction (Eight 0x75) "ld (HL) L" (ldRegAddrReg (H, L) L)
--TODO 0x76 "HALT"
decodeOp (Eight 0x77) = Instruction (Eight 0x77) "ld (HL) A" (ldRegAddrReg (H, L) A)
decodeOp (Eight 0x78) = Instruction (Eight 0x78) "ld A B" (ldReg A B)
decodeOp (Eight 0x79) = Instruction (Eight 0x79) "ld A C" (ldReg A C)
decodeOp (Eight 0x7A) = Instruction (Eight 0x7A) "ld A D" (ldReg A D)
decodeOp (Eight 0x7B) = Instruction (Eight 0x7B) "ld A E" (ldReg A E)
decodeOp (Eight 0x7C) = Instruction (Eight 0x7C) "ld A H" (ldReg A H)
decodeOp (Eight 0x7D) = Instruction (Eight 0x7D) "ld A L" (ldReg A L)
decodeOp (Eight 0x7E) = Instruction (Eight 0x7E) "ld A (HL)" (ldRegRegAddr A (H, L))
decodeOp (Eight 0x7F) = Instruction (Eight 0x7F) "ld A A" id
decodeOp (Eight 0x80) = Instruction (Eight 0x80) "ADD A, B" (addReg B)
decodeOp (Eight 0x81) = Instruction (Eight 0x81) "ADD A, C" (addReg C)
decodeOp (Eight 0x82) = Instruction (Eight 0x82) "ADD A, D" (addReg D)
decodeOp (Eight 0x83) = Instruction (Eight 0x83) "ADD A, E" (addReg E)
decodeOp (Eight 0x84) = Instruction (Eight 0x84) "ADD A, H" (addReg H)
decodeOp (Eight 0x85) = Instruction (Eight 0x85) "ADD A, L" (addReg L)
--TODO 0x86 "ADD A, (HL)"
decodeOp (Eight 0x87) = Instruction (Eight 0x87) "ADD A, A" (addReg A)
--TODO 0x88 - 0xFF

evalInstruction :: Gameboy -> Instruction -> Gameboy
evalInstruction gb inst = gb & inst ^. operation

fetchNextInstr :: Gameboy -> Instruction
fetchNextInstr gb = decodeOp $ getRegister gb PC

stepGameboy :: Gameboy -> Gameboy
stepGameboy gb = evalInstruction gb $ fetchNextInstr gb
