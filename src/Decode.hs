{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Decode (module Decode) where

import Cpu
import Memory

import Data.Word
import Control.Lens

type CpuOpCpu       = Cpu -> IO Cpu
type CpuMemOpCpu    = Memory -> Cpu -> IO Cpu
type CpuMemOpMem    = Memory -> Cpu -> IO Memory
type CpuMemOpCpuMem = Memory -> Cpu -> IO (Cpu, Memory)

data Operation = OpCpuOpCpu CpuOpCpu | OpCpuMemOpCpu CpuMemOpCpu | OpCpuMemOpMem CpuMemOpMem | OpCpuMemOpCpuMem CpuMemOpCpuMem

type Cycles = Cpu -> Integer

-- TODO Clean these up.
f g = OpCpuOpCpu g
g h = OpCpuMemOpCpu h
h j = OpCpuMemOpMem j
j k = OpCpuMemOpCpuMem k

-- | Represents an instruction to the Cpu's processor.
data Instruction =
  Instruction
  {
    _opcode    :: Word8,
    _name      :: String,
    _time      :: Cycles,
    _operation :: Operation
  }
makeLenses ''Instruction

-- | Instance of show for converting Instructions to a String.
instance Show Instruction where
  show instr = (show $ instr ^. opcode) Prelude.++ (show $ instr ^. name)

const4  = const 4
const8  = const 8
const12 = const 12
const16 = const 16
const20 = const 20
const24 = const 24
const28 = const 28
const32 = const 32

condTime :: Bool -> Integer -> Integer -> Integer
condTime True t _ = t
condTime _    _ t = t

fixCpu :: (Cpu -> Cpu) -> (Cpu -> IO Cpu)
fixCpu f = \cpu -> return $ f cpu

-- TODO/NOTE/WARNING this function has layers upon layers of lazy hacks on it. There be dragons beyond this point.
decodeOp :: Word8 -> Instruction
decodeOp 0x00 = Instruction 0x00 "NOP" const4 $ OpCpuOpCpu (\cpu -> return cpu)
decodeOp 0x01 = Instruction 0x01 "LD BC, d16" const12 $ OpCpuMemOpCpu (ldRegRegWithData (B, C))
decodeOp 0x02 = Instruction 0x02 "LD (BC), A" const8 $ OpCpuMemOpMem (ldMemRegRegWithReg (B, C) A)
decodeOp 0x03 = Instruction 0x03 "INC BC" const8 $ OpCpuOpCpu (\cpu -> return $ incrementRegistersWithoutFlags (B, C) cpu)
decodeOp 0x04 = Instruction 0x04 "INC B" const4 $ OpCpuOpCpu (\cpu -> return $ incrementRegisterWithFlags B cpu)
decodeOp 0x05 = Instruction 0x05 "DEC B" const4 $ OpCpuOpCpu (\cpu -> return $ decrementRegisterWithFlags B cpu)
decodeOp 0x06 = Instruction 0x06 "LD B, d8" const8 $ OpCpuMemOpCpu (ldRegWithData B)
decodeOp 0x07 = Instruction 0x07 "RLCA" const4 $ OpCpuOpCpu (\cpu -> return $ rotateLeftACarry cpu)
decodeOp 0x08 = Instruction 0x08 "LD (a16), SP" const20 $ h $ ldMemDataWithRegReg (SHI, PLO)
decodeOp 0x09 = Instruction 0x09 "ADD HL, BC" const8 $ f $ (\cpu -> return $ addRegRegWithRegRegWithFlags (H, L) (B, C) cpu)
decodeOp 0x0A = Instruction 0x0A "LD A, (BC)" const8 $ g $ ldRegWithRegRegMem A (B, C)
decodeOp 0x0B = Instruction 0x0B "DEC BC" const8 $ f $ (\cpu -> return $ decrementRegistersWithoutFlags (B, C) cpu)
decodeOp 0x0C = Instruction 0x0C "INC C" const4 $ f $ (\cpu -> return $ incrementRegisterWithFlags C cpu)
decodeOp 0x0D = Instruction 0x0D "DEC C" const4 $ f $ (\cpu -> return $ decrementRegisterWithFlags C cpu)
decodeOp 0x0E = Instruction 0x0E "LD C, d8" const8 $ g $ ldRegWithData C
decodeOp 0x0F = Instruction 0x0F "RRCA" const4 $ f $ (\cpu -> return $ rotateRightACarry cpu)
--TODO 0x10 "STOP 0"
decodeOp 0x11 = Instruction 0x11 "LD DE, d16" const12 $ g $ ldRegRegWithData (D, E)
decodeOp 0x12 = Instruction 0x12 "LD (DE), A" const8 $ h $ ldMemRegRegWithReg (D, E) A
decodeOp 0x13 = Instruction 0x13 "INC DE" const8 $ f $ (\cpu -> return $ incrementRegistersWithoutFlags (D, E) cpu)
decodeOp 0x14 = Instruction 0x14 "INC D" const4 $ f $ (\cpu -> return $ incrementRegisterWithFlags D cpu)
decodeOp 0x15 = Instruction 0x15 "DEC D" const4 $ f $ (\cpu -> return $ decrementRegisterWithFlags D cpu)
decodeOp 0x16 = Instruction 0x16 "LD D, d8" const8 $ g $ ldRegWithData D
decodeOp 0x17 = Instruction 0x17 "RLA" const4 $ f $ (\cpu -> return $ rotateLeftA cpu)
decodeOp 0x18 = Instruction 0x18 "JR r8" const12 $ g $ (\mem -> (\cpu -> jumpRelative cpu mem)) -- TODO this is a nasty hack.
decodeOp 0x19 = Instruction 0x19 "ADD HL, DE" const8 $ f $ (\cpu -> return $ addRegRegWithRegRegWithFlags (H, L) (D, E) cpu)
decodeOp 0x1A = Instruction 0x1A "LD A, (DE)" const8 $ g $ ldRegWithRegRegMem A (D, E)
decodeOp 0x1B = Instruction 0x1B "DEC BC" const8 $ f $ (\cpu -> return $ decrementRegistersWithoutFlags (D, E) cpu)
decodeOp 0x1C = Instruction 0x1C "INC E" const4 $ f $ (\cpu -> return $ incrementRegisterWithFlags E cpu)
decodeOp 0x1D = Instruction 0x1D "DEC E" const4 $ f $ (\cpu -> return $ decrementRegisterWithFlags E cpu)
decodeOp 0x1E = Instruction 0x1E "LD C, d8" const8 $ g $ ldRegWithData C
decodeOp 0x1F = Instruction 0x1F "RRA" const4 $ f $ (\cpu -> return $ rotateRightA cpu)
decodeOp 0x20 = Instruction 0x20 "JR NZ, r8" (\gb -> condTime (not $ getFlag gb zeroFlag) 12 8) $ g $
                (\mem -> (\cpu -> jumpIfRelative (not $ getFlag cpu zeroFlag) cpu mem))
decodeOp 0x21 = Instruction 0x21 "LD HL, d16" const12 $ g $ ldRegRegWithData (H, L)
decodeOp 0x22 = Instruction 0x22 "LD (HL+), A" const8 $ g $ (\mem -> (\cpu -> do { ldedGB <- ldMemRegRegWithReg (H, L) A mem cpu -- NOTE this is fishy.
                                                                                 ; return $ incrementRegistersWithoutFlags (H, L) cpu }))
decodeOp 0x23 = Instruction 0x23 "INC HL" const8 $ f $ (\cpu -> return $ incrementRegistersWithoutFlags (H, L) cpu)
decodeOp 0x24 = Instruction 0x24 "INC H" const4 $ f $ (\cpu -> return $ incrementRegisterWithFlags H cpu)
decodeOp 0x25 = Instruction 0x25 "DEC H" const4 $ f $ (\cpu -> return $ decrementRegisterWithFlags H cpu)
decodeOp 0x26 = Instruction 0x26 "LD H, d8" const8 $ g $ ldRegWithData H
decodeOp 0x27 = Instruction 0x27 "DAA" const4 $ f $ (\cpu -> return $ daa cpu)
decodeOp 0x28 = Instruction 0x28 "JR Z,r8" (\gb -> condTime (getFlag gb zeroFlag) 12 8) $ g $
                (\mem -> (\cpu ->  jumpIfRelative (getFlag cpu zeroFlag) cpu mem))
decodeOp 0x29 = Instruction 0x29 "ADD HL, HL" const8 $ f $ (\cpu -> return $ addRegRegWithRegRegWithFlags (H, L) (H, L) cpu)
decodeOp 0x2A = Instruction 0x2A "LD A, (HL+)" const8 $ g $ (\mem -> (\cpu -> do { ldedGB <- ldRegWithRegRegMem A (H,L) mem cpu -- NOTE this is fishy
                                                                                 ; return $ incrementRegistersWithoutFlags (H, L) cpu }))
decodeOp 0x2B = Instruction 0x2B "DEC HL" const8 $ f $ (\cpu -> return $ decrementRegistersWithoutFlags (H, L) cpu)
decodeOp 0x2C = Instruction 0x2C "INC L" const4 $ f $ (\cpu -> return $ incrementRegisterWithFlags L cpu)
decodeOp 0x2D = Instruction 0x2D "DEC L" const4 $ f $ (\cpu -> return $ decrementRegisterWithFlags L cpu)
decodeOp 0x2E = Instruction 0x2E "LD L, d8" const8 $ g $ ldRegWithData L
decodeOp 0x2F = Instruction 0x2F "CPL" const4 $ f $ (\cpu -> return $ cpl cpu)
decodeOp 0x30 = Instruction 0x30 "JR NC, r8" (\gb -> condTime (not $ getFlag gb carryFlag) 12 8) $ g $
                (\mem -> (\cpu -> jumpIfRelative (not $ getFlag cpu carryFlag) cpu mem))
decodeOp 0x31 = Instruction 0x31 "LD SP, d16" const12 $ g $ ldRegRegWithData (SHI, PLO)
decodeOp 0x32 = Instruction 0x32 "LD (HL-), A" const8 $ g $ (\mem -> (\cpu -> do { ldedGB <- ldMemRegRegWithReg (H, L) A mem cpu -- NOTE this is fishy.
                                                                                 ; return $ decrementRegistersWithoutFlags (H, L) cpu }))
decodeOp 0x33 = Instruction 0x33 "INC SP" const8 $ f $ (\cpu -> return $ incrementRegistersWithoutFlags (SHI, PLO) cpu)
decodeOp 0x34 = Instruction 0x34 "INC (HL)" const12 $ j $ incrementMemoryRegReg (H, L)
decodeOp 0x35 = Instruction 0x35 "DEC (HL)" const12 $ j $ decrementMemoryRegReg (H, L)
decodeOp 0x36 = Instruction 0x36 "LD (HL), d8" const12 $ j $ ldMemRegRegWithData (H, L)
decodeOp 0x37 = Instruction 0x37 "SCF" const4 $ f $ (\cpu -> return $ scf cpu)
decodeOp 0x38 = Instruction 0x38 "JR C, r8" (\gb -> condTime (getFlag gb carryFlag) 12 8) $ g $
                (\mem -> (\cpu -> jumpIfRelative (getFlag cpu carryFlag) cpu mem))
decodeOp 0x39 = Instruction 0x39 "ADD HL, SP" const8 $ f $ (\cpu -> return $ addRegRegWithRegRegWithFlags (H, L) (SHI, PLO) cpu)
decodeOp 0x3A = Instruction 0x3A "LD A, (HL-)" const8 $ g $ (\mem -> (\cpu -> do { ldedGB <- ldRegWithRegRegMem A (H, L) mem cpu -- NOTE this is fishy.
                                                                                 ; return $ decrementRegistersWithoutFlags (H, L) cpu }))
decodeOp 0x3B = Instruction 0x3B "DEC SP" const8 $ f $ fixCpu $ decrementRegistersWithoutFlags (SHI, PLO)
decodeOp 0x3C = Instruction 0x3C "INC A" const4 $ f $ fixCpu $ incrementRegisterWithFlags A
decodeOp 0x3D = Instruction 0x3D "DEC A" const4 $ f $ fixCpu $ decrementRegisterWithFlags A
decodeOp 0x3E = Instruction 0x3E "LD A, d8" const8 $ g $ ldRegWithData A
decodeOp 0x3F = Instruction 0x3F "CCF" const4 $ f $ fixCpu ccf
decodeOp 0x40 = Instruction 0x40 "LD B, B" const4 $ f $ fixCpu id
decodeOp 0x41 = Instruction 0x41 "LD B, C" const4 $ f $ fixCpu $ ldRegWithReg B C
decodeOp 0x42 = Instruction 0x42 "LD B, D" const4 $ f $ fixCpu $ ldRegWithReg B D
decodeOp 0x43 = Instruction 0x43 "LD B, E" const4 $ f $ fixCpu $ ldRegWithReg B E
decodeOp 0x44 = Instruction 0x44 "LD B, H" const4 $ f $ fixCpu $ ldRegWithReg B H
decodeOp 0x45 = Instruction 0x45 "LD B, L" const4 $ f $ fixCpu $ ldRegWithReg B L
decodeOp 0x46 = Instruction 0x46 "LD B, (HL)" const8 $ g $ ldRegWithRegRegMem B (H, L)
decodeOp 0x47 = Instruction 0x46 "LD B, A" const4 $ f $ fixCpu $ ldRegWithReg B A
decodeOp 0x48 = Instruction 0x48 "LD C, B" const4 $ f $ fixCpu $ ldRegWithReg C B
decodeOp 0x49 = Instruction 0x49 "LD C, C" const4 $ f $ fixCpu id
decodeOp 0x4A = Instruction 0x4A "LD C, D" const4 $ f $ fixCpu $ ldRegWithReg C D
decodeOp 0x4B = Instruction 0x4B "LD C, E" const4 $ f $ fixCpu $ ldRegWithReg C E
decodeOp 0x4C = Instruction 0x4C "LD C, H" const4 $ f $ fixCpu $ ldRegWithReg C H
decodeOp 0x4D = Instruction 0x4D "LD C, L" const4 $ f $ fixCpu $ ldRegWithReg C L
decodeOp 0x4E = Instruction 0x4E "LD C, (HL)" const8 $ g $ ldRegWithRegRegMem C (H, L)
decodeOp 0x4F = Instruction 0x4F "LD C, A" const4 $ f $ fixCpu $ ldRegWithReg C A
decodeOp 0x50 = Instruction 0x50 "LD D, B" const4 $ f $ fixCpu $ ldRegWithReg D B
decodeOp 0x51 = Instruction 0x51 "LD D, C" const4 $ f $ fixCpu $ ldRegWithReg D C
decodeOp 0x52 = Instruction 0x52 "LD D, D" const4 $ f $ fixCpu id
decodeOp 0x53 = Instruction 0x53 "LD D, E" const4 $ f $ fixCpu $ ldRegWithReg D E
decodeOp 0x54 = Instruction 0x54 "LD D, H" const4 $ f $ fixCpu $ ldRegWithReg D H
decodeOp 0x55 = Instruction 0x55 "LD D, L" const4 $ f $ fixCpu $ ldRegWithReg D L
decodeOp 0x56 = Instruction 0x56 "LD D, (HL)" const8 $ g $ ldRegWithRegRegMem D (H, L)
decodeOp 0x57 = Instruction 0x57 "LD D, A" const4 $ f $ fixCpu $ ldRegWithReg D A
decodeOp 0x58 = Instruction 0x58 "LD E, B" const4 $ f $ fixCpu $ ldRegWithReg E B
decodeOp 0x59 = Instruction 0x59 "LD E, C" const4 $ f $ fixCpu $ ldRegWithReg E C
decodeOp 0x5A = Instruction 0x5A "LD E, D" const4 $ f $ fixCpu $ ldRegWithReg E D
decodeOp 0x5B = Instruction 0x5B "LD E, E" const4 $ f $ fixCpu id
decodeOp 0x5C = Instruction 0x5C "LD E, H" const4 $ f $ fixCpu $ ldRegWithReg E H
decodeOp 0x5D = Instruction 0x5D "LD E, L" const4 $ f $ fixCpu $ ldRegWithReg E L
decodeOp 0x5E = Instruction 0x5E "LD E, (HL)" const8 $ g $ ldRegWithRegRegMem E (H, L)
decodeOp 0x5F = Instruction 0x5F "LD E, A" const4 $ f $ fixCpu $ ldRegWithReg E A
decodeOp 0x60 = Instruction 0x60 "LD H, B" const4 $ f $ fixCpu $ ldRegWithReg H B
decodeOp 0x61 = Instruction 0x61 "LD H, C" const4 $ f $ fixCpu $ ldRegWithReg H C
decodeOp 0x62 = Instruction 0x62 "LD H, D" const4 $ f $ fixCpu $ ldRegWithReg H D
decodeOp 0x63 = Instruction 0x63 "LD H, E" const4 $ f $ fixCpu $ ldRegWithReg H E
decodeOp 0x64 = Instruction 0x64 "LD H, H" const4 $ f $ fixCpu id
decodeOp 0x65 = Instruction 0x65 "LD H, L" const4 $ f $ fixCpu $ ldRegWithReg H L
decodeOp 0x66 = Instruction 0x66 "LD H, (HL)" const8 $ g $ ldRegWithRegRegMem H (H, L)
decodeOp 0x67 = Instruction 0x67 "LD H, A" const4 $ f $ fixCpu $ ldRegWithReg H A
decodeOp 0x68 = Instruction 0x68 "LD L, B" const4 $ f $ fixCpu $ ldRegWithReg L B
decodeOp 0x69 = Instruction 0x69 "LD L, C" const4 $ f $ fixCpu $ ldRegWithReg L C
decodeOp 0x6A = Instruction 0x6A "LD L, D" const4 $ f $ fixCpu $ ldRegWithReg L D
decodeOp 0x6B = Instruction 0x6B "LD L, E" const4 $ f $ fixCpu $ ldRegWithReg L E
decodeOp 0x6C = Instruction 0x6C "LD L, H" const4 $ f $ fixCpu $ ldRegWithReg L H
decodeOp 0x6D = Instruction 0x6D "LD L, L" const4 $ f $ fixCpu id
decodeOp 0x6E = Instruction 0x6E "LD L, (HL)" const8 $ g $ ldRegWithRegRegMem L (H, L)
decodeOp 0x6F = Instruction 0x6F "LD L, A" const4 $ f $ fixCpu $ ldRegWithReg L A
decodeOp 0x70 = Instruction 0x70 "LD (HL) B" const8 $ h $ ldMemRegRegWithReg (H, L) B
decodeOp 0x71 = Instruction 0x71 "LD (HL) C" const8 $ h $ ldMemRegRegWithReg (H, L) C
decodeOp 0x72 = Instruction 0x72 "LD (HL) D" const8 $ h $ ldMemRegRegWithReg (H, L) D
decodeOp 0x73 = Instruction 0x73 "LD (HL) E" const8 $ h $ ldMemRegRegWithReg (H, L) E
decodeOp 0x74 = Instruction 0x74 "LD (HL) H" const8 $ h $ ldMemRegRegWithReg (H, L) H
decodeOp 0x75 = Instruction 0x75 "LD (HL) L" const8 $ h $ ldMemRegRegWithReg (H, L) L
--TODO 0x76 "HALT"
decodeOp 0x77 = Instruction 0x77 "LD (HL) A" const8 $ h $ ldMemRegRegWithReg (H, L) A
decodeOp 0x78 = Instruction 0x78 "LD A B" const4 $ f $ fixCpu $ ldRegWithReg A B
decodeOp 0x79 = Instruction 0x79 "LD A C" const4 $ f $ fixCpu $ ldRegWithReg A C
decodeOp 0x7A = Instruction 0x7A "LD A D" const4 $ f $ fixCpu $ ldRegWithReg A D
decodeOp 0x7B = Instruction 0x7B "LD A E" const4 $ f $ fixCpu $ ldRegWithReg A E
decodeOp 0x7C = Instruction 0x7C "LD A H" const4 $ f $ fixCpu $ ldRegWithReg A H
decodeOp 0x7D = Instruction 0x7D "LD A L" const4 $ f $ fixCpu $ ldRegWithReg A L
decodeOp 0x7E = Instruction 0x7E "LD A (HL)" const8 $ g $ ldRegWithRegRegMem A (H, L)
decodeOp 0x7F = Instruction 0x7F "LD A A" const4 $ f $ fixCpu id
decodeOp 0x80 = Instruction 0x80 "ADD A, B" const4 $ f $ fixCpu $ addRegWithRegWithFlags A B
decodeOp 0x81 = Instruction 0x81 "ADD A, C" const4 $ f $ fixCpu $ addRegWithRegWithFlags A C
decodeOp 0x82 = Instruction 0x82 "ADD A, D" const4 $ f $ fixCpu $ addRegWithRegWithFlags A D
decodeOp 0x83 = Instruction 0x83 "ADD A, E" const4 $ f $ fixCpu $ addRegWithRegWithFlags A E
decodeOp 0x84 = Instruction 0x84 "ADD A, H" const4 $ f $ fixCpu $ addRegWithRegWithFlags A H
decodeOp 0x85 = Instruction 0x85 "ADD A, L" const4 $ f $ fixCpu $ addRegWithRegWithFlags A L
decodeOp 0x86 = Instruction 0x86 "ADD A, (HL)" const8 $ g $ addRegWithRegRegMemWithFlags A (H, L)
decodeOp 0x87 = Instruction 0x87 "ADD A, A" const4 $ f $ fixCpu $ addRegWithRegWithFlags A A
decodeOp 0x88 = Instruction 0x88 "ADC A, B" const4 $ f $ fixCpu $ addRegWithRegWithFlagsPlusC A B
decodeOp 0x89 = Instruction 0x89 "ADC A, C" const4 $ f $ fixCpu $ addRegWithRegWithFlagsPlusC A C
decodeOp 0x8A = Instruction 0x8A "ADC A, D" const4 $ f $ fixCpu $ addRegWithRegWithFlagsPlusC A D
decodeOp 0x8B = Instruction 0x8B "ADC A, E" const4 $ f $ fixCpu $ addRegWithRegWithFlagsPlusC A E
decodeOp 0x8C = Instruction 0x8C "ADC A, H" const4 $ f $ fixCpu $ addRegWithRegWithFlagsPlusC A H
decodeOp 0x8D = Instruction 0x8D "ADC A, L" const4 $ f $ fixCpu $ addRegWithRegWithFlagsPlusC A B
decodeOp 0x8E = Instruction 0x8E "ADC A, (HL)" const8 $ g $ addRegWithRegRegMemWithFlagsPlusC A (H, L)
decodeOp 0x8F = Instruction 0x8F "ADC A, A" const4 $ f $ fixCpu $ addRegWithRegWithFlagsPlusC A A
decodeOp 0x90 = Instruction 0x90 "SUB B" const4 $ f $ fixCpu $ subRegWithRegWithFlags A B
decodeOp 0x91 = Instruction 0x91 "SUB C" const4 $ f $ fixCpu $ subRegWithRegWithFlags A C
decodeOp 0x92 = Instruction 0x92 "SUB D" const4 $ f $ fixCpu $ subRegWithRegWithFlags A D
decodeOp 0x93 = Instruction 0x93 "SUB E" const4 $ f $ fixCpu $ subRegWithRegWithFlags A E
decodeOp 0x94 = Instruction 0x94 "SUB H" const4 $ f $ fixCpu $ subRegWithRegWithFlags A H
decodeOp 0x95 = Instruction 0x95 "SUB L" const4 $ f $ fixCpu $ subRegWithRegWithFlags A L
--TODO 0x96
decodeOp 0x97 = Instruction 0x97 "SUB A" const4 $ f $ fixCpu $ subRegWithRegWithFlags A A
decodeOp 0x98 = Instruction 0x98 "SBC A, B" const4 $ f $ fixCpu $ subRegWithRegWithFlagsPlusC A B
decodeOp 0x99 = Instruction 0x99 "SBC A, C" const4 $ f $ fixCpu $ subRegWithRegWithFlagsPlusC A C
decodeOp 0x9A = Instruction 0x9A "SBC A, D" const4 $ f $ fixCpu $ subRegWithRegWithFlagsPlusC A D
decodeOp 0x9B = Instruction 0x9B "SBC A, E" const4 $ f $ fixCpu $ subRegWithRegWithFlagsPlusC A E
decodeOp 0x9C = Instruction 0x9C "SBC A, H" const4 $ f $ fixCpu $ subRegWithRegWithFlagsPlusC A H
decodeOp 0x9D = Instruction 0x9D "SBC A, L" const4 $ f $ fixCpu $ subRegWithRegWithFlagsPlusC A L
--TODO 0x9E
decodeOp 0x9F = Instruction 0x9F "SBC A, A" const8 $ f $ fixCpu $ subRegWithRegWithFlagsPlusC A A
decodeOp 0xA0 = Instruction 0xA0 "AND B" const4 $ f $ fixCpu $ andReg B
decodeOp 0xA1 = Instruction 0xA1 "AND C" const4 $ f $ fixCpu $ andReg C
decodeOp 0xA2 = Instruction 0xA2 "AND D" const4 $ f $ fixCpu $ andReg D
decodeOp 0xA3 = Instruction 0xA3 "AND E" const4 $ f $ fixCpu $ andReg E
decodeOp 0xA4 = Instruction 0xA4 "AND H" const4 $ f $ fixCpu $ andReg H
decodeOp 0xA5 = Instruction 0xA5 "AND L" const4 $ f $ fixCpu $ andReg L
--TODO 0xA6
decodeOp 0xA7 = Instruction 0xA7 "AND A" const4 $ f $ fixCpu $ andReg A
decodeOp 0xA8 = Instruction 0xA8 "XOR B" const4 $ f $ fixCpu $ xorReg B
decodeOp 0xA9 = Instruction 0xA9 "XOR C" const4 $ f $ fixCpu $ xorReg C
decodeOp 0xAA = Instruction 0xAA "XOR D" const4 $ f $ fixCpu $ xorReg D
decodeOp 0xAB = Instruction 0xAB "XOR E" const4 $ f $ fixCpu $ xorReg E
decodeOp 0xAC = Instruction 0xAC "XOR H" const4 $ f $ fixCpu $ xorReg H
decodeOp 0xAD = Instruction 0xAD "XOR L" const4 $ f $ fixCpu $ xorReg L
--TODO 0xAE
decodeOp 0xAF = Instruction 0xAF "XOR A" const4 $ f $ fixCpu $ xorReg A
decodeOp 0xB0 = Instruction 0xB0 "OR B" const4 $ f $ fixCpu $ orReg B
decodeOp 0xB1 = Instruction 0xB1 "OR C" const4 $ f $ fixCpu $ orReg C
decodeOp 0xB2 = Instruction 0xB2 "OR D" const4 $ f $ fixCpu $ orReg D
decodeOp 0xB3 = Instruction 0xB3 "OR E" const4 $ f $ fixCpu $ orReg E
decodeOp 0xB4 = Instruction 0xB4 "OR H" const4 $ f $ fixCpu $ orReg H
decodeOp 0xB5 = Instruction 0xB5 "OR L" const4 $ f $ fixCpu $ orReg L
--TODO 0xB6 - 0xC0
decodeOp 0xB7 = Instruction 0xB7 "OR A" const4 $ f $ fixCpu $ orReg A
decodeOp 0xB8 = Instruction 0xB8 "CP B" const4 $ f $ fixCpu $ cpReg B
decodeOp 0xB9 = Instruction 0xB9 "CP C" const4 $ f $ fixCpu $ cpReg C
decodeOp 0xBA = Instruction 0xBA "CP D" const4 $ f $ fixCpu $ cpReg D
decodeOp 0xBB = Instruction 0xBB "CP E" const4 $ f $ fixCpu $ cpReg E
decodeOp 0xBC = Instruction 0xBC "CP H" const4 $ f $ fixCpu $ cpReg H
decodeOp 0xBD = Instruction 0xBD "CP L" const4 $ f $ fixCpu $ cpReg L
--TODO 0xBE
decodeOp 0xBF = Instruction 0xBF "CP A" const4 $ f $ fixCpu $ cpReg A
--TODO 0xC0
decodeOp 0xC1 = Instruction 0xC1 "POP BC" const12 $ g $ pop (B, C)
--TODO 0xC2 - 0xC4
decodeOp 0xC5 = Instruction 0xC5 "PUSH BC" const16 $ g $ (\mem -> (\cpu -> push (getRegisters (B, C) cpu) mem cpu))
--TODO 0xC6 - 0xC8
decodeOp 0xC9 = Instruction 0xC9 "RET" const16 $ g $ (\mem -> (\cpu -> ret cpu mem))
--TODO 0xCA
decodeOp 0xCB = Instruction 0xCB "[CB Instruction]" const8 $ g $
                (\mem -> (\cpu -> do { cb <- fetchCb (incrementRegistersWithoutFlags (PHI, CLO) cpu) mem
                                     ; return $ decodeCb cb (incrementRegistersWithoutFlags (PHI,CLO) cpu)}))
--TODO 0xCC
decodeOp 0xCD = Instruction 0xCD "CALL a8" const24 $ g $ (\mem -> (\cpu -> call cpu mem))
--TODO 0xCE - 0xDF
decodeOp 0xE0 = Instruction 0xE0 "LDH (a8), A" const12 $ h $ (\mem -> (\cpu -> ldFFAndMemOffsetWithA cpu mem))
--TODO 0xE1
decodeOp 0xE2 = Instruction 0xE2 "LD (C), A" const8 $ h $ ldFFRegAddrReg C A
--TODO 0xE3 - E9
decodeOp 0xEA = Instruction 0xEA "LD (a16), A" const16 $ h $ ldMemDataWithReg A
--TODO 0xEB - 0xEF
decodeOp 0xF0 = Instruction 0xF0 "LDH A, (a8)" const12 $ g $ (\mem -> (\cpu -> ldAWithFFAndMemOffset cpu mem))
--TODO 0xF1 - 0xFD
decodeOp 0xFE = Instruction 0xFE "CP d8" const8 $ g $ (\mem -> (\cpu -> cp8 cpu mem))
--TODO 0xFF

fetchCb :: Cpu -> Memory -> IO Word8
fetchCb cpu mem = getMemory (getRegisters (PHI, CLO) cpu) mem

--TODO alot of cb instructions need to be implemented
decodeCb :: Word8 -> Cpu -> Cpu
decodeCb 0x11 = rotateLeft C
decodeCb 0x7C = testBitReg H 7

-- | Handles any dispatchable interrupts the gameboy has.
  -- This function also hanles all of the weird halting behavior.
  -- TODO verify that HALT behaves properly.
  -- TODO !!! EXTREME BUG !!! This crashes when getDispatchableInterrupts is [].
  -- TODO fix this.
--handleInterrupts :: Cpu -> IO Cpu
--handleInterrupts gb
--  | gb ^. ime = do { is <- getDispatchableInterrupts (gb ^. memory)
--                   ; let int = getHighestPriorityInterrupt is
--                     in if gb ^. halt
--                     then dispatchInterrupt int       .
--                          increaseClock 24            .
--                          (\gb -> gb & halt .~ False) .
--                          (\gb -> gb & ime  .~ False) $ gb
--                     else dispatchInterrupt int       .
--                          increaseClock 20            .
--                          (\gb -> gb & ime  .~ False) $ gb }
--  | otherwise = if gb ^. halt
--                then return $ increaseClock const4 . (\gb -> gb & halt .~ False) $ gb
--                else return gb

--fetchNextInstr :: Cpu -> IO Instruction
--fetchNextInstr gb = getMemory (getRegisters (PHI, CLO) gb) gb >>= \x -> return $ decodeOp x

--cacheNextInstr :: Cpu -> IO Cpu
--cacheNextInstr gb = do { inst <- fetchNextInstr gb
--                       ; let t = (inst ^. time) gb
--                             o = (inst ^. operation)
--                         in  return $ (gb & opWait .~ t) & currOp .~ o }

--advanceCycle :: Cpu -> IO Cpu
--advanceCycle gb
--  | gb ^. opWait == 0 = do { evalGb <- (gb ^. currOp) gb
--                           ; intGb  <- handleInterrupts evalGb --TODO interrupts are still based on my old clocking system.
--                           ; cacheNextInstr gb }
--  | otherwise = return $ (increaseClock 1 gb) & opWait %~ (\i -> i - 1) --TODO handle other hardware between machine cycles.
