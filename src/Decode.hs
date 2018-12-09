{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Decode (module Decode) where

import Core
import Interrupts
import Data.Word
import Control.Lens
import Cpu

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

decodeOp :: Word8 -> Instruction
decodeOp 0x00 = Instruction 0x00 "NOP" const4 $ (\gb -> return gb)
decodeOp 0x01 = Instruction 0x01 "LD BC, d16" const12 $ ldRegRegWithData (B, C)
decodeOp 0x02 = Instruction 0x02 "LD (BC), A" const8 $ ldMemRegRegWithReg (B, C) A
decodeOp 0x03 = Instruction 0x03 "INC BC" const8 $ fixGB $ incrementRegistersWithoutFlags (B, C)
decodeOp 0x04 = Instruction 0x04 "INC B" const4 $ fixGB $ incrementRegisterWithFlags B
decodeOp 0x05 = Instruction 0x05 "DEC B" const4 $ fixGB $ decrementRegisterWithFlags B
decodeOp 0x06 = Instruction 0x06 "LD B, d8" const8 $ ldRegWithData B
decodeOp 0x07 = Instruction 0x07 "RLCA" const4 $ fixGB rotateLeftACarry
decodeOp 0x08 = Instruction 0x08 "LD (a16), SP" const20 $ ldMemDataWithRegReg (SHI, PLO)
decodeOp 0x09 = Instruction 0x09 "ADD HL, BC" const8 $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (B, C)
decodeOp 0x0A = Instruction 0x0A "LD A, (BC)" const8 $ ldRegWithRegRegMem A (B, C)
decodeOp 0x0B = Instruction 0x0B "DEC BC" const8 $ fixGB $ decrementRegistersWithoutFlags (B, C)
decodeOp 0x0C = Instruction 0x0C "INC C" const4 $ fixGB $ incrementRegisterWithFlags C
decodeOp 0x0D = Instruction 0x0D "DEC C" const4 $ fixGB $ decrementRegisterWithFlags C
decodeOp 0x0E = Instruction 0x0E "LD C, d8" const8 $ ldRegWithData C
decodeOp 0x0F = Instruction 0x0F "RRCA" const4 $ fixGB rotateRightACarry
--TODO 0x10 "STOP 0"
decodeOp 0x11 = Instruction 0x11 "LD DE, d16" const12 $ ldRegRegWithData (D, E)
decodeOp 0x12 = Instruction 0x12 "LD (DE), A" const8 $ ldMemRegRegWithReg (D, E) A
decodeOp 0x13 = Instruction 0x13 "INC DE" const8 $ fixGB$ incrementRegistersWithoutFlags (D, E)
decodeOp 0x14 = Instruction 0x14 "INC D" const4 $ fixGB $ incrementRegisterWithFlags D
decodeOp 0x15 = Instruction 0x15 "DEC D" const4 $ fixGB $ decrementRegisterWithFlags D
decodeOp 0x16 = Instruction 0x16 "LD D, d8" const8 $ ldRegWithData D
decodeOp 0x17 = Instruction 0x17 "RLA" const4 $ fixGB rotateLeftA
decodeOp 0x18 = Instruction 0x18 "JR r8" const12 $ jumpRelative
decodeOp 0x19 = Instruction 0x19 "ADD HL, DE" const8 $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (D, E)
decodeOp 0x1A = Instruction 0x1A "LD A, (DE)" const8 $ ldRegWithRegRegMem A (D, E)
decodeOp 0x1B = Instruction 0x1B "DEC BC" const8 $ fixGB $ decrementRegistersWithoutFlags (D, E)
decodeOp 0x1C = Instruction 0x1C "INC E" const4 $ fixGB $ incrementRegisterWithFlags E
decodeOp 0x1D = Instruction 0x1D "DEC E" const4 $ fixGB $ decrementRegisterWithFlags E
decodeOp 0x1E = Instruction 0x1E "LD C, d8" const8 $ ldRegWithData C
decodeOp 0x1F = Instruction 0x1F "RRA" const4 $ fixGB rotateRightA
decodeOp 0x20 = Instruction 0x20 "JR NZ, r8" (\gb -> condTime (not $ getFlag gb zeroFlag) 12 8) $
                (\gb -> jumpIfRelative (not $ getFlag gb zeroFlag) gb)
decodeOp 0x21 = Instruction 0x21 "LD HL, d16" const12 $ ldRegRegWithData (H, L)
decodeOp 0x22 = Instruction 0x22 "LD (HL+), A" const8 $ (\gb -> do { ldedGB <- ldMemRegRegWithReg (H, L) A gb
                                                                   ; return $ incrementRegistersWithoutFlags (H, L) ldedGB })
decodeOp 0x23 = Instruction 0x23 "INC HL" const8 $ fixGB $ incrementRegistersWithoutFlags (H, L)
decodeOp 0x24 = Instruction 0x24 "INC H" const4 $ fixGB $ incrementRegisterWithFlags H
decodeOp 0x25 = Instruction 0x25 "DEC H" const4 $ fixGB $ decrementRegisterWithFlags H
decodeOp 0x26 = Instruction 0x26 "LD H, d8" const8 $ ldRegWithData H
decodeOp 0x27 = Instruction 0x27 "DAA" const4 $ fixGB daa
decodeOp 0x28 = Instruction 0x28 "JR Z,r8" (\gb -> condTime (getFlag gb zeroFlag) 12 8) $
                (\gb -> jumpIfRelative (getFlag gb zeroFlag) gb)
decodeOp 0x29 = Instruction 0x29 "ADD HL, HL" const8 $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (H, L)
decodeOp 0x2A = Instruction 0x2A "LD A, (HL+)" const8 $ (\gb -> do { ldedGB <- ldRegWithRegRegMem A (H,L) gb
                                                                   ; return $ incrementRegistersWithoutFlags (H, L) ldedGB })
decodeOp 0x2B = Instruction 0x2B "DEC HL" const8 $ fixGB $ decrementRegistersWithoutFlags (H, L)
decodeOp 0x2C = Instruction 0x2C "INC L" const4 $ fixGB $ incrementRegisterWithFlags L
decodeOp 0x2D = Instruction 0x2D "DEC L" const4 $ fixGB $ decrementRegisterWithFlags L
decodeOp 0x2E = Instruction 0x2E "LD L, d8" const8 $ ldRegWithData L
decodeOp 0x2F = Instruction 0x2F "CPL" const4 $ fixGB cpl
decodeOp 0x30 = Instruction 0x30 "JR NC, r8" (\gb -> condTime (not $ getFlag gb carryFlag) 12 8) $
                (\gb -> jumpIfRelative (not $ getFlag gb carryFlag) gb)
decodeOp 0x31 = Instruction 0x31 "LD SP, d16" const12 $ ldRegRegWithData (SHI, PLO)
decodeOp 0x32 = Instruction 0x32 "LD (HL-), A" const8 $ (\gb -> do { ldedGB <- ldMemRegRegWithReg (H, L) A gb
                                                                    ; return $ decrementRegistersWithoutFlags (H, L) ldedGB })
decodeOp 0x33 = Instruction 0x33 "INC SP" const8 $ fixGB $ incrementRegistersWithoutFlags (SHI, PLO)
decodeOp 0x34 = Instruction 0x34 "INC (HL)" const12 $ incrementMemoryRegReg (H, L)
decodeOp 0x35 = Instruction 0x35 "DEC (HL)" const12 $ decrementMemoryRegReg (H, L)
decodeOp 0x36 = Instruction 0x36 "LD (HL), d8" const12 $ ldMemRegRegWithData (H, L)
decodeOp 0x37 = Instruction 0x37 "SCF" const4 $ fixGB scf
decodeOp 0x38 = Instruction 0x38 "JR C, r8" (\gb -> condTime (getFlag gb carryFlag) 12 8) $
                (\gb -> jumpIfRelative (getFlag gb carryFlag) gb)
decodeOp 0x39 = Instruction 0x39 "ADD HL, SP" const8 $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (SHI, PLO)
decodeOp 0x3A = Instruction 0x3A "LD A, (HL-)" const8 $ (\gb -> do { ldedGB <- ldRegWithRegRegMem A (H, L) gb
                                                                   ; return $ decrementRegistersWithoutFlags (H, L) ldedGB })
decodeOp 0x3B = Instruction 0x3B "DEC SP" const8 $ fixGB $ decrementRegistersWithoutFlags (SHI, PLO)
decodeOp 0x3C = Instruction 0x3C "INC A" const4 $ fixGB $ incrementRegisterWithFlags A
decodeOp 0x3D = Instruction 0x3D "DEC A" const4 $ fixGB $ decrementRegisterWithFlags A
decodeOp 0x3E = Instruction 0x3E "LD A, d8" const8 $ ldRegWithData A
decodeOp 0x3F = Instruction 0x3F "CCF" const4 $ fixGB ccf
decodeOp 0x40 = Instruction 0x40 "LD B, B" const4 $ fixGB id
decodeOp 0x41 = Instruction 0x41 "LD B, C" const4 $ fixGB $ ldRegWithReg B C
decodeOp 0x42 = Instruction 0x42 "LD B, D" const4 $ fixGB $ ldRegWithReg B D
decodeOp 0x43 = Instruction 0x43 "LD B, E" const4 $ fixGB $ ldRegWithReg B E
decodeOp 0x44 = Instruction 0x44 "LD B, H" const4 $ fixGB $ ldRegWithReg B H
decodeOp 0x45 = Instruction 0x45 "LD B, L" const4 $ fixGB $ ldRegWithReg B L
decodeOp 0x46 = Instruction 0x46 "LD B, (HL)" const8 $ ldRegWithRegRegMem B (H, L)
decodeOp 0x47 = Instruction 0x46 "LD B, A" const4 $ fixGB $ ldRegWithReg B A
decodeOp 0x48 = Instruction 0x48 "LD C, B" const4 $ fixGB $ ldRegWithReg C B
decodeOp 0x49 = Instruction 0x49 "LD C, C" const4 $ fixGB id
decodeOp 0x4A = Instruction 0x4A "LD C, D" const4 $ fixGB $ ldRegWithReg C D
decodeOp 0x4B = Instruction 0x4B "LD C, E" const4 $ fixGB $ ldRegWithReg C E
decodeOp 0x4C = Instruction 0x4C "LD C, H" const4 $ fixGB $ ldRegWithReg C H
decodeOp 0x4D = Instruction 0x4D "LD C, L" const4 $ fixGB $ ldRegWithReg C L
decodeOp 0x4E = Instruction 0x4E "LD C, (HL)" const8 $ ldRegWithRegRegMem C (H, L)
decodeOp 0x4F = Instruction 0x4F "LD C, A" const4 $ fixGB $ ldRegWithReg C A
decodeOp 0x50 = Instruction 0x50 "LD D, B" const4 $ fixGB $ ldRegWithReg D B
decodeOp 0x51 = Instruction 0x51 "LD D, C" const4 $ fixGB $ ldRegWithReg D C
decodeOp 0x52 = Instruction 0x52 "LD D, D" const4 $ fixGB id
decodeOp 0x53 = Instruction 0x53 "LD D, E" const4 $ fixGB $ ldRegWithReg D E
decodeOp 0x54 = Instruction 0x54 "LD D, H" const4 $ fixGB $ ldRegWithReg D H
decodeOp 0x55 = Instruction 0x55 "LD D, L" const4 $ fixGB $ ldRegWithReg D L
decodeOp 0x56 = Instruction 0x56 "LD D, (HL)" const8 $ ldRegWithRegRegMem D (H, L)
decodeOp 0x57 = Instruction 0x57 "LD D, A" const4 $ fixGB $ ldRegWithReg D A
decodeOp 0x58 = Instruction 0x58 "LD E, B" const4 $ fixGB $ ldRegWithReg E B
decodeOp 0x59 = Instruction 0x59 "LD E, C" const4 $ fixGB $ ldRegWithReg E C
decodeOp 0x5A = Instruction 0x5A "LD E, D" const4 $ fixGB $ ldRegWithReg E D
decodeOp 0x5B = Instruction 0x5B "LD E, E" const4 $ fixGB id
decodeOp 0x5C = Instruction 0x5C "LD E, H" const4 $ fixGB $ ldRegWithReg E H
decodeOp 0x5D = Instruction 0x5D "LD E, L" const4 $ fixGB $ ldRegWithReg E L
decodeOp 0x5E = Instruction 0x5E "LD E, (HL)" const8 $ ldRegWithRegRegMem E (H, L)
decodeOp 0x5F = Instruction 0x5F "LD E, A" const4 $ fixGB $ ldRegWithReg E A
decodeOp 0x60 = Instruction 0x60 "LD H, B" const4 $ fixGB $ ldRegWithReg H B
decodeOp 0x61 = Instruction 0x61 "LD H, C" const4 $ fixGB $ ldRegWithReg H C
decodeOp 0x62 = Instruction 0x62 "LD H, D" const4 $ fixGB $ ldRegWithReg H D
decodeOp 0x63 = Instruction 0x63 "LD H, E" const4 $ fixGB $ ldRegWithReg H E
decodeOp 0x64 = Instruction 0x64 "LD H, H" const4 $ fixGB id
decodeOp 0x65 = Instruction 0x65 "LD H, L" const4 $ fixGB $ ldRegWithReg H L
decodeOp 0x66 = Instruction 0x66 "LD H, (HL)" const8 $ ldRegWithRegRegMem H (H, L)
decodeOp 0x67 = Instruction 0x67 "LD H, A" const4 $ fixGB $ ldRegWithReg H A
decodeOp 0x68 = Instruction 0x68 "LD L, B" const4 $ fixGB $ ldRegWithReg L B
decodeOp 0x69 = Instruction 0x69 "LD L, C" const4 $ fixGB $ ldRegWithReg L C
decodeOp 0x6A = Instruction 0x6A "LD L, D" const4 $ fixGB $ ldRegWithReg L D
decodeOp 0x6B = Instruction 0x6B "LD L, E" const4 $ fixGB $ ldRegWithReg L E
decodeOp 0x6C = Instruction 0x6C "LD L, H" const4 $ fixGB $ ldRegWithReg L H
decodeOp 0x6D = Instruction 0x6D "LD L, L" const4 $ fixGB id
decodeOp 0x6E = Instruction 0x6E "LD L, (HL)" const8 $ ldRegWithRegRegMem L (H, L)
decodeOp 0x6F = Instruction 0x6F "LD L, A" const4 $ fixGB $ ldRegWithReg L A
decodeOp 0x70 = Instruction 0x70 "LD (HL) B" const8 $ ldMemRegRegWithReg (H, L) B
decodeOp 0x71 = Instruction 0x71 "LD (HL) C" const8 $ ldMemRegRegWithReg (H, L) C
decodeOp 0x72 = Instruction 0x72 "LD (HL) D" const8 $ ldMemRegRegWithReg (H, L) D
decodeOp 0x73 = Instruction 0x73 "LD (HL) E" const8 $ ldMemRegRegWithReg (H, L) E
decodeOp 0x74 = Instruction 0x74 "LD (HL) H" const8 $ ldMemRegRegWithReg (H, L) H
decodeOp 0x75 = Instruction 0x75 "LD (HL) L" const8 $ ldMemRegRegWithReg (H, L) L
--TODO 0x76 "HALT"
decodeOp 0x77 = Instruction 0x77 "LD (HL) A" const8 $ ldMemRegRegWithReg (H, L) A
decodeOp 0x78 = Instruction 0x78 "LD A B" const4 $ fixGB $ ldRegWithReg A B
decodeOp 0x79 = Instruction 0x79 "LD A C" const4 $ fixGB $ ldRegWithReg A C
decodeOp 0x7A = Instruction 0x7A "LD A D" const4 $ fixGB $ ldRegWithReg A D
decodeOp 0x7B = Instruction 0x7B "LD A E" const4 $ fixGB $ ldRegWithReg A E
decodeOp 0x7C = Instruction 0x7C "LD A H" const4 $ fixGB $ ldRegWithReg A H
decodeOp 0x7D = Instruction 0x7D "LD A L" const4 $ fixGB $ ldRegWithReg A L
decodeOp 0x7E = Instruction 0x7E "LD A (HL)" const8 $ ldRegWithRegRegMem A (H, L)
decodeOp 0x7F = Instruction 0x7F "LD A A" const4 $ fixGB id
decodeOp 0x80 = Instruction 0x80 "ADD A, B" const4 $ fixGB $ addRegWithRegWithFlags A B
decodeOp 0x81 = Instruction 0x81 "ADD A, C" const4 $ fixGB $ addRegWithRegWithFlags A C
decodeOp 0x82 = Instruction 0x82 "ADD A, D" const4 $ fixGB $ addRegWithRegWithFlags A D
decodeOp 0x83 = Instruction 0x83 "ADD A, E" const4 $ fixGB $ addRegWithRegWithFlags A E
decodeOp 0x84 = Instruction 0x84 "ADD A, H" const4 $ fixGB $ addRegWithRegWithFlags A H
decodeOp 0x85 = Instruction 0x85 "ADD A, L" const4 $ fixGB $ addRegWithRegWithFlags A L
decodeOp 0x86 = Instruction 0x86 "ADD A, (HL)" const8 $ addRegWithRegRegMemWithFlags A (H, L)
decodeOp 0x87 = Instruction 0x87 "ADD A, A" const4 $ fixGB $ addRegWithRegWithFlags A A
decodeOp 0x88 = Instruction 0x88 "ADC A, B" const4 $ fixGB $ addRegWithRegWithFlagsPlusC A B
decodeOp 0x89 = Instruction 0x89 "ADC A, C" const4 $ fixGB $ addRegWithRegWithFlagsPlusC A C
decodeOp 0x8A = Instruction 0x8A "ADC A, D" const4 $ fixGB $ addRegWithRegWithFlagsPlusC A D
decodeOp 0x8B = Instruction 0x8B "ADC A, E" const4 $ fixGB $ addRegWithRegWithFlagsPlusC A E
decodeOp 0x8C = Instruction 0x8C "ADC A, H" const4 $ fixGB $ addRegWithRegWithFlagsPlusC A H
decodeOp 0x8D = Instruction 0x8D "ADC A, L" const4 $ fixGB $ addRegWithRegWithFlagsPlusC A B
decodeOp 0x8E = Instruction 0x8E "ADC A, (HL)" const8 $ addRegWithRegRegMemWithFlagsPlusC A (H, L)
decodeOp 0x8F = Instruction 0x8F "ADC A, A" const4 $ fixGB $ addRegWithRegWithFlagsPlusC A A
decodeOp 0x90 = Instruction 0x90 "SUB B" const4 $ fixGB $ subRegWithRegWithFlags A B
decodeOp 0x91 = Instruction 0x91 "SUB C" const4 $ fixGB $ subRegWithRegWithFlags A C
decodeOp 0x92 = Instruction 0x92 "SUB D" const4 $ fixGB $ subRegWithRegWithFlags A D
decodeOp 0x93 = Instruction 0x93 "SUB E" const4 $ fixGB $ subRegWithRegWithFlags A E
decodeOp 0x94 = Instruction 0x94 "SUB H" const4 $ fixGB $ subRegWithRegWithFlags A H
decodeOp 0x95 = Instruction 0x95 "SUB L" const4 $ fixGB $ subRegWithRegWithFlags A L
--TODO 0x96
decodeOp 0x97 = Instruction 0x97 "SUB A" const4 $ fixGB $ subRegWithRegWithFlags A A
decodeOp 0x98 = Instruction 0x98 "SBC A, B" const4 $ fixGB $ subRegWithRegWithFlagsPlusC A B
decodeOp 0x99 = Instruction 0x99 "SBC A, C" const4 $ fixGB $ subRegWithRegWithFlagsPlusC A C
decodeOp 0x9A = Instruction 0x9A "SBC A, D" const4 $ fixGB $ subRegWithRegWithFlagsPlusC A D
decodeOp 0x9B = Instruction 0x9B "SBC A, E" const4 $ fixGB $ subRegWithRegWithFlagsPlusC A E
decodeOp 0x9C = Instruction 0x9C "SBC A, H" const4 $ fixGB $ subRegWithRegWithFlagsPlusC A H
decodeOp 0x9D = Instruction 0x9D "SBC A, L" const4 $ fixGB $ subRegWithRegWithFlagsPlusC A L
--TODO 0x9E
decodeOp 0x9F = Instruction 0x9F "SBC A, A" const8 $ fixGB $ subRegWithRegWithFlagsPlusC A A
decodeOp 0xA0 = Instruction 0xA0 "AND B" const4 $ fixGB $ andReg B
decodeOp 0xA1 = Instruction 0xA1 "AND C" const4 $ fixGB $ andReg C
decodeOp 0xA2 = Instruction 0xA2 "AND D" const4 $ fixGB $ andReg D
decodeOp 0xA3 = Instruction 0xA3 "AND E" const4 $ fixGB $ andReg E
decodeOp 0xA4 = Instruction 0xA4 "AND H" const4 $ fixGB $ andReg H
decodeOp 0xA5 = Instruction 0xA5 "AND L" const4 $ fixGB $ andReg L
--TODO 0xA6
decodeOp 0xA7 = Instruction 0xA7 "AND A" const4 $ fixGB $ andReg A
decodeOp 0xA8 = Instruction 0xA8 "XOR B" const4 $ fixGB $ xorReg B
decodeOp 0xA9 = Instruction 0xA9 "XOR C" const4 $ fixGB $ xorReg C
decodeOp 0xAA = Instruction 0xAA "XOR D" const4 $ fixGB $ xorReg D
decodeOp 0xAB = Instruction 0xAB "XOR E" const4 $ fixGB $ xorReg E
decodeOp 0xAC = Instruction 0xAC "XOR H" const4 $ fixGB $ xorReg H
decodeOp 0xAD = Instruction 0xAD "XOR L" const4 $ fixGB $ xorReg L
--TODO 0xAE
decodeOp 0xAF = Instruction 0xAF "XOR A" const4 $ fixGB $ xorReg A
decodeOp 0xB0 = Instruction 0xB0 "OR B" const4 $ fixGB $ orReg B
decodeOp 0xB1 = Instruction 0xB1 "OR C" const4 $ fixGB $ orReg C
decodeOp 0xB2 = Instruction 0xB2 "OR D" const4 $ fixGB $ orReg D
decodeOp 0xB3 = Instruction 0xB3 "OR E" const4 $ fixGB $ orReg E
decodeOp 0xB4 = Instruction 0xB4 "OR H" const4 $ fixGB $ orReg H
decodeOp 0xB5 = Instruction 0xB5 "OR L" const4 $ fixGB $ orReg L
--TODO 0xB6 - 0xC0
decodeOp 0xB7 = Instruction 0xB7 "OR A" const4 $ fixGB $ orReg A
decodeOp 0xB8 = Instruction 0xB8 "CP B" const4 $ fixGB $ cpReg B
decodeOp 0xB9 = Instruction 0xB9 "CP C" const4 $ fixGB $ cpReg C
decodeOp 0xBA = Instruction 0xBA "CP D" const4 $ fixGB $ cpReg D
decodeOp 0xBB = Instruction 0xBB "CP E" const4 $ fixGB $ cpReg E
decodeOp 0xBC = Instruction 0xBC "CP H" const4 $ fixGB $ cpReg H
decodeOp 0xBD = Instruction 0xBD "CP L" const4 $ fixGB $ cpReg L
--TODO 0xBE
decodeOp 0xBF = Instruction 0xBF "CP A" const4 $ fixGB $ cpReg A
--TODO 0xC0
decodeOp 0xC1 = Instruction 0xC1 "POP BC" const12 $ pop (B, C)
--TODO 0xC2 - 0xC4
decodeOp 0xC5 = Instruction 0xC5 "PUSH BC" const16 $ (\gb -> push (getRegisters (B, C) gb) gb)
--TODO 0xC6 - 0xC8
decodeOp 0xC9 = Instruction 0xC9 "RET" const16 $ ret
--TODO 0xCA
decodeOp 0xCB = Instruction 0xCB "[CB Instruction]" const8 $
                \gb -> do { cb <- fetchCb $ incrementRegistersWithoutFlags (PHI, CLO) gb
                          ; return $ decodeCb cb (incrementRegistersWithoutFlags (PHI,CLO) gb) }
--TODO 0xCC
decodeOp 0xCD = Instruction 0xCD "CALL a8" const24 $ call
--TODO 0xCE - 0xDF
decodeOp 0xE0 = Instruction 0xE0 "LDH (a8), A" const12 $ ldFFAndMemOffsetWithA
--TODO 0xE1
decodeOp 0xE2 = Instruction 0xE2 "LD (C), A" const8 $ ldFFRegAddrReg C A
--TODO 0xE3 - E9
decodeOp 0xEA = Instruction 0xEA "LD (a16), A" const16 $ ldMemDataWithReg A
--TODO 0xEB - 0xEF
decodeOp 0xF0 = Instruction 0xF0 "LDH A, (a8)" const12 $ ldAWithFFAndMemOffset
--TODO 0xF1 - 0xFD
decodeOp 0xFE = Instruction 0xFE "CP d8" const8 $ cp8
--TODO 0xFF

fetchCb :: Gameboy -> IO Word8
fetchCb gb = getMemory (getRegisters (PHI, CLO) gb) gb

--TODO alot of cb instructions need to be implemented
decodeCb :: Word8 -> (Gameboy -> Gameboy)
decodeCb 0x11 = rotateLeft C
decodeCb 0x7C = testBitReg H 7

-- | Handles any dispatchable interrupts the gameboy has.
  -- This function also hanles all of the weird halting behavior.
  -- TODO verify that HALT behaves properly.
  -- TODO !!! EXTREME BUG !!! This crashes when getDispatchableInterrupts is [].
  -- TODO fix this.
handleInterrupts :: Gameboy -> IO Gameboy
handleInterrupts gb
  | gb ^. ime = do { is <- getDispatchableInterrupts (gb ^. memory)
                   ; let int = getHighestPriorityInterrupt is
                     in if gb ^. halt
                     then dispatchInterrupt int       .
                          increaseClock 24            .
                          (\gb -> gb & halt .~ False) .
                          (\gb -> gb & ime  .~ False) $ gb
                     else dispatchInterrupt int       .
                          increaseClock 20            .
                          (\gb -> gb & ime  .~ False) $ gb }
  | otherwise = if gb ^. halt
                then return $ increaseClock 4 . (\gb -> gb & halt .~ False) $ gb
                else return gb

fetchNextInstr :: Gameboy -> IO Instruction
fetchNextInstr gb = getMemory (getRegisters (PHI, CLO) gb) gb >>= \x -> return $ decodeOp x

cacheNextInstr :: Gameboy -> IO Gameboy
cacheNextInstr gb = do { inst <- fetchNextInstr gb
                       ; let t = (inst ^. time) gb
                             o = (inst ^. operation)
                         in  return $ (gb & opWait .~ t) & currOp .~ o }

advanceCycle :: Gameboy -> IO Gameboy
advanceCycle gb
  | gb ^. opWait == 0 = do { evalGb <- (gb ^. currOp) gb
                           ; intGb  <- handleInterrupts evalGb --TODO interrupts are still based on my old clocking system.
                           ; cacheNextInstr gb }
  | otherwise = return $ (increaseClock 1 gb) & opWait %~ (\i -> i - 1) --TODO handle other hardware between machine cycles.
