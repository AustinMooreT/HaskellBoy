{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Decode (module Decode) where

import Core
import Data.Word
import Cpu

decodeOp :: Word8 -> Instruction
decodeOp 0x00 = Instruction 0x00 "NOP" $ (\gb -> return gb) . increaseClock 4
decodeOp 0x01 = Instruction 0x01 "LD BC, d16" $ ldRegRegWithData (B, C) . increaseClock 12
decodeOp 0x02 = Instruction 0x02 "LD (BC), A" $ ldMemRegRegWithReg (B, C) A . increaseClock 8
decodeOp 0x03 = Instruction 0x03 "INC BC" $ fixGB $ incrementRegistersWithoutFlags (B, C) . increaseClock 8
decodeOp 0x04 = Instruction 0x04 "INC B" $ fixGB $ incrementRegisterWithFlags B . increaseClock 4
decodeOp 0x05 = Instruction 0x05 "DEC B" $ fixGB $ decrementRegisterWithFlags B . increaseClock 4
decodeOp 0x06 = Instruction 0x06 "LD B, d8" $ ldRegWithData B . increaseClock 8
decodeOp 0x07 = Instruction 0x07 "RLCA" $ fixGB rotateLeftACarry . increaseClock 4
decodeOp 0x08 = Instruction 0x08 "LD (a16), SP" $ ldMemDataWithRegReg (SHI, PLO) . increaseClock 20
decodeOp 0x09 = Instruction 0x09 "ADD HL, BC" $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (B, C) . increaseClock 8
decodeOp 0x0A = Instruction 0x0A "LD A, (BC)" $ ldRegWithRegRegMem A (B, C) . increaseClock 8
decodeOp 0x0B = Instruction 0x0B "DEC BC" $ fixGB $ decrementRegistersWithoutFlags (B, C) . increaseClock 8
decodeOp 0x0C = Instruction 0x0C "INC C" $ fixGB $ incrementRegisterWithFlags C . increaseClock 4
decodeOp 0x0D = Instruction 0x0D "DEC C" $ fixGB $ decrementRegisterWithFlags C . increaseClock 4
decodeOp 0x0E = Instruction 0x0E "LD C, d8" $ ldRegWithData C . increaseClock 8
decodeOp 0x0F = Instruction 0x0F "RRCA" $ fixGB rotateRightACarry . increaseClock 4
--TODO 0x10 "STOP 0"
decodeOp 0x11 = Instruction 0x11 "LD DE, d16" $ ldRegRegWithData (D, E) . increaseClock 12
decodeOp 0x12 = Instruction 0x12 "LD (DE), A" $ ldMemRegRegWithReg (D, E) A . increaseClock 8
decodeOp 0x13 = Instruction 0x13 "INC DE" $ fixGB$ incrementRegistersWithoutFlags (D, E) . increaseClock 8
decodeOp 0x14 = Instruction 0x14 "INC D" $ fixGB $ incrementRegisterWithFlags D . increaseClock 4
decodeOp 0x15 = Instruction 0x15 "DEC D" $ fixGB $ decrementRegisterWithFlags D . increaseClock 4
decodeOp 0x16 = Instruction 0x16 "LD D, d8" $ ldRegWithData D . increaseClock 8
decodeOp 0x17 = Instruction 0x17 "RLA" $ fixGB rotateLeftA . increaseClock 4
decodeOp 0x18 = Instruction 0x18 "JR r8" $ jumpRelative . increaseClock 12
decodeOp 0x19 = Instruction 0x19 "ADD HL, DE" $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (D, E) . increaseClock 8
decodeOp 0x1A = Instruction 0x1A "LD A, (DE)" $ ldRegWithRegRegMem A (D, E) . increaseClock 8
decodeOp 0x1B = Instruction 0x1B "DEC BC" $ fixGB $ decrementRegistersWithoutFlags (D, E) . increaseClock 8
decodeOp 0x1C = Instruction 0x1C "INC E" $ fixGB $ incrementRegisterWithFlags E . increaseClock 4
decodeOp 0x1D = Instruction 0x1D "DEC E" $ fixGB $ decrementRegisterWithFlags E . increaseClock 4
decodeOp 0x1E = Instruction 0x1E "LD C, d8" $ ldRegWithData C . increaseClock 8
decodeOp 0x1F = Instruction 0x1F "RRA" $ fixGB rotateRightA . increaseClock 4
decodeOp 0x20 = Instruction 0x20 "JR NZ, r8" $ (\gb -> if (not $ getFlag gb zeroFlag)
                                                 then jumpIfRelative (not $ getFlag gb zeroFlag) . increaseClock 12 $ gb
                                                 else jumpIfRelative (not $ getFlag gb zeroFlag) . increaseClock 8 $ gb)
decodeOp 0x21 = Instruction 0x21 "LD HL, d16" $ ldRegRegWithData (H, L) . increaseClock 12
decodeOp 0x22 = Instruction 0x22 "LD (HL+), A" $ (\gb -> do { ldedGB <- ldMemRegRegWithReg (H, L) A gb
                                                           ; return $ incrementRegistersWithoutFlags (H, L) ldedGB }) . increaseClock 8
decodeOp 0x23 = Instruction 0x23 "INC HL" $ fixGB $ incrementRegistersWithoutFlags (H, L) . increaseClock 8
decodeOp 0x24 = Instruction 0x24 "INC H" $ fixGB $ incrementRegisterWithFlags H . increaseClock 4
decodeOp 0x25 = Instruction 0x25 "DEC H" $ fixGB $ decrementRegisterWithFlags H . increaseClock 4
decodeOp 0x26 = Instruction 0x26 "LD H, d8" $ ldRegWithData H . increaseClock 8
decodeOp 0x27 = Instruction 0x27 "DAA" $ fixGB daa . increaseClock 4
decodeOp 0x28 = Instruction 0x28 "JR Z,r8" $ (\gb -> if (getFlag gb zeroFlag)
                                               then jumpIfRelative (getFlag gb zeroFlag) . increaseClock 12 $ gb
                                               else jumpIfRelative (getFlag gb zeroFlag) . increaseClock 8 $ gb)
decodeOp 0x29 = Instruction 0x29 "ADD HL, HL" $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (H, L) . increaseClock 8
decodeOp 0x2A = Instruction 0x2A "LD A, (HL+)" $ (\gb -> do { ldedGB <- ldRegWithRegRegMem A (H,L) gb
                                                           ; return $ incrementRegistersWithoutFlags (H, L) ldedGB }) . increaseClock 8
decodeOp 0x2B = Instruction 0x2B "DEC HL" $ fixGB $ decrementRegistersWithoutFlags (H, L) . increaseClock 8
decodeOp 0x2C = Instruction 0x2C "INC L" $ fixGB $ incrementRegisterWithFlags L . increaseClock 4
decodeOp 0x2D = Instruction 0x2D "DEC L" $ fixGB $ decrementRegisterWithFlags L . increaseClock 4
decodeOp 0x2E = Instruction 0x2E "LD L, d8" $ ldRegWithData L . increaseClock 8
decodeOp 0x2F = Instruction 0x2F "CPL" $ fixGB cpl . increaseClock 4
decodeOp 0x30 = Instruction 0x30 "JR NC, r8" $ (\gb -> if (not $ getFlag gb carryFlag)
                                                 then jumpIfRelative (not $ getFlag gb carryFlag) . increaseClock 12 $ gb
                                                 else jumpIfRelative (not $ getFlag gb carryFlag) . increaseClock 8 $ gb)
decodeOp 0x31 = Instruction 0x31 "LD SP, d16" $ ldRegRegWithData (SHI, PLO) . increaseClock 12
decodeOp 0x32 = Instruction 0x32 "LD (HL-), A" $ (\gb -> do { ldedGB <- ldMemRegRegWithReg (H, L) A gb
                                                           ; return $ decrementRegistersWithoutFlags (H, L) ldedGB }) . increaseClock 8
decodeOp 0x33 = Instruction 0x33 "INC SP" $ fixGB $ incrementRegistersWithoutFlags (SHI, PLO) . increaseClock 8
decodeOp 0x34 = Instruction 0x34 "INC (HL)" $ incrementMemoryRegReg (H, L) . increaseClock 12
decodeOp 0x35 = Instruction 0x35 "DEC (HL)" $ decrementMemoryRegReg (H, L) . increaseClock 12
decodeOp 0x36 = Instruction 0x36 "LD (HL), d8" $ ldMemRegRegWithData (H, L) . increaseClock 12
decodeOp 0x37 = Instruction 0x37 "SCF" $ fixGB scf . increaseClock 4
decodeOp 0x38 = Instruction 0x38 "JR C, r8" $ (\gb -> if (getFlag gb carryFlag)
                                                then jumpIfRelative (getFlag gb carryFlag) . increaseClock 12 $ gb
                                                else jumpIfRelative (getFlag gb carryFlag) . increaseClock 8 $ gb)
decodeOp 0x39 = Instruction 0x39 "ADD HL, SP" $ fixGB $ addRegRegWithRegRegWithFlags (H, L) (SHI, PLO) . increaseClock 8
decodeOp 0x3A = Instruction 0x3A "LD A, (HL-)" $ (\gb -> do { ldedGB <- ldRegWithRegRegMem A (H, L) gb
                                                           ; return $ decrementRegistersWithoutFlags (H, L) ldedGB }) . increaseClock 8
decodeOp 0x3B = Instruction 0x3B "DEC SP" $ fixGB $ decrementRegistersWithoutFlags (SHI, PLO) . increaseClock 8
decodeOp 0x3C = Instruction 0x3C "INC A" $ fixGB $ incrementRegisterWithFlags A . increaseClock 4
decodeOp 0x3D = Instruction 0x3D "DEC A" $ fixGB $ decrementRegisterWithFlags A . increaseClock 4
decodeOp 0x3E = Instruction 0x3E "LD A, d8" $ ldRegWithData A . increaseClock 8
decodeOp 0x3F = Instruction 0x3F "CCF" $ fixGB ccf . increaseClock 4
decodeOp 0x40 = Instruction 0x40 "LD B, B" $ fixGB id . increaseClock 4
decodeOp 0x41 = Instruction 0x41 "LD B, C" $ fixGB $ ldRegWithReg B C . increaseClock 4
decodeOp 0x42 = Instruction 0x42 "LD B, D" $ fixGB $ ldRegWithReg B D . increaseClock 4
decodeOp 0x43 = Instruction 0x43 "LD B, E" $ fixGB $ ldRegWithReg B E . increaseClock 4
decodeOp 0x44 = Instruction 0x44 "LD B, H" $ fixGB $ ldRegWithReg B H . increaseClock 4
decodeOp 0x45 = Instruction 0x45 "LD B, L" $ fixGB $ ldRegWithReg B L . increaseClock 4
decodeOp 0x46 = Instruction 0x46 "LD B, (HL)" $ ldRegWithRegRegMem B (H, L) . increaseClock 8
decodeOp 0x47 = Instruction 0x46 "LD B, A" $ fixGB $ ldRegWithReg B A . increaseClock 4
decodeOp 0x48 = Instruction 0x48 "LD C, B" $ fixGB $ ldRegWithReg C B . increaseClock 4
decodeOp 0x49 = Instruction 0x49 "LD C, C" $ fixGB id . increaseClock 4
decodeOp 0x4A = Instruction 0x4A "LD C, D" $ fixGB $ ldRegWithReg C D . increaseClock 4
decodeOp 0x4B = Instruction 0x4B "LD C, E" $ fixGB $ ldRegWithReg C E . increaseClock 4
decodeOp 0x4C = Instruction 0x4C "LD C, H" $ fixGB $ ldRegWithReg C H . increaseClock 4
decodeOp 0x4D = Instruction 0x4D "LD C, L" $ fixGB $ ldRegWithReg C L . increaseClock 4
decodeOp 0x4E = Instruction 0x4E "LD C, (HL)" $ ldRegWithRegRegMem C (H, L) . increaseClock 8
decodeOp 0x4F = Instruction 0x4F "LD C, A" $ fixGB $ ldRegWithReg C A . increaseClock 4
decodeOp 0x50 = Instruction 0x50 "LD D, B" $ fixGB $ ldRegWithReg D B . increaseClock 4
decodeOp 0x51 = Instruction 0x51 "LD D, C" $ fixGB $ ldRegWithReg D C . increaseClock 4
decodeOp 0x52 = Instruction 0x52 "LD D, D" $ fixGB id . increaseClock 4
decodeOp 0x53 = Instruction 0x53 "LD D, E" $ fixGB $ ldRegWithReg D E . increaseClock 4
decodeOp 0x54 = Instruction 0x54 "LD D, H" $ fixGB $ ldRegWithReg D H . increaseClock 4
decodeOp 0x55 = Instruction 0x55 "LD D, L" $ fixGB $ ldRegWithReg D L . increaseClock 4
decodeOp 0x56 = Instruction 0x56 "LD D, (HL)" $ ldRegWithRegRegMem D (H, L) . increaseClock 8
decodeOp 0x57 = Instruction 0x57 "LD D, A" $ fixGB $ ldRegWithReg D A . increaseClock 4
decodeOp 0x58 = Instruction 0x58 "LD E, B" $ fixGB $ ldRegWithReg E B . increaseClock 4
decodeOp 0x59 = Instruction 0x59 "LD E, C" $ fixGB $ ldRegWithReg E C . increaseClock 4
decodeOp 0x5A = Instruction 0x5A "LD E, D" $ fixGB $ ldRegWithReg E D . increaseClock 4
decodeOp 0x5B = Instruction 0x5B "LD E, E" $ fixGB id . increaseClock 4
decodeOp 0x5C = Instruction 0x5C "LD E, H" $ fixGB $ ldRegWithReg E H . increaseClock 4
decodeOp 0x5D = Instruction 0x5D "LD E, L" $ fixGB $ ldRegWithReg E L . increaseClock 4
decodeOp 0x5E = Instruction 0x5E "LD E, (HL)" $ ldRegWithRegRegMem E (H, L) . increaseClock 8
decodeOp 0x5F = Instruction 0x5F "LD E, A" $ fixGB $ ldRegWithReg E A . increaseClock 4
decodeOp 0x60 = Instruction 0x60 "LD H, B" $ fixGB $ ldRegWithReg H B . increaseClock 4
decodeOp 0x61 = Instruction 0x61 "LD H, C" $ fixGB $ ldRegWithReg H C . increaseClock 4
decodeOp 0x62 = Instruction 0x62 "LD H, D" $ fixGB $ ldRegWithReg H D . increaseClock 4
decodeOp 0x63 = Instruction 0x63 "LD H, E" $ fixGB $ ldRegWithReg H E . increaseClock 4
decodeOp 0x64 = Instruction 0x64 "LD H, H" $ fixGB id . increaseClock 4
decodeOp 0x65 = Instruction 0x65 "LD H, L" $ fixGB $ ldRegWithReg H L . increaseClock 4
decodeOp 0x66 = Instruction 0x66 "LD H, (HL)" $ ldRegWithRegRegMem H (H, L) . increaseClock 8
decodeOp 0x67 = Instruction 0x67 "LD H, A" $ fixGB $ ldRegWithReg H A . increaseClock 4
decodeOp 0x68 = Instruction 0x68 "LD L, B" $ fixGB $ ldRegWithReg L B . increaseClock 4
decodeOp 0x69 = Instruction 0x69 "LD L, C" $ fixGB $ ldRegWithReg L C . increaseClock 4
decodeOp 0x6A = Instruction 0x6A "LD L, D" $ fixGB $ ldRegWithReg L D . increaseClock 4
decodeOp 0x6B = Instruction 0x6B "LD L, E" $ fixGB $ ldRegWithReg L E . increaseClock 4
decodeOp 0x6C = Instruction 0x6C "LD L, H" $ fixGB $ ldRegWithReg L H . increaseClock 4
decodeOp 0x6D = Instruction 0x6D "LD L, L" $ fixGB id . increaseClock 4
decodeOp 0x6E = Instruction 0x6E "LD L, (HL)" $ ldRegWithRegRegMem L (H, L) . increaseClock 8
decodeOp 0x6F = Instruction 0x6F "LD L, A" $ fixGB $ ldRegWithReg L A . increaseClock 4
decodeOp 0x70 = Instruction 0x70 "LD (HL) B" $ ldMemRegRegWithReg (H, L) B . increaseClock 8
decodeOp 0x71 = Instruction 0x71 "LD (HL) C" $ ldMemRegRegWithReg (H, L) C . increaseClock 8
decodeOp 0x72 = Instruction 0x72 "LD (HL) D" $ ldMemRegRegWithReg (H, L) D . increaseClock 8
decodeOp 0x73 = Instruction 0x73 "LD (HL) E" $ ldMemRegRegWithReg (H, L) E . increaseClock 8
decodeOp 0x74 = Instruction 0x74 "LD (HL) H" $ ldMemRegRegWithReg (H, L) H . increaseClock 8
decodeOp 0x75 = Instruction 0x75 "LD (HL) L" $ ldMemRegRegWithReg (H, L) L . increaseClock 8
--TODO 0x76 "HALT"
decodeOp 0x77 = Instruction 0x77 "LD (HL) A" $ ldMemRegRegWithReg (H, L) A . increaseClock 8
decodeOp 0x78 = Instruction 0x78 "LD A B" $ fixGB $ ldRegWithReg A B . increaseClock 4
decodeOp 0x79 = Instruction 0x79 "LD A C" $ fixGB $ ldRegWithReg A C . increaseClock 4
decodeOp 0x7A = Instruction 0x7A "LD A D" $ fixGB $ ldRegWithReg A D . increaseClock 4
decodeOp 0x7B = Instruction 0x7B "LD A E" $ fixGB $ ldRegWithReg A E . increaseClock 4
decodeOp 0x7C = Instruction 0x7C "LD A H" $ fixGB $ ldRegWithReg A H . increaseClock 4
decodeOp 0x7D = Instruction 0x7D "LD A L" $ fixGB $ ldRegWithReg A L . increaseClock 4
decodeOp 0x7E = Instruction 0x7E "LD A (HL)" $ ldRegWithRegRegMem A (H, L) . increaseClock 8
decodeOp 0x7F = Instruction 0x7F "LD A A" $ fixGB id . increaseClock 4
decodeOp 0x80 = Instruction 0x80 "ADD A, B" $ fixGB $ addRegWithRegWithFlags A B . increaseClock 4
decodeOp 0x81 = Instruction 0x81 "ADD A, C" $ fixGB $ addRegWithRegWithFlags A C . increaseClock 4
decodeOp 0x82 = Instruction 0x82 "ADD A, D" $ fixGB $ addRegWithRegWithFlags A D . increaseClock 4
decodeOp 0x83 = Instruction 0x83 "ADD A, E" $ fixGB $ addRegWithRegWithFlags A E . increaseClock 4
decodeOp 0x84 = Instruction 0x84 "ADD A, H" $ fixGB $ addRegWithRegWithFlags A H . increaseClock 4
decodeOp 0x85 = Instruction 0x85 "ADD A, L" $ fixGB $ addRegWithRegWithFlags A L . increaseClock 4
decodeOp 0x86 = Instruction 0x86 "ADD A, (HL)" $ addRegWithRegRegMemWithFlags A (H, L) . increaseClock 8
decodeOp 0x87 = Instruction 0x87 "ADD A, A" $ fixGB $ addRegWithRegWithFlags A A . increaseClock 4
decodeOp 0x88 = Instruction 0x88 "ADC A, B" $ fixGB $ addRegWithRegWithFlagsPlusC A B . increaseClock 4
decodeOp 0x89 = Instruction 0x89 "ADC A, C" $ fixGB $ addRegWithRegWithFlagsPlusC A C . increaseClock 4
decodeOp 0x8A = Instruction 0x8A "ADC A, D" $ fixGB $ addRegWithRegWithFlagsPlusC A D . increaseClock 4
decodeOp 0x8B = Instruction 0x8B "ADC A, E" $ fixGB $ addRegWithRegWithFlagsPlusC A E . increaseClock 4
decodeOp 0x8C = Instruction 0x8C "ADC A, H" $ fixGB $ addRegWithRegWithFlagsPlusC A H . increaseClock 4
decodeOp 0x8D = Instruction 0x8D "ADC A, L" $ fixGB $ addRegWithRegWithFlagsPlusC A B . increaseClock 4
decodeOp 0x8E = Instruction 0x8E "ADC A, (HL)" $ addRegWithRegRegMemWithFlagsPlusC A (H, L) . increaseClock 8
decodeOp 0x8F = Instruction 0x8F "ADC A, A" $ fixGB $ addRegWithRegWithFlagsPlusC A A . increaseClock 4
decodeOp 0x90 = Instruction 0x90 "SUB B" $ fixGB $ subRegWithRegWithFlags A B . increaseClock 4
decodeOp 0x91 = Instruction 0x91 "SUB C" $ fixGB $ subRegWithRegWithFlags A C . increaseClock 4
decodeOp 0x92 = Instruction 0x92 "SUB D" $ fixGB $ subRegWithRegWithFlags A D . increaseClock 4
decodeOp 0x93 = Instruction 0x93 "SUB E" $ fixGB $ subRegWithRegWithFlags A E . increaseClock 4
decodeOp 0x94 = Instruction 0x94 "SUB H" $ fixGB $ subRegWithRegWithFlags A H . increaseClock 4
decodeOp 0x95 = Instruction 0x95 "SUB L" $ fixGB $ subRegWithRegWithFlags A L . increaseClock 4
--TODO 0x96
decodeOp 0x97 = Instruction 0x97 "SUB A" $ fixGB $ subRegWithRegWithFlags A A . increaseClock 4
decodeOp 0x98 = Instruction 0x98 "SBC A, B" $ fixGB $ subRegWithRegWithFlagsPlusC A B . increaseClock 4
decodeOp 0x99 = Instruction 0x99 "SBC A, C" $ fixGB $ subRegWithRegWithFlagsPlusC A C . increaseClock 4
decodeOp 0x9A = Instruction 0x9A "SBC A, D" $ fixGB $ subRegWithRegWithFlagsPlusC A D . increaseClock 4
decodeOp 0x9B = Instruction 0x9B "SBC A, E" $ fixGB $ subRegWithRegWithFlagsPlusC A E . increaseClock 4
decodeOp 0x9C = Instruction 0x9C "SBC A, H" $ fixGB $ subRegWithRegWithFlagsPlusC A H . increaseClock 4
decodeOp 0x9D = Instruction 0x9D "SBC A, L" $ fixGB $ subRegWithRegWithFlagsPlusC A L . increaseClock 4
--TODO 0x9E
decodeOp 0x9F = Instruction 0x9F "SBC A, A" $ fixGB $ subRegWithRegWithFlagsPlusC A A . increaseClock 8
decodeOp 0xA0 = Instruction 0xA0 "AND B" $ fixGB $ andReg B . increaseClock 4
decodeOp 0xA1 = Instruction 0xA1 "AND C" $ fixGB $ andReg C . increaseClock 4
decodeOp 0xA2 = Instruction 0xA2 "AND D" $ fixGB $ andReg D . increaseClock 4
decodeOp 0xA3 = Instruction 0xA3 "AND E" $ fixGB $ andReg E . increaseClock 4
decodeOp 0xA4 = Instruction 0xA4 "AND H" $ fixGB $ andReg H . increaseClock 4
decodeOp 0xA5 = Instruction 0xA5 "AND L" $ fixGB $ andReg L . increaseClock 4
--TODO 0xA6
decodeOp 0xA7 = Instruction 0xA7 "AND A" $ fixGB $ andReg A . increaseClock 4
decodeOp 0xA8 = Instruction 0xA8 "XOR B" $ fixGB $ xorReg B . increaseClock 4
decodeOp 0xA9 = Instruction 0xA9 "XOR C" $ fixGB $ xorReg C . increaseClock 4
decodeOp 0xAA = Instruction 0xAA "XOR D" $ fixGB $ xorReg D . increaseClock 4
decodeOp 0xAB = Instruction 0xAB "XOR E" $ fixGB $ xorReg E . increaseClock 4
decodeOp 0xAC = Instruction 0xAC "XOR H" $ fixGB $ xorReg H . increaseClock 4
decodeOp 0xAD = Instruction 0xAD "XOR L" $ fixGB $ xorReg L . increaseClock 4
--TODO 0xAE
decodeOp 0xAF = Instruction 0xAF "XOR A" $ fixGB $ xorReg A . increaseClock 4
decodeOp 0xB0 = Instruction 0xB0 "OR B" $ fixGB $ orReg B . increaseClock 4
decodeOp 0xB1 = Instruction 0xB1 "OR C" $ fixGB $ orReg C . increaseClock 4
decodeOp 0xB2 = Instruction 0xB2 "OR D" $ fixGB $ orReg D . increaseClock 4
decodeOp 0xB3 = Instruction 0xB3 "OR E" $ fixGB $ orReg E . increaseClock 4
decodeOp 0xB4 = Instruction 0xB4 "OR H" $ fixGB $ orReg H . increaseClock 4
decodeOp 0xB5 = Instruction 0xB5 "OR L" $ fixGB $ orReg L . increaseClock 4
--TODO 0xB6 - 0xC0
decodeOp 0xB7 = Instruction 0xB7 "OR A" $ fixGB $ orReg A . increaseClock 4
decodeOp 0xB8 = Instruction 0xB8 "CP B" $ fixGB $ cpReg B . increaseClock 4
decodeOp 0xB9 = Instruction 0xB9 "CP C" $ fixGB $ cpReg C . increaseClock 4
decodeOp 0xBA = Instruction 0xBA "CP D" $ fixGB $ cpReg D . increaseClock 4
decodeOp 0xBB = Instruction 0xBB "CP E" $ fixGB $ cpReg E . increaseClock 4
decodeOp 0xBC = Instruction 0xBC "CP H" $ fixGB $ cpReg H . increaseClock 4
decodeOp 0xBD = Instruction 0xBD "CP L" $ fixGB $ cpReg L . increaseClock 4
--TODO 0xBE
decodeOp 0xBF = Instruction 0xBF "CP A" $ fixGB $ cpReg A . increaseClock 4
--TODO 0xC0
decodeOp 0xC1 = Instruction 0xC1 "POP BC" $ pop (B, C) . increaseClock 12
--TODO 0xC2 - 0xC4
decodeOp 0xC5 = Instruction 0xC5 "PUSH BC" $ (\gb -> push (getRegisters (B, C) gb) gb) . increaseClock 16
--TODO 0xC6 - 0xC8
decodeOp 0xC9 = Instruction 0xC9 "RET" $ ret . increaseClock 16
--TODO 0xCA
decodeOp 0xCB = Instruction 0xCB "[CB Instruction]" $ \gb -> do { cb <- fetchCb $ incrementRegistersWithoutFlags (PHI, CLO) gb
                                                                ; return $ decodeCb cb (incrementRegistersWithoutFlags (PHI,CLO) gb) }
--TODO 0xCC
decodeOp 0xCD = Instruction 0xCD "CALL a8" $ call . increaseClock 24
--TODO 0xCE - 0xDF
decodeOp 0xE0 = Instruction 0xE0 "LDH (a8), A" $ ldFFAndMemOffsetWithA . increaseClock 12
--TODO 0xE1
decodeOp 0xE2 = Instruction 0xE2 "LD (C), A" $ ldFFRegAddrReg C A . increaseClock 8
--TODO 0xE3 - E9
decodeOp 0xEA = Instruction 0xEA "LD (a16), A" $ ldMemDataWithReg A . increaseClock 16
--TODO 0xEB - 0xEF
decodeOp 0xF0 = Instruction 0xF0 "LDH A, (a8)" $ ldAWithFFAndMemOffset . increaseClock 12
--TODO 0xF1 - 0xFD
decodeOp 0xFE = Instruction 0xFE "CP d8" $ cp8 . increaseClock 8
--TODO 0xFF

fetchCb :: Gameboy -> IO Word8
fetchCb gb = getMemory (getRegisters (PHI, CLO) gb) gb

--TODO alot of cb instructions need to be implemented
decodeCb :: Word8 -> (Gameboy -> Gameboy)
decodeCb 0x11 = rotateLeft C . increaseClock 8
decodeCb 0x7C = testBitReg H 7 . increaseClock 8
