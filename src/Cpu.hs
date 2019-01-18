{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Cpu (module Cpu) where

import Lib

import Control.Lens
import Data.Word
import Memory

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

composeRegisterLenses :: Lens' Cpu Word8 -> Lens' Cpu Word8 -> Lens' Cpu Word16
composeRegisterLenses reg1 reg2 = lens getter setter
  where
    getter cpu = combineData (cpu ^. reg1) (cpu ^. reg2)
    setter cpu d = cpu & reg1 .~ (breakHi d) & reg2 .~ (breakLo d)

registerHL :: Lens' Cpu Word16
registerHL = composeRegisterLenses registerH registerL

registerSP :: Lens' Cpu Word16
registerSP = composeRegisterLenses registerS_hi registerP_lo

registerPC :: Lens' Cpu Word16
registerPC = composeRegisterLenses registerP_hi registerP_lo

instance Show Cpu where
  show cpu = "A:[" Prelude.++ (show $ cpu ^. registerA) Prelude.++ "]\n" Prelude.++
             "B:[" Prelude.++ (show $ cpu ^. registerB) Prelude.++ "]\n" Prelude.++
             "C:[" Prelude.++ (show $ cpu ^. registerC) Prelude.++ "]\n" Prelude.++
             "D:[" Prelude.++ (show $ cpu ^. registerD) Prelude.++ "]\n" Prelude.++
             "E:[" Prelude.++ (show $ cpu ^. registerE) Prelude.++ "]\n" Prelude.++
             "F:[" Prelude.++ (show $ cpu ^. registerF) Prelude.++ "]\n" Prelude.++
             "H:[" Prelude.++ (show $ cpu ^. registerH) Prelude.++ "]\n" Prelude.++
             "L:[" Prelude.++ (show $ cpu ^. registerL) Prelude.++ "]\n" Prelude.++
             "SP:[" Prelude.++ (show $ cpu ^. registerSP) Prelude.++ "]\n" Prelude.++
             "PC:[" Prelude.++ (show $ cpu ^. registerPC) Prelude.++ "]\n"

-- | Default cpu on startup.
defaultCpu :: Cpu
defaultCpu = Cpu 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00

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

type Register  = Lens' Cpu Word8
type Registers = Lens' Cpu Word16


setRegisterFromAddress :: Cpu -> Memory -> Register -> Address -> IO Cpu
setRegisterFromAddress cpu mem reg addr = getMemory addr mem >>= \d -> return $ cpu & reg .~ d

setRegisterFromIndexedAddress :: Cpu -> Memory -> Lens' Cpu Word8 -> Lens' Cpu Word16 -> IO Cpu
setRegisterFromIndexedAddress cpu mem reg regs = setRegisterFromAddress cpu mem reg (cpu ^. regs)

setMemoryFromRegister :: Cpu -> Memory -> Word8 -> Lens' Cpu Word16 -> IO ()
setMemoryFromRegister cpu mem byte reg = setMemory (cpu ^. reg) byte mem

setMemoryToRegisterFromRegister :: Cpu -> Memory -> ScopedLens Cpu Word8 -> ScopedLens Cpu Word16 -> IO ()
setMemoryToRegisterFromRegister cm (d, addr) = cm \.~ (fst cm ^. d', addr)

(/++!) :: Cpu -> Lens' Cpu Word8 -> Cpu
(/++!) cpu reg = cpu & reg %~ (+1)

(//++!) :: Cpu -> Lens' Cpu Word16 -> Cpu
(//++!) cpu regs = cpu & regs %~ (+1)

step :: Cpu -> Cpu
step cpu = cpu //++! registerPC


(\&.~) :: (Cpu, Memory) -> Lens' Cpu Word16 -> (Cpu, IO ())
(\&.~) (cpu, mem) regs = (cpu', moveMemory (cpu ^. registerPC) (cpu ^. regs) mem)
  where cpu' = step cpu

