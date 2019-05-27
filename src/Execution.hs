module Execution (module Execution) where

import Decode
import Cpu
import Memory
import BootRom
import Lib
import Lcd

import Data.Word

import Control.Lens

{- NOTE/TODO alot of the stuff in this file might not stick around in here.
 - This is more or less just a breeding ground for ideas on how to synchronize the new cpu
 - api with the current lcd api.
 -}

-- | TODO this is honestly kinda ugly. I'd like to maybe change this up someday.
  -- Applys an operation to the cpu/memory
applyOp :: Operation -> Cpu -> Memory -> IO (Cpu, Memory)
applyOp (OpCpuOpCpu f)       cpu mem = f cpu >>= \x -> return (x, mem)
applyOp (OpCpuMemOpCpu f)    cpu mem = f mem cpu >>= \x -> return (x, mem)
applyOp (OpCpuMemOpMem f)    cpu mem = f mem cpu >>= \x -> return (cpu, x)
applyOp (OpCpuMemOpCpuMem f) cpu mem = f mem cpu

-- TODO/NOTE none of this contains any interrupt code.

-- | Given an instruction executes it.
executeInstr :: Instruction -> Cpu -> Memory -> IO (Cpu, Memory)
executeInstr instr cpu mem = applyOp (instr ^. operation) cpu mem

-- | Fetches the next instruction from memory.
fetchInstr :: Cpu -> Memory -> IO Instruction
fetchInstr cpu mem = do { byte <- getMemory (getRegisters (PHI, CLO) cpu) mem
                        ; return $ decodeOp byte }

-- | Executes to a given cycle.
executeTillCycle :: Cycles -> Cpu -> Memory -> IO (Cpu, Memory)
executeTillCycle cycles cpu mem
  | cycles cpu <= 0 = return (cpu, mem)
  | otherwise       = do { instr   <- fetchInstr cpu mem
                         ; results <- executeInstr instr cpu mem
                         ; executeTillCycle
                           (const $ (cycles cpu) - (instr ^. time $ cpu))
                           (fst results) (snd results) }

-- | Returns the number of cpu cycles needed to finish an lcd mode.
lcdModeToTiming :: LcdMode -> Cycles
lcdModeToTiming HBlank      = const 204
lcdModeToTiming OamSearch   = const 80
lcdModeToTiming LcdTransfer = const 172
lcdModeToTiming VBlank      = const 0 -- TODO

-- | Given an lcd mode executes the cpu/memory the required number of cycles to complete that lcd mode
executeLcdMode :: LcdMode -> Cpu -> Memory -> IO (Cpu, Memory)
executeLcdMode mode cpu mem = executeTillCycle (lcdModeToTiming mode) cpu mem


