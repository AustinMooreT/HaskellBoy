module Execution (module Execution) where

import Decode
import Cpu
import Memory
import BootRom
import Lib
import Lcd

import Data.Word

import Control.Lens


-- | TODO this is honestly kinda ugly. I'd like to maybe change this up someday.
  -- Applys an operation to the cpu/memory
applyOp :: Operation -> Cpu -> Memory -> IO (Cpu, Memory)
applyOp (OpCpuOpCpu f) cpu mem = f cpu >>= \x -> return (x, mem)
applyOp (OpCpuMemOpCpu f) cpu mem = f mem cpu >>= \x -> return (x, mem)
applyOp (OpCpuMemOpMem f) cpu mem = f mem cpu >>= \x -> return (cpu, x)
applyOp (OpCpuMemOpCpuMem f) cpu mem = f mem cpu

-- | Given an instruction executes it.
executeInstr :: Instruction -> Cpu -> Memory -> IO (Cpu, Memory)
executeInstr instr cpu mem = applyOp (instr ^. operation) cpu mem

-- | Fetches the next instruction from memory.
fetchInstr :: Cpu -> Memory -> IO Instruction
fetchInstr cpu mem = do { byte <- getMemory (getRegisters (PHI, CLO) cpu) mem
                        ; return $ decodeOp byte }

-- | Executes to a given cycle.
executeTillCycle :: Integer -> Cpu -> Memory -> IO (Cpu, Memory)
executeTillCycle cycles cpu mem
  | cycles <= 0 = return (cpu, mem)
  | otherwise   = do { instr   <- fetchInstr cpu mem
                     ; results <- executeInstr instr cpu mem
                     ; executeTillCycle
                       (cycles - (instr ^. time $ cpu))
                       (fst results) (snd results) }

--runTillHblank :: Cpu -> Memory -> IO (Cpu, Memory)
--runTillHblank cpu mem =
