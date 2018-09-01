module Execution (module Execution) where

import Core
import Decode
import Cpu
import BootRom
import Lib

import Control.Lens

evalInstruction :: Gameboy -> Instruction -> IO Gameboy
evalInstruction gb inst = gb & inst ^. operation

fetchNextInstr :: Gameboy -> IO Instruction
fetchNextInstr gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb) gb
                       ; return $ decodeOp mem }

stepGameboy :: Gameboy -> IO Gameboy
stepGameboy gb = do { instr <- fetchNextInstr gb
                    ; evGB  <- evalInstruction gb instr
                    ; return $ gb1 evGB }
  where gb1 = incrementRegistersWithoutFlags (PHI, CLO)

stepNGameboy :: Int -> (Gameboy -> IO Gameboy)
stepNGameboy n = Prelude.foldl (.|) (fixGB id) $ Prelude.replicate n stepGameboy

runGameboyNSteps :: Int -> IO Gameboy
runGameboyNSteps n = do { gb   <- defaultGameboy
                        ; boot <- loadBootRom gb
                        ; stepNGameboy n boot }


