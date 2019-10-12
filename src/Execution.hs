module Execution (module Execution) where


import Cpu
import Lib
import Lcd
import Memory
import Decode
import BootRom
import qualified Interrupts as INT

import Data.Word
import Numeric (showHex)
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
executeInstr instr cpu mem = do { cpumem  <- applyOp (instr ^. operation) cpu mem
                                ; cpumem' <- return (incrementRegistersWithoutFlags (PHI, CLO) (fst cpumem), (snd cpumem))
                                ; lcd     <- getLcd (snd cpumem')
                                --; _       <- putStrLn . show . snd $ lcd ^. lcdControl . bgWindowTileSelect
                                --; _       <- putStrLn $ "LY: " ++ (show $ lcd ^. ly)
                                --; _       <- putStrLn $ "A: " ++ (show $ getRegister A (fst cpumem'))
                                --; nb      <- return $ getRegisters (PHI, CLO) (fst cpumem')
                                --; _       <- putStrLn $ "nb:" ++ (showHex nb "")
                                ;  INT.checkAndEvalInterrupts (fst cpumem') (snd cpumem') }

-- | Fetches the next instruction from memory.
fetchInstr :: Cpu -> Memory -> IO Instruction
fetchInstr cpu mem = do { byte <- getMemory (getRegisters (PHI, CLO) cpu) mem
                         --; if (getRegisters (PHI, CLO) cpu) > 0x0100 then
                             --putStrLn (showHex byte "")
                           --else
                                  --return ()
                         ; (return $ decodeOp byte) }

-- | Executes to a given cycle.
executeTillCycle :: Cycles -> Cpu -> Memory -> IO (Cpu, Memory)
executeTillCycle cycles cpu mem
  | cycles cpu <= 0 = return (cpu, mem)
  | otherwise       = do { instr   <- fetchInstr cpu mem
                         --; if (and [(instr ^. name) /= "HALT", True]) then
                           -- do { putStrLn ((instr ^. name) ++ " : " ++ (showHex (getRegisters (PHI, CLO) cpu) ""))
                             --  ; putStrLn $ show $ cycles cpu
                               --; putStrLn ("A : " ++ (showHex (getRegister A cpu) ""))
                               --; putStrLn ("B : " ++ (showHex (getRegister B cpu) ""))
                               --; putStrLn ("C : " ++ (showHex (getRegister C cpu) ""))
                               --; putStrLn ("D : " ++ (showHex (getRegister D cpu) ""))
                               --; putStrLn ("E : " ++ (showHex (getRegister E cpu) ""))
                               --; putStrLn ("F : " ++ (showHex (getRegister F cpu) ""))
                               --; putStrLn ("H : " ++ (showHex (getRegister H cpu) ""))
                               --; putStrLn ("L : " ++ (showHex (getRegister L cpu) ""))
                               --; putStrLn ("SHI : " ++ (showHex (getRegister SHI cpu) ""))
                               --; putStrLn ("PLO : " ++ (showHex (getRegister PLO cpu) ""))
                               --; putStrLn ("PHI : " ++ (showHex (getRegister PHI cpu) ""))
                               --; putStrLn ("CLO : " ++ (showHex (getRegister CLO cpu) ""))
                               --; lcd <- getLcd mem
                               --; putStrLn ("LY: " ++ (showHex (lcd ^. ly) ""))
                               --; return "" }
                           --else
                               --return ""
                         ; results <- executeInstr instr cpu mem
                         ; executeTillCycle
                           (const $ (cycles cpu) - (instr ^. time $ cpu))
                           (fst results) (snd results) }


executeCurrInstr :: IO (Cpu, Memory) -> IO (Cpu, Memory)
executeCurrInstr cpumem = do { cpumem' <- cpumem
                             ; instr   <- fetchInstr (fst cpumem') (snd cpumem')
                             --; _ <- putStrLn $ instr ^. name
                             ; executeInstr instr (fst cpumem') (snd cpumem') }


-- | Returns the number of cpu cycles needed to finish an lcd mode.
  -- TODO this really belongs in the LCD module.
lcdModeToTiming :: LcdMode -> Cycles
lcdModeToTiming HBlank      = const 204
lcdModeToTiming OamSearch   = const 80
lcdModeToTiming LcdTransfer = const 172
lcdModeToTiming VBlank      = const 4560 -- NOTE this may be wildly inaccurate

-- | Given an lcd mode executes the cpu/memory the required number of cycles to complete that lcd mode
executeLcdMode :: LcdMode -> Cpu -> Memory -> IO (Cpu, Memory)
executeLcdMode mode cpu mem = executeTillCycle (lcdModeToTiming mode) cpu mem

executeTillHBlank :: Cpu -> Memory -> IO (Cpu, Memory)
executeTillHBlank cpu mem = do { lcd     <- getLcd mem
                               ; cpumem' <- executeTillCycle (lcdModeToTiming $ lcd ^. lcdStatus . modeFlag) cpu mem
                               ; lcd'    <- getLcd (snd cpumem')
                               ; mem'    <- if lcd' ^. lcdControl . lcdEnabled then
                                              setLcd (stepLcd lcd') (snd cpumem')
                                            else
                                              return (snd cpumem')
                               ; lcd''   <- getLcd mem'
                               ; if (lcd'' ^. lcdStatus . modeFlag) /= HBlank then
                                   executeTillHBlank (fst cpumem') mem'
                                 else
                                   return (fst cpumem', mem') }

-- | Execute till lcd transition TODO this is kinda tentative thing here.
runSystem :: Cpu -> Memory -> IO (Cpu, Memory)
runSystem cpu mem = executeTillHBlank cpu mem

executeToPc :: Word16 -> IO (Cpu, Memory) -> IO (Cpu, Memory)
executeToPc break cpumem = do { cpumem' <- cpumem
                              ; if getRegisters (PHI, CLO) (fst cpumem') == break then
                                  cpumem
                                else
                                  executeToPc break $! executeCurrInstr $! cpumem }

-- NOTE this should probably be moved to debug module, but it's garbage.
debugRegister :: Register -> IO (Cpu, Memory) -> IO Word8
debugRegister reg cpumem = do { cpumem' <- cpumem
                              ; return $ getRegister reg (fst cpumem') }

-- NOTE this should probably be moved to debug module, but it's garbage.
debugRegisters :: (Register, Register) -> IO (Cpu, Memory) -> IO Word16
debugRegisters regs cpumem = do { cpumem' <- cpumem
                                ; return $ getRegisters regs (fst cpumem') }
