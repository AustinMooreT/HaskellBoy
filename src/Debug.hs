{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Debug (module Debug) where

import Cpu
import Decode
import Execution
import Lcd
import BootRom
import Memory
import Lib

import Control.Lens
import System.Console.ANSI as A
import Data.Word
import Numeric (showHex)

-- TODO Debug is really nasty and needs some serious love.

regToDoc :: Register -> String
regToDoc A   = "A"
regToDoc B   = "B"
regToDoc C   = "C"
regToDoc D   = "D"
regToDoc E   = "E"
regToDoc F   = "F"
regToDoc H   = "H"
regToDoc L   = "L"
regToDoc SHI = "S"
regToDoc PLO = "P"
regToDoc PHI = "P"
regToDoc CLO = "C"

regRegToDoc :: (Register, Register) -> String
regRegToDoc (r1, r2) = regToDoc r1 ++ regToDoc r2

data PrintRegister =
  PrintRegister
  {
    _printRegName  :: String,
    _printRegValue :: String,
    _printRegColor :: A.Color
  } | PrintRegNone
makeLenses ''PrintRegister

prettyPrintReg :: Register -> Cpu -> PrintRegister
prettyPrintReg reg cpu_ = PrintRegister (regToDoc reg) (valStr ++ (replicate (4 - (length valStr)) ' ')) A.White
  where valStr = showHex (cpu_ ^. (registerToLens reg)) ""
prettyPrintRegReg :: (Register, Register) -> Cpu -> PrintRegister
prettyPrintRegReg rs cpu_ = PrintRegister (regRegToDoc rs) (valStr ++ (replicate (5 - (length valStr)) ' ')) A.White
  where valStr = showHex (cpu_ ^. (composeRegisterLenses rs)) ""

evalPrintRegisters
  :: [PrintRegister] -> IO ()
evalPrintRegisters xs = putStrLn (fst str) >> putStrLn (snd str)
  where
    str = foldl (\(n1, v1) -> \(n2, v2) ->
                    (n1 ++ "   |" ++ n2, v1 ++ "|" ++ v2))
          (head mapped) (tail mapped)
    mapped = (map (\x -> (x ^. printRegName, x ^. printRegValue)) xs)

prettyPrintCpu :: Cpu -> IO ()
prettyPrintCpu cpu_ = evalPrintRegisters [(prettyPrintReg A cpu_),
                                          (prettyPrintReg B cpu_),
                                          (prettyPrintReg C cpu_),
                                          (prettyPrintReg D cpu_),
                                          (prettyPrintReg E cpu_),
                                          (prettyPrintReg F cpu_),
                                          (prettyPrintReg H cpu_),
                                          (prettyPrintReg L cpu_),
                                          (prettyPrintRegReg (H,L) cpu_),
                                          (prettyPrintRegReg (SHI,PLO) cpu_),
                                          (prettyPrintRegReg (PHI,CLO) cpu_)]



-- ^ Idk what I was doing up there.
-- | New debugging code starts here.

-- | Prints the contents of address to memory
printMemory :: Memory -> Word16 -> IO ()
printMemory mem addr = getMemory addr mem >>= \x -> putStrLn $ showHex x ""

-- | Print reg prints contents of a register
--printReg :: Cpu -> Register -> IO ()
--printReg cpu reg = putStrLn $ showHex (getRegister reg cpu) ""

-- TODO I'd like to put this everywhere in the project.
type Registers = (Register, Register)

data DebugActionData = Word16 | Register | Registers | NullActionData
  deriving (Eq)

data DebugAction = DebugIdentity                         |
                   DebugStep                             |
                   DebugEnableBreak                      |
                   DebugDisableBreak                     |
                   DebugSetBreak Word16                  |
                   DebugEnableLcd                        |
                   DebugDisableLcd                       |
                   DebugEnableInterrupts                 |
                   DebugDisableInterrupts                |
                   DebugPrintReg Register                |
                   DebugPrintRegReg (Register, Register) |
                   DebugGetMem Word16                    |
                   DebugContinue
  deriving (Eq)

getDebugActionData :: DebugAction -> DebugActionData
getDebugActionData DebugIdentity          = NullActionData
getDebugActionData DebugStep              = NullActionData
getDebugActionData DebugEnableBreak       = NullActionData
getDebugActionData DebugDisableBreak      = NullActionData
getDebugActionData (DebugSetBreak _)      = Word16
getDebugActionData DebugEnableLcd         = NullActionData
getDebugActionData DebugDisableLcd        = NullActionData
getDebugActionData DebugEnableInterrupts  = NullActionData
getDebugActionData DebugDisableInterrupts = NullActionData
getDebugActionData (DebugPrintReg _)      = Register
getDebugActionData (DebugPrintRegReg _)   = Registers
getDebugActionData (DebugGetMem _)        = Word16
getDebugActionData DebugContinue          = NullActionData


data DebugState =
  DebugState
  {
    _dbgCurrAction  :: DebugAction,
    _dbgBreak       :: Word16,
    _dbgEnableBreak :: Bool,
    _dbgEnableInts  :: Bool,
    _dbgEnableLcd   :: Bool,
    _dbgCpu         :: Cpu,
    _dbgMem         :: Memory,
    _dbgLcd         :: Lcd,
    _dbgClock       :: Cycles
  }
makeLenses ''DebugState

setBreak :: DebugState -> Word16 -> DebugState
setBreak dbg w16 = dbg & dbgBreak .~ w16

enableBreak :: DebugState -> DebugState
enableBreak dbg = dbg & dbgEnableBreak .~ True

disableBreak :: DebugState -> DebugState
disableBreak dbg = dbg & dbgEnableBreak .~ False

printMem :: DebugState -> Word16 -> IO String
printMem dbg w16 = getMemory w16 (dbg ^. dbgMem) >>= \x -> return $ showHex x ""

printReg :: DebugState -> Register -> String
printReg dbg reg = showHex (getRegister reg (dbg ^. dbgCpu)) ""

setDbgCpuMem :: (Cpu, Memory) -> DebugState -> DebugState
setDbgCpuMem cpumem dbg = uncurry (\cpu mem -> dbg & dbgCpu .~ cpu & dbgMem .~ mem) cpumem

dbgStep :: DebugState -> IO DebugState
dbgStep dbg = do { cpumem <- executeCurrInstr (return ((dbg ^. dbgCpu), (dbg ^. dbgMem)))
                 ; return $ setDbgCpuMem cpumem dbg }

dbgContinue :: DebugState -> IO DebugState
dbgContinue dbg
  | dbg ^. dbgEnableBreak = do { cpumem <- executeToPc (dbg ^. dbgBreak) (return ((dbg ^. dbgCpu), (dbg ^. dbgMem)))
                                 ; return $ setDbgCpuMem cpumem dbg }
  | otherwise             = dbgStep dbg

--TODO this is ugly as shit, maybe replace this with rankN types.
dbgGetWord16 :: DebugAction -> Word16
dbgGetWord16 (DebugSetBreak w) = w
dbgGetWord16 (DebugGetMem w)   = w

dbgGetRegister :: DebugAction -> Register
dbgGetRegister (DebugPrintReg r) = r

dbgGetRegisters :: DebugAction -> Registers
dbgGetRegisters (DebugPrintRegReg rs) = rs

debugEvalWord16 :: DebugState -> DebugAction -> IO DebugState
debugEvalWord16 dbg (DebugSetBreak w) = return $ dbg & dbgBreak .~ w
debugEvalWord16 dbg (DebugGetMem w) = do { d <- getMemory w (dbg ^. dbgMem)
                                         ; putStrLn $ showHex d ""
                                         ; return dbg }

debugEvalRegister :: DebugState -> DebugAction -> IO DebugState
debugEvalRegister dbg (DebugPrintReg r) = putStrLn (showHex (getRegister r (dbg ^. dbgCpu)) "") >> return dbg

debugEvalRegisters :: DebugState -> DebugAction -> IO DebugState
debugEvalRegisters dbg (DebugPrintRegReg rs) = putStrLn (showHex (getRegisters rs (dbg ^. dbgCpu)) "") >> return dbg

dbgEval :: DebugState -> IO DebugState
dbgEval dbg
  | (dbg ^. dbgCurrAction) == DebugIdentity                  = return dbg
  | (dbg ^. dbgCurrAction) == DebugEnableBreak               = return $ dbg & dbgEnableBreak .~ True
  | (dbg ^. dbgCurrAction) == DebugDisableBreak              = return $ dbg & dbgEnableBreak .~ False
  | (dbg ^. dbgCurrAction) == DebugEnableLcd                 = return $ dbg & dbgEnableLcd .~ True
  | (dbg ^. dbgCurrAction) == DebugDisableLcd                = return $ dbg & dbgEnableLcd .~ False
  | (dbg ^. dbgCurrAction) == DebugEnableInterrupts          = return $ dbg & dbgEnableInts .~ True
  | (dbg ^. dbgCurrAction) == DebugDisableInterrupts         = return $ dbg & dbgEnableInts .~ False
  | (dbg ^. dbgCurrAction) == DebugContinue                  = dbgContinue dbg
  | (getDebugActionData (dbg ^. dbgCurrAction)) == Word16    = debugEvalWord16 dbg (dbg ^. dbgCurrAction)
  | (getDebugActionData (dbg ^. dbgCurrAction)) == Register  = debugEvalRegister dbg (dbg ^. dbgCurrAction)
  | (getDebugActionData (dbg ^. dbgCurrAction)) == Registers = debugEvalRegisters dbg (dbg ^. dbgCurrAction)

parseHex :: String -> Word16
parseHex str = read str

-- TODO implement this.
--strToReg :: String -> Register

-- TODO implement this.
--getDbgInfo :: String -> DebugState -> DebugState
--getDbgInfo str dbg
--  | str == "s"  = dbg & dbgCurrAction .~ DebugStep
--  | str == "eb" = dbg & dbgCurrAction .~ DebugEnableBreak
--  | str == "db" = dbg & dbgCurrAction .~ DebugDisableBreak
--  | str == "el" = dbg & dbgCurrAction .~ DebugEnableLcd
--  | str == "dl" = dbg & dbgCurrAction .~ DebugDisableLcd
--  | str == "ei" = dbg & dbgCurrAction .~ DebugEnableInterrupts
--  | str == "di" = dbg & dbgCurrAction .~ DebugDisableInterrupts
--  | str == "c"  = dbg & dbgCurrAction .~ DebugContinue
--  | (head str) == 'b' = dbg & dbgCurrAction .~ (DebugSetBreak (parseHex (tail (tail str))))
--  | (head str) == 'm' = dbg & dbgCurrAction .~ (DebugGetMem (parseHex (tail (tail str))))
--  | (head str) == 'r' = dbg &
