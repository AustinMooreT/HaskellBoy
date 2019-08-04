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
printReg :: Cpu -> Register -> IO ()
printReg cpu reg = putStrLn $ showHex (getRegister reg cpu) ""

data DebugState =
  DebugState
  {
    _break      :: Word16,
    _enableInts :: Bool
  }
makeLenses ''DebugState

data DebugCommand =
  PrintMem Word16           |
  PrintReg (Cpu -> Word16)  |
  SetBreak Word16           |
  StepForward               |
  Continue                  |
  Error

strToReg :: String -> (Cpu -> Word16)
strToReg "A"   = \cpu -> toWord16 $ getRegister A cpu
strToReg "B"   = \cpu -> toWord16 $ getRegister B cpu
strToReg "C"   = \cpu -> toWord16 $ getRegister C cpu
strToReg "D"   = \cpu -> toWord16 $ getRegister D cpu
strToReg "E"   = \cpu -> toWord16 $ getRegister E cpu
strToReg "F"   = \cpu -> toWord16 $ getRegister F cpu
strToReg "H"   = \cpu -> toWord16 $ getRegister H cpu
strToReg "L"   = \cpu -> toWord16 $ getRegister L cpu
strToReg "S"   = \cpu -> toWord16 $ getRegister SHI cpu
strToReg "PLO" = \cpu -> toWord16 $ getRegister PLO cpu
strToReg "PHI" = \cpu -> toWord16 $ getRegister PHI cpu
strToReg "CLO" = \cpu -> toWord16 $ getRegister CLO cpu
strToReg "HL"  = \cpu -> getRegisters (H, L) cpu
strToReg "SP"  = \cpu -> getRegisters (SHI, PLO) cpu
strToReg "PC"  = \cpu -> getRegisters (PHI, CLO) cpu
strToReg _     = \cpu -> getRegisters (PHI, CLO) cpu

decodeDebugCommand :: [String] -> DebugCommand
decodeDebugCommand str
  | head str == "prmem" = PrintMem $ read (head . drop 1 $ str)
  | head str == "prreg" = PrintReg $ strToReg (head . drop 1 $ str)

--fetchCommand :: IO DebugCommand
