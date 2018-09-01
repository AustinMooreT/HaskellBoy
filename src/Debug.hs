{-# LANGUAGE TemplateHaskell #-}

module Debug (module Debug) where

import Core
import Cpu
import Execution

import Control.Lens
import System.Console.ANSI as A
import Data.Word
import Numeric (showHex)

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

evalPrintRegisters :: [PrintRegister] -> IO ()
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

prettyPrintLineOfMemory :: Word16 -> Gameboy -> IO String
prettyPrintLineOfMemory addr gb = do { mem <- getMemory addr gb
                                     ; return ((replicate (4 - (length (showHex addr ""))) ' ') ++
                                                (showHex addr "") ++
                                                " : " ++ (showHex mem "")) }

prettyPrintMem :: Word16 -> Gameboy -> [IO String]
prettyPrintMem addr gb = let printaddr = \addr_ -> prettyPrintLineOfMemory addr_ gb
                         in map printaddr $ lower ++ [addr] ++ higher
  where addrs  = [1 .. 5]
        lower  = map (addr-) (reverse addrs)
        higher = map (addr+) addrs

attachTagsPC :: Gameboy -> [IO String] -> [IO String]
attachTagsPC gb strs =  let tagSurround = \istr -> do { str <- istr
                                                   ; return $ str ++ (replicate 25 ' ') }
                            tagMain     = \istr -> do { str <- istr
                                                      ; instr <- fetchNextInstr gb
                                                      ; let instrText = instr ^. name
                                                        in return $ str ++
                                                           " <-- " ++
                                                           instrText ++
                                                           (replicate (21 - (length instrText)) ' ') }
                        in (map tagSurround (take 5 strs)) ++
                           (map tagMain (take 1 . drop 5 $ strs)) ++
                           (map tagSurround (drop 6 strs))

testMem :: Gameboy -> [IO String] -> IO ()
testMem gb x = do { strs <- sequence (attachTagsPC gb x)
                  ; print_ <- foldl (>>) (return ())
                              (map (\x -> putStrLn x) strs)
                  ; return print_ }

prettyPrintGb :: IO Gameboy -> IO ()
prettyPrintGb igb = do { gb <- igb
                       ; let pCpu = prettyPrintCpu (gb ^. cpu)
                             pMem = testMem gb (prettyPrintMem (getRegisters (PHI, CLO) gb) gb)
                         in pCpu >> putStrLn "" >> pMem }

debugMode :: IO Gameboy -> IO Gameboy
debugMode igb = do { gb    <- igb
                   ; _     <- putStrLn "step: "
                   ; steps <- getLine
                   ; sgb   <- stepNGameboy (read steps) gb
                   ; _     <- prettyPrintGb (return sgb)
                   ; fgb   <- debugMode (return sgb)
                   ; return fgb }
