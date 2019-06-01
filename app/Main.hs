module Main where

import Lcd
import Cpu
import Memory
import BootRom
import Execution

import Control.Lens

--renderScanLine :: Memory -> DisplayBuffer -> IO DisplayBuffer
--displayGlossBuffer :: DisplayBuffer -> Bool -> IO ()

main :: IO ()
main = mainBuffer >>= \x -> main' initSystem x

main' :: IO (Cpu, Memory) -> DisplayBuffer -> IO ()
main' cpumem db = do { cpumem'  <- cpumem
                     ; cpumem'' <- runSystem (fst cpumem') (snd cpumem')
                     ; lcd      <- (getLcd (snd cpumem''))
                     ; display  <-
                         if (lcd ^. lcdStatus . modeFlag) == HBlank then
                           renderScanLine (snd cpumem'') db
                         else
                           return db
                     ; _        <- displayGlossBuffer display True
                     ; main' (return cpumem'') display }
initSystem :: IO (Cpu, Memory)
initSystem = do { mem  <- defaultMemory
                ; mem' <- loadBootRom mem
                ; return (defaultCpu, mem') }

