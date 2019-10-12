module Main where

import Lcd
import Cpu
import Memory
import BootRom
import Execution
import Decode

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Lens

--renderScanLine :: Memory -> DisplayBuffer -> IO DisplayBuffer
--displayGlossBuffer :: DisplayBuffer -> Bool -> IO ()

main :: IO ()
main = runGlossBuffer mainBuffer (defaultMemory >>= \x -> loadMemorySnapshot "/home/maurice/Downloads/haskellboytests/cpu_instrs/individual/jrjp.gb" x >>= \y -> return (defaultCpu, y)) True

profiling :: IO (IO DisplayBuffer, IO (Cpu, Memory)) -> IO ()
profiling d = do {d' <- d
                 ; profiling $ runToHBlank d'}

render :: Bool -> (IO DisplayBuffer, IO (Cpu, Memory)) -> IO Picture
render False cb = db >>= \b -> return (bitmapOfForeignPtr (b ^. width) (b ^. height)
                                       (BitmapFormat TopToBottom PxRGBA) (b ^. forPtr) False)
  where db = (fst cb)
render True cb = db >>= \b -> return (scale 5.0 5.0 $ bitmapOfForeignPtr (b ^. width) (b ^. height)
                                      (BitmapFormat TopToBottom PxRGBA) (b ^. forPtr) False)
  where db = (fst cb)

runGlossBuffer :: IO DisplayBuffer -> IO (Cpu, Memory) -> Bool -> IO ()
runGlossBuffer db cpumem False = do {b <- db
                                    ; playIO
                                      (InWindow "BestWindow" (b ^. width, b ^. height) (0,0))
                                      black
                                      8597
                                      (db, cpumem)
                                      (render False)
                                      (\_ w -> return w)
                                      stepper }
runGlossBuffer db cpumem True = do {b <- db
                                   ; playIO
                                     (InWindow "BestWindow" (1000,1000) (0,0))
                                     black
                                     8597
                                     (db, cpumem)
                                     (render True)
                                     (\_ w -> return w)
                                     stepper }

stepper :: Float -> (IO DisplayBuffer, IO (Cpu, Memory)) -> IO (IO DisplayBuffer, IO (Cpu, Memory))
stepper _ m = runToHBlank m

runToHBlank :: (IO DisplayBuffer, IO (Cpu, Memory)) -> IO (IO DisplayBuffer, IO (Cpu, Memory))
runToHBlank dbc = do { cpumem'   <- cpumem
                     ; db        <- d
                     ; cpumem''  <- runSystem (fst cpumem') (snd cpumem')
                     ; lcd       <- (getLcd (snd cpumem''))
                     ; display'  <- renderScanLine (snd cpumem'') db
                     ; return (return display', return cpumem'') }
  where cpumem = (snd dbc)
        d      = (fst dbc)

initSystem :: IO (Cpu, Memory)
initSystem = do { mem   <- defaultMemory
                ; mem'  <- loadMemorySnapshot "/home/maurice/Roms/GB/Tetris.gb" mem
                ; return (defaultCpu, mem') }

