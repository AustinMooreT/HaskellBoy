module Main where

import Lcd
import Cpu
import Memory
import BootRom
import Execution

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Control.Lens

--renderScanLine :: Memory -> DisplayBuffer -> IO DisplayBuffer
--displayGlossBuffer :: DisplayBuffer -> Bool -> IO ()

main :: IO ()
main = profiling $ return (mainBuffer, (defaultMemory >>= \x -> (loadBootRom x >>= \y -> return (defaultCpu, y))))

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
runGlossBuffer db cpumem False = db >>= \b -> simulateIO
                                             (InWindow "BestWindow" (b ^. width, b ^. height) (0,0))
                                             white
                                             8597
                                             (db, cpumem)
                                             (render False)
                                             stepper
runGlossBuffer db cpumem True = db >>= \b -> simulateIO
                                             (InWindow "BestWindow" (1000,1000) (0,0))
                                             white
                                             8597
                                             (db, cpumem)
                                             (render True)
                                             stepper

stepper :: ViewPort -> Float -> (IO DisplayBuffer, IO (Cpu, Memory)) -> IO (IO DisplayBuffer, IO (Cpu, Memory))
stepper _ _ m = runToHBlank m


runToHBlank :: (IO DisplayBuffer, IO (Cpu, Memory)) -> IO (IO DisplayBuffer, IO (Cpu, Memory))
runToHBlank dbc = do { cpumem'   <- cpumem
                     ; db        <- d
                     ; cpumem''  <- runSystem (fst cpumem') (snd cpumem')
                     ; lcd       <- (getLcd (snd cpumem''))
                     ; putStrLn (show $ getRegister H (fst cpumem''))
                     ; display'  <-
                         if (lcd ^. lcdStatus . modeFlag) == HBlank then
                           renderScanLine (snd cpumem'') db
                         else
                           return db
                     ; return (return display', return cpumem'') }
  where cpumem = (snd dbc)
        d      = (fst dbc)

initSystem :: IO (Cpu, Memory)
initSystem = do { mem  <- defaultMemory
                ; mem' <- loadBootRom mem
                ; return (defaultCpu, mem') }

