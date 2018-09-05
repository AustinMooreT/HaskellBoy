{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module Lcd (module Lcd) where

import Core
import Cpu
import Memory
import Lib

import Control.Lens
import Control.Monad
import Data.Word
import Data.Bits
import Data.Array.IO

data LcdMemoryBank = Bank Word16 Word16

data SpriteSize = Tall | Short

data LcdControl =
  LcdControl
  {
    _lcdEnabled              :: Bool,
    _windowTileMapSelect     :: LcdMemoryBank,
    _windowEnabled           :: Bool,
    _bgWindowTileSelect      :: LcdMemoryBank,
    _bgTileMapSelect         :: LcdMemoryBank,
    _spriteSize              :: SpriteSize,
    _spritesEnabled          :: Bool,
    _bgWindowDisplayPriority :: Bool
  }
makeLenses ''LcdControl

getLcdControl :: Gameboy -> IO LcdControl
getLcdControl gb = do { byte <- getMemory 0xFF40 gb
                      ; let lcdEnabled_               = testBit byte 7
                            windowTilemap             = if testBit byte 6 then
                                                          Bank 0x9C00 0x9FFF
                                                        else
                                                          Bank 0x9800 0x9BFF
                            windowEnabled_            = testBit byte 5
                            bgWindowTile              = if testBit byte 4 then
                                                          Bank 0x8000 0x8FFF
                                                        else
                                                          Bank 0x8800 0x97FF
                            bgTileMap                 = if testBit byte 3 then
                                                          Bank 0x9C00 0x9FFF
                                                        else
                                                          Bank 0x9800 0x9BFF
                            spriteSize_               = if testBit byte 2 then
                                                          Tall
                                                        else
                                                          Short
                            spritesEnabled_           = testBit byte 1
                            bgWindowDisplayPriotrity_ = testBit byte 0
                        in return $ LcdControl
                           lcdEnabled_
                           windowTilemap
                           windowEnabled_
                           bgWindowTile
                           bgTileMap
                           spriteSize_
                           spritesEnabled_
                           bgWindowDisplayPriotrity_ }


data LcdMode = HBlank | VBlank | OamSearch | LcdTransfer

data LcdStatus =
  LcdStatus
  {
    _coincidenceInterruptEnabled :: Bool,
    _mode2OamInterruptEnabled    :: Bool,
    _mode1VBlankInterruptEnabled :: Bool,
    _mode0HBlankInterruptEnabled :: Bool,
    _coincidenceFlag             :: Bool,
    _modeFlag                    :: LcdMode
  }
makeLenses ''LcdStatus

twoBitsToMode :: Bool -> Bool -> LcdMode
twoBitsToMode False False = HBlank
twoBitsToMode False True  = VBlank
twoBitsToMode True  False = OamSearch
twoBitsToMode True  True  = LcdTransfer

getLcdStatus :: Gameboy -> IO LcdStatus
getLcdStatus gb = do { byte <- getMemory 0xFF41 gb
                     ; let coincidenceInterrupt = testBit byte 6
                           mode2OamInterrupt    = testBit byte 5
                           mode1VBlankInterrupt = testBit byte 4
                           mode0HBlankInterrupt = testBit byte 3
                           coincidenceFlag_     = testBit byte 2
                           modeFlag_            = twoBitsToMode (testBit byte 1) (testBit byte 0)
                       in return $ LcdStatus
                          coincidenceInterrupt
                          mode2OamInterrupt
                          mode1VBlankInterrupt
                          mode0HBlankInterrupt
                          coincidenceFlag_
                          modeFlag_ }

getModeBit :: Bool -> LcdMode -> Bool
getModeBit True  HBlank      = False
getModeBit False HBlank      = False
getModeBit True  VBlank      = False
getModeBit False VBlank      = True
getModeBit True  OamSearch   = True
getModeBit False OamSearch   = False
getModeBit True  LcdTransfer = True
getModeBit False LcdTransfer = True

lcdStatusToByte :: LcdStatus -> Word8
lcdStatusToByte l = maybeSetBit8 (l ^. coincidenceInterruptEnabled) 6 .
                    maybeSetBit8 (l ^. mode2OamInterruptEnabled)    5 .
                    maybeSetBit8 (l ^. mode1VBlankInterruptEnabled) 4 .
                    maybeSetBit8 (l ^. mode0HBlankInterruptEnabled) 3 .
                    maybeSetBit8 (l ^. coincidenceFlag)             2 .
                    maybeSetBit8 (getModeBit True  (l ^. modeFlag)) 1 .
                    maybeSetBit8 (getModeBit False (l ^. modeFlag)) 0 $ 0b10000000


setLcdStatus :: LcdStatus -> (Gameboy -> IO Gameboy)
setLcdStatus ls = setMemory 0xFF41 (lcdStatusToByte ls)

data Shade = White | LightGray | DarkGray | Black | Transparent

data Palette =
  LcdPalette
  {
    _color0 :: Shade,
    _color1 :: Shade,
    _color2 :: Shade,
    _color3 :: Shade
  }
makeLenses ''Palette

bitsToShade :: Bool -> Bool -> Shade
bitsToShade False False = White
bitsToShade False True  = LightGray
bitsToShade True  False = DarkGray
bitsToShade True  True  = Black

decodeShade :: Palette -> Bool -> Bool -> Shade
decodeShade pal False False = pal ^. color0
decodeShade pal False True  = pal ^. color1
decodeShade pal True  False = pal ^. color2
decodeShade pal True  True  = pal ^. color3

getBackgroundPalette :: Gameboy -> IO Palette
getBackgroundPalette gb = do { byte <- getMemory 0xFF47 gb
                             ; let color0_ = bitsToShade (testBit byte 1) (testBit byte 0)
                                   color1_ = bitsToShade (testBit byte 3) (testBit byte 2)
                                   color2_ = bitsToShade (testBit byte 5) (testBit byte 4)
                                   color3_ = bitsToShade (testBit byte 7) (testBit byte 6)
                               in return $ LcdPalette
                               color0_
                               color1_
                               color2_
                               color3_ }

getObjectPalette :: Gameboy -> Word16 -> IO Palette
getObjectPalette gb w = do { byte <- getMemory w gb
                           ; let color0_ = Transparent
                                 color1_ = bitsToShade (testBit byte 3) (testBit byte 2)
                                 color2_ = bitsToShade (testBit byte 5) (testBit byte 4)
                                 color3_ = bitsToShade (testBit byte 7) (testBit byte 6)
                             in return $ LcdPalette
                                color0_
                                color1_
                                color2_
                                color3_ }

getObjectPalette0 :: Gameboy -> IO Palette
getObjectPalette0 gb = getObjectPalette gb 0xFF48

getObjectPalette1 :: Gameboy -> IO Palette
getObjectPalette1 gb = getObjectPalette gb 0xFF49

getScrollX :: Gameboy -> IO Word8
getScrollX = getMemory 0xFF43

setScrollX :: Word8 -> (Gameboy -> IO Gameboy)
setScrollX = setMemory 0xFF43

getScrollY :: Gameboy -> IO Word8
getScrollY = getMemory 0xFF42

setScrollY :: Word8 -> (Gameboy -> IO Gameboy)
setScrollY = setMemory 0xFF42

getWindowX :: Gameboy -> IO Word8
getWindowX = getMemory 0xFF4B

setWindowX :: Word8 -> (Gameboy -> IO Gameboy)
setWindowX = setMemory 0xFF4B

getWindowY :: Gameboy -> IO Word8
getWindowY = getMemory 0xFF4A

setWindowY :: Word8 -> (Gameboy -> IO Gameboy)
setWindowY = setMemory 0xFF4A

getLY :: Gameboy -> IO Word8
getLY = getMemory 0xFF44

-- | TODO Writing to this register will reset the value.
  -- Come up with way to intercept internal writes from the cpu.
setLY :: Word8 -> (Gameboy -> IO Gameboy)
setLY = setMemory 0xFF44 . \x -> x `mod` 155

-- | TODO Replace the word16 in tile with this data structure.
  -- Requires writing an instance of the appropriate TypeClass in IOUArray.
data TileLine =
  TileLine
  {
    _pixels :: IOUArray Integer Shade
  }
makeLenses ''TileLine

data Tile =
  Tile
  {
    _tileLines :: IOUArray Integer Word16
  }
makeLenses ''Tile

data Display =
  Display
  {
    _tiles :: IOUArray Integer Tile
  }
makeLenses ''Display

-- | TODO maybe replace this if it turns out tuples aren't O(1).
  -- Because this update is going to be happening 60 * 8 * 32 * 32 times a second.
byteToShades :: Palette -> Word8 -> (Shade, Shade, Shade, Shade)
byteToShades pal byte = ((decodeShade pal (testBit byte 7) (testBit byte 6)),
                         (decodeShade pal (testBit byte 5) (testBit byte 4)),
                         (decodeShade pal (testBit byte 3) (testBit byte 2)),
                         (decodeShade pal (testBit byte 1) (testBit byte 0)))

updateTileLine :: Tile -> Word16 -> Integer  -> Gameboy -> IO ()
updateTileLine tile addr i gb = do { b1 <- getMemory addr gb
                                           ; b2 <- getMemory (addr + 1) gb
                                           ; writeArray pix i $ combineData b1 b2 }

  where pix = tile ^. tileLines

updateTile :: Tile -> Word16 -> Gameboy -> IO ()
updateTile tile addr gb = mapM_ (\i -> updateTileLine tile (addr + i) (fromIntegral i) gb) [0..7]

