{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module Lcd (module Lcd) where

import Core
import Cpu
import Memory
import Lib

import Control.Lens
import Data.Word
import Data.Bits

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
getScrollX gb = getMemory 0xFF43 gb

getScrollY :: Gameboy -> IO Word8
getScrollY gb = getMemory 0xFF42 gb
