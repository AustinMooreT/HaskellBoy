{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}

module Lcd (module Lcd) where

import Cpu
import Memory
import Lib

import Graphics.Gloss
import Control.Lens
import Control.Monad
import Data.Word
import Data.Bits
import Data.Array.IO
import Data.Array.MArray
import Data.Array.IArray
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr

-- | Data structure reprsenting the memory bank
  -- used by the lcd.
  -- First 16 bit word is the start address;
  -- While the second is end address.
data LcdMemoryBank = Bank Word16 Word16

-- | Used to determine wether or not sprites are 16x8 or 8x8
data SpriteSize = Tall | Short

-- | Represents the control register for the LCD.
  -- lcdEnabled - Determines wether or not the lcd is on/off.
    -- If off render white screen.
  -- windowTileMapSelect - Determines memory bank for window tileMap.
  -- windowEnabled - Determines wether or not the lcd draws the window buffer.
  -- bgWindowTileSelect - TODO figure out what this is.
  -- spriteSize - 8x8 or 16x8 sprites.
  -- spritesEnabled - determines wether or not the lcd draws sprites.
  -- bgWidnowDisplayPriority - TODO figure out what this is.
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

-- | Fetches the LcdControl structure from it's address in address space.
getLcdControl :: Memory -> IO LcdControl
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


-- | LcdMode represents the four given states the lcd can be in.
  -- HBlank - Horizontal blanking period interrupt is triggered.
  -- VBlank - Vertical blanking period interrupt is triggered.
  -- OamSearch - TODO figure out what this is.
  -- LcdTransfer - TODO figure out what this is.
data LcdMode = HBlank | VBlank | OamSearch | LcdTransfer

-- | LcdStatus represents the overall status of the lcd.
  -- coincidenceInterruptEnabled - TODO figure out what this is.
  -- mode2OamInterruptEnabled - TODO figure out what this is.
  -- mode0HBlankInterruptEnabled - TODO figure out what this is.
  -- coincindenceFlag - TODO figure out what this is.
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

-- | Converts the value from two given bits to an LcdMode
twoBitsToMode :: Bool -> Bool -> LcdMode
twoBitsToMode False False = HBlank
twoBitsToMode False True  = VBlank
twoBitsToMode True  False = OamSearch
twoBitsToMode True  True  = LcdTransfer

-- | Given some memory fetches the current LcdStatus register
 -- state from the memory.
getLcdStatus :: Memory -> IO LcdStatus
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

-- | TODO Redocument this.
  -- I think if you pass in what bit you want 0th or 1st where
  -- 0 is false and 1 is true then a LcdMode it returns what you should set
  -- that bit to.
getModeBit :: Bool -> LcdMode -> Bool
getModeBit True  HBlank      = False
getModeBit False HBlank      = False
getModeBit True  VBlank      = False
getModeBit False VBlank      = True
getModeBit True  OamSearch   = True
getModeBit False OamSearch   = False
getModeBit True  LcdTransfer = True
getModeBit False LcdTransfer = True

-- | Coverts the LcdStatus structure to a byte for storing it back
  -- in memory.
lcdStatusToByte :: LcdStatus -> Word8
lcdStatusToByte l = maybeSetBit8 (l ^. coincidenceInterruptEnabled) 6 .
                    maybeSetBit8 (l ^. mode2OamInterruptEnabled)    5 .
                    maybeSetBit8 (l ^. mode1VBlankInterruptEnabled) 4 .
                    maybeSetBit8 (l ^. mode0HBlankInterruptEnabled) 3 .
                    maybeSetBit8 (l ^. coincidenceFlag)             2 .
                    maybeSetBit8 (getModeBit True  (l ^. modeFlag)) 1 .
                    maybeSetBit8 (getModeBit False (l ^. modeFlag)) 0 $ 0b10000000

-- | Set's the LcdStatus register in address space to whatever a local data structure has.
setLcdStatus :: LcdStatus -> (Memory -> IO Memory)
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

getBackgroundPalette :: Memory -> IO Palette
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

getObjectPalette :: Memory -> Word16 -> IO Palette
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

getObjectPalette0 :: Memory -> IO Palette
getObjectPalette0 gb = getObjectPalette gb 0xFF48

getObjectPalette1 :: Memory -> IO Palette
getObjectPalette1 gb = getObjectPalette gb 0xFF49

getScrollX :: Memory -> IO Word8
getScrollX = getMemory 0xFF43

setScrollX :: Word8 -> (Memory -> IO Memory)
setScrollX = setMemory 0xFF43

getScrollY :: Memory -> IO Word8
getScrollY = getMemory 0xFF42

setScrollY :: Word8 -> (Memory -> IO Memory)
setScrollY = setMemory 0xFF42

getWindowX :: Memory -> IO Word8
getWindowX = getMemory 0xFF4B

setWindowX :: Word8 -> (Memory -> IO Memory)
setWindowX = setMemory 0xFF4B

getWindowY :: Memory -> IO Word8
getWindowY = getMemory 0xFF4A

setWindowY :: Word8 -> (Memory -> IO Memory)
setWindowY = setMemory 0xFF4A

getLY :: Memory -> IO Word8
getLY = getMemory 0xFF44

-- | TODO Writing to this register will reset the value.
  -- Come up with way to intercept internal writes from the cpu.
setLY :: Word8 -> (Memory -> IO Memory)
setLY = setMemory 0xFF44 . \x -> x `mod` 155

data GlossDisplayBuffer =
  GlossDisplayBuffer
  {
    _arrPtr :: Ptr Word8,
    _forPtr :: ForeignPtr Word8,
    _width  :: Int,
    _height :: Int
  }
makeLenses ''GlossDisplayBuffer

mainBuffer :: IO GlossDisplayBuffer
mainBuffer = do { p <- mallocArray 92160
                ; f <- newForeignPtr finalizerFree p :: IO (ForeignPtr Word8)
                ; return $ GlossDisplayBuffer p f 160 144 }


byteToScanlineOffset :: Word8 -> Int
byteToScanlineOffset b = (fromIntegral b) * 640

convertShadeToRGBA :: Shade -> [Word8]
convertShadeToRGBA White     = [255, 255, 255, 255]
convertShadeToRGBA LightGray = [192, 192, 192, 255]
convertShadeToRGBA DarkGray  = [96 , 96 , 96 , 255]
convertShadeToRGBA Black     = [0  , 0  , 0  , 255]

renderScanline :: [Shade] -> Word8 -> GlossDisplayBuffer -> IO ()
renderScanline s l b = pokeArray (plusPtr (b ^. arrPtr) (byteToScanlineOffset l))
                       $ foldl (++) [] $ map convertShadeToRGBA s

displayGlossBuffer :: GlossDisplayBuffer -> Bool -> IO ()
displayGlossBuffer b False = display
                             (InWindow "BestWindow" (b ^. width, b ^. height) (0,0)) white
                             (bitmapOfForeignPtr (b ^. width) (b ^. height)
                               (BitmapFormat TopToBottom PxRGBA) (b ^. forPtr) False)
displayGlossBuffer b True  = display
                             (InWindow "BestWindow" (1000,1000) (0,0)) white
                             (scale 5.0 5.0 $ bitmapOfForeignPtr (b ^. width) (b ^. height)
                               (BitmapFormat TopToBottom PxRGBA) (b ^. forPtr) False)

