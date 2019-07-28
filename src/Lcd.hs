{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Lcd (module Lcd) where

import Lib
import Memory

import Numeric -- TODO remove this.

import Graphics.Gloss
import Control.Lens
import Data.Word
import Data.Bits
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr

{-
   Misc lcd info.
   The lcd display is 160x144 pixels. Where each pixel is one of four shades of grey.
   It can show a background and a window
   (the window is an additonal background that overlays the other og)
   It can display 40 sprites 10 per line of 8x8 or 8x16 sprites.
-}

{- | BEGIN BACKGROUND SCROLL -}

-- | X offset in BG buffer.
type ScrollX = Word8

-- | Fetch the scroll x lcd register.
getScrollX :: Memory -> IO ScrollX
getScrollX = getMemory 0xFF43

-- | Set the scroll x lcd register.
setScrollX :: ScrollX -> (Memory -> IO Memory)
setScrollX = setMemory 0xFF43

-- | Y offset in BG buffer.
type ScrollY = Word8

-- | Fetch the scroll y lcd register.
getScrollY :: Memory -> IO ScrollY
getScrollY = getMemory 0xFF42

-- | Set the scroll y lcd register.
setScrollY :: ScrollY -> (Memory -> IO Memory)
setScrollY = setMemory 0xFF42

{- ^ END BACKGROUND SCROLL -}

{- | BEGIN WINDOW SCROLL -}

-- | X offset in window buffer.
type WindowX = Word8

-- | get window x register.
getWindowX :: Memory -> IO WindowX
getWindowX = getMemory 0xFF4B


-- | set window x register.
setWindowX :: WindowX -> (Memory -> IO Memory)
setWindowX = setMemory 0xFF4B

-- | Y offset in Window buffer.
type WindowY = Word8

-- | get window y register.
getWindowY :: Memory -> IO WindowY
getWindowY = getMemory 0xFF4A

-- | set window y register.
setWindowY :: WindowY -> (Memory -> IO Memory)
setWindowY = setMemory 0xFF4A

{- ^ END WINDOW SCROLL -}

{- | BEGIN HORIZONTAL LINE COUNT -}

-- | Line number register.
type LY = Word8

-- | get the line number register.
getLY :: Memory -> IO LY
getLY = getMemory 0xFF44

-- | set's the line number register.
  -- NOTE I believe that setting the line number register just resets it.
  -- And that is implemented in the memory module.
setLY :: LY -> (Memory -> IO Memory)
setLY = setMemory 0xFF44

-- | Line number interrupt/control register.
type LYC = Word8

-- | Get the line number interrupt/control register.
getLYC :: Memory -> IO LYC
getLYC = getMemory 0xFF45

-- | Set the value at the line number interrupt/control register.
setLYC :: LYC -> (Memory -> IO Memory)
setLYC = setMemory 0xFF45

{- ^ END HORIZONTAL LINE COUNT-}

{- | BEGIN SPRITE SIZE -}

-- | Used to determine wether or not sprites are 16x8 or 8x8
data SpriteSize = Tall | Short

-- | Converts a sprite size to its corresponding bit
  -- for easy setting.
spriteSizeDecodeBit :: SpriteSize -> Bool
spriteSizeDecodeBit Tall  = True
spriteSizeDecodeBit Short = False

{- ^ END SPRITE SIZE -}

{- | BEGIN LCD CONTROL -}

-- | Addressing Mode's determine how an 8bit value is converted into an address.
  -- The Bool in the pair refers to writing the mode to memory.
  -- For example if bool is false then the addressing mode corresponds to a 0 bit in some byte.
type AddressingMode = (Word8 -> Word16, Bool)

-- | This function is a selector for the addressing mode on the LCD control register.
  -- The boolean value is wether or not the bit is set or reset.
addressingMode :: Bool -> AddressingMode
addressingMode False = (\offset -> if offset > 127 then
                                    0x8800 + ((toWord16 $ (offset - 128)) * 16)
                                  else
                                    0x9000 + ((toWord16 offset) * 16), False)
addressingMode True  = (\offset -> 0x8000 + ((toWord16 offset) * 16), True)

-- | Represents a chunk of memory for the lcd reserved for a specific purpose.
data TileMapMemoryBank =
  Bank
  {
    _bankStart :: Word16,
    _bankStop  :: Word16,
    _bankBit   :: Bool
  }
makeLenses ''TileMapMemoryBank

-- | Represents the control register for the LCD.
  -- lcdEnabled - Determines wether or not the lcd is on/off.
    -- If off render white screen.
  -- windowTileMapSelect - Determines memory bank for window tileMap.
  -- windowEnabled - Determines wether or not the lcd draws the window buffer.
  -- bgWindowTileSelect - Determines what addressing mode to use for fetching bg/window tiles.
  -- bgTileMapSelect - determines memory bank for bgTileMap
  -- spriteSize - 8x8 or 16x8 sprites.
  -- spritesEnabled - determines wether or not the lcd draws sprites.
  -- bgEnabled - Enables/Disables the background
    -- Disabled Background is white.
data LcdControl =
  LcdControl
  {
    _lcdEnabled          :: Bool,
    _windowTileMapSelect :: TileMapMemoryBank,
    _windowEnabled       :: Bool,
    _bgWindowTileSelect  :: AddressingMode,
    _bgTileMapSelect     :: TileMapMemoryBank,
    _spriteSize          :: SpriteSize,
    _spritesEnabled      :: Bool,
    _bgEnabled           :: Bool
  }
makeLenses ''LcdControl

-- | Fetches the LcdControl structure from it's address in address space.
getLcdControl :: Memory -> IO LcdControl
getLcdControl gb = do { byte <- getMemory 0xFF40 gb
                      ; let lcdEnabled_     = testBit byte 7
                            windowTilemap   = if testBit byte 6 then
                                                Bank 0x9C00 0x9FFF True
                                              else
                                                Bank 0x9800 0x9BFF False
                            windowEnabled_  = testBit byte 5
                            bgWindowTile    = addressingMode $ testBit byte 4
                            bgTileMap       = if testBit byte 3 then
                                                Bank 0x9C00 0x9FFF True
                                              else
                                                Bank 0x9800 0x9BFF False
                            spriteSize_     = if testBit byte 2 then
                                                Tall
                                              else
                                                Short
                            spritesEnabled_ = testBit byte 1
                            bgEnabled_      = testBit byte 0
                        in return $ LcdControl
                           lcdEnabled_
                           windowTilemap
                           windowEnabled_
                           bgWindowTile
                           bgTileMap
                           spriteSize_
                           spritesEnabled_
                           bgEnabled_ }


-- | Converts the lcd control data structure into
  -- an 8 bit byte to be written to memory.
lcdControlToByte :: LcdControl -> Word8
lcdControlToByte lc = maybeSetBit8 (lc ^. lcdEnabled) 7                       .
                      maybeSetBit8 (lc ^. windowTileMapSelect . bankBit) 6    .
                      maybeSetBit8 (lc ^. windowEnabled) 5                    .
                      maybeSetBit8 (snd $ lc ^. bgWindowTileSelect) 4         .
                      maybeSetBit8 (lc ^. bgTileMapSelect . bankBit) 3        .
                      maybeSetBit8 (spriteSizeDecodeBit $ lc ^. spriteSize) 2 .
                      maybeSetBit8 (lc ^. spritesEnabled) 1                   .
                      maybeSetBit8 (lc ^. bgEnabled) 0 $ 0b00000000


-- | Writes the state of a given LcdControl register to memory.
setLcdControl :: LcdControl -> Memory -> IO Memory
setLcdControl l m = setMemory 0xFF40 (lcdControlToByte l) m

{- ^ END LCD CONTROL -}

{- | BEGIN LCD STATUS -}

-- | LcdMode represents the four given states the lcd can be in.
  -- HBlank - Horizontal blanking period interrupt is triggered.
  -- VBlank - Vertical blanking period interrupt is triggered.
  -- OamSearch - TODO figure out what this is.
  -- LcdTransfer - TODO figure out what this is.
data LcdMode = HBlank | VBlank | OamSearch | LcdTransfer deriving Eq

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

{- ^ END LCD STATUS -}

{- | BEGIN SHADE -}

-- | Shade of a given pixel.
data Shade = White | LightGray | DarkGray | Black | Transparent
  deriving (Show)

-- | Convert two bits to a shade.
bitsToColor :: Bool -> Bool -> Shade
bitsToColor False False = White
bitsToColor False True  = LightGray
bitsToColor True False  = DarkGray
bitsToColor True True   = Black

-- | Given a shade convert it to a tuple of the coresponding bits.
colorToBits :: Shade -> (Bool, Bool)
colorToBits White       = (False, False)
colorToBits LightGray   = (False, True)
colorToBits DarkGray    = (True, False)
colorToBits Black       = (True, True)
colorToBits Transparent = (False, False) -- NOTE This was just a hack.

{- ^ END SHADE -}

{- | BEGIN PALETTE -}

-- | Palette used to determine the shade of a pixel.
data Palette =
  LcdPalette
  {
    _color0 :: Shade,
    _color1 :: Shade,
    _color2 :: Shade,
    _color3 :: Shade
  } deriving (Show)
makeLenses ''Palette

-- | Takes two booleans to the appropriate Shade
  -- based on the current Palette selected.
bitsToShade :: Bool -> Bool -> Palette -> Shade
bitsToShade False False p = p ^. color0
bitsToShade False True  p = p ^. color1
bitsToShade True False  p = p ^. color2
bitsToShade True True   p = p ^. color3

-- | Converts a byte to a Palette
byteToPalette :: Word8 -> Palette
byteToPalette w8 = LcdPalette
                   (bitsToColor (testBit w8 1) (testBit w8 0))
                   (bitsToColor (testBit w8 3) (testBit w8 2))
                   (bitsToColor (testBit w8 5) (testBit w8 4))
                   (bitsToColor (testBit w8 7) (testBit w8 6))

-- | Converts a palette to it's corresponding byte representation.
paletteToByte :: Palette -> Word8
paletteToByte pl = setBitIf 0 b0 .
                   setBitIf 1 b1 .
                   setBitIf 2 b2 .
                   setBitIf 3 b3 .
                   setBitIf 4 b4 .
                   setBitIf 5 b5 .
                   setBitIf 6 b6 .
                   setBitIf 7 b7 $ 0
  where
    c0 = colorToBits (pl ^. color0)
    c1 = colorToBits (pl ^. color1)
    c2 = colorToBits (pl ^. color2)
    c3 = colorToBits (pl ^. color3)
    b0 = snd c0
    b1 = fst c0
    b2 = snd c1
    b3 = fst c1
    b4 = snd c2
    b5 = fst c2
    b6 = snd c3
    b7 = fst c3

-- | Fetches the palette for background tiles.
getBackgroundPalette :: Memory -> IO Palette
getBackgroundPalette mem = do { byte <- getMemory 0xFF47 mem
                              ; return $ byteToPalette byte }

-- | Given a background palette data structure set the memory address to the same values.
setBackgroundPalette :: Palette -> (Memory -> IO Memory)
setBackgroundPalette pl = \mem -> setMemory 0xFF47 (paletteToByte pl) mem

-- | Fetches the first obj sprite palette.
getSprite1Palette :: Memory -> IO Palette
getSprite1Palette mem = do { byte <- getMemory 0xFF48 mem
                           ; return $ (byteToPalette byte) & color0 .~ Transparent }

-- | Given a sprite palette sp1 set the memory address to the same values.
  -- NOTE this may not work due to sprites having transparent color3 palette values.
setSprite1Palette :: Palette -> (Memory -> IO Memory)
setSprite1Palette pl = \mem -> setMemory 0xFF48 (paletteToByte pl) mem

-- | Fetches the second obj sprite palette.
getSprite2Palette :: Memory -> IO Palette
getSprite2Palette mem = do { byte <- getMemory 0xFF49 mem
                           ; return $ (byteToPalette byte) & color0 .~ Transparent }

-- | Given a sprite pallete sp2 set the memory address to the same values.
  -- NOTE this may not work due to sprites having transparent color3 palette values.
setSprite2Palette :: Palette -> (Memory -> IO Memory)
setSprite2Palette pl = \mem -> setMemory 0xFF49 (paletteToByte pl) mem

{- ^ END PALETTE -}

{- | BEGIN GLOBAL LCD TODO @ Maurice
 Data structure needs palettes and getters and setters. -}

-- | Data structure representing the Gameboy's LCD controller.
data Lcd =
  Lcd
  {
    _lcdControl    :: LcdControl,
    _lcdStatus     :: LcdStatus,
    _scrollx       :: ScrollX,
    _scrolly       :: ScrollY,
    _windowx       :: WindowX,
    _windowy       :: WindowY,
    _ly            :: LY,
    _lyc           :: LYC,
    _bgPalette     :: Palette,
    _sprt1Palette  :: Palette,
    _sprt2Palette  :: Palette
  }
makeLenses ''Lcd

-- | Fetches an lcd data structure from memory.
getLcd :: Memory -> IO Lcd
getLcd mem = do { cntrl    <- getLcdControl mem
                ; status   <- getLcdStatus mem
                ; scrlx    <- getScrollX mem
                ; scrly    <- getScrollY mem
                ; winx     <- getWindowX mem
                ; winy     <- getWindowY mem
                ; __ly     <- getLY mem
                ; __lyc    <- getLYC mem
                ; bgPal    <- getBackgroundPalette mem
                ; sprt1Pal <- getSprite1Palette mem
                ; sprt2Pal <- getSprite2Palette mem
                ; return $ Lcd cntrl status scrlx scrly winx winy __ly __lyc bgPal sprt1Pal sprt2Pal }

-- | Given an lcd data structure set it's data in memory.
setLcd :: Lcd -> (Memory -> IO Memory)
setLcd lcd = \mem -> setLcdControl        (lcd ^. lcdControl)   mem >>
                     setLcdStatus         (lcd ^. lcdStatus)    mem >>
                     setScrollX           (lcd ^. scrollx)      mem >>
                     setScrollY           (lcd ^. scrolly)      mem >>
                     setWindowX           (lcd ^. windowx)      mem >>
                     setWindowY           (lcd ^. windowy)      mem >>
                     setLY                (lcd ^. ly)           mem >>
                     setLYC               (lcd ^. lyc)          mem >>
                     setBackgroundPalette (lcd ^. bgPalette)    mem >>
                     setSprite1Palette    (lcd ^. sprt1Palette) mem >>
                     setSprite2Palette    (lcd ^. sprt2Palette) mem >>
                     return mem

{- ^ END GLOBAL LCD -}

{- | BEGIN LCD STATE TRANSITIONS -}

-- | Transitions the lcd mode to the next value in the drawing loop.
modeTransition :: Lcd -> Lcd
modeTransition lcd
  | lcd ^. lcdStatus . modeFlag == OamSearch   = lcd & lcdStatus . modeFlag .~ LcdTransfer
  | lcd ^. lcdStatus . modeFlag == LcdTransfer = lcd & lcdStatus . modeFlag .~ HBlank
  | lcd ^. lcdStatus . modeFlag == VBlank      = lcd & lcdStatus . modeFlag .~ OamSearch
  | otherwise                                  = if lcd ^. ly == 144 then
                                                   lcd & lcdStatus . modeFlag .~ VBlank
                                                 else
                                                   lcd & lcdStatus . modeFlag .~ OamSearch

-- | Updates the ly counter for every line drawn.
lyUpdate :: Lcd -> Lcd
lyUpdate lcd
  | lcd ^. lcdStatus . modeFlag == HBlank = lcd & ly %~ (+1)
  | lcd ^. lcdStatus . modeFlag == VBlank = lcd & ly .~ 0
  | otherwise                             = lcd


-- | Steps the lcd forward as a whole
stepLcd :: Lcd -> Lcd
stepLcd lcd = modeTransition . lyUpdate $ lcd


{- ^ END LCD STATE TRANSITIONS -}

{- | BEGIN SCANLINE RENDERING -}

-- | Fetches a tile row from an address in ram.
getTileRow :: Word16 -> Memory -> Word8 -> Word8 -> Palette -> IO [Shade]
getTileRow  addr mem start stop = \pal -> do { bh <- getMemory addr mem
                                             ; bl <- getMemory (addr + 1) mem
                                             ; return $ reverse (map
                                                                 (\n -> bitsToShade (testBit bl n) (testBit bh n) pal)
                                                                 [(fromIntegral start') .. (fromIntegral stop')]) }
  where start' = start `mod` 8
        stop'  = stop `mod` 8

getScanlineFromX :: ScrollX -> Memory -> Palette -> [Word16 -> IO [Shade]]
getScanlineFromX scx mem pal = map (\(x,y) addr -> getTileRow addr mem x y pal) ((scxm,7):replicate 19 (0,7) ++ [(0, 7-scxm)])
  where scxm = scx `mod` 8
        scxo = (scx - scxm) `div` 8


-- | Given values for the scrollx scrolly and ly fetches an lcd scanline from a background map.
  -- NOTE/TODO foldl (++) is O(n^2) fix this.
renderLcdBgScanline :: ScrollX -> ScrollY -> LY -> TileMapMemoryBank -> AddressingMode -> Memory -> Palette -> IO [Shade]
renderLcdBgScanline x y yoff (Bank start _ _) (adrMode, bo) mem pal = mapM (\(l,m) -> l >>= \l' -> m $ l')
                                                                      (zip
                                                                       (map (\l -> getMemory (baseAddr + l) mem >>= \q -> return $ adrMode q) [0 .. 19])
                                                                       tileLines) >>= \o -> return $ concat o
  where x'        = toWord16 ((x - (x `mod` 8)) `div` 8)
        y'        = toWord16 (((y + yoff) - ((y + yoff) `mod` 8)) `div` 8)
        baseAddr  = ((start + 32*y') + x')
        tileLines = getScanlineFromX x mem pal

{- ^ END SCANLINE RENDERING -}


{- | BEGIN DISPLAY BUFFER -}

-- | Internal data structure representing the internal framebuffer we are drawing to.
data DisplayBuffer =
  DisplayBuffer
  {
    _arrPtr :: Ptr Word8,
    _forPtr :: ForeignPtr Word8,
    _width  :: Int,
    _height :: Int
  }
makeLenses ''DisplayBuffer

-- | Default Frame buffer for standard gameboy.
mainBuffer :: IO DisplayBuffer
mainBuffer = do { p <- mallocArray 92160
                ; f <- newForeignPtr finalizerFree p :: IO (ForeignPtr Word8)
                ; return $ DisplayBuffer p f 160 144 }

-- | Converts a shade data type to the corresponding byte array for that color.
convertShadeToRGBA :: Shade -> [Word8]
convertShadeToRGBA White     = [255, 255, 255, 255]
convertShadeToRGBA LightGray = [192, 192, 192, 255]
convertShadeToRGBA DarkGray  = [96 , 96 , 96 , 255]
convertShadeToRGBA Black     = [0  , 0  , 0  , 255]

byteToScanlineOffset :: Word8 -> Int
byteToScanlineOffset b = (fromIntegral b) * 640

-- | Given a list of writes them to the display buffer.
renderBufferLine :: [Shade] -> Word8 -> DisplayBuffer -> IO DisplayBuffer
renderBufferLine s l b = (sequence $ map (\(x,y) -> pokeArray (plusPtr (b ^. arrPtr) ((byteToScanlineOffset l) + y)) x)
                          (map (\(x,y) -> (x,4*y)) (zip shades [0..(length shades)]))) >> return b
  where
    shades = (map convertShadeToRGBA s)

-- | Given some memory and a display buffer render a scanline
renderScanLine :: Memory -> DisplayBuffer -> IO DisplayBuffer
renderScanLine mem db = do { lcd <- getLcd mem
                           ; pal <- getBackgroundPalette mem
                           ; rl  <- renderLcdBgScanline
                                    (lcd ^. scrollx)
                                    (lcd ^. scrolly)
                                    (lcd ^. ly)
                                    (lcd ^. lcdControl . bgTileMapSelect)
                                    (lcd ^. lcdControl . bgWindowTileSelect) mem pal
                           ; renderBufferLine rl (lcd ^. ly) db }

-- Why in the world did I name a function foo.
-- God help me.
foo mem db = do { _    <- sequence $ (map (\_ -> renderScanLine mem db) [0..143])
                ; displayGlossBuffer db True }

-- | Draws the current internal buffer to the screen.
  -- if b is true then the resolution is the same as the original gameboy.
  -- if b is false scale the resolution by 1000 times.
  -- TODO @ Maurice : Add in the ability to scale by any factor.
displayGlossBuffer :: DisplayBuffer -> Bool -> IO ()
displayGlossBuffer b False = display
                             (InWindow "BestWindow" (b ^. width, b ^. height) (0,0)) white
                             (bitmapOfForeignPtr (b ^. width) (b ^. height)
                               (BitmapFormat TopToBottom PxRGBA) (b ^. forPtr) False)
displayGlossBuffer b True  = display
                             (InWindow "BestWindow" (1000,1000) (0,0)) white
                             (scale 5.0 5.0 $ bitmapOfForeignPtr (b ^. width) (b ^. height)
                               (BitmapFormat TopToBottom PxRGBA) (b ^. forPtr) False)


{- ^ END DISPLAY BUFFER -}

-- | NOTE/TODO Looking at this on 05/17/2019, and have no idea what it's for.
  -- Looks like I was starting to make a binary tree. I won't remove it yet for fear of realizing I needed it.
data DList = NULL | Node DList DList
