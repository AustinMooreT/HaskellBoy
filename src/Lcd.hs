{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Lcd (module Lcd) where

import Lib
import Memory

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

{- | BEGIN TILE -}

-- | Data structure representing a row in a tile.
data TileRow =
  TileRow
  {
    _shade0 :: Shade,
    _shade1 :: Shade,
    _shade2 :: Shade,
    _shade3 :: Shade,
    _shade4 :: Shade,
    _shade5 :: Shade,
    _shade6 :: Shade,
    _shade7 :: Shade
  }
makeLenses ''TileRow

-- | Data structure representing a tile.
data Tile =
  Tile
  {
    _tileRow0 :: TileRow,
    _tileRow1 :: TileRow,
    _tileRow2 :: TileRow,
    _tileRow3 :: TileRow,
    _tileRow4 :: TileRow,
    _tileRow5 :: TileRow,
    _tileRow6 :: TileRow,
    _tileRow7 :: TileRow
  }
makeLenses ''Tile

-- | Fetches a tile row from an address in ram.
getTileRow :: Word16 -> Memory -> (Palette -> IO TileRow)
getTileRow addr mem = \pal -> do { bh <- getMemory addr mem
                                 ; bl <- getMemory (addr + 1) mem
                                 ; return $ TileRow
                                   (bitsToShade (testBit bl 7) (testBit bh 7) pal)
                                   (bitsToShade (testBit bl 6) (testBit bh 6) pal)
                                   (bitsToShade (testBit bl 5) (testBit bh 5) pal)
                                   (bitsToShade (testBit bl 4) (testBit bh 4) pal)
                                   (bitsToShade (testBit bl 3) (testBit bh 3) pal)
                                   (bitsToShade (testBit bl 2) (testBit bh 2) pal)
                                   (bitsToShade (testBit bl 1) (testBit bh 1) pal)
                                   (bitsToShade (testBit bl 0) (testBit bh 0) pal) }

-- | Fetches a tile from an address in ram.
getTile :: Word16 -> Memory -> (Palette -> IO Tile)
getTile addr mem = \pal -> do { r0 <- getTileRow addr mem pal
                              ; r1 <- getTileRow (addr + 2)  mem pal
                              ; r2 <- getTileRow (addr + 4)  mem pal
                              ; r3 <- getTileRow (addr + 6)  mem pal
                              ; r4 <- getTileRow (addr + 8)  mem pal
                              ; r5 <- getTileRow (addr + 10) mem pal
                              ; r6 <- getTileRow (addr + 12) mem pal
                              ; r7 <- getTileRow (addr + 14) mem pal
                              ; return $ Tile r0 r1 r2 r3 r4 r5 r6 r7 }

-- | Converts a word to a functor for indexing the tile rows of a tile.
wordToTileRowLens :: Word8 -> Lens' Tile TileRow
wordToTileRowLens 0 = tileRow0
wordToTileRowLens 1 = tileRow1
wordToTileRowLens 2 = tileRow2
wordToTileRowLens 3 = tileRow3
wordToTileRowLens 4 = tileRow4
wordToTileRowLens 5 = tileRow5
wordToTileRowLens 6 = tileRow6
wordToTileRowLens 7 = tileRow7
wordToTileRowLens n = wordToTileRowLens $ n `mod` 8

-- | Converts a word to a functor for indexing the shades of a tile row.
wordToShadeLens :: Word8 -> Lens' TileRow Shade
wordToShadeLens 0 = shade0
wordToShadeLens 1 = shade1
wordToShadeLens 2 = shade2
wordToShadeLens 3 = shade3
wordToShadeLens 4 = shade4
wordToShadeLens 5 = shade5
wordToShadeLens 6 = shade6
wordToShadeLens 7 = shade7
wordToShadeLens n = wordToShadeLens $ n `mod` 8

{- ^ END TILE -}

{- | BEGIN RENDERING BACKGROUND MAP - NOTE Let's not talk about how ugly this is. -}

-- | This data structure represents a row in the BackgroundMap.
data BackgroundMapRow =
  BackgroundMapRow
  {
    _tile0  :: Tile,
    _tile1  :: Tile,
    _tile2  :: Tile,
    _tile3  :: Tile,
    _tile4  :: Tile,
    _tile5  :: Tile,
    _tile6  :: Tile,
    _tile7  :: Tile,
    _tile8  :: Tile,
    _tile9  :: Tile,
    _tile10 :: Tile,
    _tile11 :: Tile,
    _tile12 :: Tile,
    _tile13 :: Tile,
    _tile14 :: Tile,
    _tile15 :: Tile,
    _tile16 :: Tile,
    _tile17 :: Tile,
    _tile18 :: Tile,
    _tile19 :: Tile,
    _tile20 :: Tile,
    _tile21 :: Tile,
    _tile22 :: Tile,
    _tile23 :: Tile,
    _tile24 :: Tile,
    _tile25 :: Tile,
    _tile26 :: Tile,
    _tile27 :: Tile,
    _tile28 :: Tile,
    _tile29 :: Tile,
    _tile30 :: Tile,
    _tile31 :: Tile
  }
makeLenses ''BackgroundMapRow

-- | This data structure represents the whole gameboy's background map in memory.
data BackgroundMap =
  BackgroundMap
  {
    _bgRow0  :: BackgroundMapRow,
    _bgRow1  :: BackgroundMapRow,
    _bgRow2  :: BackgroundMapRow,
    _bgRow3  :: BackgroundMapRow,
    _bgRow4  :: BackgroundMapRow,
    _bgRow5  :: BackgroundMapRow,
    _bgRow6  :: BackgroundMapRow,
    _bgRow7  :: BackgroundMapRow,
    _bgRow8  :: BackgroundMapRow,
    _bgRow9  :: BackgroundMapRow,
    _bgRow10 :: BackgroundMapRow,
    _bgRow11 :: BackgroundMapRow,
    _bgRow12 :: BackgroundMapRow,
    _bgRow13 :: BackgroundMapRow,
    _bgRow14 :: BackgroundMapRow,
    _bgRow15 :: BackgroundMapRow,
    _bgRow16 :: BackgroundMapRow,
    _bgRow17 :: BackgroundMapRow,
    _bgRow18 :: BackgroundMapRow,
    _bgRow19 :: BackgroundMapRow,
    _bgRow20 :: BackgroundMapRow,
    _bgRow21 :: BackgroundMapRow,
    _bgRow22 :: BackgroundMapRow,
    _bgRow23 :: BackgroundMapRow,
    _bgRow24 :: BackgroundMapRow,
    _bgRow25 :: BackgroundMapRow,
    _bgRow26 :: BackgroundMapRow,
    _bgRow27 :: BackgroundMapRow,
    _bgRow28 :: BackgroundMapRow,
    _bgRow29 :: BackgroundMapRow,
    _bgRow30 :: BackgroundMapRow,
    _bgRow31 :: BackgroundMapRow
  }
makeLenses ''BackgroundMap

-- | Fetches a row in the background map.
getBackgroundMapRow :: TileMapMemoryBank -> AddressingMode -> Memory -> (Palette -> IO BackgroundMapRow)
getBackgroundMapRow (Bank start _ _) (adrMode, _) mem = \pal -> do { t0  <- getMemory (start) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t1  <- getMemory (start + 1)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t2  <- getMemory (start + 2)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t3  <- getMemory (start + 3)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t4  <- getMemory (start + 4)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t5  <- getMemory (start + 5)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t6  <- getMemory (start + 6)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t7  <- getMemory (start + 7)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t8  <- getMemory (start + 8)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t9  <- getMemory (start + 9)  mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t10 <- getMemory (start + 10) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t11 <- getMemory (start + 11) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t12 <- getMemory (start + 12) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t13 <- getMemory (start + 13) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t14 <- getMemory (start + 14) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t15 <- getMemory (start + 15) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t16 <- getMemory (start + 16) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t17 <- getMemory (start + 17) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t18 <- getMemory (start + 18) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t19 <- getMemory (start + 19) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t20 <- getMemory (start + 20) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t21 <- getMemory (start + 21) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t22 <- getMemory (start + 22) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t23 <- getMemory (start + 23) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t24 <- getMemory (start + 24) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t25 <- getMemory (start + 25) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t26 <- getMemory (start + 26) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t27 <- getMemory (start + 27) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t28 <- getMemory (start + 28) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t29 <- getMemory (start + 29) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t30 <- getMemory (start + 30) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; t31 <- getMemory (start + 31) mem >>= \x -> getTile (adrMode x) mem pal
                                                                   ; return $ BackgroundMapRow t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
                                                                     t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 }

-- | Fetches the entire BackgroundMap.
getBackgroundMap :: TileMapMemoryBank -> AddressingMode -> Memory -> (Palette -> IO BackgroundMap)
getBackgroundMap (Bank start stop b0) (adrMode, b1) mem = \pal -> do { r0  <- getBackgroundMapRow (Bank start stop b0)             (adrMode, b1) mem pal
                                                                     ; r1  <- getBackgroundMapRow (Bank (start + 32) stop b0)      (adrMode, b1) mem pal
                                                                     ; r2  <- getBackgroundMapRow (Bank (start + (32*2)) stop b0)  (adrMode, b1) mem pal
                                                                     ; r3  <- getBackgroundMapRow (Bank (start + (32*3)) stop b0)  (adrMode, b1) mem pal
                                                                     ; r4  <- getBackgroundMapRow (Bank (start + (32*4)) stop b0)  (adrMode, b1) mem pal
                                                                     ; r5  <- getBackgroundMapRow (Bank (start + (32*5)) stop b0)  (adrMode, b1) mem pal
                                                                     ; r6  <- getBackgroundMapRow (Bank (start + (32*6)) stop b0)  (adrMode, b1) mem pal
                                                                     ; r7  <- getBackgroundMapRow (Bank (start + (32*7)) stop b0)  (adrMode, b1) mem pal
                                                                     ; r8  <- getBackgroundMapRow (Bank (start + (32*8)) stop b0)  (adrMode, b1) mem pal
                                                                     ; r9  <- getBackgroundMapRow (Bank (start + (32*9)) stop b0)  (adrMode, b1) mem pal
                                                                     ; r10 <- getBackgroundMapRow (Bank (start + (32*10)) stop b0) (adrMode, b1) mem pal
                                                                     ; r11 <- getBackgroundMapRow (Bank (start + (32*11)) stop b0) (adrMode, b1) mem pal
                                                                     ; r12 <- getBackgroundMapRow (Bank (start + (32*12)) stop b0) (adrMode, b1) mem pal
                                                                     ; r13 <- getBackgroundMapRow (Bank (start + (32*13)) stop b0) (adrMode, b1) mem pal
                                                                     ; r14 <- getBackgroundMapRow (Bank (start + (32*14)) stop b0) (adrMode, b1) mem pal
                                                                     ; r15 <- getBackgroundMapRow (Bank (start + (32*15)) stop b0) (adrMode, b1) mem pal
                                                                     ; r16 <- getBackgroundMapRow (Bank (start + (32*16)) stop b0) (adrMode, b1) mem pal
                                                                     ; r17 <- getBackgroundMapRow (Bank (start + (32*17)) stop b0) (adrMode, b1) mem pal
                                                                     ; r18 <- getBackgroundMapRow (Bank (start + (32*18)) stop b0) (adrMode, b1) mem pal
                                                                     ; r19 <- getBackgroundMapRow (Bank (start + (32*19)) stop b0) (adrMode, b1) mem pal
                                                                     ; r20 <- getBackgroundMapRow (Bank (start + (32*20)) stop b0) (adrMode, b1) mem pal
                                                                     ; r21 <- getBackgroundMapRow (Bank (start + (32*21)) stop b0) (adrMode, b1) mem pal
                                                                     ; r22 <- getBackgroundMapRow (Bank (start + (32*22)) stop b0) (adrMode, b1) mem pal
                                                                     ; r23 <- getBackgroundMapRow (Bank (start + (32*23)) stop b0) (adrMode, b1) mem pal
                                                                     ; r24 <- getBackgroundMapRow (Bank (start + (32*24)) stop b0) (adrMode, b1) mem pal
                                                                     ; r25 <- getBackgroundMapRow (Bank (start + (32*25)) stop b0) (adrMode, b1) mem pal
                                                                     ; r26 <- getBackgroundMapRow (Bank (start + (32*26)) stop b0) (adrMode, b1) mem pal
                                                                     ; r27 <- getBackgroundMapRow (Bank (start + (32*27)) stop b0) (adrMode, b1) mem pal
                                                                     ; r28 <- getBackgroundMapRow (Bank (start + (32*28)) stop b0) (adrMode, b1) mem pal
                                                                     ; r29 <- getBackgroundMapRow (Bank (start + (32*29)) stop b0) (adrMode, b1) mem pal
                                                                     ; r30 <- getBackgroundMapRow (Bank (start + (32*30)) stop b0) (adrMode, b1) mem pal
                                                                     ; r31 <- getBackgroundMapRow (Bank (start + (32*31)) stop b0) (adrMode, b1) mem pal
                                                                     ; return $ BackgroundMap r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15
                                                                     r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31 }

-- TODO add documentation
getBackgroundMapFromLcd :: Lcd -> Memory -> IO BackgroundMap
getBackgroundMapFromLcd lcd mem = getBackgroundMap
                                  (lcd ^. lcdControl . bgTileMapSelect)
                                  (lcd ^. lcdControl . bgWindowTileSelect)
                                  mem
                                  (lcd ^. bgPalette)

-- | Get's the appropriate tile row from the background map based on scroll y
indexBackgroundMapRow :: ScrollY -> BackgroundMap -> BackgroundMapRow
indexBackgroundMapRow 0   bm = bm ^. bgRow0
indexBackgroundMapRow 8   bm = bm ^. bgRow1
indexBackgroundMapRow 16  bm = bm ^. bgRow2
indexBackgroundMapRow 24  bm = bm ^. bgRow3
indexBackgroundMapRow 32  bm = bm ^. bgRow4
indexBackgroundMapRow 40  bm = bm ^. bgRow5
indexBackgroundMapRow 48  bm = bm ^. bgRow6
indexBackgroundMapRow 56  bm = bm ^. bgRow7
indexBackgroundMapRow 64  bm = bm ^. bgRow8
indexBackgroundMapRow 72  bm = bm ^. bgRow9
indexBackgroundMapRow 80  bm = bm ^. bgRow10
indexBackgroundMapRow 88  bm = bm ^. bgRow11
indexBackgroundMapRow 96  bm = bm ^. bgRow12
indexBackgroundMapRow 104 bm = bm ^. bgRow13
indexBackgroundMapRow 112 bm = bm ^. bgRow14
indexBackgroundMapRow 120 bm = bm ^. bgRow15
indexBackgroundMapRow 128 bm = bm ^. bgRow16
indexBackgroundMapRow 136 bm = bm ^. bgRow17
indexBackgroundMapRow 144 bm = bm ^. bgRow18
indexBackgroundMapRow 152 bm = bm ^. bgRow19
indexBackgroundMapRow 160 bm = bm ^. bgRow20
indexBackgroundMapRow 168 bm = bm ^. bgRow21
indexBackgroundMapRow 176 bm = bm ^. bgRow22
indexBackgroundMapRow 184 bm = bm ^. bgRow23
indexBackgroundMapRow 192 bm = bm ^. bgRow24
indexBackgroundMapRow 200 bm = bm ^. bgRow25
indexBackgroundMapRow 208 bm = bm ^. bgRow26
indexBackgroundMapRow 216 bm = bm ^. bgRow27
indexBackgroundMapRow 224 bm = bm ^. bgRow28
indexBackgroundMapRow 232 bm = bm ^. bgRow29
indexBackgroundMapRow 240 bm = bm ^. bgRow30
indexBackgroundMapRow 248 bm = bm ^. bgRow31
indexBackgroundMapRow n   bm = indexBackgroundMapRow (n - (n `mod` 8)) bm

-- | Get's the appropriate tile from a background map row based on scroll x
indexBackgroundMapTile :: ScrollX -> BackgroundMapRow -> Tile
indexBackgroundMapTile 0   bm = bm ^. tile0
indexBackgroundMapTile 8   bm = bm ^. tile1
indexBackgroundMapTile 16  bm = bm ^. tile2
indexBackgroundMapTile 24  bm = bm ^. tile3
indexBackgroundMapTile 32  bm = bm ^. tile4
indexBackgroundMapTile 40  bm = bm ^. tile5
indexBackgroundMapTile 48  bm = bm ^. tile6
indexBackgroundMapTile 56  bm = bm ^. tile7
indexBackgroundMapTile 64  bm = bm ^. tile8
indexBackgroundMapTile 72  bm = bm ^. tile9
indexBackgroundMapTile 80  bm = bm ^. tile10
indexBackgroundMapTile 88  bm = bm ^. tile11
indexBackgroundMapTile 96  bm = bm ^. tile12
indexBackgroundMapTile 104 bm = bm ^. tile13
indexBackgroundMapTile 112 bm = bm ^. tile14
indexBackgroundMapTile 120 bm = bm ^. tile15
indexBackgroundMapTile 128 bm = bm ^. tile16
indexBackgroundMapTile 136 bm = bm ^. tile17
indexBackgroundMapTile 144 bm = bm ^. tile18
indexBackgroundMapTile 152 bm = bm ^. tile19
indexBackgroundMapTile 160 bm = bm ^. tile20
indexBackgroundMapTile 168 bm = bm ^. tile21
indexBackgroundMapTile 176 bm = bm ^. tile22
indexBackgroundMapTile 184 bm = bm ^. tile23
indexBackgroundMapTile 192 bm = bm ^. tile24
indexBackgroundMapTile 200 bm = bm ^. tile25
indexBackgroundMapTile 208 bm = bm ^. tile26
indexBackgroundMapTile 216 bm = bm ^. tile27
indexBackgroundMapTile 224 bm = bm ^. tile28
indexBackgroundMapTile 232 bm = bm ^. tile29
indexBackgroundMapTile 240 bm = bm ^. tile30
indexBackgroundMapTile 248 bm = bm ^. tile31
indexBackgroundMapTile n   bm = indexBackgroundMapTile (n - (n `mod` 8)) bm

-- | Given values for the scrollx and scrolly registers grabs the shade at that pixel for the background map.
indexBackgroundMap :: ScrollX -> ScrollY -> BackgroundMap -> Shade
indexBackgroundMap x y bm = (indexBackgroundMapTile x (indexBackgroundMapRow y bm)) ^. (wordToTileRowLens y) . (wordToShadeLens x)

-- | Given values for the scrollx scrolly and ly fetches an lcd scanline from a background map.
renderLcdBgScanline :: ScrollX -> ScrollY -> LY -> BackgroundMap -> [Shade]
renderLcdBgScanline x y yoff bm = map (\z -> indexBackgroundMap (x + z) (y + yoff) bm) [0..159]

{- ^ END RENDERING BACKGROUND MAP -}


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

-- | Gicen a list of writes them to the display buffer.
renderBufferLine :: [Shade] -> Word8 -> DisplayBuffer -> IO DisplayBuffer
renderBufferLine s l b = (pokeArray (plusPtr (b ^. arrPtr) (byteToScanlineOffset l))
                          $ foldl (++) [] $ map convertShadeToRGBA s) >> return b

-- | Given some memory and a display buffer render a scanline
renderScanLine :: Memory -> DisplayBuffer -> IO DisplayBuffer
renderScanLine mem db = do { lcd <- getLcd mem
                           ; pal <- getBackgroundPalette mem
                           ; bgm <- getBackgroundMap (lcd ^. lcdControl . bgTileMapSelect) (lcd ^. lcdControl . bgWindowTileSelect) mem pal
                           ; renderBufferLine
                             (renderLcdBgScanline (lcd ^. scrollx) (lcd ^. scrolly) (lcd ^. ly) bgm)
                             (lcd ^. ly)
                             db }

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
