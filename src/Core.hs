{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Core (module Core) where

import Cpu
import qualified Memory as Mem
import Lib
import Lcd

import Data.Word
import Control.Lens
import Data.Array.IO as IOA
import Data.Bits
import Control.Monad


-- | Represents a gameboy.
data Gameboy =
  Gameboy
  {
    _cpu     :: Cpu,
    _memory  :: Mem.Memory,
    _clock   :: Integer,
    _opWait  :: Integer, --time left until currOp is performed on gameboy.
    _currOp  :: Gameboy -> IO Gameboy, --the current gameboy op.
    _ime     :: Bool, -- This is the master interrupt flag.
    _halt    :: Bool, -- Flag keeping track of wether or not execution is halted.
    _lcd     :: Lcd,
    _dispBuf :: DisplayBuffer
  }
makeLenses ''Gameboy


-- | Default gameboy used on startup.
defaultGameboy :: IO Gameboy
defaultGameboy = do { mem <- Mem.defaultMemory
                    ; l   <- getLcd mem
                    ; dsp <- mainBuffer
                    ; return $ Gameboy
                      defaultCpu --cpu
                      mem --memory
                      0 --current clock
                      0 --op wait
                      (\gb -> return gb) --current op
                      False -- master interrupt flag.
                      False -- halt flag.
                      l
                      dsp
                    }

-- | adds two registers r1 and r2 together and stores the value in r1 and sets the appropriate flags.
addRegWithRegWithFlags :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithFlags r1 r2 gb = addWithFlags8 (getRegister r1 gb) (getRegister r2 gb) (\d -> (\gb1 -> setRegister r1 d gb)) gb


-- | adds two registers r1 and r2 together and stores the value in r1 and sets the appropriate flags.
addRegWithRegWithFlagsPlusC :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithFlagsPlusC r1 r2 gb = addWithFlags8PlusC (getRegister r1 gb) (getRegister r2 gb) (getFlag gb carryFlag)
                                       (\d -> (\gb1 -> setRegister r1 d gb)) gb


-- | adds two register together r1 and r2 and stores the value in r1 and sets no flags.
addRegWithRegWithoutFlags :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithoutFlags r1 r2 gb = setRegister r1 ((getRegister r1 gb) + (getRegister r2 gb)) gb


-- | adds two register pairs together rs1 and rs2 and stores the result in rs1 and sets the appropriate flags.
addRegRegWithRegRegWithFlags :: (Register, Register) -> (Register, Register) -> (Gameboy -> Gameboy)
addRegRegWithRegRegWithFlags rs1 rs2 gb = addWithFlags16 (getRegisters rs1 gb) (getRegisters rs2 gb) (\d -> (\gb1 -> setRegisters rs1 d gb1)) gb


-- | adds two register pairs together rs1 and rs2 and stores the result in rs1 and sets no flags.
addRegRegWithRegRegWithoutFlags :: (Register, Register) -> (Register, Register) -> (Gameboy -> Gameboy)
addRegRegWithRegRegWithoutFlags rs1 rs2 gb = setRegisters rs1 ((getRegisters rs1 gb) + (getRegisters rs2 gb)) gb


-- | subs r2 from r1 and stores the result back in r1
subRegWithRegWithFlags :: Register -> Register -> (Gameboy -> Gameboy)
subRegWithRegWithFlags r1 r2 gb = subWithFlags8 (getRegister r1 gb) (getRegister r2 gb) (\d -> (\gb1 -> setRegister r1 d gb)) gb


-- | subs r2 from r1 and stores the result back in r1
subRegWithRegWithFlagsPlusC :: Register -> Register -> (Gameboy -> Gameboy)
subRegWithRegWithFlagsPlusC r1 r2 gb = subWithFlags8PlusC (getRegister r1 gb) (getRegister r2 gb) (getFlag gb carryFlag)
                                       (\d -> (\gb1 -> setRegister r1 d gb)) gb


-- | given a gameboy and a flag constant return wether or not it is set.
-- TODO change the api to Word8 -> Bool -> Gameboy pls.
getFlag :: Gameboy -> Word8 -> Bool
getFlag gb w = testBit (getRegister F gb) (flagToInt w)


-- | Given a gameboy rotate it's accumulator to the left and store the 7th bit in the carry.
rotateLeftACarry :: (Gameboy -> Gameboy)
rotateLeftACarry = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) .
                   (\gb -> setFlag carryFlag (testBit (getRegister A gb) 0) gb) .
                   (\gb -> setRegister A (rotateL (getRegister A gb) 1) gb)


-- | Rotate a register to the left and set the carry appropriately TODO check logic.
rotateLeft :: Register -> (Gameboy -> Gameboy)
rotateLeft r gb = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) $
                 (\gb1 -> (setRegister r (((getRegister r gb1) .&. 0b11111110) .|. carryBeforeMask) gb1)) $
                 setFlag carryFlag carryAfter $
                 setRegister r (rotateL (getRegister r gb) 1) gb
  where
    carryBefore = getFlag gb carryFlag
    carryAfter  = testBit (getRegister r gb) 7
    carryBeforeMask = if carryBefore then 0b00000001 else 0b00000000


-- | Rotate a register to the left and set the carry appropriately TODO check logic.
rotateRight :: Register -> (Gameboy -> Gameboy)
rotateRight r gb = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) $
                  (\gb1 -> (setRegister r (((getRegister r gb1) .&. 0b11111110) .|. carryBeforeMask) gb1)) $
                  setFlag carryFlag carryAfter $
                  setRegister r (rotateR (getRegister r gb) 1) gb
  where
    carryBefore = getFlag gb carryFlag
    carryAfter  = testBit (getRegister r gb) 0
    carryBeforeMask = if carryBefore then 0b10000000 else 0b00000000


-- | Rotate left but for the accumulator.
rotateLeftA :: Gameboy -> Gameboy
rotateLeftA = rotateLeft A


-- | Rotate right but for the accumulator.
rotateRightA :: Gameboy -> Gameboy
rotateRightA = rotateRight A


-- | Rotate accumlator right and set the carry flag tot he value in the 0th bit.
rotateRightACarry :: (Gameboy -> Gameboy)
rotateRightACarry = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) .
                    (\gb -> setFlag carryFlag (testBit (getRegister A gb) 7) gb) .
                    (\gb -> setRegister A (rotateR (getRegister A gb) 1) gb)

-- TODO everything below this comment needs documentation.

xorReg :: Register -> (Gameboy -> Gameboy)
xorReg r gb = setRegister A (xor (getRegister A gb) (getRegister r gb)) gb

andReg :: Register -> (Gameboy -> Gameboy)
andReg r gb = setRegister A ((.&.) (getRegister A gb) (getRegister r gb)) gb

orReg :: Register -> (Gameboy -> Gameboy)
orReg r gb = setRegister A ((.|.) (getRegister A gb) (getRegister r gb)) gb

ldFFRegAddrReg :: Register -> Register -> (Gameboy -> IO Gameboy)
ldFFRegAddrReg d s gb = setMemory (0xFF00 + (toWord16 $ getRegister d gb)) (getRegister s gb) gb

--TODO reimplement this using do notation.
ldFFAndMemOffsetWithA :: Gameboy -> IO Gameboy
ldFFAndMemOffsetWithA gb = join (\mem -> return $ setMemory (0xFF00 + (toWord16 mem)) (getRegister A gb1) gb1) =<<
                           getMemory (getRegisters (PHI,CLO) gb1) gb1
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

ldAWithFFAndMemOffset :: Gameboy -> IO Gameboy
ldAWithFFAndMemOffset gb = do { offset <- getMemory (getRegisters (PHI,CLO) gb1) gb1
                              ; value  <- getMemory (0xFF00 + (toWord16 offset)) gb1
                              ; return $ setRegister A value gb1 }
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

(.:) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c
f .: g = liftM f . g

push :: Word16 -> (Gameboy -> IO Gameboy)
push d gb = join (\gb_ -> return $ pushByte dhi gb_) =<< (pushByte dlo gb)
  where
    dhi = toWord8 $ shiftR d 8
    dlo = toWord8 $ 0x00FF .&. d
    pushByte = \b  -> decrementRegistersWithoutFlags (SHI, PLO) .:
               (\gb -> setMemory (getRegisters (SHI, PLO) gb) b gb)

pop :: (Register, Register) -> (Gameboy -> IO Gameboy)
pop rs gb = do { mem1 <- getMemory (getRegisters (SHI, PLO) gb1) gb
               ; mem2 <- getMemory (getRegisters (SHI, PLO) gb2) gb
               ; let combinedMem = combineData mem1 mem2
                 in return $ setRegisters rs combinedMem gb2}
  where gb1 = incrementRegistersWithoutFlags (SHI, PLO) gb
        gb2 = incrementRegistersWithoutFlags (SHI, PLO) gb1


--TODO Combine emu data makes me very uncomfortable here. I'm doing something wrong.
call :: Gameboy -> IO Gameboy
call gb = do { mem1 <- getMemory (getRegisters (PHI, CLO) gb1) gb1
             ; mem2 <- getMemory (getRegisters (PHI, CLO) gb2) gb2
             ; let combinedMem = combineData mem2 mem1
               in do { pushedGb <- push (getRegisters (PHI, CLO) gb3) gb
                     ; let setGb = setRegisters (PHI, CLO) combinedMem pushedGb
                       in return $ decrementRegistersWithoutFlags (PHI, CLO) setGb }}
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb
    gb2 = incrementRegistersWithoutFlags (PHI, CLO) gb1
    gb3 = incrementRegistersWithoutFlags (PHI, CLO) gb2

ret :: Gameboy -> IO Gameboy
ret gb = do { mem1 <- getMemory (getRegisters (SHI, PLO) gb1) gb
            ; mem2 <- getMemory (getRegisters (SHI, PLO) gb2) gb
            ; let combinedMem = combineData mem1 mem2
              in return $ setRegisters (PHI, CLO) (combinedMem - 1) gb2 }
   where
     gb1 = incrementRegistersWithoutFlags (SHI, PLO) gb
     gb2 = incrementRegistersWithoutFlags (SHI, PLO) gb1

--TODO double check logic
ldMemDataWithRegReg :: (Register, Register) -> (Gameboy -> IO Gameboy)
ldMemDataWithRegReg (r1, r2) gb = do { mem1 <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                                     ; mem2 <- getMemory (getRegisters (PHI, CLO) gb2) gb2
                                     ; let combinedMem = combineData mem1 mem2
                                       in  do { gbMem1 <- setMemory combinedMem (getRegister r2 gb2) gb2
                                              ; setMemory (combinedMem + 1) (getRegister r1 gbMem1) gbMem1 }}
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb
    gb2 = incrementRegistersWithoutFlags (PHI, CLO) gb1

ldMemDataWithReg :: Register -> (Gameboy -> IO Gameboy)
ldMemDataWithReg r gb = do { mem1 <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                           ; mem2 <- getMemory (getRegisters (PHI, CLO) gb2) gb2
                           ; let combinedMem = combineData mem1 mem2
                             in setMemory combinedMem (getRegister r gb2) gb2 }
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb
    gb2 = incrementRegistersWithoutFlags (PHI, CLO) gb1

jumpRelative :: (Gameboy -> IO Gameboy)
jumpRelative gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                     ; let signedJmp = wordToSignedInt mem
                       in return $ setRegisters (PHI, CLO)
                       (fromIntegral ((fromIntegral $ getRegisters (PHI, CLO) gb1) + signedJmp)) gb1 }
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

jumpIfRelative :: Bool -> (Gameboy -> IO Gameboy)
jumpIfRelative False gb = return $ incrementRegistersWithoutFlags (PHI, CLO) gb
jumpIfRelative True  gb = jumpRelative gb



--TODO daa sets flags apparently.
daa :: (Gameboy -> Gameboy)
daa gb = setRegister A (regA + calculateHigh + calculateLow) gb
    where
      regA          = getRegister A gb
      nibbleHigh    = regA .&. 0xF0
      nibbleLow     = regA .&. 0x0F
      calculateHigh = if (nibbleHigh > 144) || (getFlag gb carryFlag)
                      then 0x60
                      else 0x00
      calculateLow  = if (nibbleLow > 9) || (getFlag gb halfCarryFlag)
                      then 0x06
                      else 0x00

scf :: (Gameboy -> Gameboy)
scf = (setFlag carryFlag True) . (setFlag (halfCarryFlag .&. subtractFlag) False)

cpl :: (Gameboy -> Gameboy)
cpl gb = setRegister A (xor (getRegister A gb) 0xFF) gb

ccf :: (Gameboy -> Gameboy)
ccf gb = (setFlag carryFlag (not $ getFlag gb carryFlag)) . (setFlag (halfCarryFlag .&. subtractFlag) False) $ gb

addRegWithRegRegMemWithFlags :: Register -> (Register, Register) -> (Gameboy -> IO Gameboy)
addRegWithRegRegMemWithFlags r rs gb = do { mem <- getMemory (getRegisters rs gb) gb
                                          ; return $ addWithFlags8 (getRegister A gb) mem (\w -> setRegister A w) gb }

addRegWithRegRegMemWithFlagsPlusC :: Register -> (Register, Register) -> (Gameboy -> IO Gameboy)
addRegWithRegRegMemWithFlagsPlusC r rs gb = do { mem <- getMemory (getRegisters rs gb) gb
                                               ; return $ addWithFlags8PlusC (getRegister A gb) mem (getFlag gb carryFlag)
                                                 (\w -> setRegister A w) gb }

cp8 :: Gameboy -> IO Gameboy
cp8 gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb1) gb
            ; return $ subWithFlags8 (getRegister A gb) mem (\_ -> \gb2 -> gb2) gb1 }
  where gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

cpReg :: Register -> (Gameboy -> Gameboy)
cpReg r2 gb = subWithFlags8 (getRegister A gb) (getRegister r2 gb) (\_ -> \gb1 -> gb1) gb

fixGB :: (Gameboy -> Gameboy) -> (Gameboy -> IO Gameboy)
fixGB gb = (\gb_ -> return gb_ >>= (\gb__ -> return $ gb gb__))

testBitReg :: Register -> Int -> (Gameboy -> Gameboy)
testBitReg r i = \gb -> zero . sub . half $ gb
  where
    isSet = \gb -> testBit (getRegister r gb) i
    zero  = \gb -> (setFlag zeroFlag $ (not $ isSet gb)) $ gb
    sub   = setFlag subtractFlag  False
    half  = setFlag halfCarryFlag True

-- | increases the gameboys clock by i cycles.
increaseClock :: Integer -> Gameboy -> Gameboy
increaseClock i gb = gb & clock %~ (\x -> x + i)

