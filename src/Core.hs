{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Core (module Core) where

import Cpu
import Memory
import Lib

import Data.Word
import Control.Lens
import Data.Array.IO as IOA
import Data.Bits
import Control.Monad

--GAMEBOY
-- | Represents a gameboy.
data Gameboy =
  Gameboy
  {
    _cpu    :: Cpu,
    _memory :: Memory
  }
makeLenses ''Gameboy

--GAMEBOY
-- | Default gameboy used on startup.
defaultGameboy :: IO Gameboy
defaultGameboy = do { mem <- defaultMemory
                    ; return $ Gameboy defaultCpu mem }

--GAMEBOY
-- | Represents an instruction to the Gameboy's processor.
data Instruction =
  Instruction
  {
    _opcode    :: Word8,
    _name      :: String,
    _operation :: (Gameboy -> IO Gameboy)
  }
makeLenses ''Instruction

--GAMEBOY
-- | Instance of show for converting Instructions to a String.
instance Show Instruction where
  show instr = (show $ instr ^. opcode) Prelude.++ (show $ instr ^. name)

--GAMEBOY
-- | Uses 16 bit value addr to index and return an 8 bit value in memory.
getMemory :: Word16 -> Gameboy -> IO Word8
getMemory addr gb = let mem = view (memory . bytes) gb
                    in readArray mem addr

--GAMEBOY
-- | Uses 16 bit value addr as an index to set the element there to 8 bit value d.
setMemory :: Word16 -> Word8 -> (Gameboy -> IO Gameboy)
setMemory addr d gb = do { written <- (writeArray mem addr d)
                         ; return $ gb & memory .~ (Memory mem)}
  where
    mem = view (memory . bytes) gb

--GAMEBOY
-- | Sets the value in register r to some 8 bit value d.
setRegister :: Register -> Word8 -> Gameboy -> Gameboy
setRegister r d gb = gb & cpu . registerToLens r .~ d

--GAMEBOY
-- | Sets the value in the combined registers rs to a 16 bit value d.
setRegisters :: (Register, Register) -> Word16 -> Gameboy -> Gameboy
setRegisters rs d gb = gb & cpu . composeRegisterLenses rs .~ d

--GAMEBOY
-- | Fetches 8 bit value from register r.
getRegister :: Register -> Gameboy -> Word8
getRegister r gb = gb ^. cpu . registerToLens r

--GAMEBOY
-- | Fetches 16 bit combined value from the registers rs.
getRegisters :: (Register, Register) -> Gameboy -> Word16
getRegisters rs gb = gb ^. cpu . composeRegisterLenses rs

--GAMEBOY
-- | Loads data from src into dest.
ldRegWithReg :: Register -> Register -> (Gameboy -> Gameboy)
ldRegWithReg dest src gb = setRegister dest (getRegister src gb) gb

--GAMEBOY
-- | Using addr as an index grabs an 8 bit value from memory and loads it into reg.
ldRegWithMem :: Register -> Word16 -> (Gameboy -> IO Gameboy)
ldRegWithMem reg addr gb = do { mem <- getMemory addr gb
                              ; return $ setRegister reg mem gb }


--GAMEBOY
-- | Using the 16 bit value from the combined registers rs as an index
  -- grabs an 8 bit value from memory and loads it into r.
ldRegWithRegRegMem :: Register -> (Register, Register) -> (Gameboy -> IO Gameboy)
ldRegWithRegRegMem r rs gb = do { mem <- getMemory (getRegisters rs gb) gb
                                ; return $ setRegister r mem gb}

--GAMEBOY
-- | Using the combined register rs as an index set that location in memory to the value stored in r.
ldMemRegRegWithReg :: (Register, Register) -> Register -> (Gameboy -> IO Gameboy)
ldMemRegRegWithReg rs r gb = setMemory (getRegisters rs gb) (getRegister r gb) gb

--GAMEBOY
-- | Increments a register r's value by 1 while ignoring all flags.
incrementRegisterWithoutFlags :: Register -> (Gameboy -> Gameboy)
incrementRegisterWithoutFlags r gb = setRegister r (getRegister r gb + 1) gb

--GAMEBOY
-- | Increments the combined registers rs's value by 1.
incrementRegistersWithoutFlags :: (Register, Register) -> (Gameboy -> Gameboy)
incrementRegistersWithoutFlags rs gb = setRegisters rs (getRegisters rs gb + 1) gb

--GAMEBOY
-- | Using the 16 bit value stored in rs as an index set memory to the next byte from the program counter.
ldMemRegRegWithData :: (Register, Register) -> (Gameboy -> IO Gameboy)
ldMemRegRegWithData rs gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                               ; setMemory (getRegisters rs gb) mem gb1}
  where gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

--GAMEBOY
-- | Loads two registers r1 and r2 with data fetched from memory using the program counter.
ldRegRegWithData :: (Register, Register) -> (Gameboy -> IO Gameboy)
ldRegRegWithData rs gb = do { mem1 <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                            ; mem2 <- getMemory (getRegisters (PHI, CLO) gb2) gb2
                            ; return $ setRegisters rs (combineData mem2 mem1) gb2}
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb
    gb2 = incrementRegistersWithoutFlags (PHI, CLO) gb1


--GAMEBOY
-- | Loads a register r with data fetched from memory using the program counter.
ldRegWithData :: Register -> (Gameboy -> IO Gameboy)
ldRegWithData r gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                        ; return $ setRegister r mem gb1 }
  where
    gb1 = incrementRegistersWithoutFlags (PHI, CLO) gb

--GAMEBOY
-- | Set's a flag using one of the flag constants.
setFlag :: Word8 -> Bool -> Gameboy -> Gameboy
setFlag w True  gb = gb & cpu . registerToLens F %~ \w8 -> w8 .|. w
setFlag w False gb = gb & cpu . registerToLens F %~ \w8 -> w8 .&. complement w

--GAMEBOY
-- | If the byte passed in is 0 return a function setting the 0 flag appropriately.
setZero8 :: Word8 -> (Gameboy -> Gameboy)
setZero8 0 = setFlag zeroFlag True
setZero8 _ = setFlag zeroFlag False

--GAMEBOY
-- | If 16 bit value passed in is 0 return a function setting the 0 flag appropriately.
setZero16 :: Word16 -> (Gameboy -> Gameboy)
setZero16 0 = setFlag zeroFlag True
setZero16 _ = setFlag zeroFlag False

--GAMEBOY
-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowCarry8 :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setOverflowCarry8 sum addend = setFlag carryFlag (sum < addend)

--GAMEBOY
-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowCarry16 :: Word16 -> Word16 -> (Gameboy -> Gameboy)
setOverflowCarry16 sum addend = setFlag carryFlag (sum < addend)

--GAMEBOY
-- | Check wether or not the sum overflows a the least significant nibble from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowHalfCarry8 :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setOverflowHalfCarry8 sum addend = setFlag halfCarryFlag ((sum .&. 0xf) < (addend .&. 0xf))

--GAMEBOY
-- | Check wether or not the sum overflows a the least significant nibble from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setOverflowHalfCarry16 :: Word16 -> Word16 -> (Gameboy -> Gameboy)
setOverflowHalfCarry16 sum addend = setFlag halfCarryFlag ((sum .&. 0xfff ) < (addend .&. 0xfff))

--GAMEBOY
-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags8 :: Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
incrementWithFlags8 byte store = store increment .
                                 halfCarry .
                                 subtractf .
                                 zero
  where
    increment = byte + 1
    zero      = \gb -> setZero8 increment gb
    halfCarry = \gb -> setOverflowHalfCarry8 increment byte gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY
-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags8IO :: Word8 -> (Word8 -> (Gameboy -> IO Gameboy)) -> (Gameboy -> IO Gameboy)
incrementWithFlags8IO byte store = store increment .
                                 halfCarry .
                                 subtractf .
                                 zero
  where
    increment = byte + 1
    zero      = \gb -> setZero8 increment gb
    halfCarry = \gb -> setOverflowHalfCarry8 increment byte gb
    subtractf = \gb -> setFlag subtractFlag False gb


--GAMEBOY
-- | increment a byte and set all the flags associated with it.
  -- Store is a function that will take the incremented byte and put it back in the gameboy,
  -- and set all the associated flags at the same time.
incrementWithFlags16 :: Word16 -> (Word16 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
incrementWithFlags16 byte store = store increment .
                                  halfCarry .
                                  subtractf .
                                  zero
  where
    increment = byte + 1
    zero      = \gb -> setZero16 increment gb
    halfCarry = \gb -> setOverflowHalfCarry16 increment byte gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY
-- | Increments the value stored in a given register and sets the associated flags.
incrementRegisterWithFlags :: Register -> (Gameboy -> Gameboy)
incrementRegisterWithFlags r gb = incrementWithFlags8 (getRegister r gb) (\d -> (\gb1 -> (setRegister r d gb1))) gb

--GAMEBOY
-- | Increments the value stored in a register pair.
incrementRegistersWithFlags :: (Register, Register) -> (Gameboy -> Gameboy)
incrementRegistersWithFlags rs gb = incrementWithFlags16 (getRegisters rs gb) (\d -> (\gb1 -> (setRegisters rs d gb1))) gb

--GAMEBOY
-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setUnderflowCarry8 :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setUnderflowCarry8 difference subtrahend = setFlag carryFlag (difference > subtrahend)

--GAMEBOY
-- | Check wether or not the sum overflows from an addition operation.
  -- Sum is the result and addend is any addend from the addition.
setUnderflowCarry16 :: Word16 -> Word16 -> (Gameboy -> Gameboy)
setUnderflowCarry16 difference subtrahend = setFlag carryFlag (difference > subtrahend)

--GAMEBOY
-- | Checks wether the difference underflows from a subtraction opperation by looking at one of the subtrahends.
setUnderflowHalfCarry8 :: Word8 -> Word8 -> (Gameboy -> Gameboy)
setUnderflowHalfCarry8 difference subtrahend = setFlag halfCarryFlag ((difference .&. 0xf) > (subtrahend .&. 0xf))

--GAMEBOY
-- | Checks wether the difference underflows from a subtraction opperation by looking at one of the subtrahends.
setUnderflowHalfCarry16 :: Word16 -> Word16 -> (Gameboy -> Gameboy)
setUnderflowHalfCarry16 difference subtrahend = setFlag halfCarryFlag ((difference .&. 0xfff) > (subtrahend .&. 0xfff))

--GAMEBOY
-- | Decrement byte with flags
decrementWithFlags8 :: Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
decrementWithFlags8 byte store = store decrement .
                                 zero .
                                 halfCarry .
                                 subtractf
  where
    decrement = byte - 1
    zero      = \gb -> setZero8 decrement gb
    halfCarry = \gb -> setUnderflowHalfCarry8 decrement byte gb
    subtractf = \gb -> setFlag subtractFlag True gb

--GAMEBOY
-- | Decrement byte with flags
decrementWithFlags8IO :: Word8 -> (Word8 -> (Gameboy -> IO Gameboy)) -> (Gameboy -> IO Gameboy)
decrementWithFlags8IO byte store = store decrement .
                                 zero .
                                 halfCarry .
                                 subtractf
  where
    decrement = byte - 1
    zero      = \gb -> setZero8 decrement gb
    halfCarry = \gb -> setUnderflowHalfCarry8 decrement byte gb
    subtractf = \gb -> setFlag subtractFlag True gb

--GAMEBOY
-- | Decrement byte with flags
decrementWithFlags16 :: Word16 -> (Word16 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
decrementWithFlags16 byte store = store decrement .
                                  zero .
                                  halfCarry .
                                  subtractf
  where
    decrement = byte - 1
    zero      = \gb -> setZero16 decrement gb
    halfCarry = \gb -> setUnderflowHalfCarry16 decrement byte gb
    subtractf = \gb -> setFlag subtractFlag True gb

--GAMEBOY
-- | decrement a register and set the associated flags.
decrementRegisterWithFlags :: Register -> (Gameboy -> Gameboy)
decrementRegisterWithFlags r gb= decrementWithFlags8 (getRegister r gb) (\d -> (\gb1 -> setRegister r d gb1)) gb

--GAMEBOY
-- | decrement a set of registers and set the associated flags.
decrementRegisters :: (Register, Register) -> (Gameboy -> Gameboy)
decrementRegisters rs gb = decrementWithFlags16 (getRegisters rs gb) (\d -> (\gb1 -> setRegisters rs d gb1)) gb

--GAMEBOY
-- | decrement a register and don't set the associated flags.
decrementRegisterWithoutFlags :: Register -> (Gameboy -> Gameboy)
decrementRegisterWithoutFlags r gb = setRegister r ((getRegister r gb) - 1) gb

--GAMEBOY
-- | decrement a set of registers and don't set the associated flags.
decrementRegistersWithoutFlags :: (Register, Register) -> (Gameboy -> Gameboy)
decrementRegistersWithoutFlags rs gb = setRegisters rs ((getRegisters rs gb) - 1) gb

--GAMEBOY
-- | Increments a value in memory by indexing with a register pair.
incrementMemoryRegReg :: (Register, Register) -> (Gameboy -> IO Gameboy)
incrementMemoryRegReg rs gb  = do { mem <- getMemory (getRegisters rs gb) gb
                                  ; let incMem = incrementWithFlags8IO mem
                                        storeMem = \d -> \gb_ -> setMemory (getRegisters rs gb_) d gb
                                    in incMem storeMem gb }

--GAMEBOY
-- | decrement a value in memory by indexing with a register pair.
decrementMemoryRegReg :: (Register, Register) -> (Gameboy -> IO Gameboy)
decrementMemoryRegReg rs gb = do { mem <- getMemory (getRegisters rs gb) gb
                                 ; let decMem = decrementWithFlags8IO mem
                                       storeMem = \d -> \gb_ -> setMemory (getRegisters rs gb_) d gb_
                                   in decMem storeMem gb }

--GAMEBOY
-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags8 :: Word8 -> Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
addWithFlags8 e1 e2 f = f addition .
                       carry .
                       halfCarry .
                       subtractf .
                       zero
  where
    addition  = e1 + e2
    zero      = \gb -> setZero8 addition gb
    carry     = \gb -> setOverflowCarry8 addition e2 gb
    halfCarry = \gb -> setOverflowHalfCarry8 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag False gb


boolToWord :: Bool -> Word8
boolToWord True = 1
boolToWord _ = 0

--GAMEBOY
-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags8PlusC :: Word8 -> Word8 -> Bool -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
addWithFlags8PlusC e1 e2 b f = f addition .
                       carry .
                       halfCarry .
                       subtractf .
                       zero
  where
    addition  = e1 + e2 + (boolToWord b)
    zero      = \gb -> setZero8 addition gb
    carry     = \gb -> setOverflowCarry8 addition e2 gb
    halfCarry = \gb -> setOverflowHalfCarry8 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY
-- | adds e1 and e2 together and sets all apropriate flags using f to store the value back in the gameboy.
addWithFlags16 :: Word16 -> Word16 -> (Word16 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
addWithFlags16 e1 e2 f = f addition .
                       carry .
                       halfCarry .
                       subtractf
  where
    addition  = e1 + e2
    carry     = \gb -> setOverflowCarry16 addition e2 gb
    halfCarry = \gb -> setOverflowHalfCarry16 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag False gb

--GAMEBOY
-- | subtracts e2 from e1 and uses f to store the result and all of the state changes back in a gb.
subWithFlags8 :: Word8 -> Word8 -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
subWithFlags8 e1 e2 f = f addition .
                       carry .
                       halfCarry .
                       subtractf .
                       zero
  where
    addition  = e1 - e2
    zero      = \gb -> setZero8 addition gb
    carry     = \gb -> setUnderflowCarry8 addition e2 gb
    halfCarry = \gb -> setUnderflowHalfCarry8 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag True gb


--TODO comment this.
subWithFlags8PlusC :: Word8 -> Word8 -> Bool -> (Word8 -> (Gameboy -> Gameboy)) -> (Gameboy -> Gameboy)
subWithFlags8PlusC e1 e2 b f = f addition .
                       carry .
                       halfCarry .
                       subtractf .
                       zero
  where
    addition  = (e1 + (boolToWord b)) - e2
    zero      = \gb -> setZero8 addition gb
    carry     = \gb -> setUnderflowCarry8 addition e2 gb
    halfCarry = \gb -> setUnderflowHalfCarry8 addition e2 gb
    subtractf = \gb -> setFlag subtractFlag True gb


--GAMEBOOY
-- | adds two registers r1 and r2 together and stores the value in r1 and sets the appropriate flags.
addRegWithRegWithFlags :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithFlags r1 r2 gb = addWithFlags8 (getRegister r1 gb) (getRegister r2 gb) (\d -> (\gb1 -> setRegister r1 d gb)) gb

--GAMEBOY
-- | adds two registers r1 and r2 together and stores the value in r1 and sets the appropriate flags.
addRegWithRegWithFlagsPlusC :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithFlagsPlusC r1 r2 gb = addWithFlags8PlusC (getRegister r1 gb) (getRegister r2 gb) (getFlag gb carryFlag)
                                       (\d -> (\gb1 -> setRegister r1 d gb)) gb

--GAMEBOY
-- | adds two register together r1 and r2 and stores the value in r1 and sets no flags.
addRegWithRegWithoutFlags :: Register -> Register -> (Gameboy -> Gameboy)
addRegWithRegWithoutFlags r1 r2 gb = setRegister r1 ((getRegister r1 gb) + (getRegister r2 gb)) gb

--GAMEBOY
-- | adds two register pairs together rs1 and rs2 and stores the result in rs1 and sets the appropriate flags.
addRegRegWithRegRegWithFlags :: (Register, Register) -> (Register, Register) -> (Gameboy -> Gameboy)
addRegRegWithRegRegWithFlags rs1 rs2 gb = addWithFlags16 (getRegisters rs1 gb) (getRegisters rs2 gb) (\d -> (\gb1 -> setRegisters rs1 d gb1)) gb

--GAMEBOY
-- | adds two register pairs together rs1 and rs2 and stores the result in rs1 and sets no flags.
addRegRegWithRegRegWithoutFlags :: (Register, Register) -> (Register, Register) -> (Gameboy -> Gameboy)
addRegRegWithRegRegWithoutFlags rs1 rs2 gb = setRegisters rs1 ((getRegisters rs1 gb) + (getRegisters rs2 gb)) gb

--GAMEBOY
-- | subs r2 from r1 and stores the result back in r1
subRegWithRegWithFlags :: Register -> Register -> (Gameboy -> Gameboy)
subRegWithRegWithFlags r1 r2 gb = subWithFlags8 (getRegister r1 gb) (getRegister r2 gb) (\d -> (\gb1 -> setRegister r1 d gb)) gb

--GAMEBOY
-- | subs r2 from r1 and stores the result back in r1
subRegWithRegWithFlagsPlusC :: Register -> Register -> (Gameboy -> Gameboy)
subRegWithRegWithFlagsPlusC r1 r2 gb = subWithFlags8PlusC (getRegister r1 gb) (getRegister r2 gb) (getFlag gb carryFlag)
                                       (\d -> (\gb1 -> setRegister r1 d gb)) gb

--GAMEBOY
-- | given a gameboy and a flag constant return wether or not it is set.
-- TODO change the api to Word8 -> Bool -> Gameboy pls.
getFlag :: Gameboy -> Word8 -> Bool
getFlag gb w = testBit (getRegister F gb) (flagToInt w)

--GAMEBOY
-- | Given a gameboy rotate it's accumulator to the left and store the 7th bit in the carry.
rotateLeftACarry :: (Gameboy -> Gameboy)
rotateLeftACarry = (setFlag (zeroFlag .&. subtractFlag .&. halfCarryFlag) False) .
                   (\gb -> setFlag carryFlag (testBit (getRegister A gb) 0) gb) .
                   (\gb -> setRegister A (rotateL (getRegister A gb) 1) gb)

--GAMEBOY
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

--GAMEBOY
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

--GAMEBOY
-- | Rotate left but for the accumulator.
rotateLeftA :: Gameboy -> Gameboy
rotateLeftA = rotateLeft A

--GAMEBOY
-- | Rotate right but for the accumulator.
rotateRightA :: Gameboy -> Gameboy
rotateRightA = rotateRight A

--GAMEBOY
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

wordToSignedInt :: Word8 -> Int
wordToSignedInt w
  | testBit w 7 == True = - (fromIntegral $ (complement w) + 1)
  | otherwise = fromIntegral w

ldFFRegAddrReg :: Register -> Register -> (Gameboy -> IO Gameboy)
ldFFRegAddrReg d s gb = setMemory (0xFF00 + (toWord16 $ getRegister d gb)) (getRegister s gb) gb

ldhA :: Gameboy -> IO Gameboy
ldhA gb = join (\mem -> return $ setMemory (0xFF00 + (toWord16 mem)) (getRegister A gb1) gb1) =<< getMemory (getRegisters (PHI,CLO) gb1) gb1
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

jumpRelative :: (Gameboy -> IO Gameboy)
jumpRelative gb = do { mem <- getMemory (getRegisters (PHI, CLO) gb1) gb1
                     ; let signedJmp = wordToSignedInt mem
                       in return $ setRegisters (PHI, CLO)
                       (fromIntegral ((fromIntegral $ getRegisters (PHI, CLO) gb1) + signedJmp)) gb1 }
  where
    gb1 = incrementRegistersWithoutFlags (CLO, PHI) gb

jumpIfRelative :: Bool -> (Gameboy -> IO Gameboy)
jumpIfRelative False gb = return gb
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

cpReg :: Register -> (Gameboy -> Gameboy)
cpReg r2 gb = subWithFlags8 (getRegister A gb) (getRegister r2 gb) (\_ -> \gb1 -> gb1) gb

fixGB :: (Gameboy -> Gameboy) -> (Gameboy -> IO Gameboy)
fixGB gb = (\gb_ -> return gb_ >>= (\gb__ -> return $ gb gb__))



testBitReg :: Register -> Int -> (Gameboy -> Gameboy)
testBitReg r i = \gb -> zero . sub . half $ gb
  where
    isSet = \gb -> testBit (getRegister r gb) i
    zero  = \gb -> (setFlag zeroFlag $ isSet gb) $ gb
    sub   = setFlag subtractFlag  False
    half  = setFlag halfCarryFlag True
