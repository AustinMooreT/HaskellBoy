module Timing (module Timing) where

class Clocked a where
  (%++) :: a -> a --Advance the clock

data (Clocked a) => Clock a =
  Clock
  {
    _structure :: a,
    _transform :: a -> a,
    _clock     :: Integer,
    _clockWait :: Integer
  }
