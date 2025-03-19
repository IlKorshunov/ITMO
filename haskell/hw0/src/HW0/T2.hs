module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

-- a -> (a -> void) -> void
doubleNeg :: a -> Not (Not a)
doubleNeg input f = f input

-- (((a -> void) -> void) -> void) -> a -> void
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f input = f $ doubleNeg input