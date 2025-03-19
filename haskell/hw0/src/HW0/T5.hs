module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns num f start = f (num f start)

nplus :: Nat a -> Nat a -> Nat a
nplus a b f start = a f (b f start)

nmult :: Nat a -> Nat a -> Nat a
nmult a b f = a (b f)

nFromNatural :: Natural -> Nat a
nFromNatural num = case num of
  0 -> nz
  _ -> \f x -> f (nFromNatural (num - 1) f x)

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0