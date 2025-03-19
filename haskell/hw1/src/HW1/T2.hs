module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    nFromNatural,
    nToNum,
    ncmp,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Numeric.Natural

data N
  = Z
  | S N

nplus :: N -> N -> N
nplus Z x     = x
nplus x Z     = x
nplus (S m) n = S (nplus m n)

nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult (S m) n = nplus n (nmult m n)

nsub :: N -> N -> Maybe N
nsub x Z         = Just x
nsub Z _         = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp Z _         = LT
ncmp _ Z         = GT
ncmp (S m) (S n) = ncmp m n

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = 1 + nToNum x

nEven :: N -> Bool
nEven Z         = True
nEven (S Z)     = False
nEven (S (S n)) = nEven n

nOdd :: N -> Bool
nOdd n = nEven (S n)

mysub :: N -> N -> N
mysub x Z         = x
mysub (S x) (S y) = mysub x y
mysub _ _         = Z

ndiv :: N -> N -> N
ndiv _ Z = error "division by zero"
ndiv Z _ = Z
ndiv x y
  | ncmp x y == LT = Z
  | otherwise = S (ndiv (mysub x y) y)

nmod :: N -> N -> N
nmod _ Z = error "division by zero"
nmod x y
  | ncmp x y == LT = x
  | otherwise = nmod (mysub x y) y
