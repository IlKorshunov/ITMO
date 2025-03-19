module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map'= fix mapper
  where
    mapper :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    mapper _ _ [] = []
    mapper f ff (x:xs) = ff x : f ff xs

fib :: Natural -> Natural
fib = undefined

fac :: Natural -> Natural
fac = fix factorial
  where
    factorial :: (Natural -> Natural) -> Natural -> Natural
    factorial _ 0 = 1
    factorial f n = n * f (n - 1)


