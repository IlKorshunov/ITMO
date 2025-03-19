{-# LANGUAGE InstanceSigs #-}

module Data.ListZipper
  ( ListZipper (..)
  , LZ
  , extract
  , toLeft
  , toRight
  , (=>>)
  , lzWrite
  , lGenerator
  , toList
  , lGeneratorWithLimit
  , toString
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]
type LZ = ListZipper


instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ l c r) = LZ (map f l) (f c) (map f r)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _)  = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = lGenerator toLeft toRight

  extend :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
  extend f lz = f <$> duplicate lz

(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

toLeft :: ListZipper a -> ListZipper a
toLeft (LZ (x:xs) c r) = LZ xs x (c:r)
toLeft lz = lz

toRight :: ListZipper a -> ListZipper a
toRight (LZ l c (x:xs)) = LZ (c:l) x xs
toRight lz = lz

toList :: Int -> Int -> ListZipper a -> [a]
toList n1 n2 (LZ l c r) = reverse (take n1 l) ++ [c] ++ take n2 r

lzWrite :: a -> ListZipper a -> ListZipper a
lzWrite x (LZ ls _ rs) = LZ ls x rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

lGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lGenerator f g x = LZ (iterateTail f x) x (iterateTail g x)

lGeneratorWithLimit :: Int -> Int -> (a -> a) -> (a -> a) -> a -> ListZipper a
lGeneratorWithLimit n1 n2 f g x = LZ (take n1 $ iterateTail f x) x (take n2 $ iterateTail g x)

toString :: Int -> Int -> (a -> String) -> ListZipper a -> String
toString leftLen rightLen f lz = concatMap f $ toList leftLen rightLen lz