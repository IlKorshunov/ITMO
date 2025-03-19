{-# LANGUAGE InstanceSigs #-}

module Data.Grid
  ( Grid (..)
  , toUp
  , toDown
  , toLeft
  , toRight
  , writeValue
  , gGeneratorWithLimit
  , gridToString
  ) where

import Control.Comonad (Comonad (..))

import qualified Data.ListZipper as LZ

newtype Grid a = Grid { unGrid :: LZ.ListZipper (LZ.ListZipper a) }

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid g) = Grid $ fmap (fmap f) g

instance Comonad Grid where
  extract :: Grid a -> a
  extract = extract . extract . unGrid

  duplicate :: Grid a -> Grid (Grid a)
  duplicate g = Grid $ gY <$> gX g

  extend :: (Grid a -> b) -> Grid a -> Grid b
  extend f g = f <$> duplicate g

writeValue :: a -> Grid a -> Grid a
writeValue x (Grid g) = Grid $ LZ.lzWrite (LZ.lzWrite x (extract g)) g

toUp :: Grid a -> Grid a
toUp (Grid g) = Grid (LZ.toLeft g)

toDown :: Grid a -> Grid a
toDown (Grid g) = Grid (LZ.toRight g)

toLeft :: Grid a -> Grid a
toLeft (Grid g) = Grid (fmap LZ.toLeft g)

toRight :: Grid a -> Grid a
toRight (Grid g) = Grid (fmap LZ.toRight g)

gY :: Grid a -> LZ.LZ (Grid a)
gY = LZ.lGenerator toDown toUp

gX :: Grid a -> LZ.LZ (Grid a)
gX = LZ.lGenerator toLeft toRight

iterateTailG :: (LZ.ListZipper a -> LZ.ListZipper a) -> LZ.ListZipper a -> [LZ.ListZipper a]
iterateTailG f z = tail $ iterate f z

gGeneratorWithLimit :: Int -> Int -> (LZ.ListZipper a -> LZ.ListZipper a) -> (LZ.ListZipper a -> LZ.ListZipper a) -> LZ.ListZipper a -> Grid a
gGeneratorWithLimit n1 n2 u d cz = Grid $ LZ.LZ (take n1 $ iterateTailG u cz) cz (take n2 $ iterateTailG d cz)

gridToList :: Int -> Int -> Grid a -> [LZ.LZ a]
gridToList leftLen rightLen (Grid g) = LZ.toList leftLen rightLen g

gridToString :: Int -> Int -> (a -> String) -> Grid a -> String
gridToString leftLen rightLen toStr g = unlines . map (LZ.toString leftLen rightLen toStr) $ gridToList leftLen rightLen g

-- gGenerator :: (LZ.ListZipper a -> LZ.ListZipper a) -> (LZ.ListZipper a -> LZ.ListZipper a) -> LZ.ListZipper a -> Grid a
-- gGenerator u d cz = Grid $ LZ.LZ (iterateTailG u cz) cz (iterateTailG d cz)