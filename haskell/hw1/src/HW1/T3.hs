module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = M Int Int

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)

tsize :: Tree a -> Int
tsize Leaf                      = 0
tsize (Branch (M size _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                       = 0
tdepth (Branch (M _ depth) _ _ _) = depth


tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember n (Branch (M _ _) left now right)
  |now == n = True
  |n < now = tmember n left
  |otherwise = tmember n right

newMetaData :: Tree a -> Tree a -> Meta
newMetaData left right = M (tsize left + tsize right + 1) (max (tdepth left) (tdepth right) + 1)

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert n Leaf = Branch (M 1 1) Leaf n Leaf
tinsert n (Branch (M size depth) left now right)
  |now == n = Branch (M size depth) left now right
  | n < now   = let newLeft = tinsert n left
                in Branch (newMetaData newLeft right) newLeft now right
  | otherwise = let newRight = tinsert n right
                in Branch (newMetaData left newRight) left now newRight


tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf -- после тета-редукции
