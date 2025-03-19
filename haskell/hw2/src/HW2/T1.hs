module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ accum Leaf = accum
tfoldr func accum  (Branch _ left now right) =
  tfoldr func (func now (tfoldr func accum right)) left


-- let newRight = tfoldr func accum right
--     newNow = func now newRight
--     newLeft = tfoldr func newNow left
-- in newLeft

-- treeToList :: Tree a -> [a]   
-- treeToList = tfoldr (:) []

