module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

data Option a = None | Some a
  deriving (Eq, Show)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None     = None
mapOption f (Some a) = Some (f a)

data Pair a = P a a
  deriving (Eq, Show)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a b) = P (f a) (f b)

data Quad a = Q a a a a
  deriving (Eq, Show)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

data Annotated e a = a :# e
  deriving (Eq, Show)

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) =  f a :# e

data Except e a = Error e | Success a
  deriving (Eq, Show)

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e
mapExcept f (Success a) = Success (f a)

data Prioritised a = Low a | Medium a | High a
  deriving (Eq, Show)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a) = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a) = High (f a)

data Stream a = a :> Stream a
  deriving (Eq, Show)

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (x :> xs) = f x :> mapStream f xs

data List a = Nil | a :. List a
  deriving (Eq, Show)

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList f (start :. remain) = f start :. mapList f remain

data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F fun) = F (f . fun)

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree f (Branch left a right) = Branch (mapTree f left) (f a) (mapTree f right)