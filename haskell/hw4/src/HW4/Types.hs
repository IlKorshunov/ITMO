-- | This module contains the types from hw3 that are also
-- needed for hw4.
module HW4.Types
  ( Annotated (..)
  , Except (..)
  , Expr (..)
  , Prim (..)
  , State (..)
  , mapExcept
  , mapAnnotated
  , joinExcept
  , joinAnnotated
  , wrapExcept
  , wrapAnnotated
  , joinAnnotatedExcept
  ) where

data Except e a = Error e | Success a
  deriving Show

data Annotated e a = a :# e
  deriving Show

data State s a = S { runS :: s -> Annotated s a }

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success (f a)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) =  f a :# e

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e 
joinExcept (Success x) = x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1) 

joinAnnotatedExcept :: Annotated s (Except e a) -> Except e a
joinAnnotatedExcept (Error e :# _)   = Error e
joinAnnotatedExcept (Success a :# _) = Success a

wrapExcept :: a -> Except e a
wrapExcept = Success 

wrapAnnotated :: (Monoid  e) => a -> Annotated e a
wrapAnnotated val = val :# mempty

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Eq, Show)

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)
