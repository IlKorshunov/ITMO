module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S $ \input ->
  let a :# e = runS state input
  in f a :# e

wrapState :: a -> State s a
wrapState a = S $ \input -> a :# input

joinState :: State s (State s a) -> State s a
joinState (S fun) = S $ \input ->
  let S innerFunc :# state = fun input
  in innerFunc state

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \input -> () :# f input

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState

  -- s_fun имеет тип {s -> ((a->b) :# e)}
  (S s_fun) <*> (S s_val) = S $ \input ->
    let (s_fun_fun :# state) = s_fun input       -- s_fun_fun имеет тип (a -> b)
        (s_val_val :# state1) = s_val state      -- s_val_val имеет тип a
    in s_fun_fun s_val_val :# state1

instance Monad (State s) where
  (>>=) action fun = joinState (mapState fun action)

  -- let (value, desc) = start input
  --     State next = fun value
  -- in next desc

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Eq, Show)

data Expr = Val Double | Op (Prim Expr)
  deriving (Eq, Show)

instance Num Expr where
  (+) a b = Op (Add a b)
  (-) a b = Op (Sub a b)
  (*) a b = Op (Mul a b)
  abs a = Op(Abs a)
  signum a = Op(Sgn a)
  fromInteger a =  Val (fromInteger a)

instance Fractional Expr where
  (/) a b = Op (Div a b)
  fromRational a = Val (fromRational a)


eval :: Expr -> State [Prim Double] Double
eval (Val x)        = pure x
eval (Op (Add a b)) = evalBinOp Add (+) a b
eval (Op (Sub a b)) = evalBinOp Sub (-) a b
eval (Op (Mul a b)) = evalBinOp Mul (*) a b
eval (Op (Div a b)) = evalBinOp Div (/) a b
eval (Op (Abs a))   = evalUnOp Abs abs a
eval (Op (Sgn a))   = evalUnOp Sgn signum a

evalBinOp :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Expr -> Expr -> State [Prim Double] Double
evalBinOp cons op a b = do
    first <- eval a
    second <- eval b
    modifyState (\list -> cons first second : list)
    pure (first `op` second)

evalUnOp :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> State [Prim Double] Double
evalUnOp cons op a = do
    first <- eval a
    modifyState (\list -> cons first : list)
    pure (op first)
