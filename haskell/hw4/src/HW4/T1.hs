{-# LANGUAGE InstanceSigs #-}
module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState fun (ES run) = ES $ \state -> mapExcept (mapAnnotated fun) (run state)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState value = ES $ \state -> wrapExcept $ value :# state


-- Except e (Annotated s (ExceptState e s a))
-- Annotated s (ExceptState e s a)
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES run) = ES $ \state ->
  case run state of
    Error e -> Error e
    Success (ES run2 :# s) -> joinAnnotatedExcept (run2 s :# s)

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState fun = ES $ \state -> wrapExcept $ () :# fun state

throwExceptState :: e -> ExceptState e s a
throwExceptState er = ES $ \_ -> Error er

instance Functor (ExceptState e s) where
  fmap :: (a -> b) -> ExceptState e s a -> ExceptState e s b
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState

  (<*>) :: ExceptState e s (a -> b) -> ExceptState e s a -> ExceptState e s b
  (ES f) <*> exState = ES $ \state -> case f state of
    Error e -> Error e
    Success (innerFunc :# innerState) -> runES (mapExceptState innerFunc exState) innerState

instance Monad (ExceptState e s) where
  (>>=) :: ExceptState e s a -> (a -> ExceptState e s b) -> ExceptState e s b
  (>>=) action fun = joinExceptState (mapExceptState fun action)

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x)        = pure x
eval (Op (Add a b)) = evalBinOp Add (+) False a b
eval (Op (Sub a b)) = evalBinOp Sub (-) False a b
eval (Op (Mul a b)) = evalBinOp Mul (*) False a b
eval (Op (Div a b)) = evalBinOp Div (/) True a b
eval (Op (Abs a))   = evalUnOp Abs abs a
eval (Op (Sgn a))   = evalUnOp Sgn signum a

evalBinOp :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Bool -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
evalBinOp cons op flag a b = do
    first <- eval a
    second <- eval b
    if flag && second == 0 then
        throwExceptState DivideByZero
    else do
        modifyExceptState (\list -> cons first second : list)
        pure (first `op` second)


evalUnOp :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
evalUnOp cons op a = do
    first <- eval a
    modifyExceptState (\list -> cons first : list)
    pure (op first)