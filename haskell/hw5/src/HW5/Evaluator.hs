{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module HW5.Evaluator
  ( eval
  ) where


import           Control.Monad.Trans.Except
import           Data.Ratio                 (denominator, numerator)
import           Data.Semigroup             (stimes)
import qualified Data.Sequence              as Seq
import Data.Foldable (toList) 
import Control.Monad (foldM)  
import qualified Data.Text                  as T
import           Data.Time                  (diffUTCTime)
import           Data.Time.Clock            (addUTCTime)
import           Debug.Trace                (trace)
import           HW5.Base                   (HiError (..), HiExpr (..),
                                             HiFun (..), HiValue (..))




eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evalExpression expr)

evalExpression :: Monad m => HiExpr -> ExceptT HiError m HiValue
evalExpression = \case
  HiExprValue val -> return val
  HiExprApply fn args -> evalExprApply fn args


evalExprApply :: Monad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalExprApply fn args = do
  func <- evalExpression fn
  case func of
    HiValueFunction HiFunFold -> evalFold args
    HiValueFunction HiFunIf ->
       evalClosure args
    HiValueFunction HiFunAnd ->
      evalClosureAnd args
    HiValueFunction HiFunOr ->
      evalClosureOr args
    HiValueString str -> case args of
      [HiExprValue (HiValueNumber start)] -> evalSubstring [HiValueString str, HiValueNumber start]
      [HiExprValue (HiValueNumber start), HiExprValue (HiValueNumber len)] -> evalSubstring [HiValueString str, HiValueNumber start, HiValueNumber len]
      _ -> throwE HiErrorArityMismatch
    HiValueFunction f -> do
      evalArgs <- mapM evalExpression args
      case f of
        HiFunToUpper -> case evalArgs of
          [arg] -> evalToUpper arg
          _     -> throwE HiErrorArityMismatch
        HiFunToLower -> case evalArgs of
          [arg] -> evalToLower arg
          _     -> throwE HiErrorArityMismatch
        HiFunReverse -> case evalArgs of
          [arg] -> evalReverse arg
          _     -> throwE HiErrorArityMismatch
        HiFunTrim -> case evalArgs of
          [arg] -> evalTrim arg
          _     -> throwE HiErrorArityMismatch
        HiFunAdd            -> evalAdd evalArgs
        HiFunSub            -> evalSub evalArgs
        HiFunMul            -> evalMul evalArgs
        HiFunDiv            -> evalDiv evalArgs
        HiFunNot            -> evalNot evalArgs
        HiFunLessThan       -> evalLessThan evalArgs
        HiFunGreaterThan    -> evalGreaterThan evalArgs
        HiFunNotLessThan    -> evalNotLessThan evalArgs
        HiFunNotGreaterThan -> evalNotGreaterThan evalArgs
        HiFunEquals         -> evalEqual evalArgs
        HiFunNotEquals      -> evalNotEqual evalArgs
        HiFunLength         -> evalLength evalArgs
        HiFunList           -> evalList evalArgs
        _                   -> throwE HiErrorInvalidFunction
    _ -> throwE HiErrorInvalidFunction


evalAdd :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalAdd [HiValueNumber a, HiValueNumber b] =
  return $ HiValueNumber (a + b)
evalAdd [HiValueString a, HiValueString b] =
  return $ HiValueString (T.append a b)
evalAdd [HiValueTime a, HiValueNumber b] =
  return $ HiValueTime $ addUTCTime (fromRational b) a
evalAdd [_, _] = throwE HiErrorArityMismatch
evalAdd _ = throwE HiErrorArityMismatch


evalSub :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalSub   [HiValueNumber a, HiValueNumber b] =
    return $ HiValueNumber (a - b)
evalSub [HiValueTime a, HiValueTime b] =
    return . HiValueNumber . toRational $ diffUTCTime a b
evalSub [_, _] = throwE HiErrorInvalidArgument
evalSub _ = throwE HiErrorArityMismatch


evalMul :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalMul args = case args of
  [HiValueNumber a, HiValueNumber b] ->
    return $ HiValueNumber (a * b)

  [HiValueString s, HiValueNumber n] ->
    return $ HiValueString (stimes (fromInteger (round n)) s)

  [HiValueNumber n, HiValueString s] ->
    return $ HiValueString (stimes (fromInteger (round n)) s)
  [_, _] -> throwE HiErrorInvalidArgument
  _ -> throwE HiErrorArityMismatch


evalDiv :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalDiv args = case args of
  [HiValueNumber a, HiValueNumber b] ->
    if b == 0
      then throwE HiErrorDivideByZero
      else return $ HiValueNumber (a / b)
  [HiValueString a, HiValueString b] ->
    return $ HiValueString (a <> "/" <> b)
  [_, _] -> throwE HiErrorInvalidArgument
  _ -> throwE HiErrorArityMismatch


-- logic

evalNot :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalNot args = case args of
  [HiValueBool a] ->
    return $ HiValueBool (not a)
  [_] ->
    throwE HiErrorInvalidArgument
  _ ->
    throwE HiErrorArityMismatch


evalEqual :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalEqual args = case args of
  [a, b] ->
    return $ HiValueBool (a == b)
  _ -> trace (show args) throwE HiErrorArityMismatch

evalNotEqual :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalNotEqual args = do
  equalResult <- evalEqual args
  case equalResult of
    HiValueBool val -> return $ HiValueBool (not val)
    _               -> throwE HiErrorInvalidArgument

evalLessThan :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalLessThan args = case args of
  [HiValueBool _, HiValueNumber _] ->
    return $ HiValueBool True
  [HiValueNumber _, HiValueBool _] ->
    return $ HiValueBool False
  [a, b] ->
    return $ HiValueBool (a < b)
  _ -> throwE HiErrorArityMismatch

evalGreaterThan :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalGreaterThan args = case args of
  [HiValueBool _, HiValueNumber _] ->
    return $ HiValueBool False
  [HiValueNumber _, HiValueBool _] ->
    return $ HiValueBool True
  [a, b] ->
    return $ HiValueBool (a > b)
  _ -> throwE HiErrorArityMismatch

evalNotLessThan :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalNotLessThan args = do
  result <- evalLessThan args
  case result of
    HiValueBool val -> return $ HiValueBool (not val)
    _               -> throwE HiErrorInvalidArgument

evalNotGreaterThan :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalNotGreaterThan args = do
  result <- evalGreaterThan args
  case result of
    HiValueBool val -> return $ HiValueBool (not val)
    _               -> throwE HiErrorInvalidArgument


evalLength :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalLength args = case args of
  [HiValueString txt] ->
    return $ HiValueNumber (toRational (T.length txt))
  _ -> throwE HiErrorInvalidArgument

evalToUpper :: Monad m => HiValue -> ExceptT HiError m HiValue
evalToUpper input = case input of
  HiValueString txt -> return $ HiValueString (T.toUpper txt)
  _                 -> throwE HiErrorInvalidArgument

evalToLower :: Monad m => HiValue -> ExceptT HiError m HiValue
evalToLower input = case input of
  HiValueString txt -> return $ HiValueString (T.toLower txt)
  _                 -> throwE HiErrorInvalidArgument


evalReverse :: Monad m => HiValue -> ExceptT HiError m HiValue
evalReverse input = case input of
  HiValueString txt -> return $ HiValueString (T.reverse txt)
  _                 -> throwE HiErrorInvalidArgument


evalTrim :: Monad m => HiValue -> ExceptT HiError m HiValue
evalTrim input = case input of
  HiValueString txt ->
    let trimmed = T.dropWhile (== ' ') (T.dropWhileEnd (== ' ') txt)
    in return $ HiValueString trimmed
  _ -> throwE HiErrorInvalidArgument

evalFromNumToInt :: Rational -> Maybe Int
evalFromNumToInt r =
  if denominator r == 1
  then Just (fromIntegral (numerator r))
  else Nothing


evalSubstring :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalSubstring [HiValueString str, HiValueNumber start] =
  case evalFromNumToInt start of
    Just startIndex
      | T.null str && startIndex == 0 ->
          return (HiValueString "")
      | startIndex < 0 || startIndex >= T.length str ->
          return HiValueNull
      | otherwise ->
          return $ HiValueString (T.singleton (T.index str startIndex))
    Nothing -> throwE HiErrorInvalidArgument

evalSubstring [HiValueString str, HiValueNumber start, HiValueNumber end] =
  case (evalFromNumToInt start, evalFromNumToInt end) of
    (Just startIndex, Just endIndex)
      | T.null str && startIndex == endIndex && startIndex == 0 ->
          return (HiValueString "")
      | startIndex < 0 || endIndex < startIndex || endIndex > T.length str ->
          return HiValueNull
      | otherwise ->
          return $ HiValueString (T.take (endIndex - startIndex) (T.drop startIndex str))
    _ -> throwE HiErrorInvalidArgument


evalSubstring _ = throwE HiErrorInvalidArgument


evalList :: Monad m => [HiValue] -> ExceptT HiError m HiValue
evalList args = return $ HiValueList (Seq.fromList args)

evalClosure :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
evalClosure [cond, a, b] = do
  condResult <- evalExpression cond
  case condResult of
    HiValueBool True  -> evalExpression a
    HiValueBool False -> evalExpression b
    _                 -> throwE HiErrorInvalidArgument
evalClosure _ = throwE HiErrorArityMismatch


evalClosureAnd :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
evalClosureAnd [a, b] = do
  firstRes <- evalExpression a
  case firstRes of
    HiValueBool False -> return firstRes
    HiValueNull       -> return firstRes
    _                 -> evalExpression b


evalClosureOr :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
evalClosureOr [a, b] = do
  firstRes <- evalExpression a
  case firstRes of
    HiValueBool False -> evalExpression b
    HiValueNull       -> evalExpression b
    _                 -> return firstRes

evalFold :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
evalFold args = case args of
  [a, b] -> do
    funVal <- evalExpression a
    listVal <- evalExpression b
    case (funVal, listVal) of
      (HiValueFunction fun, HiValueList list) -> do
        let curList = toList list
        if null curList
          then return $ HiValueList Seq.empty
          else do
            let first = head curList
            let rest = tail curList
            foldM (\acc x -> evalExprApply (HiExprValue (HiValueFunction fun)) [HiExprValue acc, HiExprValue x]) first rest
      _ -> throwE HiErrorInvalidArgument
  _ -> throwE HiErrorArityMismatch



