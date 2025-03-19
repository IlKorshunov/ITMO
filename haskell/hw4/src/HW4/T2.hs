{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use $>" #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Prelude hiding ((*>), (<*))
import Control.Applicative (many, optional, some, Alternative (..))
import Control.Monad (mfilter, msum, void, MonadPlus, Functor (fmap))
import Numeric.Natural (Natural)
import HW4.T1 (ExceptState (..), mapExceptState, wrapExceptState, joinExceptState, modifyExceptState, throwExceptState)
import HW4.Types
import Data.List (break)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Control.Arrow (ArrowChoice(left))

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

pEof :: Parser ()
pEof = P $ ES $ \(pos, input) ->
  case input of
    [] -> Success (() :# (pos, input))
    _  -> Error (ErrorAtPos pos)

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (P first) (P second) = P $ ES $ \input -> case runES first input of
    Error _ -> runES second input
    success -> success

instance MonadPlus Parser

predicate :: (Char -> Bool) -> Parser Char
predicate fun = P $ ES $ \(pos, str) -> case str of
  ""       -> Error (ErrorAtPos pos)
  (c:cs)   -> if fun c then Success (c :# (pos + 1, cs)) else Error (ErrorAtPos pos)

charPredicate :: Char -> Parser Char
charPredicate = predicate . (==)

(<*) :: Parser a -> Parser b -> Parser a
(P pa) <* (P pb) = P $ mapExceptState const pa <*> pb

(*>) :: Parser a -> Parser b -> Parser b
(P pa) *> (P pb) = P $ mapExceptState (\_ x -> x) pa <*> pb

digitToInt :: Char -> Int
digitToInt c = fromEnum c - fromEnum '0'

splitByDot :: String -> (String, String)
splitByDot s = case break (== '.') s of
    (before, '.' : after) -> (before, after)
    (before, _)           -> (before, "")

evaluateStart :: Double -> Char -> Double
evaluateStart acc curChar = acc * 10 + fromIntegral (digitToInt curChar)

evaluateEnd :: Char -> Double -> Double
evaluateEnd curChar acc = acc * 0.1 + fromIntegral (digitToInt curChar) * 0.1

stringToDouble :: String -> Double
stringToDouble input =
  let (part1, part2) = splitByDot input
      start = foldl evaluateStart 0 part1
      end = foldr evaluateEnd 0 part2
  in start + end

runP :: Parser a -> String -> Except ParseError a
runP (P parser) string = case runES parser (0, string) of
  Error e -> Error e
  Success (val :# _) -> Success val

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (parseSpaces *> parseLow <* parseSpaces <* pEof)

parseConst :: Parser Expr
parseConst = fmap (Val . stringToDouble)
       (some (predicate isDigit <|> charPredicate '.'))

parseSpaces :: Parser ()
parseSpaces = void $ many (charPredicate ' ') 

parseLow :: Parser Expr
parseLow = parseBinOp parseHigh (parseOperator '+' Add <|> parseOperator '-' Sub)

parseHigh :: Parser Expr
parseHigh = parseBinOp parseTerm (parseOperator '*' Mul <|> parseOperator '/' Div)

parseOperator :: Char -> (Expr -> Expr -> Prim Expr) -> Parser (Expr -> Expr -> Expr)
parseOperator op operation = parseSpaces *> charPredicate op *> parseSpaces *> pure (\a b -> Op (operation a b))

parseTerm :: Parser Expr
parseTerm = parseConst <|> parseWithBrack
  where
    parseWithBrack = do
      parseSpaces
      _ <- charPredicate '('
      expr <- parseLow
      _ <- charPredicate ')'
      parseSpaces
      return expr

parseBinOp :: Parser a -> Parser (a -> a -> a) -> Parser a
parseBinOp parseVal parseOp = parseVal >>= recFun
  where
    recFun leftVal = (parseSpaces *> parseOp <*> pure leftVal <*> parseVal >>= recFun) <|> pure leftVal