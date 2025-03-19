{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.MyParser 
  ( parse
  , parseConfig     
  , makeConfig      
  , skipSpaces      
  ) where

import           Text.Megaparsec              (Parsec, ParseErrorBundle, runParser, eof)
import           Text.Megaparsec.Char         (string, hspace)
import           Text.Megaparsec.Char.Lexer   as Lexer
import           GHC.Base
import           HW6.T3 (Config(..)) 

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) Config
parse = runParser (skipSpaces *> parseConfig <* skipSpaces <* eof) ""

parseConfig :: Parser Config
parseConfig = do
    prob <- parseProb "-p"
    incub <- parseFlagAndVal "-i"
    ill <- parseFlagAndVal "-ill"
    immun <- parseFlagAndVal "-imm"
    gridSize <- parseFlagAndVal "-gs"
    iters <- parseFlagAndVal "-it"
    return $ makeConfig prob incub ill immun gridSize iters

makeConfig :: Double -> Int -> Int -> Int -> Int -> Int -> Config
makeConfig prob incub ill immun gridSize iters = Config
  { probability = prob
  , incubationPeriod = incub
  , illnessDuration = ill
  , immunityDuration = immun
  , gridSide = gridSize
  , iterations = iters
  }

parseFlagAndVal :: String -> Parser Int
parseFlagAndVal input = skipSpaces *> string input *> skipSpaces *> Lexer.decimal

parseProb :: String -> Parser Double
parseProb input = skipSpaces *> string input *> skipSpaces *> Lexer.float

skipSpaces :: Parser ()
skipSpaces = hspace
