module HW5.Parser
  ( parse
  ) where

import           Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, InfixN), makeExprParser)
import           Data.Void                      (Void)
import           HW5.Base                       (HiExpr (..), HiFun (..), HiValue (..))
import           Text.Megaparsec                hiding (parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as Lexer
import qualified Data.Text                      as T
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (spaceConsumer *> parseHiExpr <* eof) ""

parseHiExpr :: Parser HiExpr
parseHiExpr = makeExprParser parseTerm operatorTable

parseTerm :: Parser HiExpr
parseTerm = choice
  [ try parseList
  , try parseApplication
  , HiExprValue <$> parseHiValue
  , parens parseHiExpr
  ]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [
    [ InfixL (symbol "*" >> return (binary HiFunMul))
    , InfixL (notFollowedBy (string "/=") *> symbol "/" >> return (binary HiFunDiv)) ]
  ,
    [ InfixL (symbol "+" >> return (binary HiFunAdd))
    , InfixL (symbol "-" >> return (binary HiFunSub)) ]
  ,
    [ InfixN (symbol "/=" >> return (binary HiFunNotEquals))
    , InfixN (symbol "<=" >> return (binary HiFunNotGreaterThan))
    , InfixN (symbol ">=" >> return (binary HiFunNotLessThan))
    , InfixN (symbol "<"  >> return (binary HiFunLessThan))
    , InfixN (symbol ">"  >> return (binary HiFunGreaterThan))
    , InfixN (symbol "==" >> return (binary HiFunEquals))
    , InfixN (symbol "!=" >> return (binary HiFunNotEquals)) ]
  ,
    [ InfixR (symbol "&&" >> return (binary HiFunAnd)) ]
  ,
  [ InfixR (symbol "||" >> return (binary HiFunOr)) ]
  ]


binary :: HiFun -> HiExpr -> HiExpr -> HiExpr
binary fun x y =
    HiExprApply (HiExprValue (HiValueFunction fun)) [x, y]

parseApplication :: Parser HiExpr
parseApplication = do
  funExpr <- parseHiValue
  args <- many (parens (parseHiExpr `sepBy` symbol ","))
  return $ foldl  HiExprApply (HiExprValue funExpr) args

parseHiValue :: Parser HiValue
parseHiValue = choice
  [ try (HiValueNumber <$> parseNumber)
  , try (HiValueFunction <$> parseFunction)
  , try (HiValueBool <$> parseBool)
  , try (HiValueString <$> parseString)
  , try (symbol "null" >> return HiValueNull)
  ]

parseList :: Parser HiExpr
parseList = do
  elements <- between (symbol "[") (symbol "]") (parseHiExpr `sepBy` symbol ",")
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) elements


parseBool :: Parser Bool
parseBool =
  (symbol "true" >> return True) <|> (symbol "false" >> return False)

parseNumber :: Parser Rational
parseNumber = lexemeConsumer (toRational <$> Lexer.signed space Lexer.scientific)

parseFunction :: Parser HiFun
parseFunction = choice
  [ symbol "div" >> return HiFunDiv
  , symbol "mul" >> return HiFunMul
  , symbol "add" >> return HiFunAdd
  , symbol "sub" >> return HiFunSub
  , symbol "list" >> return HiFunList
  , symbol "range" >> return HiFunRange
  , symbol "fold" >> return HiFunFold
  , symbol "not-less-than" >> return HiFunNotLessThan
  , symbol "not-greater-than" >> return HiFunNotGreaterThan
  , symbol "not-equals" >> return HiFunNotEquals
  , symbol "/=" >> return HiFunNotEquals
  , symbol "/"  >> return HiFunDiv
  , symbol "*"  >> return HiFunMul
  , symbol "+"  >> return HiFunAdd
  , symbol "-"  >> return HiFunSub
  , symbol ">=" >> return HiFunNotLessThan
  , symbol "<=" >> return HiFunNotGreaterThan
  , symbol "<"  >> return HiFunLessThan
  , symbol ">"  >> return HiFunGreaterThan
  , symbol "==" >> return HiFunEquals
  , symbol "!=" >> return HiFunNotEquals
  , symbol "&&" >> return HiFunAnd
  , symbol "||" >> return HiFunOr
  , symbol "length" >> return HiFunLength
  , symbol "to-upper" >> return HiFunToUpper
  , symbol "to-lower" >> return HiFunToLower
  , symbol "reverse" >> return HiFunReverse
  , symbol "trim" >> return HiFunTrim
  , symbol "not" >> return HiFunNot
  , symbol "and" >> return HiFunAnd
  , symbol "or" >> return HiFunOr
  , symbol "less-than" >> return HiFunLessThan
  , symbol "greater-than" >> return HiFunGreaterThan
  , symbol "equals" >> return HiFunEquals
  , symbol "if" >> return HiFunIf
  ]

parseString :: Parser T.Text
parseString = do
  str <- lexemeConsumer (char '"' >> manyTill L.charLiteral (char '"'))
  return (T.pack str)


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexemeConsumer :: Parser a -> Parser a
lexemeConsumer = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
