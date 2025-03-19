{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module HW5.Pretty
  ( prettyValue
  ) where

import Data.Ratio (numerator, denominator)

import Prettyprinter.Render.Terminal (AnsiStyle)
import HW5.Base (HiValue(..), HiFun(..))
import qualified Data.Map as Map
import Data.Scientific (fromRationalRepetendUnlimited, floatingOrInteger)
import Prettyprinter (Doc, pretty, dquotes)

funMap :: Map.Map  HiFun String
funMap =
  Map.fromList
    [ (HiFunDiv, "div")
    , (HiFunMul, "mul")
    , (HiFunAdd, "add")
    , (HiFunSub, "sub")
    , (HiFunNot, "not")
    , (HiFunAnd, "and")
    , (HiFunOr, "or")
    , (HiFunLessThan, "less-than")
    , (HiFunGreaterThan, "greater-than")
    , (HiFunEquals, "equals")
    , (HiFunNotLessThan, "not-less-than")
    , (HiFunNotGreaterThan, "not-greater-than")
    , (HiFunNotEquals, "not-equals")
    , (HiFunIf, "if")
    , (HiFunLength, "length")
    , (HiFunToUpper, "to-upper")
    , (HiFunToLower, "to-lower")
    , (HiFunReverse, "reverse")
    , (HiFunTrim, "trim")
    , (HiFunList, "list")
    , (HiFunRange, "range")
    , (HiFunFold, "fold")
    , (HiFunPackBytes, "pack-bytes")
    , (HiFunUnpackBytes, "unpack-bytes")
    , (HiFunEncodeUtf8, "encode-utf8")
    , (HiFunDecodeUtf8, "decode-utf8")
    , (HiFunZip, "zip")
    , (HiFunUnzip, "unzip")
    , (HiFunSerialise, "serialise")
    , (HiFunDeserialise, "deserialise")
    , (HiFunRead, "read")
    , (HiFunWrite, "write")
    , (HiFunMkDir, "mkdir")
    , (HiFunChDir, "cd")
    , (HiFunParseTime, "parse-time")
    , (HiFunRand, "rand")
    , (HiFunEcho, "echo")
    , (HiFunCount, "count")
    , (HiFunKeys, "keys")
    , (HiFunValues, "values")
    , (HiFunInvert, "invert")
    ]

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber n) = prettyNumber n
prettyValue (HiValueFunction f) = prettyFunc f
prettyValue (HiValueBool b) = prettyBoolean b
prettyValue (HiValueString s) = dquotes (pretty s)
prettyValue HiValueNull = pretty "null"
prettyValue _ = pretty "Unknown value"


prettyFunc :: HiFun -> Doc AnsiStyle
prettyFunc f = maybe (pretty (show f)) pretty (Map.lookup f funMap)

signChar :: Integer -> Char
signChar n = case signum n of
  1  -> '+'
  -1 -> '-'
  _  -> ' '

-- prettyFraction :: Rational -> String
-- prettyFraction num =
--   let (sci, repetend) = fromRationalRepetendUnlimited num
--       sciStr = formatScientific Fixed Nothing sci
--   in case repetend of
--        Nothing -> sciStr  
--        Just idx ->
--          let repPart = drop (idx + 1) (dropWhile (/= '.') sciStr)
--              first_part = take (length sciStr - length repPart) sciStr
--          in first_part ++ "(" ++ repPart ++ ")"

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber num =
  let (sci, repetend) = fromRationalRepetendUnlimited num
      numer = numerator num
      denom = denominator num
      (whole, remainder) = numer `quotRem` denom
  in case repetend of
    Nothing -> case (floatingOrInteger sci :: Either Double Integer) of
                Left f  -> pretty f
                Right i -> pretty i
    Just _ ->
      case (whole, remainder) of
            (0, 0) -> pretty "0"
            (0, _) -> pretty remainder <>  pretty '/' <> pretty denom
            (_, 0) -> pretty whole
            _      -> pretty whole <> pretty (signChar remainder) <> pretty (abs remainder) <> pretty '/' <> pretty (abs denom)



prettyBoolean :: Bool -> Doc AnsiStyle
prettyBoolean flag =
  if flag
    then pretty "true"
    else pretty "false"
