module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty(..))

-- You may add necessary constraints here
splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn sep array = foldr (\y (x :| xs) ->
  if y == sep
  then [] :| (x : xs)
  else (y : x) :| xs) ([] :| []) array




joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (x :| xs) = x ++ concatMap (sep :) xs




