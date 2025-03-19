{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1


distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some a, Some b) = Some (a, b)

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

wrapPair :: a -> Pair a
wrapPair val = P val val

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q la lb lc ld, Q ra rb rc rd) = Q (la, ra) (lb, rb) (lc, rc) (ld, rd)

wrapQuad :: a -> Quad a
wrapQuad val = Q val val val val

-- You may add necessary constraints here
distAnnotated :: (Semigroup  e) => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated  (la :# le, ra :# re) = (la, ra) :# (le <> re)

-- You may add necessary constraints here
wrapAnnotated :: (Monoid  e) => a -> Annotated e a
wrapAnnotated val = val :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error a, _) = Error a
distExcept (_, Error  b) = Error b
distExcept (Success a, Success b) = Success (a, b)

wrapExcept :: a -> Except e a
wrapExcept = Success 

convertPriorityToInt :: Prioritised a -> Int
convertPriorityToInt = \case
  Low _    -> 0
  Medium _ -> 1
  High _   -> 2

convertIntToPriority :: Int -> a -> Prioritised a
convertIntToPriority n
  | n >= 0 && n < length importance = importance !! n
  | otherwise = error "Invalid value"
  where
    importance = [Low, Medium, High]

extract :: Prioritised a -> a
extract = \case 
  High a-> a 
  Medium a -> a
  Low a -> a 

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (a, b) = 
  let priority = max (convertPriorityToInt a) (convertPriorityToInt b) 
  in convertIntToPriority priority (extract a, extract b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> la, b :> rb) = (a, b) :> distStream (la, rb)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

appendList :: List a -> List a -> List a
appendList Nil ys = ys
appendList (x :. xs) ys = x :. appendList xs ys

concatList :: (a -> List b) -> List a -> List b
concatList _ Nil = Nil
concatList f (a :. xa) = appendList (f a) (concatList f xa)

distList :: (List a, List b) -> List (a, b)
distList (la, ra) = concatList (\a -> mapList (a,) ra) la

wrapList :: a -> List a
wrapList a = a :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\i -> (f i, g i))

wrapFun :: a -> Fun i a
wrapFun a = F (const a)
