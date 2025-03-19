{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->)(Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib input = case input of
  Left a -> (Left a, Left a)
  Right (b, c) -> (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso leftToTight rightToLeft
  where 
    leftToTight :: (a, (b, c)) -> ((a, b), c)
    leftToTight (a, (b, c)) = ((a, b), c)

    rightToLeft :: ((a, b), c) -> (a, (b,c))
    rightToLeft ((a, b), c) = (a, (b, c))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso leftToRight rightToLeft
  where 
    leftToRight :: Either a (Either b c) -> Either (Either a b) c
    leftToRight input = case input of
      Left a -> Left (Left a) 
      Right (Left b) ->  Left (Right b)
      Right (Right c) -> Right c

    rightToLeft :: Either (Either a b) c -> Either a (Either b c)
    rightToLeft input = case input of
      Left (Left a) -> Left a  
      Left (Right b) -> Right (Left b)
      Right c -> Right (Right c)