module HW2.T3
  ( epart
  , mcat
  ) where

myFromMaybe :: Monoid a => Maybe a -> a
myFromMaybe Nothing  = mempty
myFromMaybe (Just a) = a

-- You may add necessary constraints here
mcat :: Monoid a => [Maybe a] -> a
mcat = foldr ((<>) . myFromMaybe) mempty

myFromEither :: (Monoid a, Monoid b) => Either a b -> (a, b)
myFromEither (Left a)  = (a, mempty)
myFromEither (Right b)  = (mempty, b)

-- You may add necessary constraints here
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap myFromEither