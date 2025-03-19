module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where
data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last x <> other = x :+ other
  (x :+ xs) <> other = x :+ (xs <> other)

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints here
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This a <> This b = This (a <> b)
  That a <> That b = That (a <> b)
  This a <> That b = Both a b
  That b <> This a = Both a b
  This a <> Both b c = Both (a <> b) c
  That a <> Both b c = Both b (a <> c)
  Both a b <> This c = Both (a <> c) b
  Both a b <> That c = Both a (b <> c)
  Both a b <> Both c d = Both (a <> c) (b <> d)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  DS left <> DS right
    | left == "" = DS right          
    | right == "" = DS left          
    | otherwise = DS (left ++ "." ++ right)  
-- я не понимаю, почему это должно работать для полугруппы таким образом
-- ведь "" + b = .b для полугруппы по идее.
-- в более ранних версиях хаскеля был введен mappend для подобных ситуаций, но я не стал им здесь 
-- пользоваться, поскольку его запретили вроде
-- P.S товарищи сказали, что они на это "забили" и у них прошли тесты


instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F a <> F b = F (a . b)

instance Monoid (Fun a) where
  mempty = F id