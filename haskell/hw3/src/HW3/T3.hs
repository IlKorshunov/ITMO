module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some x) = x


joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e 
joinExcept (Success x) = x

-- You may add necessary constraints here
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1) 

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (Nil :. a) = joinList a
joinList ((a :. xa) :. xs) = a :. joinList (xa :. xs)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F fun) = F (\i -> 
  let (F innerFunc) = fun i  
  in innerFunc i)            
-- fun имеет тип (i -> Fun i a), поэтому innerFunc будет иметь тип (i -> a), и вызов innerFunc i вернет значение типа a.
