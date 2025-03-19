{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  ( TSet
  , Contains
  , Add
  , Delete
  ) where

import           Data.Type.Bool     (If)
import           Data.Type.Equality
import           GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains s '[] = 'False
  Contains s (x ': xs) = If (s == x) 'True (Contains s xs)

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete n '[]      = '[]
  Delete x (x ': xs) = xs
  Delete n (x ': xs) = x ': Delete n xs

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add v s = If (Contains v s) s (v ': s)