module Cata where

import Prelude
import Data.Newtype (class Newtype, unwrap)

newtype Fix f = In (f (Fix f))
derive instance newtypeFix :: Newtype (Fix f) _

type Algebra f a = f a -> a

cata :: forall f a. Functor f => Algebra f a -> Fix f -> a
cata f = unwrap >>> map (\x -> cata f x) >>> f

data AddF a
  = Num Int
  | Add a a
derive instance functorAddF :: Functor AddF

one :: Fix AddF
one = In (Num 1)

two :: Fix AddF
two = In (Add one one)

add :: Algebra AddF Int
add (Num n) = n
add (Add n1 n2) = n1 + n2

evalAdd :: Fix AddF -> Int
evalAdd = cata add
