module MatTest where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Functor.Mu (Mu)
import Matryoshka (class Corecursive, Algebra, cata, embed)

data ListF a t = Nil | Cons a t

derive instance functorListF :: Functor (ListF a)

type List a = Mu (ListF a)

nil :: forall a t. Corecursive t (ListF a) => t
nil = embed Nil

cons :: forall a t. Corecursive t (ListF a) => a -> t -> t
cons h t = embed (Cons h t)

sumList :: Algebra (ListF Int) Int
sumList Nil = 0
sumList (Cons x y) = x + y

foo :: List Int
foo = cons 1 (cons 2 (cons 3 nil))

main :: Effect Unit
main = do
  logShow $ cata sumList foo
