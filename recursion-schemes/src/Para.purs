module Para where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), snd)

{-
  Catamorphisms lose information about the current structure.
  Because these are bottom-up transformations, the current application of f
  doesn't have access to the original sub-expression.

  For example, `In (Add (In (Int 1)) (In (Int 1)))`
  When f is applied to the `Add` node, then the subexpressions `In (Int 1)`
  ave already been replaced with integers.
  The current invocation of `f` has no way to know if those integers were the
  result of integer literals `In (Int 1)` or the result of sub-expressions,
  nested `Add` expressions.

  A paramorphism will give you the original node, with all of the subexpressions
  and the `a` that those subexpressions stand for.

  How does this work when there are more than one sub-expressions of type a?

  A paramorphism applies an "R-algebra" to the structure. An R-algebra is a
  function `f (Term f, a) -> a`.

  "Para" is from a word that originally meant "beside", "next to", or "alongside",
  like parallel.
  View the original structure **beside** the structure being transformed.

  I believe that the R in R-algebra refers to "ring".
-}

newtype Fix f = In (f (Fix f))
derive instance newtypeFix :: Newtype (Fix f) _

type RAlgebra f a = f (Tuple (Fix f) a) -> a
-- Purescript's recursion scheme library uses a GAlgebra instead
type GAlgebra f w a = f (w a) -> a

para :: forall f a. Functor f => RAlgebra f a -> Fix f -> a
para f = unwrap >>> map (\x -> fanout x) >>> f
  where fanout t = Tuple t (para f t)

data AddF a
  = Num Int
  | Add a a
derive instance functorAddF :: Functor AddF

one :: Fix AddF
one = In (Num 1)

two :: Fix AddF
two = In (Add one one)

type Algebra f a = f a -> a

add :: Algebra AddF Int
add (Num n) = n
add (Add n1 n2) = n1 + n2

paraLift :: forall f a. Functor f => (f a -> a) -> (f (Tuple (Fix f) a) -> a)
paraLift f = map snd >>> f

evalAdd = para (paraLift add)

addP :: RAlgebra AddF Int
addP (Num n) = n
addP (Add (Tuple e1 n1) (Tuple e2 n2)) = n1 + n2

evalAdd' = para addP
