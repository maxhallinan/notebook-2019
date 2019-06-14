module Apo where

import Prelude
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple

newtype Fix f = In (f (Fix f))
derive instance newtypeFix :: Newtype (Fix f) _

{-- instance showFix :: Show f => Show (Fix f) where --}
{--   show (In x) = "(In " <> show x <> ")" --}

data AddF a
  = Num Int
  | Add a a
derive instance functorAddF :: Functor AddF

{-- instance showAddF :: Show a => Show (AddF a) where --}
{--   show (Num n) = "(Num " <> show n <> ")" --}
{--   show (Add x1 x2) = "(Add " <> show x1 <> show x2 <> ")" --}

-- Algebra is dual to Coalgebra

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

-- cata is dual to ana

cata :: forall f a. Functor f => Algebra f a -> Fix f -> a
cata f = unwrap >>> map (\x -> cata f x) >>> f

ana :: forall f a. Functor f => Coalgebra f a -> a -> Fix f
ana f = In <<< map (\x -> ana f x) <<< f

add' :: Algebra AddF Int
add' (Num n) = n
add' (Add n1 n2) = n1 + n2

-- RAlgebra is dual to RCoalgebra

type RAlgebra f a = f (Tuple (Fix f) a) -> a

type RCoalgebra f a = a -> f (Either (Fix f) a)

para :: forall f a. Functor f => RAlgebra f a -> Fix f -> a
para f = unwrap >>> map fanout >>> f
  where fanout x = Tuple x (para f x)

apo :: forall f a. Functor f => RCoalgebra f a -> a -> Fix f
apo f = In <<< map (\x -> fanin x) <<< f
  where 
    fanin :: Either (Fix f) a -> Fix f
    fanin = either identity (\x -> apo f x)

buildNum :: RCoalgebra AddF Int
buildNum n
  | n < 0 = Add (Left $ In (Num 0)) (Left $ In (Num 0))
  | n == 0 = Add (Left $ In (Num 1)) (Left $ In (Num 0))
  | n == 1 = Add (Left $ In (Num 1)) (Left $ In (Num 0))
  | otherwise = Add (Right $ n - 1) (Right $ n - 2)
