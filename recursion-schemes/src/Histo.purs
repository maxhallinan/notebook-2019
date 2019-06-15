module Histo where

import Prelude
import Data.Array (filter, partition)
import Data.Foldable (length, sum)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Fix f = In (f (Fix f))
derive instance newtypeFix :: Newtype (Fix f) _

instance showFix :: (Show (f String), Functor f) => Show (Fix f) where
  show (In x) = "In (" <> show (map show x) <> ")"

{--   show = genericShow --}

newtype Attr f a = Attr { attribute :: a, hole :: f (Attr f a) }
derive instance newtypeAttr :: Newtype (Attr f a) _
derive instance genericAttr :: Generic (Attr f a) _

instance showAttr :: (Show (f String), Show a, Functor f) => Show (Attr f a) where
  show (Attr attr) = "Attr { attribute: " <> show attr.attribute <> ", hole: " <> show (map show attr.hole) <> " }"

type CVAlgebra f a = f (Attr f a) -> a

-- i don't understand the difference between these two implementations:

-- this recurses through the tree twice, once for attribute and once for hole.
histo :: forall f a. Functor f => CVAlgebra f a -> Fix f -> a
histo f = unwrap >>> map worker >>> f
  where
    worker :: Fix f -> Attr f a
    worker term = Attr { attribute: histo f term
                       , hole: map worker (unwrap term)
                       }

histo' :: forall f a. Functor f => CVAlgebra f a -> Fix f -> a
histo' f = worker >>> getAttribute
  where
    worker = unwrap >>> map (\x -> worker x) >>> mkAttr
    mkAttr term = Attr { attribute: f term
                       , hole: term
                       }
    getAttribute = unwrap >>> _.attribute

histo'' :: forall f a. Functor f => CVAlgebra f a -> Fix f -> Attr f a
histo'' f = worker
  where
    worker = unwrap >>> map (\x -> worker x) >>> mkAttr
    mkAttr term = Attr { attribute: f term
                       , hole: term
                       }

data Nat a
  = Zero
  | Next a
derive instance functorNat :: Functor Nat
derive instance genericNat :: Generic (Nat a) _

instance showNat :: Show a => Show (Nat a) where
  show = genericShow

data AddF a
  = Num Int
  | Add a a
derive instance functorAddF :: Functor AddF
derive instance genericAddF :: Generic (AddF a) _

instance showAddF :: Show a => Show (AddF a) where
  show = genericShow

addOne :: Fix AddF
addOne = In (Num 1)

addTwo :: Fix AddF
addTwo = In (Add addOne addOne)

add :: CVAlgebra AddF Int
add (Num n) = n
add (Add (Attr attr1) (Attr attr2)) = attr1.attribute + attr2.attribute

evalAdd :: Fix AddF -> Attr AddF Int
evalAdd = histo'' add

{-- one :: Fix Nat --}
{-- one = In (Next (In Zero)) --}

{-- two :: Fix Nat --}
{-- two = In (Next one) --}

{-- three :: Fix Nat --}
{-- three = In (Next two) --}

{-- expandNat :: Int -> Fix Nat --}
{-- expandNat 0 = In Zero --}
{-- expandNat n = In (Next (expandNat $ n - 1)) --}

{-- compressNat :: forall a. Nat (Attr Nat a) -> Int --}
{-- compressNat Zero = 0 --}
{-- compressNat (Next (Attr { hole })) = 1 + (compressNat hole) --}

{-- type Cent = Int --}

{-- coins :: Array Cent --}
{-- coins = [50, 25, 10, 5, 1] --}

{-- change :: Cent -> Attr Nat Int --}
{-- change amount = histo'' go (expandNat amount) --} 
{--   where --}
{--     go :: CVAlgebra Nat Int --}
{--     go Zero = 1 --}
{--     go current@(Next attr) = --}
{--       let --}
{--         -- convert Nat to Int --}
{--         given = compressNat current --}
{--         -- can't make change for 0.40 with a 50 cent coin --}
{--         validCoins = filter (_ <= given) coins --}
{--         -- for each of the valid coins, what is the remainder. for 0.40, this --} 
{--         -- would be [15, 30, 35, 39] --}
{--         remaining = map (given - _) validCoins --}
{--         -- separate the zero and non-zero remainders --}
{--         { yes: zeroes, no: toProcess } = partition (_ == 0) remaining --}
{--         -- the type of attr is `Attr Nat Int` --}
{--         results = sum $ map (lookup attr) toProcess --}
{--       in --} 
{--         length zeroes + results --}

{-- lookup :: forall a. Attr Nat a -> Int -> a --}
{-- lookup (Attr cache) n = --} 
{--   case cache.hole of --}
{--     Next inner -> --}
{--       lookup inner (n - 1) --}
{--     Zero -> --}
{--       cache.attribute --}
