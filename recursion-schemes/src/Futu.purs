module Futo where

import Prelude
import Data.Monoid (mempty)
import Control.Apply (lift3)
import Control.Monad (class Monad, (=<<))
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomInt)
import Data.Tuple (Tuple(..))
import Prelude
import Data.Newtype (class Newtype, unwrap)

newtype Fix f = In (f (Fix f))
derive instance newtypeFix :: Newtype (Fix f) _

newtype Attr f a = Attr { attribute :: a, hole :: f (Attr f a) }

type CVAlgebra f a = f (Attr f a) -> a

-- `CoAttr f a` is dual to `Attr f a`
data CoAttr f a
  -- "automatically" continue the fold at this level
  -- analogous to Continue
  = Automatic a
  -- "manually" specify how the fold proceeds at this level
  -- analogous to Stop
  | Manual (f (CoAttr f a))

-- CVCoalgebra is dual to CVAlgebra
type CVCoalgebra f a = a -> f (CoAttr f a)

futu :: forall f a. Functor f => CVCoalgebra f a -> a -> Fix f
futu f = In <<< map worker <<< f
  where
    -- Automatic means continue recursion
    worker (Automatic a) = futu f a
    -- Manual means do one more iteration
    worker (Manual g) = In $ map worker g

data Plant a
  = Root a
  | Stalk a
  | Fork a a a
  | Bloom
derive instance functorPlant :: Functor Plant

instance foldablePlant :: Foldable Plant where
  -- forall a b. (a -> b -> b) -> b -> f a -> b
  foldr f y (Root x) = f x y
  foldr f y (Stalk x) = f x y
  foldr f y (Fork x1 x2 x3) = f x3 $ f x2 $ f x1 y
  foldr _ y Bloom = y

  -- forall a b. (b -> a -> b) -> b -> f a -> b
  foldl f y (Root x) = f y x
  foldl f y (Stalk x) = f y x
  foldl f y (Fork x1 x2 x3) = f (f (f y x3) x2) x1 
  foldl _ y Bloom = y

  -- foldMap :: a m. Monoid m => (a -> m) -> f a -> m
  foldMap f (Root x) = f x
  foldMap f (Stalk x) = f x
  foldMap f (Fork x1 x2 x3) = f x1 <> f x2 <> f x3
  foldMap _ Bloom = mempty

instance traversablePlant :: Traversable Plant where
  -- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m t 
  traverse f (Root x) = map Root $ f x
  traverse f (Stalk x) = map Stalk $ f x
  traverse f (Fork x1 x2 x3) = lift3 Fork (f x1) (f x2) (f x3)
  traverse _ Bloom = pure Bloom

  sequence (Root m) = map Root m
  sequence (Stalk m) = map Stalk m
  sequence (Fork m1 m2 m3) = lift3 Fork m1 m2 m3
  sequence Bloom = pure Bloom

data Action
  = Flower
  | Upwards
  | Branch

newtype Seed = Seed { height :: Int }

grow :: Seed -> Effect { action :: Action, left :: Seed, right :: Seed }
grow seed@(Seed s) = do
  choice <- randomInt 1 5
  pure { action: choose choice
       , left: leftSeed
       , right: rightSeed
       }
  where
    leftSeed = Seed { height: s.height + 1 }
    rightSeed = Seed { height: s.height + 1 }
    choose 1 = Flower
    choose 2 = Branch
    choose _ = Upwards

sow :: CVCoalgebraM Effect Plant Seed
sow (Seed seed) = do
  { action, left, right } <- grow (Seed seed)
  pure $ go action left right
  where
    go action left right =
      case Tuple action seed.height of
            (Tuple _ 0) -> Root (Automatic left)
            (Tuple _ 10) -> Bloom
            (Tuple Flower _) -> Bloom
            (Tuple Upwards _) -> Stalk (Automatic right)
            (Tuple Branch _) -> Fork (Manual (Stalk (Automatic left)))
                                     (Manual Bloom)
                                     (Manual (Stalk (Automatic right)))

type GCoalgebra n f a = a -> f (n a)

type GCoalgebraM n m f a = a -> m (f (n a))

-- type CVCoalgebra f a = a -> f (CoAttr f a)

-- embed: :: f t -> t
-- resume: https://pursuit.purescript.org/packages/purescript-free/5.2.0/docs/Control.Monad.Free#v:resume
-- Unwraps a single layer of the functor f

-- futu :: forall f a. Functor f => CVCoalgebra f a -> a -> Fix f
-- call f on a
-- f returns m (f (CoAttr f a)
-- Seed -> Effect (Plant (CoAttr Plant Seed))
type CVCoalgebraM m f a = a -> m (f (CoAttr f a))

-- futuM :: forall m f a. Monad m => Functor f => a -> m (f (CoAttr f a)) -> a -> m (Fix f)
futuM :: forall m f a. Monad m => Traversable f => Functor f => CVCoalgebraM m f a -> a -> m (Fix f)
--- futuM f = traverse In <<< traverse (map worker) <<< f
futuM f = go
  where
    go a = map In <<< traverse worker =<< f a
    {-- boo = f a --}
    {-- blah :: Effect Unit --}
    {-- blah = traverse worker --}
    -- worker (Automatic a) = futu f a
    worker (Automatic a) = futuM f a 
    {-- worker (Manual g) = traverse In $ map worker g --}
    worker (Manual g) = map In $ traverse worker g

main :: Effect Unit
main = do
  rand <- randomInt 1 5
  ourPlant <- futuM sow (Seed { height: 0 })
  pure unit
