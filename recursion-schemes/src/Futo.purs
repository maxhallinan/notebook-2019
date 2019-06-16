module Futo where

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
derive instance functorPlan :: Functor Plant

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
    leftSeed = Seed { height: s.height }
    rightSeed = Seed { height: s.height }
    choose 1 = Flower
    choose 2 = Branch
    choose _ = Upwards

sow :: CVCoalgebra Effect Seed
sow (Seed seed) = do
  { action, left, right } <- grow (Seed seed)
  pure $ go action left right
  where 
    {-- go :: forall a. Action -> Seed -> Seed -> Plant (CoAttr a) --}
    go action left right =
      case Tuple action seed.height of
            (Tuple _ 0) -> Root (Automatic left)
            (Tuple _ 10) -> Bloom
            (Tuple Flower _) -> Bloom
            (Tuple Upwards _) -> Stalk (Automatic right)
            (Tuple Branch _) -> Fork (Manual (Stalk (Automatic left)))
                                     (Manual Bloom)
                                     (Manual (Stalk (Automatic right)))

main :: Effect Unit
main = do
  rand <- randomInt 1 5
  let ourPlant :: Fix Plant
      ourPlant = futu sow (Seed { height: 0, rand: rand })
  pure unit
