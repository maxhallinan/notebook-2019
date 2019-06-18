module MatEx where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Foldable (class Foldable, foldr, foldl, foldMap)

data List a = Nil | Cons a (List a)

instance foldableList :: Foldable List where
  -- foldr :: forall a b. (a -> b -> b) -> b -> t a -> t b
  foldr _ y Nil = y
  foldr f y (Cons h t) = foldr f (f h y) t

  -- foldl :: forall a b. (b -> a -> b) -> b -> t a -> t b
  foldl _ y Nil = y
  foldl f y (Cons h t) = foldl f (f y h) t

  -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  foldMap _ Nil = mempty
  foldMap f (Cons h t) = f h <> foldMap f t

foo :: List Int
foo = Cons 1 (Cons 2 (Cons 3 Nil))

main :: Effect Unit
main = do
  -- 6
  logShow $ foldr (+) 0 foo
  -- 6
  logShow $ foldr (*) 1 foo
