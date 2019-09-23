module Foo (makeValidFoo, makeInvalidFoo) where

import Effect (Effect)
import Foreign (Foreign)

makeValidFoo :: Effect Foreign
makeValidFoo = _makeValidFoo

makeInvalidFoo :: Effect Foreign
makeInvalidFoo = _makeInvalidFoo

foreign import _makeValidFoo :: Effect Foreign
foreign import _makeInvalidFoo :: Effect Foreign
