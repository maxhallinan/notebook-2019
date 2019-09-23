module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept, withExcept)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Console (log)
import Debug.Trace (spy)
import Data.Either (Either, either)
import Data.String.CodePoints (length)
import Foo (makeValidFoo, makeInvalidFoo)
import Foreign (F, Foreign)
import Foreign as F
import Foreign.Index as FI
import Data.Validation.Semigroup as V
{-- import Validation --}

newtype User = User { username :: String
                    , password :: String
                    , age :: Int
                    }

type Err = { field :: String, rule :: Rule }

data Rule = Length | Required | TooShort | Minimum

{-- toUser :: Decoder e o --}
{-- toUser = User <<< { username: _, password: _, age: _ } --}
  {-- <$> field "username" validateUsername --}
  {-- <*> field "password" validatePassword --}
  {-- <*> field "age" validateAge --}

{-- decode :: forall e i o. Decode i => Decoder e o -> i -> (Either e o) --}

validateUser :: User -> Either (Array Err) User
validateUser (User user) = V.toEither $
  (\u p a -> User { username: u, password: p, age: a })
  <$> validateName user.username
  <*> validatePassword user.password
  <*> validateAge user.age

validateName :: String -> V.V (Array Err) String
validateName name = 
  if length name > 0
  then pure name
  else V.invalid [{ field: "username", rule: Required }]

validatePassword :: String -> V.V (Array Err) String
validatePassword pw =
  if length pw == 0
  then V.invalid [{ field: "password", rule: Required }]
  else 
    if length pw < 6
    then V.invalid [{field: "password", rule: Length }]
    else pure pw

validateAge :: Int -> V.V (Array Err) Int
validateAge age =
  if age == 0
  then V.invalid [{ field: "age", rule: Minimum }]
  else pure age

decodeUser :: Foreign -> F User
decodeUser f = (\u p a -> User { username: u, password: p, age: a })
  <$> (errorsAt "username" $ FI.readProp "username" f >>= F.readString)
  <*> (errorsAt "password" $ FI.readProp "password" f >>= F.readString)
  <*> (errorsAt "age" $ FI.readProp "age" f >>= F.readInt)

errorsAt :: forall a. String -> F a -> F a
errorsAt prop = withExcept (map $ FI.errorAt prop)

data Foo = Bar String | Baz Int

decodeFoo :: Foreign -> F Foo
decodeFoo f = (Bar <$> F.readString f) <|> (Baz <$> F.readInt f)

decodeBaz :: Foreign -> F String
decodeBaz f = withExcept (map $ FI.errorAt "foo") $ F.readString f

data ReadResult e a
  = ReadError F.MultipleErrors
  | InvalidError e
  | Success a

readUser :: Foreign -> ReadResult (Array Err) User
readUser = 
  decodeUser
  >>> runExcept
  >>> either ReadError (validateUser >>> either InvalidError Success)

run :: forall e a. (Foreign -> F a) -> (a -> Either e a) -> Foreign -> ReadResult e a
run read validate =
  read
  >>> runExcept
  >>> either ReadError (validate >>> either InvalidError Success)

main :: Effect Unit
main = do
  -- foo <- makeValidFoo
  validUser <- map (run decodeUser validateUser) makeValidFoo 
  invalidUser <- map readUser makeInvalidFoo
  -- let x = spy "" $ decodeFoo foo
  logResult validUser
  logResult invalidUser
  pure unit

logResult :: ReadResult (Array Err) User -> Effect Unit
logResult r =
  case r of
    ReadError readErrs -> do
      let foo = spy "READ_ERROR" readErrs
      pure unit
    InvalidError errs -> do
      let foo = spy "INVALID_ERROR" errs
      pure unit
    Success a -> do
      let foo = spy "SUCCESS" a
      pure unit
