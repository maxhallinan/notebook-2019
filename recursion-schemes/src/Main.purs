module Main where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (sum)

data ExprF a 
  = Literal { intVal :: Int }
  | Ident { name :: String }
  | Index { target :: a, idx :: a }
  | Unary { op :: String, target :: a }
  | Binary { lhs :: a, op :: String, rhs :: a}
  | Call { func :: a, args :: Array a }
  | Paren { target :: a }

derive instance functorExprF :: Functor ExprF
derive instance eqExprF :: Eq a => Eq (ExprF a)

newtype Term f = In { out :: f (Term f) }
derive instance newtypeTerm :: Newtype (Term a) _

toTerm :: forall f. f (Term f) -> Term f
toTerm f = In { out: f }

ten :: Term ExprF
ten = toTerm (Literal { intVal: 10 })

add :: Term ExprF
add = toTerm (Ident { name: "add" })

call :: Term ExprF
call = toTerm (Call { func: add, args: [ten, ten]})

bottomUp :: forall a. Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn = 
  unwrap
  >>> _.out
  >>> map (bottomUp fn)
  >>> toTerm
  >>> fn

mystery :: forall f a. Functor f => (f a -> a) -> Term f -> a
mystery fn =
  unwrap 
  >>> _.out
  >>> map (mystery fn)
  >>> fn

countNodes :: ExprF Int -> Int
countNodes (Unary x) = x.target + 1
countNodes (Binary x) = x.lhs + x.rhs + 1
countNodes (Call x) = x.func + sum x.args + 1
countNodes (Index x) = x.target + x.idx + 1
countNodes (Paren x) = x.target + 1
countNodes (Literal _) = 1
countNodes (Ident _) = 1

newtype Attr f a = 
  Attr { attribute :: a
       , hole      :: f (Attr f a)
       }

type CVAlgebra f a = f (Attr f a) -> a 

histo :: forall f a. Functor f => CVAlgebra f a -> Term f -> a
histo h = unwrap >>> _.out >>> map worker >>> h
  where worker t = Attr { attribute: histo h t
                        , hole: map worker $ (_.out <<< unwrap) t
                        }

histo' :: forall f a. Functor f => CVAlgebra f a -> Term f -> a
histo' h = worker >>> unwrap >>> _.attribute
  where worker = unwrap >>> _.out >>> map worker >>> (h &&& id) >>> mkAttr
        mkAttr (a, b) = Attr { attribute: a, hole: b }

type Cent = Int

data Nat a 
  = Zero
  | Next a

derive instance functorNat :: Functor Nat

expand :: Int -> Term Nat
expand 0 = Zero
expand n = In (Next (expand $ n - 1))

compress :: Nat (Attr Nat a) -> Int
compress Zero = 0
compress (Next x) = 1 + (compress x)

coins :: Array Cent
coins = [50,25,10,5,1]

change :: Cent -> Int
change amount = histo go (expand amount)

main :: Effect Unit
main = do
  log "Hello sailor!"
