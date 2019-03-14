# Recursion Schemes

## [An Introduction to Recursion Schemes](https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/)

- composable combinators
- generalizes the process of traversing recursive data structures
- replaces ad-hoc recursive functions of recursive data
- separates _how_ the function traverses the data structure and what the function
  does at each step

## A naively implemented AST

A basic AST, as an example of a recursive data structure:

```haskell
data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data Expr 
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary String Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Lit
  deriving (Show, Eq)

data Stmt 
  = Break
  | Continue
  | Empty
  | IfElse Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | While Expr [Stmt]
  | Expression Expr
  deriving (Show, Eq)
```

A recursive function of the AST:

```haskell
flatten :: Expr -> Expr
flatten (Literal i) = Literal i
flatten (Paren e) = flatten e
flatten (Index e i) = Index (flatten e) (flatten i)
flatten (Call e args) = Call (flatten e) (map flatten args)
flatten (Unary op arg) = Unary op (flatten arg)
flatten (Binary l op r) = Binary (flatten l) op (flatten r)
```

It's tedious to write this function.
There's a lot of just recursing on nested data.
There's a lot of syntactic noise, easy to make mistakes when modifying the
definition.

Can write a function `apply` that transforms each node of the AST:

```haskell
applyExpr :: (Expr -> Expr) -> Expr -> Expr
applyExpr f (Literal i) = Literal i
applyExpr f (Paren p) = Paren (f p)
applyExpr f (Index e i) = Index (f e) (f i)
applyExpr f (Call e args) = Call (f e) (map f args)
applyExpr f (Unary op args) = Unary op (f arg)
applyExpr f (Binary l op r) = Binary (f l) op (f r)

flatten :: Expr -> Expr
flatten (Paren e) = flatten e
flatten x = applyExpr flatten x
```

The problem is that for every new datatype, a new apply function has to be 
written.

> A sufficiently smart compiler could write them for us. And GCH, being a very
> smart compiler, can.

First, the data structure is generalized:

### Generalize the AST

```haskell
data ExprF a 
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a 
  | Literal Lit
  deriving (Show, Eq, Functor)
```

Because this type is higher-kinded, an instance of `Functor` can be derived for 
`ExprF`.
To automatically derive the Functor instance, you have to use GHC's `DeriveFunctor`
extension.
Instances of `Foldable` and `Traversable` can also be derived.

### First problem: recursion to arbitrary depth

`Expr` can represent arbitrarily-nested `Expr` values but `ExprF` cannot.

- `ExprF Lit` is not recursive.
- `ExprF (ExprF Lit)` is recursive to one depth.
- `ExprF (ExprF (ExprF Lit))` is recursive to two-depths.

And so on. 
The recursion is not arbitrarily nested.

To represent arbitrary recursive depth, the type substituted for `a` must yield
arbitrarily nested `ExprF` values.

The Y-combinator `y(f)` repeatedly applies `f` to itself.
You can define a Y-combinator for types too.
The type looks like this: `Y f = f (Y f)`.
So `f` must have kind `* -> *`.

```haskell
data Baz a 
  = Foo 
  | Bar a

-- unwraps the In structure
newtype Term f = In { out :: (f (Y f)) }

-- Term Baz could be (In Foo)
-- Term Baz could be (In (Bar (In Foo)))
-- Term Baz could be (In (Bar (In Bar (In Foo))))
-- the Term type enables Baz to be arbitrarily recursive
```

Applying the `In` constructor to each node of the AST enables the type to be 
arbitrarily recursive without having to hardcode the recursion into the AST's
type definition.

The `out` function can be used to unwrap `Y ExprF` into an arbitrarily recursive 
`ExprF a`.

This is called [_codata_](https://tac-tics.net/blog/data-vs-codata).

## Generic traversals

Bottom-up traversal steps:

1. Unpack the term to access its children
2. Recursively traverse each child of the unpacked term with f
3. Repack the term
4. Apply f to it

This means that the child nodes are transformed by f before transforms the 
current node.

```haskell
bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f =
  out -- unpack
  >> fmap (bottomUp f) -- recurse
  >> In -- repack
  >> f -- transform
```

`bottomUp` is a recursion scheme.
This will transform any recursive data structure defined with Term.
Here is the `flatten` function defined with `bottomUp`.

```haskell
Expr = Term ExprF

flattenTerm :: Expr -> Expr
flattenTerm (In (ParenF e)) = e
flattenTerm x = x

flatten :: Expr -> Expr
flatten = bottomUp flattenTerm
```

### Questions

- Why aren't `Foldable` and `Traversable` sufficient abstractions of recursion?
