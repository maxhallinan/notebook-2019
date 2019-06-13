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

### Bottom-up

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
  >>> fmap (bottomUp f) -- recurse
  >>> In -- repack
  >>> f -- transform
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

### Top-down

Top-down is symmetrical to bottom-up.

Steps:

1. Apply f to the term
2. Unpack the term to access the children
3. Recursively traverse each child of the term with f
4. Repack the term

```haskell
bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f

topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown f = In <<< fmap (topDown f) <<< out f
```

### Questions

- Why aren't `Foldable` and `Traversable` sufficient abstractions of recursion?

## [Program Reduction: A Win for Recursion Schemes](http://www.newartisans.com/2018/04/win-for-recursion-schemes/)

## [Recursion Schemes, Part II: A Mob of Morphisms](https://blog.sumtypeofway.com/recursion-schemes-part-2/)

A function `f a -> a` is called an algebra:

```purescript
type Algebra f a = f a -> a
```

Algebra in this case means "reunification" or restoration from `f a` to `a`.

### Catamorphisms

```purescript
type Algebra f a = f a -> a

newtype Term f = In { out :: f (Term f) }
toTerm f = In { out: f }

cata :: forall f a. Functor f => Algebra f a -> Term f -> a
cata fn = unwrap >>> _.out >>> map (cata fn) >>> toTerm >>> fn
```

A catamorphism collapses a container of value `f a` into a single value `a`.

You can define different catamorphisms without having to redefine the recursion
itself.

```purescript
countNodes :: Algebra ExprF Int
countNodes (Unary x) = x.target + 1
countNodes (Binary x) = x.lhs + x.rhs + 1
countNodes (Call x) = x.func + sum x.args + 1
countNodes (Index x) = x.target + x.idx + 1
countNodes (Paren x) = x.target + 1
countNodes (Literal _) = 1
countNodes (Ident _) = 1

prettyPrint :: Algebra ExprF Doc
prettyPrint (Unary x) = (P.text x.op) <> x.target
prettyPrint (Binary x) = x.lhs <> (P.text x.op) <> x.rhs
prettyPrint (Call x) = x.func <> P.parens (P.punctuate "," x.args)
prettyPrint (Index x) = x.target <> P.brackets x.idx
prettyPrint (Paren x) = P.parens x.target
prettyPrint (Literal x) = P.int x.intVal
prettyPrint (Ident x) = P.text x.name

cata countNodes call
cata prettyPrint call
```

In both cases, you just define a transformation on the node.
You don't have to build in explicit recursion.
Then you can use the same recursion scheme (catamorphism) to apply these two
very different transformations.

#### Laws for catamorphisms

**1. Identity**

```
cata In call === id call
```

If you provide `In` as the `f` to `cata`, that's the same as calling `id` on the
second argument.

**2. Fuse**

```
cata (alg >>> fmap func) => (cata alg) >>> func
```

****

1. identity law
2. fuse
3. compose

## [Recursion Schemes, Part III: Folds in Context](https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/)

### Paramorphisms

Catamorphisms lose the context of the original structure.
The transformation only has access to the current node.

This is often a problem.
For the pretty printer catamorphism, if you wanted to check that args list is
empty, you'd have to use `==` to match on the doc type.
In this case, it would be nice to have access to the original representation.
This is called an R-algebra.

```purescript
type Algebra f a = f a -> a

type RAlgebra f a = f (Term f, a) -> a
```

In the tuple, `Term f` is the corresponding Term for `a`.
A _paramorphism_ enables you to traverse a structure with an R-algebra.
"Para" means "beside", "next to", or "alongside".
In this case, paramorphism means that you can view the original structure beside
the structure that is being transformed.

```purescript
para :: forall f a. Functor f => RAlgebra f a -> Term f -> a
para rAlg = out >>> map fanout >>> rAlg
  where fanout t = (t, para rAlg t)
-- or
para' :: forall f a. Functor f => RAlgebra f a -> Term f -> a
para' f = out >>> map (id &&& para' f) >>> f
```

```purescript
type RAlgebra f a = f (Term f, a) -> a

fastPretty :: RAlgebra Expr Doc
fastPretty _ (Literal x) = Pretty.int x.intVal
fastPretty _ (Ident x) = Pretty.text x.name
-- if this is a call to the identity function, replace it with its argument
fastPretty (In (Call { func: "id" })) (Call { args: [arg] }) = arg

para fastPretty call
```

### Anamorphism

A coalgebra is the dual of an algebra.

```purescript
type Algebra f a = f a -> a

type Coalgebra f a = a -> f a
```

An anamorphism is the dual of a catamorphism.
An anamorphism unfolds a value `a` into a structure `f a`.

```purescript
ana :: forall f a. Functor f => (a -> f a) -> a -> f a
cata :: forall f a. (f a -> a) -> f a -> a
```

```purescript
type Coalgebra f a = a -> f a

ana :: forall f a. Functor f => (a -> f a) -> a -> f a
```

The dual of a paramorphism is an apomorphism. 

```purescript
type RCoalgebra f a = a -> f (Either (Term f) a)
```

If the function returns a `Left`, then the apomorphism stops unfolding.
If it returns a `Right`, then the unfolding continues.

```purescript
apo f :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< map fanin <<< f 
  where fanin = either id (apo f)
```
