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

A coalgebra is dual to an algebra.

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

## Recap

Recursion schemes are generalized folds of recursive structures.
You generalize by taking your recursive structure and parameterizing it.

### Representing recursive types

Recursion schemes are generalized to all recursive types.
They separate the recursion logic for any function of a recursive type from
logic that is specific to that type.
For this to work, you must also generalize the type's recursive definition.
To do this, replace the recursive bits with a type parameter.

For example, this:

```purescript
data Arith =
  Val Int
  Sum Arith Arith
```

becomes this:

```purescript
data ArithF a =
  Val Int
  Sum a a
```

Then you used a fixed-point combinator to make the type arbitrarily recursive.
`Fix` here represents the least-fixed-point combinator, also called Mu.
There is also a greatest-fixed-point combinator, Nu.

```purescript
data ArithF a =
  Val Int
  Sum a a

data Fix f = In (f (Fix f))

data Arith = Fix ArithF
```

Now the type `Arith` can be traversed using a recursion scheme that knows
only knows about the type `Fix` and nothing about the type `ArithF`.

**Questions**

- Why do you have to go from an explicitly recursive type to one that is made
  recursive using a fixed-point combinator? What problem does that solve?

Links

- [purescript-fixed-points](https://pursuit.purescript.org/packages/purescript-fixed-points/5.1.0)

### Varieties of recursion schemes

#### Catamorphism

A catamorphism is a recursion scheme that breaks down structure.
Just as a fold on a list transforms a `List a` to an `a`, so a catamorphism
transforms any `Fix f` to `a`, by way of a function `f a -> a`.
Cata comes from a Greek word meaning "downwards", "into", or "collapse".

```purescript
cata :: forall f a. Functor f => (f a -> a) -> Fix f -> a
```

The function `f a -> a` is called an Algebra.
Algebra comes from a word meaning "restoration" or "reunion".
The reunion here is that a structure `f a`, which can contain many instances of
`a`, is collapsed to a single `a`.

```purescript
type Algebra f a = f a -> a

cata :: forall f a. Functor f => (f a -> a) -> Fix f -> a
cata f = unwrap >>> map (\x -> cata f x) >>> f
```

This is a bottom-up traversal. The function `f` is applied to the leaf nodes of
the structure first, then cata works its way back up to the root node.

We could evaluate our add expression using cata.
First, we just define our algebra.

```purescript
add :: Algebra AddF Int
add (Num n) = n
add (Add n1 n2) = n1 + n2
```

Then we can use `cata` to apply `add` to an arbitrarily recursive expression.

```purescript
one :: Fix AddF
one = In (Num 1)

two :: Fix AddF
two = In (Add one one)

evalAdd :: Fix AddF -> Int
evalAdd = cata add

evalAdd two
2
```

That evaluates in this way:

```purescript
evalAdd two
cata add (In (Add (In Num 1) (In Num 1)))
add $ map (\x -> cata add x) (unwrap (In (Add (In Num 1) (In Num 1))))
add $ map (\x -> cata add x) (Add (In Num 1) (In Num 1))
add $ map (\x -> cata add x) (Add (In Num 1) (In Num 1))
add $ Add (cata add (In (Num 1))) (cata add (In (Num 1)))
add $ Add (add $ map (\x -> cata add x) (unwrap (In (Num 1)))) (add $ map (\x -> cata add x) (unwrap (In (Num 1))))
add $ Add (add $ map (\x -> cata add x) (Num 1)) (add $ map (\x -> cata add x) (Num 1))
add $ Add (add $ (Num 1)) (add $ (Num 1))
add $ Add 1 1
2
```

#### Anamorphism

> A function that generates a sequence by repeated application of itself to its
> previous result.
>
> https://en.wikipedia.org/wiki/Anamorphism

> In functional programming, an anamorphism is a generalization of the concept
> of unfolds on coinductive lists.
> Formally, anamorphisms are generic functions that can corecursively construct
> a result of a certain type and which is parameterized by functions that
> determine the next single step of construction.
>
> https://en.wikipedia.org/wiki/Anamorphism

An anamorphism is the dual of a catamorphism.

##### Side note: Inductive and Coinductive types

> An Inductive Data Type is a Data Type whose elements can be "built" starting
> from some basic elements by means of some "data constructors".
> The constructors can use elements of the inductive Data Type itself as
> arguments (that's why we call them "recursive" Data Types; in Haskell they are
> also called "algebraic" Data Types), but also elements of other data types.
>
> http://www.dipmat.unict.it/~barba/PROG-LANG/PROGRAMMI-TESTI/READING-MATERIAL/NotesTypesHASK.htm

An example of an inductive type in Haskell is the `List`.
Coinduction is dual to induction.
If induction is defined as the least solution of a certain form of inequation,
then a coinductive type is the greatest solution of a certain form of
inequation.

#### Paramorphism

"Para-" means next to, alongside, or beside.

A catamorphism is a bottom-up transformation.
For each call of `f`, any sub-expressions within `f` have already been
transformed.
This means that `f` loses the information within those subexpressions.
For example, if you are using `cata` to evaluate a tree of `Add` expressions,
then `add` will get something like this:  `Add 2 1`.
`add` doesn't know if those numbers were originally literals (`Num Int`) or the
result of sub-expressions, e.g. the 2 might be `Add 1 1` and the 1 might be
`Add 1 0`.

Sometimes it is useful to have that information.
A paramorphism gives you that information for each sub-expression alongside the
result of apply f to the sub-expression.

Here's what that looks like.

The argument to an `Algebra AddF Int` looks like this:

```purescript
Num 1
Add 1 1
```

The argument to an `RAlgebra AddF Int` looks like this:

```purescript
Num 1
Add (Tuple (In (Num 1)) 1) (Tuple (In (Num 1)) 1)
```

The tuple is only found where `a` is found in the type definition, only where
there are instances of recursion.

The paramorphism itself is defined this way:

```purescript
para :: forall f a. Functor f => RAlgebra f a -> Fix f -> a
para f = unwrap >>> map (\x -> fanout x) >>> f
  where fanout x = Tuple x (para f x)
```

This is how that evaluates:

```purescript
para addP (In (Add (In (Num 1)) (In (Num 1))))
addP $ map (\x -> Tuple x (para addP x)) (unwrap (In (Add (In (Num 1)) (In (Num 1)))))
addP $ map (\x -> Tuple x (para addP x)) (Add (In (Num 1)) (In (Num 1)))
addP $ Add $ (Tuple (In (Num 1)) (para addP (In (Num 1)))) (Tuple (In (Num 1)) (para addP (In (Num 1))))
addP $ Add $ (Tuple (In (Num 1)) (addP $ map addP (unwrap (In (Num 1))))) (Tuple (In (Num 1)) (addP $ map addP (unwrap (In (Num 1)))))
addP $ Add $ (Tuple (In (Num 1)) (addP $ map addP (Num 1))) (Tuple (In (Num 1)) (addP $ map addP (Num 1)))
addP $ Add $ (Tuple (In (Num 1)) (addP (Num 1))) (Tuple (In (Num 1)) (addP (Num 1)))
addP $ Add $ (Tuple (In (Num 1)) 1) (Tuple (In (Num 1)) 1)
addP $ Add (Tuple (In (Num 1)) 1) (Tuple (In (Num 1)) 1)
2
```

#### Apomorphisms

An apomorphism is dual to a paramorphism.
"Par-" means beside.
"Apo-" means away form or separate.

A paramorphism applies an R-algebra to a structure `Fix f`.

```purescript
type RAlgebra f a = f (Tuple (Fix f) a) -> a

para :: forall f a. Functor f => RAlgebra f a -> Fix f -> a
```

An apomorphism will take a type `a` and build up a structure `Fix f` by applying
an R-coalgebra.

```purescript
type RCoalgebra f a = a -> f (Either (Fix a) a)

apo :: forall f a. Functor f => RCoalgebra f a -> a -> Fix a
```

The implementation works by first applying `f` to `a`.
`f` returns `f (Either (Fix f) a)`.
`map` lifts over the returned `f`.
`fanin` is applied to the inner `Either (Fix f) a`.
If the `Either` is a `Left`, then `identity` is applied to the inner `Fix f`.
If the `Either` is a `Right`, then `apo f` is applied to the inner `a`.
This keeps happening until `f` produces a `Left`.
Then this `Left` is packed into the `In` constructor, producing the return type
`Fix f`.

```purescript
apo :: forall f a. Functor f => RCoalgebra f a -> a -> Fix f
apo f = In <<< map (\x -> fanin x) <<< f
  where
    fanin :: Either (Fix f) a -> Fix f
    fanin = either identity (\x -> apo f x)
```

If the R-coalgebra returns a `Left`, then the process is stopped.
If the R-coalgebra returns a `Right`, then the process goes another iteration.

I tried to find an example use-case for an apomorphism.
Here is a very contrived example:

```purescript
buildNum :: RCoalgebra AddF Int
buildNum n
  | n < 0 = Add (Left $ In (Num 0)) (Left $ In (Num 0))
  | n == 0 = Add (Left $ In (Num 1)) (Left $ In (Num 0))
  | n == 1 = Add (Left $ In (Num 1)) (Left $ In (Num 0))
  | otherwise = Add (Right $ n - 1) (Right $ n - 2)
```

```purescript
apo buildNum 3
In $ map (x -> either identity (\y -> apo buildNum y) x) $ buildNum 3
In $ map (x -> either identity (\y -> apo buildNum y) x) $ Add (Right 2) (Right 1)
In $ Add $
  (either identity (\x -> apo buildNum x) (Right 2))
  (either identity (\x -> apo buildNum x) (Right 1))
In $ Add $ (apo buildNum 2) (apo buildNum 1)
In $ Add $
  (In $ map (either identity (apo buildNum)) $ (buildNum 2))
  (In $ map (either identity (apo buildNum)) $ (buildNum 1))
In $ Add $
  (In $ map (either identity (apo buildNum)) $ (Add (Right 1) (Right 0)))
  (In $ map (either identity (apo buildNum)) $ (Add (Left (In (Num 1))) (Left (In (Num 0)))))
In $ Add $
  (In $ Add $ (either identity (apo buildNum) (Right 1)) (either identity (apo buildNum) (Right 0))
  (In $ Add $ (either identity (apo buildNum) (Left (In (Num 1)))) (either identity (apo buildNum) (Left (In (Num 0))))
In $ Add $
  (In $ Add $ (apo buildNum 1) (apo buildNum 0)
  (In $ Add $ (In (Num 1)) (Num 0))
In $ Add $
  (In $ Add $ (In $ map (either identity (apo buildNum)) $ buildNum 1) (In $ map (either identity (apo buildNum)) $ buildNum 0)
  (In $ Add $ (In (Num 1)) (Num 0))
In $ Add $
  (In $ Add $ (In $ map (either identity (apo buildNum)) $ (Add (Left $ In (Num 1)) (Left $ In (Num 0)))) (In $ map (either identity (apo buildNum)) $ (Add (Left $ In (Num 1)) (Left $ In (Num 0)))
  (In $ Add $ (In (Num 1)) (Num 0))
In $ Add $
  (In $ Add $ 
    (In $ Add (either identity (apo buildNum) (Left (In (Num 1)))) (either identity (apo buildNum) (Left (In (Num 0))))
    (In $ Add (either identity (apo buildNum) (Left (In (Num 1)))) (either identity (apo buildNum) (Left (In (Num 0))))
  (In $ Add $ (In (Num 1)) (Num 0))
In $ Add $
  (In $ Add $ 
    (In $ Add (In (Num 1)) (In (Num 0)))
    (In $ Add (In (Num 1)) (In (Num 0)))
  (In $ Add $ (In (Num 1)) (Num 0))
```

So this builds up a structure of `Fix AddF` equivalent to the int 3.

```purescript
(cata add <<< apo buildNum) 3 == 3
```

### Questions

- Best way to represent the Term type?
- Why does `Term f` use the record syntax: `In { out: f (Term f) }`.
- What is the difference between the "least fixed point" and the "greatest fixed
  point" combinators.
- Go back and review the laws for a catamorphism.
- I don't understand this definition of `cata`: https://github.com/slamdata/purescript-matryoshka/blob/v0.4.0/src/Matryoshka/Fold.purs#L40-L40

Omar Rizwan was tweeting about how the thing about Haskell is that, as a beginner,
you want to know how you can do side-effects because that's the motivation.
You're learning about this stuff so that you can do something cool with it and
the doing something cool is always a side-effect.
I wonder if you can somehow abstract this away completely, so you can do
something cool in the beginning without having to worry about understanding
monads, and then build toward the understanding by starting with fundamentals,
but using the fundamentals to do interesting things that include side-effects.
I'm not even sure I agree with the premise.
I really like The Little Schemer because it is completely without side-effects.
It's just about these abstract logical ideas.

How I learned Haskell.
1. Get confused. Use reading as a way to induce confusion.
2. Write out the evaluation of a confusing expression.

Revisit anamorphism
