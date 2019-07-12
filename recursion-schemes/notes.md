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

#### Histomorphism

A histomorphim first descends to the leaves of the tree.
Then it works back up toward the root of the tree.
As it does this, it builds up a second tree.
This second tree mirrors the structure of the first tree.
Each node on this tree has two things: the result of applying the function `f`
to that node and the original node.
Each time the function `f` is called, it can look through the history of the
traversal through the subtree below the current node.
So you get a history of the entire traversal, seeing what the intermediate values
were at each node of the tree.

This is called **course-of-value recursion**.
Course-of-value recrusion records intermediate values of the fold and preserves
the original structure.

The structure is represented by the `Attr` type:
The `attribute` field is the current value of the fold.
The `hole` field is the original subtree.

```purescript
newtype Attr f a = Attr { attribute :: a, hole :: f (Attr f a) }
```

The folding function is a course-of-value algebra, or a `CVAlgebra`.
A `CVAlgebra` is a function of this `Attr` type:

```purescript
type CVAlgebra f a = f (Attr f a) -> a
```

Here is a very simple of the history of the evaluation of the the expression
`In (Add (In (Num 1)) (In (Num 1)))`.

```purescript
Attr
  { attribute: 2
  , hole: Add (Attr
                { attribute: 1
                , hole: Num 1
                })
              (Attr
                { attribute: 1
                , hole: Num 1
                })
  }
```

Here is another example for history of the evaluation of the expression
`In (Add (In (Add (In (Num 1)) (In (Num 1)))) (In (Add (In (Num 1)) (In (Num 1)))))`.

```purescript
Attr
  { attribute: 4
  , hole: Add (Attr { attribute 2
                    , hole: Add (Attr { attribute: 1
                                      , hole: Num 1
                                      })
                                (Attr { attribute: 1
                                      , hole: Num 1
                                      })})
              (Attr { attribute 2
                    , hole: Add (Attr { attribute: 1
                                      , hole: Num 1
                                      })
                                (Attr {})
                    })
  }
```

##### A note on implementations of histo

This implemenation involves several folds/traversals of the structure.
At each node, it's calling `histo` recursively and it's calling `worker`
recursively.
So they split into two process and the work isn't shared.

```purescript
histo :: forall f a. Functor f => CVAlgebra f a -> Fix f -> a
histo f = unwrap >>> map worker >>> f
  where
    worker term = Attr { attribute: histo f term
                       , hole: map worker (unwrap term)
                       }
```

This is a more performant implementation.
In this implementation, `worker` descends completely to the leaves of the tree
before doing anything.
That's what `unwrap >>> map (\x -> worker x)` does.
This will go to the leaf nodes before it applies `mkAttr`.
Once the recursion terminates, then `mkAttr` is called on the node.
And `mkAttr` defines both the `attribute` and `hole` fields using that node.
So there is only one fold whereas in the first example, there are several folds
going on.

```purescript
histo' :: forall f a. Functor f => CVAlgebra f a -> Fix f -> a
histo' f = worker >>> getAttribute
  where
    worker = unwrap >>> map (\x -> worker x) >>> mkAttr
    mkAttr term = Attr { attribute: f term
                       , hole: term
                       }
    getAttribute = unwrap >>> _.attribute
```

##### Futumorphism

A futumorphism is dual to a histomorphism.
A futomorphism is also a kind of unfold, just like an anamorphism and an
apomorphism.

This requires using the dual of `Attr`.
The dual of `Attr f a` is `CoAttr f a`.
`Attr` is a product type.
`CoAttr` is a union type.

```purescript
newtype Attr f a = Attr { attribute :: a, hole :: f (Attr f a) }

data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))
```

Then you also need the dual of `CVAlgebra f a`.

```purescript
type CVAlgebra f a = f (Attr f a) -> a

type CVCoalgebra f a = a -> f (CoAttr f a)
```

The `CoAttr` constructors are used to control the unfold.
`Automatic` means to continue the unfold process "automatically".
I don't know what it means to "continue automatically"
`Manual` takes manual control of the unfold.
I also don't really know what that means.

```purescript
futu :: forall f a. Functor f => CVCoalgebra f a -> a -> Fix f
futu f = In <<< map worker <<< f
  where
    worker (Automatic a) = futu f a
    worker (Manual g) = In $ map worker g
```

```purescript
futu sow (Seed { height: 0})
In $ map worker $ f (Seed { height: 0 })
In $ map worker (Root (Automatic (Seed { height: 2 })))
In $ Root $ worker (Automatic (Seed { height: 2}))
In $ Root $ futu sow (Seed { height: 2 })
In $ Root $ In $ map worker $ sow (Seed { height: 2 })
In $ Root $ In $ map worker $ Fork (Manual (Stalk (Automatic (Seed { height: 3}))))
                                   (Manual Bloom)
                                   (Manual (Stalk (Automatic (Seed { height: 3}))))
In $ Root $ In $ Fork (worker (Manual (Stalk (Automatic (Seed { height: 3})))))
                      (worker (Manual Bloom))
                      (worker (Manual (Stalk (Automatic (Seed { height: 3})))))
In $ Root $ In $ Fork (In $ map worker (Stalk (Automatic (Seed { height: 3}))))
                      (In $ map worker Bloom)
                      (In $ map worker (Stalk (Automatic (Seed { height: 3}))))
In $ Root $ In $ Fork (In $ Stalk $ worker (Automatic (Seed { height: 3})))
                      (In $ map worker Bloom)
                      (In $ Stalk $ worker (Automatic (Seed { height: 3})))
In $ Root $ In $ Fork (In $ Stalk $ futo sow (Seed { height: 3}))
                      (In $ Bloom)
                      (In $ Stalk $ futo sow (Seed { height: 3}))
```

So I think if you use `Automatic`, then that means `futu` is called again.
And if you call `futu` again, then `f` is called again.
But if you use `Manual`, then only `worker` is called again.
`f` is not called again until you hit another `Automatic`.
So `Manual (Root (Manual Stalk (Manual Bloom)))` just becomes `In (Root (In (Stalk (In Bloom)))`.
You can manually specify not just one new node in the unfolding structure, you
can specify a whole subtree.
You can't do that with `Automatic`.
The type prevents you from doing that.
`Automatic` is used to add a single new node to the unfolding structure.
`Manual` is used to insert a substructure into the unfolding structure.

###### futuM

The example given for `futu` is a cellular automata.
This cellular automata required a random number generator.
Random number generation in PureScript requires an `Effect`.
In Haskell, there's a way to inject a generator, so you don't need to build
`IO` into the function.

My first attempt was to take the type `CVCoalgebra Plant Seed` and make it into
a `CVCoalgebra Effect Seed`.
The first type is saying that the function takes a `Seed` as an argument and
returns an `Plant (CoAttr Plant Seed)`.
So instead of `Seed -> Plant (CoAttr Plant Seed)`, I would be returning a
`Seed -> Effect (CoAttr Effect Seed)`.
Which means that my `CoAttr` is either `Automatic (Seed { height: 1 })` or
`Manual (Effect (Manual (Effect (Manual (Effect (...))))))` or
`Manual (Effect (Manual (Effect (Automatic (Seed { height: 1 })))))`.
You can never get to the base case of `Bloom`.
And my `sow` and `grow` functions didn't work because of the recursion.
They kept trying to interleave `CoAttr` and `Plant`, not lifting into `Effect`
each time.
So it didn't compile.

What I realized I needed is another `futu` implementation that is for monads.
This allows you to get back a `m (Fix f)` instead of a `Fix f`.
This requires a change to the coalgebra type and to the futu implementation.

```purescript
type CVCoalgebraM m f a = a -> m (f (CoAttr f a))

futuM :: forall m f a. Monad m => Traversable f => Functor f => CVCoalgebraM m f a -> a -> m (Fix f)
futuM f = go
  where
    go a = map In <<< traverse worker =<< f a
    worker (Automatic a) = futuM f a
    worker (Manual g) = map In $ traverse worker g
```

How does this work?

1. Call `f` on `a` which returns `m (f (CoAttr f a))`
1. Now you want to apply `worker` to the inner `CoAttr`.
1. You have two layers you must lift over: `m` and `f`.
1. Use `=<<` to lift `traverse worker` over the layer `m`.
1. Use `traverse` to lift `worker` over `f`
1. `worker` is `CoAttr f a -> m (Fix f)`
1. `traverse worker` is `f (CoAttr f a) -> m (f (Fix f))`
1. Finally, you need to go from `m (f (Fix f))` to `m (Fix f)`.
1. So use `map` to lift `In` over `m`, transforming `m (f (Fix f))` into `m (Fix f)`.

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

1. Why is it call `Attr` and not something like `History`?

## [Base Functors](https://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/)

It's a bit tedious to have to deal with `Fix` explicitly.
It would be nicer to just deal with the plain underlying data type.
The `Base` type family and the `Recursive` and `Corecursive` type classes enable
this.

Use Haskell's `[a]` as an example.
`[a]` is defined as:

```haskell
infixr 5 :
data [] a = a : [a] | []
```

You can't use this with `Fix` because it prevents elements from being stored in
the list.
The cons cells are filled with `In`:

```haskell
foo :: Fix [a]
foo = In : In : In : In : []
```

To use with `Fix`, you have to define the list type in a slightly different way:

```haskell
data ListF a b = Nil | Cons a b
```

Then you can store stuff in the List this way:

```haskell
foo :: Fix ListF
foo = In (Cons 1 (In (Cons 2 (In (Cons 3 (In Nil))))))
```

However, using alternate representations for common data types is tedious.
Instead, you can use the `Base` type family to associate the plain data type
with its "base" representation.

```haskell
type family Base t :: * -> *

instance Base [a] = ListF a
```

`Base [a]` is another way of writing `ListF a`.
I'm not sure I understand why you can't just use a type alias here.

This only gets us halfway.
Then the `Recursive` and `Corecursive` type classes are used to go between the
plain data type and its base representation.
To define an instance of these type classes, the data type must be a member of
the `Base` type family.

`Recursive` gives you two things:

- `project`: a way to go from the plain type to the `Base` representation
- `cata`: a way to fold the plain type by way of the `Base` representation

```haskell
class (Functor (Base t)) => Recursive t where
  project :: t -> Base t t
  cata :: (Base t a -> a) -> t -> a
```

`Corecursive` is dual to `Recursive`.
`Corecursive` also gives you two things:

- `embed`: a way to go from the base representation to the plain type
- `ana`: an unfold from an initial seed value to the plain type, by way of the 
  base representation.

```haskell
class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t
  ana :: (a -> Base t a) -> a -> t
```

So the algebras are always dealing with the Base representation, e.g. with 
`ListF a` instead of `[a]`.
But the inputs and outputs are always the plain representation, e.g. `[a]`.

`cata` can be defined in terms of `project` and `ana` can be defined in terms of
`embed`:

```haskell
cata :: (Base t a -> a) -> t -> a
cata f = c where c = f . fmap c . project

ana :: (a -> Base t a) -> a -> t
ana g = a where a = embed . fmap a . g 
```

`Recursive` and `Corecursive` instances for `ListF a`:

```haskell
instance Recursive [a] where
  project (x:xs) = Cons x xs
  project [] = Nil

instance Corecursive [a] where
  embed (Cons x xs) = Cons x : xs
  embed Nil = []
```

Because of the MINIMAL pragma, you don't need to define `cata` and `ana`.

Then you can sum the items in a list like this:

```haskell
sumList :: Num a => [a] -> a
sumList = cata go where
  go Nil = 0
  go (Cons a acc) = a + acc
```

Because `Recursive` has an instance for `[a]`, you can use `cata` without 
explicitly using `Fix`.

**Questions***

Question: why don't you have to define `project` and `embed` recursively? 
Question: why does the recursion-schemes package use `para` and `apo` instead of
`cata` and `ana`?
Question: where did `Fix` go?

### PureScript's approach

PureScript does not have type families.
Matryoshka has `Corecursive` and `Recursive` classes.

```purescript
class (Functor f) <= Recursive t f | t -> f where
  project :: t -> t f
```

```purescript
class (Functor f) <= Corecursive t f | t -> f where
  embed :: f t -> t
```

### Explanation

Recursion schemes are a way to separate the logic of traversing a data structure
from the logic that transforms the data structure.

To sum a list:

We might be used to seeing `List` defined this way:

```purescript
data List a = Nil | Cons a (List a)
```

List is a recursive type.
The recursion is hardcoded into the type.

To sum a `List Int`, we might write a function like this:

```purescript
sumList :: List Int -> Int
sumList Nil = 0
sumList (Cons h t) = h + sumList t
```

Then we can sum a list of ints like this:

```purescript
sumList (Cons 1 (Cons 2 (Cons 3 Nil)))
-- 6
```

Now, if we wanted to get the product of the list, we have to duplicate the 
recursion logic.

```purescript
prodList :: List Int -> Int
prodList Nil = 1
prodList (Cons h t) = h * prodList t
```

A second way to do this is to use a fold:

```purescript
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
```

I'm not sure that using `List` is such a strong motivating example.
A `Foldable` instance accomplishes the same thing.

Question: what problem does Recursion schemes solve that Foldable doesn't?

This is how to do the same thing with recursion schemes:

First, remove the explicit recursion from the List type.
Define a `ListF a t` type.

```purescript
data ListF a t = Nil | Cons a t 

derive instance functorListF :: Functor (ListF a)
```

Now the question is, how do we make a list using this type?

```purescript
foo :: ListF Int ?
foo = Cons 1 ?

foo :: ListF Int (ListF Int ?)
foo = Cons 1 (Cons 2 ?)
```

Now we can see two problems.
`ListF` is not a recursive type.
We can't construct an arbitrarily recursive `ListF` in the way that we can 
construct an arbitrarily recursive `List Int`.
Each layer of recursion has to be spelled out explicitly in the type.
Second, if we try to make `ListF` recursive, we find that we can't terminate 
`ListF`.
We have to stick some other type at the end.

```purescript
foo :: ListF Int (ListF Int (ListF Int ?))
foo = Cons 1 (Cons 2 Nil)
```

So `ListF` is more like a tuple.
But we want it to have the same recursive properties as our original `List` 
type.

This is where the fixed point combinators come in.
The Y-combinator is a combinator that makes a function recursive.
There are also fixed-point combinators that can be used to make any type with 
kind * -> * recursive.
These are called the fixed points of functors.
One of these is the least-fixed point combinator, or Mu after the Greek letter.

```purescript
newtype Mu f = In (f (Mu f))
```

So we can see that `Mu` applies the functor `f` to `Mu f`.

`Mu` can be used like this:

```purescript
foo :: Mu (ListF Int)
foo = In Nil

bar :: Mu (ListF Int)
bar = In (Cons 1 (In Nil))

baz :: Mu (ListF Int)
baz = In (Cons 1 (In (Cons 2 (In Nil))))
```

So `Mu ListF` has the same recursive property as our original `List a` type.
Let's call `Mu ListF` `List`.

```purescript
type List a = Mu (ListF Int)
```

For the sake of convenience, let's write some functions that construct a list for
us:

```purescript
nil :: forall a. List a
nil = In Nil

cons :: forall a. a -> List a -> List a
cons h t = In (Cons h t)
```

Now, `Mu` isn't the only fixed point combinator.
We'd rather not tie our type to any particular fixed point combinator.
So instead of explicitly constructing the type `Mu` using `In`, we create a 
typeclass called `Recursive` that gives us a way to construct anything like `Mu`.
todo: say this better

```purescript
class Functor f <= Corecursive t f | t -> f where
  embed :: f t -> t
```

And we define an instance for `Mu`.

```purescript
instance corecursiveMu :: Functor f => Corecursive (Mu f) f where
  embed = In
```

Now we can refactor our smart constructors this way:

```purescript
nil :: forall a. List a
nil = embed Nil

cons :: forall a. a -> List a -> List a
cons h t = embed (Cons h t)
```

`Recursive` works in the opposite way.
Where `Corecursive` is an "unfold", `Recursive` is a "fold".

```purescript
class Functor f <= Recursive t f | t -> f where
  project :: t -> f t  

instance recursiveMu :: Functor f => Recursive (Mu f) f where
  project (In x) = x
```

Now there are two things left to do.
Define a recursion scheme and define an "algebra".
Algebra is a term for the type of function that the recursion scheme will apply
to the data structure, in this case `List a`.
There are many different kinds of algebras.
The algebra we'll define is just called algebra.
todo: maybe leave this out until the end.

```purescript
type Algebra f a = f a -> a

sumList :: Algebra ListF Int
sumList Nil = 0
sumList (Cons x y) = x + y
```

So now we've written our transformation without including any of the 
transformation logic.

Finally, the recursion scheme `cata`.

```purescript
cata :: forall f a. Algebra f a -> f a -> a
cata f = go
  go x = f <<< map go <<< project x
```

First unwrap our `Mu (ListF Int)` to `ListF Int`.
Then lift `go` over `ListF`.
This continues until you hit the base case `Nil`.
Then it starts working back up, applying `f` to each cons cell until you've got
the result.

And then:

```purescript
cata sumList foo
-- 6
```

Questions:

- What is the most effective way to organize all of this information?
- What do you need to do to introduce these concepts to beginners?
- What is the most minimal definition of recursion schemes?
- What is a good motivating example, one where foldl doesn't work?

#### Hylomorphism

catamorphism: fold a data structure
anamorphism: unfold a data structure
paramorphism: fold with additional information
apomorphism: unfold with additional information
histomorphism: fold with the complete history of the fold 
futumorphism: unfold with control flow

What happens when you compose a fold and an unfold?

Anamorphism is an unfold: a -> t a
Catamorphism is a fold: t a -> a.
Hylorphism transforms `a` into `t b`,  then `t b` into `b`.
A hylomorphism takes both an Algebra and a Coalgebra.
The Coalgebra produces values for the Algebra to consume.
This is a "refold".

```purescript
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = ana coalg >>> cata alg 
```

"Hylo" means matter in Greek, meaning the substance out of which an object is 
formed.

> ...we can read 'hylomorphism' as a function that forms a result object out of
> some intermediate, constituent matter.

Uses for a hylomorphism:

- data aggregations like finding the mean or median of a dataset.
- divide and conquer techniques like quicksort, mergesort, fourier transform
- determine difference between data structures

##### Reverse polish notation calculator with hylomorphism

In postfix notation (or Reverse Polish notation), all of the operands come 
first, followed by all of the operators.

```purescript
data Token
  = Lit Int
  | Op (Int -> Int -> Int)
```

```purescript
parseToken :: String -> Token
parseToken "+" = Op (+)
parseToken "-" = Op (-)
parseToken "*" = Op (*)
parseToken "/" = Op div
parseToken num = Lit $ read num 
```

```purescript
data Lst a b 
  = Cons a b
  | Nil
  deriving (Show, Eq, Functor)

type Coalgebra f a = a -> f a

parseRPN :: Coalgebra (List Token) String
parseRPN "" = Nil
parseRPN str = Cons token newSeed
  where (x, rest) = span (not . isSpace) str
        token     = parseToken x
        newSeed   = dropWhile isSpace rest

type Algebra f a = f a -> a

-- f :: List Token

type Stack = [Int]

evalRPN :: Algebra (List Token) (Stack -> Stack)
evalRPN Nil stack = stack
evalRPN (Cons (Lit i) cont) stack = cont (i : stack)
evalRPN (Cons (Op fn) cont) (a:b:rest) = cont (fn b a : rest)
evalRPN _ stack = error ("too few arguments on stack" <> show stack)
```


