# PureScript's Foreign Function Interface

Dependencies:

- purescript-foreign: provides a datatype and functions for working with untyped 
  data.
- purescript-foreign-generic: adds support for datatype generic programming to 
  purescript-foreign.

## Calling a PureScript function from JavaScript

A function defined in a PureScript module:

```purescript
module Foo where

foo :: forall a. a -> a
foo x = x

bar :: String -> String -> String
bar x y = x <> y
```

Can be compiled and called in JavaScript:

```javascript
const Foo = require('Foo');

const x = Foo.foo(1);
// x === 1
```

PureScript functions are always compiled to functions of one argument, so when 
the function has multiple arguments, you invoke it like this:

```javascript
const y = Foo.bar('foo')('bar');
// 'foobar'
```

PureScript that has been bundled for the browser is accessible under the `PS`
namespace:

```javascript
const x = PS.Foo.foo(1);
const y = PS.Foo.bar('foo')('bar');
```

### Names

It is preferred to use names consisting of alphanumeric characters for values 
that are called from JavaScript.

When PureScript code uses a reserved word in JavaScript, PureScript prefixes the
word with `$$`:

This PureScript code:

```purescript
null = []
```

Becomes

```javascript
var $$null = [];
```

Special characters are escaped with a single dollar symbol:

```purescript
example' = 0
```

```javascript
var example$prime = 0
```

### Runtime Data Representation

A `Boolean` type compiles to `true` or `false`.
`Int` and `String` compiles to JavaScript numbers and strings.
`Array` corresponds to JavaScript's array.
Records evaluate to JavaScript objects.

### Representing ADTs

```purescript
data ZeroOrOne a = Zero | One a
```

```purescript
function One(value0) {
  this.value0 = value0;
}

One.create = function(value0) {
  return new One(value0);
}

function Zero() {}

Zero.value = new Zero();
```

A data constructor with two arguments:

```purescript
data Two a b = Two a b
```

```javascript
function Two (value0, value1) {
  this.value0 = value0;
  this.value1 = value1;
}

Two.create = function(value0) {
  return function(value1) {
    return new Two(value0, value1);
  }
}
```

Newtypes don't have a runtime representation. 
At runtime, they have the representation of the data constructor's argument.

### Using JavaScript code from PureScript

Foreign import declarations are functions/values that have a corresponding 
JavaScript declaration.
The PureScript and JavaScript files must have the same name.

src/Data/URI.js

```javascript
'use strict';

exports.encodeURIComponent = encodeURIComponent;
```

src/Data/URI.purs

```purescript
module Data.URI where

foreign import encodeURIComponent :: String -> String
```

### Wrapping JavaScript values

Using foreign import declarations like the last example only works for functions
with simple types.
Most JavaScript values doesn't correspond closely to PureScript values.
In these cases, the JavaScript values are "wrapped", forced to adhere to the 
PureScript runtime representation.

Common reasons to wrap a JavaScript value:

- JavaScript functions that take multiple arguments should behave like curried
  functions.
- Use the `Eff` monad to track JavaScript side-effects.
- Handle corner cases like `undefined` and `null`.

### Defining foreign types

Here is an unsafe function:

```javascript
exports.head = function head(xs) {
  return xs[0];
}
```

```purescript
foreign import head :: forall a. Array a -> a
```

We could import it like this, but it's still unsafe.
The type checker isn't able to verify its safety.
A different approach is to use a _foreign type_.

```purescript
foreign import data Undefined :: Type -> Type
foreign import head :: forall a. Array a -> Undefined a
foreign import isUndefined :: forall a. Undefined a -> Boolean

isEmpty :: forall a. Array a -> Boolean;
isEmpty = isUndefined <<< head
```

```javascript
exports.isUndefined = function (x) {
  return x === undefined;
};
```

### Functions of multiple arguments

In general, PureScript functions are curried.
Sometimes there are reasons that a function should not be curried.
Then the question is how to represent the function in the type system.
The `purescript-functions` package defines type constructors for these 
functions.
Those constructors look like: `Fn2 a b c` which is equivalent to `a -> b -> c`.
The difference is that the compiler will generate a function of two arguments,
rather than a curried function.
You must use `runFn2` to apply the function.

### Representing side effects

The representation of the `Eff eff a` monad is a function of no arguments that 
returns a value of type `a`.

```purescript
foreign import random :: forall eff. Eff (random :: RANDOM | eff) Number
```

```javascript
exports.random = function() {
  return Math.random();
}
```

```purescript
foreign import log :: forall eff. String -> (console :: CONSOLE | eff) Unit
```

```javascript
exports.log = function (s) {
  return function () {
    console.log(s);
  }    
}
```

### Working with untyped data

```javascript
import Data.Foreign
import Data.Foreign.Generic
import Data.Foreign.JSON

parseJSON :: String -> Foreign
decodeJSON :: forall a. Decode a => String -> F a
```
