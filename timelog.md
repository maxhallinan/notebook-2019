# Time

## 2019-07-08 - 2019-06-14

| Date       | Tomatoes  | Time |
|------------|-----------|------|
| 2019-07-08 | 11 @ 0:25 | 4:35 | 
| 2019-07-09 |  5 @ 0:25 | 2:05 | 
| 2019-07-19 |  6 @ 0:25 | 2:30 | 

### 2019-07-10

Lisp stepper

- Attempted a second time with Elm. Ran into a problem with top-down traversal
  of the Lisp AST.
- Tried to debug the stack overflow in my PureScript implementation. No success.

### 2019-07-09

Lisp stepper

- Tried again to model the evaluated tree in a way that I could "step" through
  the evaluation. No success.

### 2019-07-08

Lisp stepper

- Switched from Elm to PureScript because I thought using simpler types would 
  make the problem easier to think about.
- Implemented a bare bones Lisp evaluator
- Tried a number of different ways to model the evaluated tree and ran into some
  problems. No success.

## 2019-07-01 - 2019-06-07

| Date       | Tomatoes  | Time |
|------------|-----------|------|
| 2019-07-05 |  8 @ 0:25 | 3:20 | 
| 2019-07-06 |  6 @ 0:25 | 2:30 | 

Tomatoes: 14
Hours: 5:50

### 2019-07-06

Lisp stepper

- Made an attempt at using `histo` to evaluate with history.
- Implemented `runProgram`.
- Blocked by an infinite recursion bug in `eval`.

### 2019-07-05

Lisp stepper

- Finished implementing `eval` in terms of `cataM`.

## 2019-06-24 - 2019-06-30

| Date       | Tomatoes  | Time |
|------------|-----------|------|
| 2019-06-25 | 10 @ 0:25 | 4:10 | 
| 2019-06-26 |  8 @ 0:25 | 3:20 | 
| 2019-06-27 |  8 @ 0:50 | 6:40 | 

Tomatoes: 34
Hours: 11:10

### 2019-06-27

Lisp stepper

- Implemented evaluation of special forms
- Implemented evaluation of symbols
- Implemented Foldable and Traversable instances for ExprF
- Implemented Foldable and Traversable instances for ExprAnnF
- Implemented cataM

### 2019-06-26

Lisp stepper

- Finished first version of parser
- Added the Recursive and Corecursive classes
- Added cata and histo to the Recursive module
- Debugged a cyclic declaration error in the list and expr parsers. 
- Researched implementations of Control.Lazy
- Researched implementations of ParserT and anyChar

### 2019-06-25

Lisp stepper

- AST data types
- Parser for symbols and special forms 

## 2019-06-17 - 2019-06-23

| Date | Tomatoes | Time |
|------|----------|------|
| 2019-06-17 | 7 | 2:55 | 
| 2019-06-17 | 8 | 3:20 | 

Total tomatoes: 15
Total hours: 6:15

### 2019-06-17

- Took notes and implemented futumorphism

### 2019-06-18

- Took notes on `Base` functor, `Recursive` and `Corecursive` classes

### 2019-06-19 - 2019-06-23 

_On vacation_

## 2019-06-10 - 2019-06-16

| Date | Tomatoes | Time |
|------|----------|------|
| 2019-06-14 | 11 | 4:35 | 
| 2019-06-15 |  8 | 3:20 | 
| 2019-06-15 |  5 | 2:05 | 

Total tomatoes: 24 
Total hours: 10:00

### 2019-06-14

- Took notes and implemented: catamorphism, anamorphism, paramorphism, and 
  apomorphism.
- Took notes and implemented: algebra, co-algebra, r-algebra, and r-coalgebra.
- Read a little bit about inductive types.

### 2019-06-15

- Took notes and implemented histomorphism and futumorphism
- Took notes about generic programming

### 2019-06-16

- Wrote "let expressions, I/O is also boring" blog post
