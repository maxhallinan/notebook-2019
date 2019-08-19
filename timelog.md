# Time

https://www.saving.org/time/minutes/250

## 2019-08-19 - 2019-08-25

| Date       | Tomatoes  | Time |
|------------|-----------|------|
| 2019-08-19 | 15 @ 0:25 | 6:15 |

### 2019-08-19

- [leetcode][3] LeetCode #1 and #66
- [logo-notebook][10] 
  - Integrate CodeMirror.
  - Basic styles for code cell.
  - When user presses enter on an empty code cell, replace the code cell with a 
    text cell.
- [sicp][2] Started reading section 2.2.4

## 2019-08-12 - 2019-08-18

| Date       | Tomatoes  | Time |
|------------|-----------|------|
| 2019-08-12 |  9 @ 0:25 | 3:45 |
| 2019-08-13 | 15 @ 0:25 | 6:15 |
| 2019-08-14 | 13 @ 0:25 | 5:25 |
| 2019-08-15 | 11 @ 0:25 | 4:35 |
| 2019-08-16 | 12 @ 0:25 | 5:00 |
| 2019-08-17 |  4 @ 0:25 | 1:40 |
| 2019-08-18 |  7 @ 0:25 | 2:55 |
Total: 29:35

### 2019-08-18

- [blog] Started second draft of "What is a Logo-like language?"

### 2019-08-17

- [logo-notebook] Display result or error of evaluating a code cell.
- [blog] Wrote first draft of "What Is a Logo-Like language?"

### 2019-08-16

- [leetcode] LeetCode exercises 344, 387, and 7
- [logo-notebook] Took notes on the ProseMirror guide Introduction, Document, and Schema sections
- [sicp] Continued reading SICP 2.3
- [logo-notebook] Create and update plain text cells
- [logo-notebook] Create and update code cells

### 2019-08-15

- Prototyped the text cell editor in Elm.
- Prototyped the text cell editor in JavaScript.
- Researched ProseMirror.

### 2019-08-14

- Read SICP 2.1.1 - 2.1.3
- Added contextual data to the eval error objects
- Moved top-level interpreter API to a Run module
- Attended a Forth presentation

### 2019-08-13

- Researched purescript-halogen as an option for the logo-notebook UI.
- Researched JavaScript and Elm libraries for WebGL and building a text editor 
  (e.g., CodeMirror).
- Prototyped a simple sequence of code cells with React:
  - Render cells
  - Add new cell
  - Type code in cell
  - Run code
  - Run in shared environment
- Researched PureScript's FFI
- Extended interpreter API to run a sequence of programs in a shared environment
- Extended interpreter API to read and serialize to an array of strings for 
  interop with JavaScript.

### 2019-08-12

- Wrote a simple golden-testing setup for the Logo interpreter.
- Read and experimented with the purescript-pathy library.
  Decided it was overkill for use with the golden testing script, at least at
  this current state.

## 2019-08-05 - 2019-08-11

| Date       | Tomatoes  | Time |
|------------|-----------|------|
| 2019-07-07 |  5 @ 0:25 | 2:05 |
| 2019-07-09 | 11 @ 0:25 | 4:30 |


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
