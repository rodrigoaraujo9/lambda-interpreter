# lambda-interpreter

A simple lambda calculus interpreter written in Haskell, built for a programming languages course.
Teacher provided AST and evaluation strategy.

## What it does

Parses and evaluates lambda calculus expressions, supporting:

- Variables and lambda abstractions (`\x. x` or `lambda x. x`)
- Function application (`f x`)
- Integer constants and arithmetic (`+`, `-`, `*`)
- Conditionals (`if iszero e then e else e`)
- Local definitions (`let x = e in e`)
- Fixed-point operator (`fix`)

## Evaluation strategy

- **Applicative order** — arguments are evaluated before being passed to a function
- **Weak normal form** — evaluation stops at lambda abstractions (the body is not reduced)
- **No free variables allowed** — any free variable occurrence raises an error at runtime

## How to run

You need GHC, Cabal and Happy installed.

```
happy src/Parser.y
cabal run
```

Then type an expression and press `Ctrl+D`.

## Example

```
let f = fix (lambda f. lambda x. if iszero x then 1 else x * f (x - 1)) in f 5
```
