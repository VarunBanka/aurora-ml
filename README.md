# Aurora-ML

Aurora-ML is a small, statically typed, ML-style functional language
implemented in OCaml.

It serves as a **readable reference implementation** of the core features
found in real functional languages, with an emphasis on semantic clarity
and correctness rather than performance or tooling.

---

## Language Features

* Integers and booleans
* First-class functions
* Let and let-rec bindings
* Hindley–Milner type inference (Algorithm W)
* Let-polymorphism and recursive bindings
* Pattern matching (with type inference and evaluation)
* Lexical scoping

Type annotations are not required.

---

## Architecture

The implementation mirrors the structure of real-world language
implementations and compilers:

* **Explicit abstract syntax**
* **Hindley–Milner type inference** using Algorithm W and unification
* **Separate evaluation and inference phases**
* **Small, total functions** with explicit invariants

The codebase is intentionally dependency-free and designed to be read
linearly.

---

## Repository Structure

* `ast.ml`
  Abstract syntax tree definitions.

* `types.ml`
  Type representations and polymorphic type schemes.

* `infer.ml`
  Hindley–Milner type inference (Algorithm W), including let-polymorphism,
  recursion, and pattern matching.

* `eval.ml`
  Deterministic evaluator with lexical scoping and pattern matching.

* `examples.ml`
  Example programs represented directly as ASTs.

---

## Non-Goals

Aurora-ML intentionally does not include:

* A parser or lexer (programs are constructed as ASTs)
* Performance optimizations
* A full standard library beyond a minimal primitive prelude
* Algebraic data type constructors (planned extension)

The project focuses strictly on **core language semantics**.

---

## Status

Aurora-ML implements a complete and sound core functional language.
It is suitable as:

* A learning resource for interpreters and type systems
* A reference implementation of Hindley–Milner inference
* A foundation for extending with parsing, ADTs, or optimizations

---

## Design Principles

* Types are invariants, not comments
* Prefer small, total functions
* Make illegal states unrepresentable
* Optimize for long-term readability and correctness

---

## License

This project is released under the MIT License.

--- ---