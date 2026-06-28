# AGENTS.md

## Build & Run

- Build: `sbt compile`
- Run a program: `sbt "run run <file.tape>"`
- Expand quantifiers: `sbt "run expand [--enum] <file.tape>"`
- Lex a file: `sbt "run lex <file.tape>"`

## Tests

Uses a custom bash test runner, **not** `sbt test`:

```bash
./runtests.sh replay     # run all tests, diff against expected output
./runtests.sh record     # re-record expected output (after intentional changes)
```

Expected output files live in `tests/expected/`. Test commands are defined inline in `runtests.sh`.

## Architecture

- Single sbt project, no subprojects.
- `src/main/scala/tape/Lexer.scala` — hand-written lexer, `//` line comments, strings delimited by `'`, special chars: `( ) { } [ ]`.
- `src/main/scala/tape/Atom.scala` — AST atoms: Symbol, Integer, Real, String. Equality ignores `Loc`.
- `src/main/scala/tape/Expr.scala` — AST types: `AtomExpr`, `Eval` (forced eval `[lhs op rhs]`), `Tuple`. Equality ignores `Loc` and `op` field.
- `src/main/scala/tape/SetExpr.scala` — Set expressions with union (`+`), diff (`-`), cartesian product (`*`). Magical sets `Integer`, `Real`, `String` are infinite (cannot be expanded).
- `src/main/scala/tape/Statement.scala` — Statement types: `StmtCase`, `StmtBlock`, `StmtFor`. Pattern matching, expansion, sanity checks.
- `src/main/scala/tape/Machine.scala` — Turing machine interpreter, tape, step actions.
- `src/main/scala/tape/Parser.scala` — Program parser: `let`, `case`, `for`, `run`, `trace`.
- `src/main/scala/tape/Tape.scala` — CLI entry point (`Main` object).

## Key Language Semantics

- `trace` prints each machine step; `run` prints only the final tape.
- Eval expressions `[a + b]` are forbidden in pattern matching, tapes, and set definitions.
- Unused variables in `for` loop scope within a `case` clause are a compile error.
- Case matching uses type-based pattern matching for magical sets (`Integer`, `Real`, `String`).
- Shadowing variables in nested `for` loops is an error.
- Set redefinition (including magical sets) is an error.

## Equality Quirks

- `Token` equality is by `name` only (ignores `Loc`).
- `Atom` equality ignores `Loc` (compares by value).
- `Expr` equality ignores `Loc` and `op` fields.
