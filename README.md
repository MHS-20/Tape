# Tape

Tape is an esoteric programming language based on the Turing machine, extended with set theory and universal quantification.

## Install

Requires [sbt](https://www.scala-sbt.org/) (Scala Build Tool) and JDK 11+.

```console
git clone https://github.com/example/tape && cd tape
sbt compile
```

Run a program:

```console
sbt "run run examples/01-inc.tape"
```

## Quick Start

A Tape program consists of **case rules** that describe a Turing machine, plus **run** or **trace** statements that execute it on a given tape.

```
case Inc 0 1 -> Halt
case Inc 1 0 -> Inc

trace Inc { 1 1 0 1 }
```

The trace output:

```
Inc: 1 1 0 1
     ^
Inc: 0 1 0 1
       ^
Inc: 0 0 0 1
         ^
Halt: 0 0 1 1
            ^
```

## Language Reference

### Case Rules

Each rule starts with `case` followed by five expressions:

- **State** — the current state of the machine
- **Read** — what the machine reads at the head position
- **Write** — what to write at the head position
- **Step** — where the head moves: `->` (right), `<-` (left), or `.` (stand)
- **Next** — the next state

```
case Entry % % -> I
```

### Run and Trace

- `run` executes a program silently and prints the final tape
- `trace` prints every intermediate state of the machine

You can have multiple `run` and `trace` statements in a single file; they execute sequentially.

```
trace Swap { (1 2) (2 3) & }
run   Sum  { 1 2 3 4 }
```

### Custom Head Position

To start the head at a non-terminal position, provide two tape sequences separated by a gap. The first is everything to the left of the head, the second is everything to the right:

```
trace Loop { a b c } { 1 1 1 0 }
; head starts here ------^
```

### Compound Expressions

Expressions can be nested tuples, similar to S-expressions:

```
(Start 0 1)
(Pair (a b) c)
```

### Sets and Universal Quantification

Define sets of expressions using `let`:

```
let Digits { 0 1 2 3 4 5 6 7 8 9 }
```

Use universal quantifiers with `for` to generate case rules for every element in a set:

```
for d in Digits
case Print d d -> Print
```

The above generates one case rule per digit. To apply a quantifier to a block of rules, use braces:

```
for d in Digits {
    case Inc d 1 -> Halt
    case Dec d 0 -> Halt
}
```

#### Nested Quantifiers

You can nest `for` loops. The following iterates over all pairs from a set:

```
let Numbers { 1 2 3 4 }
for a b in Numbers
case Swap (a b) (b a) -> Swap
```

Nested quantifiers that iterate over the same set can be collapsed:

```
for a b in Numbers       ; same as:
for a in Numbers         ; for a in Numbers
    for b in Numbers     ;     for b in Numbers
                         ;     case Swap (a b) ...
```

#### Anonymous Sets

Sets can be defined inline without a `let`:

```
for s in { a b c }
case S s 0 -> S
```

### Set Operations

Sets can be combined with union (`+`), difference (`-`), and Cartesian product (`*`):

```
let Numbers { 69 420 }
let Emoji { a b c }

for e in Numbers + Emoji - { b }
case Crab e x -> Crab
```

Cartesian products create sets of tuples:

```
let Number { 1 2 3 4 }
let Pair Number * Number   ; { (1 1) (1 2) (1 3) ... (4 4) }

for _ in Pair
case Skip _ _ -> Skip
```

### Magical Sets

Tape has three built-in infinite sets that are matched by type at runtime — no expansion is needed:

- **Integer** — all signed 64-bit integers
- **Real** — all 32-bit floats
- **String** — all string literals (delimited by single quotes)

```
for a b in Integer
case Swap (a b) (b a) -> Swap
```

This program swaps any pair of integers instantly, because the interpreter matches by type rather than generating cases for every integer.

### Eval Expressions

Eval expressions (`[lhs op rhs]`) perform arithmetic, comparison, and string operations at runtime:

```
for a b in Integer
case Sum (a b) [a + b] . Halt

trace Sum { (34 35) . }     ; writes 69
```

Supported operations:

| Type | Operations |
|------|-----------|
| Integer | `+` `-` `*` `/` `%` `<` `<=` `>` `>=` `==` `!=` |
| Real | `+` `-` `*` `/` `%` `<` `<=` `>` `>=` `==` `!=` |
| String | `+` `<` `<=` `>` `>=` `==` `!=` |
| Boolean | `\|\|` `&&` `==` `!=` |

Booleans are the symbols `true` and `false`. Eval expressions can be nested:

```
[[a % 15] == 0]
```

### Step Actions

| Token | Action |
|-------|--------|
| `->` | Move head right |
| `<-` | Move head left |
| `.` | Stay in place |
| `!` | Print the entire tape |

### Edge Filling

The tape is infinite in both directions. Cells beyond the explicitly provided tape are filled with the nearest edge value: the leftmost provided symbol extends left infinitely, the rightmost extends right infinitely.

## CLI Commands

```
sbt "run run <file.tape>"           # interpret and run
sbt "run expand [--enum] <file.tape># expand quantifiers inline
sbt "run lex <file.tape>"           # dump lexer tokens (debug)
sbt "run help [command]"            # print help
```

The `--enum` flag replaces all symbols with numeric IDs, producing an obfuscated output.

## Tests

```console
./runtests.sh replay     # run all tests, diff against expected output
./runtests.sh record     # re-record expected output
```

## Examples

- `examples/01-inc.tape` — increment and decrement binary counters
- `examples/02-pairs.tape` — swap elements of all pairs using sets and quantifiers
- `examples/03-magical.tape` — swap integer pairs with the magical `Integer` set
- `examples/04-eval.tape` — sum two integers with eval expressions
