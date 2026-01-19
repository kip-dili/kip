# Kip

Kip (meaning "grammatical mood" in Turkish) is an experimental programming language that uses Turkish grammatical cases as part of its type system. It demonstrates how natural language morphology—specifically Turkish noun cases and vowel harmony—can be integrated into programming language design.

This is a research/educational project exploring the intersection of linguistics and type theory, not a production programming language.

There is also [a tutorial in Turkish](https://github.com/joom/kip/wiki/K%C4%B1lavuz) and [a tutorial in English](https://github.com/joom/kip/wiki/Tutorial) that explains how to write Kip programs.

> [!NOTE]
> Kip is experimental. Expect changes in syntax and behavior over time.

For you to get a taste of what Kip looks like, here is an example program that prompts the user to enter a number and then prints that many of the Fibonacci numbers:

```
(* İlk n Fibonacci sayısını yazdırır. *)
(bu tam-sayıyı) (şu tam-sayıyı) (o tam-sayıyı) işlemek,
  (onla 0'ın eşitliği) doğruysa,
    durmaktır,
  yanlışsa,
    bunu yazıp,
    şunu (bunla şunun toplamını) (onla 1'in farkını) işlemektir.

çalıştırmak,
  "Bir sayı girin:" yazıp,
  isim olarak okuyup,
  ((ismin tam-sayı-hali)
    yokluksa,
      "Geçersiz sayı." yazmaktır,
    n'nin varlığıysa,
      0'ı 1'i n'yi işlemektir).

çalıştır.
```

## Table of Contents

- [Language Features](#language-features)
- [Installation](#installation)
- [Example Program](#example-program)
- [WASM Playground](#wasm-playground)
- [Bytecode Cache](#bytecode-cache)
- [Project Structure](#project-structure)
- [Testing](#testing)
- [Morphological Analysis](#morphological-analysis)
- [License](#license)

## Language Features

### Turkish Grammatical Cases as Types

Kip uses Turkish noun cases (ismin halleri) to determine argument relationships in function calls:

| Case | Turkish Name | Suffix | Example |
|------|-------------|--------|---------|
| Nominative | Yalın hal | (none) | `sıfır` |
| Accusative | -i hali | -i, -ı, -u, -ü | `sayıyı` |
| Dative | -e hali | -e, -a | `sayıya` |
| Locative | -de hali | -de, -da, -te, -ta | `listede` |
| Ablative | -den hali | -den, -dan, -ten, -tan | `listeden` |
| Genitive | Tamlayan eki | -in, -ın, -un, -ün | `sayının` |
| Instrumental | -le eki | -le, -la, ile | `sayıyla` |
| Possessive (3s) | Tamlanan eki | -i, -ı, -u, -ü, -si, -sı | `ardılı` |

### Flexible Argument Order

Because Turkish cases mark grammatical relationships explicitly, Kip allows flexible argument ordering. These two calls are equivalent:

```
(5'le 3'ün farkını) yaz.
(3'ün 5'le farkını) yaz.
```

As long as arguments have different case suffixes or different types, Kip can determine which argument is which.

### Inductive Data Types

Define algebraic data types with Turkish syntax:

```
Bir doğruluk ya doğru ya da yanlış olabilir.

Bir doğal-sayı
ya sıfır
ya da bir doğal-sayının ardılı
olabilir.
```

### Polymorphic Types

Type variables are supported for generic data structures:

```
Bir (öğe listesi)
ya boş
ya da bir öğenin bir öğe listesine eki
olabilir.
```

### Pattern Matching

Pattern match using the conditional suffix `-sa/-se`:

```
(bu doğruluğun) tersi,
  bu doğruysa, yanlış,
  yanlışsa, doğrudur.
```

Supports nested pattern matching, binders, and wildcard patterns (`değilse`):

```
(bu doğal-sayının) kopyası,
  bu sıfırsa, sıfır,
  öncülün ardılıysa, öncülün ardılıdır.
```

### Constants

Define named constants with `diyelim`:

```
sıfırın ardılına bir diyelim.
birin ardılına iki diyelim.
```

### Effects and I/O

Sequencing with `-ip/-ıp/-up/-üp` suffixes and binding with `olarak`:

```
selamlamak,
  isim olarak okuyup,
  ("Merhaba "yla ismin birleşimini) yazmaktır.
```

### Built-in Types and Operations

**Integers (`tam-sayı`):**
- Arithmetic: `toplamı`, `farkı`, `çarpımı`
- Comparison: `eşitliği`, `küçüklüğü`, `büyüklüğü`
- Other: `öncülü`, `sıfırlığı`, `faktöriyeli`

**Strings (`dizge`):**
- `uzunluğu` - length
- `birleşimi` - concatenation
- `tam-sayı-hali` - parse as integer

**I/O:**
- `yazmak` / `yaz` - print to stdout
- `okumak` / `oku` - read from stdin

### Comments

```
(* This is a comment *)
```

### Literals

```
5'i yaz.              (* Integer literal with case suffix *)
"merhaba"'yı yaz.     (* String literal with case suffix *)
```

## Installation

### Prerequisites

1. **[Foma](https://fomafst.github.io/)** - finite-state morphology toolkit
   - macOS: `brew install foma`
   - Debian/Ubuntu: `apt install foma libfoma-dev`
   - Fedora: `dnf install foma foma-devel`

2. **Stack** - Haskell build tool
   - See [haskellstack.org](https://docs.haskellstack.org/)

> [!TIP]
> If you only want to explore the language, you can start with `stack exec kip` after a successful build.

### Building

Clone this repository, then:

```bash
# Quick install (macOS/Linux)
chmod +x install.sh
./install.sh

# Or manual build
stack build
```

The TRmorph transducer is bundled at `vendor/trmorph.fst`.

### Running

```bash
# Start REPL
stack exec kip

# Execute a file
stack exec kip -- --exec path/to/file.kip

# Install to PATH
stack install
```

## WASM Playground

A browser playground can be built from source under `playground/`. It compiles the
non-interactive runner (`kip-playground`) to `wasm32-wasi` and ships a small
HTML/JS harness that runs Kip in the browser.

> [!NOTE]
> The `playground/dist/` directory is not included in the repository. You must build it locally following the instructions below.

See `playground/README.md` for prerequisites, toolchain setup, and build steps.

## Bytecode Cache

Kip stores a cached, type-checked version of each `.kip` file in a sibling `.iz` file. When you run a file again, Kip will reuse the `.iz` cache if both the source and its loaded dependencies are unchanged.

If you want to force a fresh parse and type-check, delete the `.iz` file next to the source.

> [!IMPORTANT]
> `.iz` files include a compiler hash. If the compiler changes, the cache is invalidated automatically.

## Example Program

```
(* Natural numbers *)
Bir doğal-sayı
ya sıfır
ya da bir doğal-sayının ardılı
olabilir.

(* Define some constants *)
sıfırın ardılına bir diyelim.
birin ardılına iki diyelim.
ikinin ardılına üç diyelim.

(* Addition function *)
(bu doğal-sayıyla) (şu doğal-sayının) toplamı,
  bu sıfırsa,
    şu,
  öncülün ardılıysa,
    (öncülle) (şunun ardılının) toplamıdır.

(* Print result *)
(ikiyle üçün toplamını) yaz.
```

## Project Structure

```
app/
└── Main.hs            - CLI entry point

src/
├── Kip/
│   ├── AST.hs         - Abstract syntax tree
│   ├── Cache.hs       - .iz cache handling
│   ├── Eval.hs        - Interpreter
│   ├── Parser.hs      - Parser
│   ├── Render.hs      - Pretty-printing with morphological inflection
│   └── TypeCheck.hs   - Type checker validating grammatical case usage
└── Language/
    └── Foma.hs        - Haskell bindings to Foma via FFI

lib/
├── giriş.kip          - Prelude module loaded by default
├── temel.kip           - Core types
├── temel-doğruluk.kip  - Boolean functions
├── temel-dizge.kip     - String functions
├── temel-etki.kip      - I/O primitives
├── temel-liste.kip     - List functions
└── temel-tam-sayı.kip  - Integer functions

tests/
├── succeed/            - Passing golden tests (.kip + .out + optional .in)
├── fail/               - Failing golden tests (.kip + .err)
└── repl/               - REPL interaction tests

vendor/
└── trmorph.fst        - TRmorph transducer
```

## Testing

```bash
stack test
```

Tests are in `tests/succeed/` (expected to pass) and `tests/fail/` (expected to fail).

## Morphological Analysis

Kip uses [TRmorph](https://github.com/coltekin/TRmorph) for Turkish morphological analysis. When a word has multiple possible parses (e.g., "takası" could be "taka + possessive" or "takas + accusative"), Kip carries all candidates through parsing and resolves ambiguity during type checking.

For intentionally ambiguous words, use an apostrophe to force a specific parse: `taka'sı` vs `takas'ı`.

## License

See LICENSE file.
