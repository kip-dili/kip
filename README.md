# Kip

Kip (meaning "grammatical mood" in Turkish) is an experimental programming language that uses Turkish grammatical cases as part of its type system. It demonstrates how natural language morphology—specifically Turkish noun cases and vowel harmony—can be integrated into programming language design.

This is a research/educational project exploring the intersection of linguistics and type theory, not a production programming language.

There is also [a tutorial in Turkish](https://github.com/joom/kip/wiki/K%C4%B1lavuz) and [a tutorial in English](https://github.com/joom/kip/wiki/Tutorial) that explains how to write Kip programs.

> [!NOTE]
> Kip is experimental. Expect changes in syntax and behavior over time.

For you to get a taste of what Kip looks like, here is an example program that reads a name and prints a greeting:

```
selamlamak,
  isim için okuyup,
  ("Merhaba "yla ismin birleşimini) yazmaktır.

selamla.
```

## Table of Contents

- [Language Features](#language-features)
- [Installation](#installation)
- [Editor Plugins](#editor-plugins)
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
| Nominative | Yalın hal | (none) | `defter` |
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

Bir trafik-ışığı
ya kırmızı
ya sarı
ya da yeşil
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
(bu trafik-ışığının) eylemi,
  bu kırmızıysa, "Dur",
  sarıysa, "Hazırlan",
  yeşilse, "Geç"tir.
```

### Constants

Define named constants with a definition sentence:

```
merhaba, "Merhaba"'dır.
dünya, "Dünya"'dır.
```

### Effects and I/O

Sequencing with `-ip/-ıp/-up/-üp` suffixes and binding with `için`:

```
selamlamak,
  isim için okuyup,
  ("Merhaba "yla ismin birleşimini) yazmaktır.
```

### Built-in Types and Operations

**Integers (`tam-sayı`):**
- Arithmetic: `toplamı`, `farkı`, `çarpımı`
- Comparison: `eşitliği`, `küçüklüğü`, `büyüklüğü`
- Other: `faktöriyeli`

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
3.14'ü yaz.           (* Floating-point literal with case suffix *)
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

# Install to PATH
stack install
```

The TRmorph transducer is bundled at `vendor/trmorph.fst`.

### Running

If you have installed to PATH, you can do:

```bash
# Start REPL
kip

# Execute a file
kip --exec path/to/file.kip

# Run in test mode (non-REPL, definition logs enabled)
kip --test path/to/file.kip

# Build/update .iz caches for a file or directory tree
kip --build path/to/file-or-dir

# Generate JavaScript
kip --codegen js path/to/file.kip

# Generate strict ES modules into a directory
kip --codegen js-modules --outdir ./out path/to/file.kip
# then run:
node ./out/entry.mjs

# Choose diagnostic language
kip --lang tr --exec path/to/file.kip
kip --lang en --exec path/to/file.kip

# Disable automatic loading of lib/giriş.kip
kip --no-prelude --exec path/to/file.kip
```

For the non-interactive playground runner binary:

```bash
stack exec kip-playground -- --exec path/to/file.kip
stack exec kip-playground -- --codegen js path/to/file.kip
```

If `kip` reports that it cannot find `vendor/trmorph.fst` or `lib/temel.kip`, the
binary is usually being run outside the installation layout Cabal expects. Kip
now searches in this order:

1. `KIP_DATADIR/<file>`
2. Cabal's installed data-files directory
3. paths relative to the executable
4. the current working directory

For custom packaging or manually relocated binaries, set:

```bash
export KIP_DATADIR=/absolute/path/to/kip
```

That directory should contain both `vendor/trmorph.fst` and `lib/temel.kip`.

`js-modules` output layout:
- `__kip_runtime.mjs` - runtime and primitive exports
- one `.mjs` per input/dependency `.kip` module
- `entry.mjs` - entrypoint that imports requested input modules

### Language Server (kip-lsp)

`kip-lsp` speaks LSP over stdio. You can run it directly for editor integration:

```bash
stack exec kip-lsp
```

#### PATH guidance

Make sure `kip-lsp` is on your `PATH`, or configure your editor to call the
absolute path from `stack exec -- which kip-lsp`.

## Editor Plugins

This repository includes:
- a Visual Studio Code extension in `vscode-plugin/`
- a Vim/Neovim plugin in `.vim/`

Both use `kip-lsp`.

### Visual Studio Code Plugin

Prerequisites:
- `kip-lsp` installed and available on `PATH`, or an absolute executable path
- Node.js and npm (for building the extension)

Install and run from source:

```bash
cd vscode-plugin
npm install
npm run compile
code --extensionDevelopmentPath=.
```

In the Visual Studio Code Extension Development Host, open a `.kip` file.

Package and install as a normal extension:

```bash
cd vscode-plugin
npm install
npm run compile
npx vsce package
code --install-extension kip-0.1.0.vsix
```

Useful Visual Studio Code settings:

```json
{
  "kip.languageServerPath": "kip-lsp",
  "kip.languageServerArgs": [],
  "kip.trace.server": "off"
}
```

### Vim Plugin

The plugin files are already in the repo under `.vim/`:
- `.vim/ftdetect/kip.vim`
- `.vim/syntax/kip.vim`
- `.vim/plugin/kip.vim`

Prerequisites:
- `kip-lsp` available on `PATH` (or set `g:kip_language_server_path`)
- Neovim built-in LSP, or `vim-lsp`, or `coc.nvim`

Use directly from this repo by adding the runtime path:

```vim
" ~/.vimrc or init.vim
set runtimepath+=/absolute/path/to/kip/.vim
```

Or copy the folders into your Vim config:

```bash
cp -R .vim/ftdetect ~/.vim/
cp -R .vim/syntax ~/.vim/
cp -R .vim/plugin ~/.vim/
```

Plugin options:

```vim
let g:kip_language_server_path = 'kip-lsp'
let g:kip_language_server_args = []
let g:kip_trace_server = 'off'
let g:kip_lsp_autostart = 1
```

Plugin commands:
- `:KipLspInfo` shows effective Kip LSP settings
- `:KipLspRestart` restarts the configured LSP client

## WASM Playground

A browser playground can be built from source under `playground/`. It compiles the
non-interactive runner (`kip-playground`) and reactor entrypoint (`kip-playground-reactor`)
to `wasm32-wasi` and ships a small HTML/JS harness that runs Kip in the browser.

See `playground/README.md` for prerequisites, toolchain setup, and build steps.

## Bytecode Cache

Kip stores a cached, type-checked version of each `.kip` file in a sibling `.iz` file. When you run a file again, Kip will reuse the `.iz` cache if both the source and its loaded dependencies are unchanged.

If you want to force a fresh parse and type-check, delete the `.iz` file next to the source.

> [!IMPORTANT]
> `.iz` files include a compiler hash. If the compiler changes, the cache is invalidated automatically.

## Project Structure

```
app/
├── Main.hs            - CLI entry point (kip executable)
├── Lsp.hs             - LSP server entry point (kip-lsp executable)
├── Playground.hs      - Non-interactive runner (kip-playground)
└── PlaygroundReactor.hs - WASI reactor entrypoint (kip-playground-reactor)

src/
├── Kip/
│   ├── AST.hs         - Abstract syntax tree
│   ├── Cache.hs       - .iz cache handling
│   ├── Codegen/
│   │   └── JS.hs       - JavaScript codegen
│   ├── Eval.hs        - Interpreter
│   ├── Parser.hs      - Parser
│   ├── Render.hs      - Pretty-printing with morphological inflection
│   ├── Runner.hs      - CLI runner utilities
│   └── TypeCheck.hs   - Type checker validating grammatical case usage
└── Language/
    └── Foma.hs        - Haskell bindings to Foma via FFI

lib/
├── giriş.kip             - Prelude module loaded by default
├── temel.kip             - Core types
├── temel/doğruluk.kip    - Boolean functions
├── temel/dizge.kip       - String functions
├── temel/etki.kip        - I/O primitives
├── temel/liste.kip       - List functions
├── temel/ondalık-sayı.kip - Floating-point functions
├── temel/tam-sayı.kip    - Integer functions
└── *.iz                  - Generated bytecode caches (if built)

tests/
├── Main.hs             - Test suite runner
├── LspTest.hs          - LSP test utilities
├── succeed/            - Passing golden tests (.kip + .out + optional .in)
├── fail/               - Failing golden tests (.kip + .err)
├── repl/               - REPL interaction tests (.repl + .out)
└── lsp/                - LSP test fixtures (.kip + .json)

playground/
└── README.md           - WASM playground build notes

vscode-plugin/
└── README.md           - VS Code extension for Kip

c/
├── morphology.c        - Foma glue (C)
└── morphology.h        - Foma glue headers

vendor/
└── trmorph.fst         - TRmorph transducer
```

## Testing

```bash
stack test
```

Test categories:
- `tests/succeed/` - Programs expected to parse, typecheck, and run successfully
- `tests/fail/` - Programs expected to fail during parsing, typechecking, or evaluation
- `tests/repl/` - REPL interaction tests with expected outputs
- `tests/lsp/` - LSP server functionality tests (hover, completion, diagnostics, etc.)

The test suite automatically runs `kip-lsp` tests if `kip-lsp` is on `PATH` or `KIP_LSP_BIN` is set.
JavaScript parity tests run only when `node` is available on `PATH` (or `NODE_BIN` is set).

Useful test env vars:
- `KIP_BIN`: path to a specific `kip` executable
- `KIP_LSP_BIN`: path to a specific `kip-lsp` executable
- `NODE_BIN`: path to a specific `node` executable

## Morphological Analysis

Kip uses [TRmorph](https://github.com/coltekin/TRmorph) for Turkish morphological analysis. When a word has multiple possible parses (e.g., "takası" could be "taka + possessive" or "takas + accusative"), Kip carries all candidates through parsing and resolves ambiguity during type checking.

For intentionally ambiguous words, use an apostrophe to force a specific parse: `taka'sı` vs `takas'ı`.

## License

See LICENSE file.
