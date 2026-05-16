# Kip Visual Studio Code Extension

Visual Studio Code support for the Kip programming language, powered by `kip-lsp`.

## Features
- Syntax highlighting
- Procedural type highlighting (local, no semantic tokens)
- Diagnostics (parse/typecheck)
- Hover and signatures
- Go to definition and type definition
- Basic completions (identifier/type/function suggestions)

## Requirements
- Install `kip-lsp`. The extension auto-detects common install paths (`~/.local/bin`, `~/.cabal/bin`, `~/.stack/bin`), or you can set `kip.languageServerPath` explicitly.

## Setup
If Visual Studio Code cannot find `kip-lsp`, set this in your user settings:

```json
{
  "kip.languageServerPath": "/absolute/path/to/kip-lsp"
}
```

## Extension Settings
- `kip.languageServerPath`: user-level path to `kip-lsp` (default: `kip-lsp`); workspace settings are ignored
- `kip.languageServerArgs`: user-level additional arguments passed to `kip-lsp`; workspace settings are ignored
- `kip.trace.server`: LSP trace level (`off`, `messages`, `verbose`)
- `kip.proceduralTypeHighlight.enabled`: enable local procedural type coloring
- `kip.proceduralTypeHighlight.maxDocumentChars`: file-size guard for procedural highlighting

## Commands
- `Kip: Show Output`
- `Kip: Go to Type Definition`

## Links
- Repository: https://github.com/kip-dili/kip
- Playground: https://kip-dili.github.io

## Development
```bash
cd vscode-plugin
npm install
npm run compile
code --extensionDevelopmentPath=.
```

Use "Run Extension" in Visual Studio Code or `npm run package` to build a `.vsix`.
