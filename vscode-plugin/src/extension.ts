import * as vscode from "vscode";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  Trace,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let typeDecoration: vscode.TextEditorDecorationType | undefined;
let refreshTimer: NodeJS.Timeout | undefined;
let kipOutputChannel: vscode.OutputChannel | undefined;

const kipKeywordList = [
  "Bir",
  "bir",
  "ya",
  "da",
  "olabilir",
  "var",
  "olamaz",
  "değilse",
  "yazdır",
  "olsun",
  "olarak",
  "dersek",
  "için",
  "yerleşik",
];

const kipKeywordSet = new Set(kipKeywordList);
const kipTokenPattern = /\d+(?:'?\p{L}+)?|\p{L}+(?:'\p{L}+)?(?:-\p{L}+)*|[(),.]/gu;

type KipTokenKind = "word" | "number" | "paren" | "comma" | "period";

type KipToken = {
  token: string;
  kind: KipTokenKind;
  start: number;
  end: number;
};

function detectCommonKipLspPath(): string | undefined {
  const home = os.homedir();
  const candidates = [
    path.join(home, ".local", "bin", "kip-lsp"),  // stack install
    path.join(home, ".cabal", "bin", "kip-lsp"),  // cabal install
    path.join(home, ".stack", "bin", "kip-lsp"),  // some stack setups
  ];
  for (const candidate of candidates) {
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      return candidate;
    } catch {
      // Try next candidate.
    }
  }
  return undefined;
}

function getGlobalKipSetting<T>(key: string, fallback: T): T {
  const cfg = vscode.workspace.getConfiguration("kip");
  const inspected = cfg.inspect<T>(key);
  return inspected?.globalValue ?? fallback;
}

function resolveServerPath(): string {
  const configured = getGlobalKipSetting("languageServerPath", "kip-lsp").trim();
  if (configured !== "" && configured !== "kip-lsp") {
    return configured;
  }
  return detectCommonKipLspPath() ?? "kip-lsp";
}

function resolveServerArgs(): string[] {
  return getGlobalKipSetting<string[]>("languageServerArgs", []);
}

function resolveTrace(): "off" | "messages" | "verbose" {
  const cfg = vscode.workspace.getConfiguration("kip");
  const trace = cfg.get<string>("trace.server") ?? "off";
  if (trace === "messages" || trace === "verbose") {
    return trace;
  }
  return "off";
}

function toClientTrace(trace: "off" | "messages" | "verbose"): Trace {
  switch (trace) {
    case "messages":
      return Trace.Messages;
    case "verbose":
      return Trace.Verbose;
    case "off":
    default:
      return Trace.Off;
  }
}

function applyTraceSetting(target: LanguageClient, trace: "off" | "messages" | "verbose"): void {
  try {
    const setter = (target as unknown as { setTrace?: (value: Trace) => void }).setTrace;
    if (setter) {
      setter.call(target, toClientTrace(trace));
    }
  } catch (err) {
    console.error("Failed to apply trace setting for kip-lsp client:", err);
  }
}

function typeHighlightEnabled(): boolean {
  const cfg = vscode.workspace.getConfiguration("kip");
  return cfg.get<boolean>("proceduralTypeHighlight.enabled") ?? true;
}

function maxProceduralChars(): number {
  const cfg = vscode.workspace.getConfiguration("kip");
  return cfg.get<number>("proceduralTypeHighlight.maxDocumentChars") ?? 200000;
}

function ensureTypeDecoration(): vscode.TextEditorDecorationType {
  if (typeDecoration) {
    return typeDecoration;
  }
  typeDecoration = vscode.window.createTextEditorDecorationType({
    color: new vscode.ThemeColor("symbolIcon.classForeground"),
  });
  return typeDecoration;
}

function tokenizeNonString(text: string): KipToken[] {
  const tokens: KipToken[] = [];
  kipTokenPattern.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = kipTokenPattern.exec(text)) !== null) {
    const token = match[0];
    const kind: KipTokenKind =
      token === "(" || token === ")"
        ? "paren"
        : token === ","
          ? "comma"
          : token === "."
            ? "period"
            : /^\d/.test(token)
              ? "number"
              : "word";
    tokens.push({
      token,
      kind,
      start: match.index,
      end: match.index + token.length,
    });
  }
  return tokens;
}

function typeWordIndices(tokens: KipToken[]): Set<number> {
  const indices = new Set<number>();

  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].kind !== "word" || tokens[i].token !== "Bir") {
      continue;
    }
    for (let j = i + 1; j < tokens.length; j += 1) {
      if (
        tokens[j].kind === "word" &&
        (tokens[j].token === "ya" || tokens[j].token === "olsun")
      ) {
        for (let k = i + 1; k < j; k += 1) {
          if (tokens[k].kind === "word") {
            indices.add(k);
          }
        }
        break;
      }
    }
  }

  for (let i = 0; i < tokens.length - 1; i += 1) {
    if (
      tokens[i].kind === "word" &&
      tokens[i].token === "ya" &&
      tokens[i + 1].kind === "word" &&
      tokens[i + 1].token === "bir"
    ) {
      for (let j = i + 2; j < tokens.length; j += 1) {
        if (tokens[j].kind === "word" && tokens[j].token === "ya") {
          for (let k = i + 2; k < j; k += 1) {
            if (tokens[k].kind === "word") {
              indices.add(k);
            }
          }
          break;
        }
      }
    }
  }

  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].kind !== "word" || tokens[i].token !== "ya") {
      continue;
    }
    let start = i + 1;
    if (
      start < tokens.length &&
      tokens[start].kind === "word" &&
      tokens[start].token === "da"
    ) {
      start += 1;
    }
    let endIndex = -1;
    for (let j = start; j < tokens.length; j += 1) {
      if (tokens[j].kind !== "word") {
        continue;
      }
      if (tokens[j].token === "ya" || tokens[j].token === "olabilir") {
        endIndex = j;
        break;
      }
    }
    if (endIndex === -1 || start >= endIndex) {
      continue;
    }
    const wordIndices: number[] = [];
    for (let j = start; j < endIndex; j += 1) {
      if (tokens[j].kind === "word") {
        wordIndices.push(j);
      }
    }
    if (wordIndices.length < 2) {
      continue;
    }
    const lastWordIndex = wordIndices[wordIndices.length - 1];
    for (const idx of wordIndices) {
      if (idx !== lastWordIndex) {
        indices.add(idx);
      }
    }
  }

  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].kind !== "word" || tokens[i].token !== "olarak") {
      continue;
    }
    let j = i - 1;
    while (j >= 0 && tokens[j].kind === "word") {
      indices.add(j);
      j -= 1;
    }
  }

  let defStart = 0;
  for (let i = 0; i <= tokens.length; i += 1) {
    if (i < tokens.length && tokens[i].kind !== "period") {
      continue;
    }
    let commaIndex = -1;
    for (let j = defStart; j < i; j += 1) {
      if (tokens[j].kind === "comma") {
        commaIndex = j;
        break;
      }
    }
    const parenStack: Array<{
      eligible: boolean;
      wordIndices: number[];
      hasNumber: boolean;
      hasEligibleChild: boolean;
    }> = [];
    let seenTopLevelToken = false;
    for (let j = defStart; j < i; j += 1) {
      const token = tokens[j];
      if (token.kind === "paren") {
        if (token.token === "(") {
          const eligible = commaIndex !== -1 && j < commaIndex && !seenTopLevelToken;
          parenStack.push({
            eligible,
            wordIndices: [],
            hasNumber: false,
            hasEligibleChild: false,
          });
        } else if (parenStack.length) {
          const top = parenStack.pop();
          if (!top) {
            continue;
          }
          const parent = parenStack.length ? parenStack[parenStack.length - 1] : null;
          if (parent && top.eligible) {
            parent.hasEligibleChild = true;
          }
          if (top.eligible && !top.hasNumber && top.wordIndices.length > 1) {
            for (let k = 1; k < top.wordIndices.length; k += 1) {
              indices.add(top.wordIndices[k]);
            }
          } else if (top.eligible && !top.hasNumber && top.wordIndices.length === 1 && top.hasEligibleChild) {
            indices.add(top.wordIndices[0]);
          }
        }
        continue;
      }
      if (!parenStack.length) {
        if (token.kind === "word" || token.kind === "number") {
          seenTopLevelToken = true;
        }
        continue;
      }
      const top = parenStack[parenStack.length - 1];
      if (!top.eligible) {
        continue;
      }
      if (token.kind === "number") {
        top.hasNumber = true;
      } else if (token.kind === "word") {
        top.wordIndices.push(j);
      }
    }
    defStart = i + 1;
  }

  return indices;
}

function collectTypeOffsetsInNonString(text: string, baseOffset: number, out: Array<[number, number]>): void {
  const tokens = tokenizeNonString(text);
  const typeIndices = typeWordIndices(tokens);
  for (let i = 0; i < tokens.length; i += 1) {
    if (!typeIndices.has(i)) {
      continue;
    }
    const token = tokens[i];
    if (token.kind !== "word" || kipKeywordSet.has(token.token)) {
      continue;
    }
    out.push([baseOffset + token.start, baseOffset + token.end]);
  }
}

function collectTypeOffsets(text: string): Array<[number, number]> {
  const out: Array<[number, number]> = [];
  let lastIndex = 0;
  let i = 0;
  let commentDepth = 0;
  let mode: "normal" | "comment" = "normal";

  while (i < text.length) {
    if (mode === "comment") {
      if (text[i] === "(" && text[i + 1] === "*") {
        commentDepth += 1;
        i += 2;
        continue;
      }
      if (text[i] === "*" && text[i + 1] === ")") {
        commentDepth -= 1;
        i += 2;
        if (commentDepth === 0) {
          lastIndex = i;
          mode = "normal";
        }
        continue;
      }
      i += 1;
      continue;
    }

    if (text[i] === "(" && text[i + 1] === "*") {
      collectTypeOffsetsInNonString(text.slice(lastIndex, i), lastIndex, out);
      commentDepth = 1;
      mode = "comment";
      i += 2;
      continue;
    }

    if (text[i] === "\"") {
      collectTypeOffsetsInNonString(text.slice(lastIndex, i), lastIndex, out);
      let j = i + 1;
      while (j < text.length) {
        if (text[j] === "\\") {
          j += 2;
          continue;
        }
        if (text[j] === "\"") {
          j += 1;
          break;
        }
        j += 1;
      }
      let end = j;
      const suffixMatch = text.slice(end).match(/^'?\p{L}+/u);
      if (suffixMatch) {
        end += suffixMatch[0].length;
      }
      i = end;
      lastIndex = end;
      continue;
    }

    i += 1;
  }

  if (mode === "normal") {
    collectTypeOffsetsInNonString(text.slice(lastIndex), lastIndex, out);
  }
  return out;
}

function applyTypeDecorations(editor: vscode.TextEditor): void {
  if (editor.document.languageId !== "kip") {
    return;
  }
  const decoration = ensureTypeDecoration();
  if (!typeHighlightEnabled()) {
    editor.setDecorations(decoration, []);
    return;
  }
  const text = editor.document.getText();
  if (text.length > maxProceduralChars()) {
    editor.setDecorations(decoration, []);
    return;
  }
  const offsets = collectTypeOffsets(text);
  const ranges = offsets.map(([start, end]) => new vscode.Range(
    editor.document.positionAt(start),
    editor.document.positionAt(end)
  ));
  editor.setDecorations(decoration, ranges);
}

function refreshVisibleTypeDecorations(): void {
  for (const editor of vscode.window.visibleTextEditors) {
    if (editor.document.languageId === "kip") {
      applyTypeDecorations(editor);
    }
  }
}

function scheduleTypeRefresh(): void {
  if (refreshTimer) {
    clearTimeout(refreshTimer);
  }
  refreshTimer = setTimeout(() => {
    refreshTimer = undefined;
    refreshVisibleTypeDecorations();
  }, 50);
}

function startClientWithLogging(
  languageClient: LanguageClient,
  outputChannel: vscode.OutputChannel,
  command: string,
  args: string[]
): void {
  outputChannel.appendLine(`Starting kip-lsp: ${command}${args.length ? ` ${args.join(" ")}` : ""}`);
  const startResult = languageClient.start() as unknown;
  void Promise.resolve(startResult)
    .then(() => {
      outputChannel.appendLine("kip-lsp started.");
    })
    .catch((err: unknown) => {
      const message = err instanceof Error ? `${err.message}\n${err.stack ?? ""}` : String(err);
      outputChannel.appendLine(`Failed to start kip-lsp:\n${message}`);
      void vscode.window.showErrorMessage("Kip: failed to start language server. Check Output > Kip.");
      const setupMessage =
        "Kip LSP could not start. If `kip-lsp` is not on VSCode's PATH, set `kip.languageServerPath` to the full executable path (for example `/Users/you/.local/bin/kip-lsp`).";
      void vscode.window
        .showWarningMessage(setupMessage, "Open Settings")
        .then((choice) => {
          if (choice === "Open Settings") {
            void vscode.commands.executeCommand("workbench.action.openSettings", "kip.languageServerPath");
          }
        });
      outputChannel.show(true);
    });
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const outputChannel = vscode.window.createOutputChannel("Kip");
  kipOutputChannel = outputChannel;
  outputChannel.appendLine("Kip extension activated.");
  outputChannel.appendLine(`Workspace: ${vscode.workspace.workspaceFolders?.map((f) => f.uri.fsPath).join(", ") ?? "<none>"}`);
  console.log("Kip extension activated.");

  const serverCommand = resolveServerPath();
  const serverArgs = resolveServerArgs();
  const serverOptions: ServerOptions = {
    command: serverCommand,
    args: serverArgs,
    transport: TransportKind.stdio,
    options: {
      env: process.env,
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "kip" }],
    outputChannel,
    initializationOptions: {},
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.kip"),
    },
    middleware: {
      provideDocumentFormattingEdits: (document, options, token, next) => {
        return next(document, options, token);
      },
    },
  };

  client = new LanguageClient(
    "kip-lsp",
    "Kip Language Server",
    serverOptions,
    clientOptions
  );

  applyTraceSetting(client, resolveTrace());

  context.subscriptions.push(outputChannel);
  context.subscriptions.push(client);
  context.subscriptions.push(ensureTypeDecoration());
  context.subscriptions.push(
    vscode.commands.registerCommand("kip.goToDefinition", async () => {
      await vscode.commands.executeCommand("editor.action.revealDefinition");
    })
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("kip.goToTypeDefinition", async () => {
      await vscode.commands.executeCommand("editor.action.goToTypeDefinition");
    })
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("kip.showOutput", async () => {
      outputChannel.show(true);
    })
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("kip.restartLanguageServer", async () => {
      await restartClient();
      outputChannel.appendLine("Kip language server restarted.");
      void vscode.window.showInformationMessage("Kip: language server restarted.");
    })
  );
  startClientWithLogging(client, outputChannel, serverCommand, serverArgs);

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (
        event.affectsConfiguration("kip.languageServerPath") ||
        event.affectsConfiguration("kip.languageServerArgs") ||
        event.affectsConfiguration("kip.trace.server")
      ) {
        void restartClient();
      }
      if (
        event.affectsConfiguration("kip.proceduralTypeHighlight.enabled") ||
        event.affectsConfiguration("kip.proceduralTypeHighlight.maxDocumentChars")
      ) {
        scheduleTypeRefresh();
      }
    })
  );

  context.subscriptions.push(
    vscode.window.onDidChangeVisibleTextEditors(() => {
      scheduleTypeRefresh();
    })
  );

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(() => {
      scheduleTypeRefresh();
    })
  );

  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument((event) => {
      if (event.document.languageId === "kip") {
        scheduleTypeRefresh();
      }
    })
  );

  scheduleTypeRefresh();
}

async function restartClient(): Promise<void> {
  if (!client) {
    return;
  }
  const newCommand = resolveServerPath();
  const newArgs = resolveServerArgs();
  const newTrace = resolveTrace();

  await client.stop();

  const serverOptions: ServerOptions = {
    command: newCommand,
    args: newArgs,
    transport: TransportKind.stdio,
    options: { env: process.env },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "kip" }],
    outputChannel: client.outputChannel,
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.kip"),
    },
  };

  client = new LanguageClient(
    "kip-lsp",
    "Kip Language Server",
    serverOptions,
    clientOptions
  );

  applyTraceSetting(client, newTrace);
  const outputChannel = kipOutputChannel ?? client.outputChannel;
  startClientWithLogging(client, outputChannel, newCommand, newArgs);
}

export async function deactivate(): Promise<void> {
  if (refreshTimer) {
    clearTimeout(refreshTimer);
    refreshTimer = undefined;
  }
  if (!client) {
    return;
  }
  await client.stop();
}
