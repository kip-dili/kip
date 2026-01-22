const workerUrl = new URL("./kip-worker.js", import.meta.url);

const sourceEl = document.getElementById("source");
const outputEl = document.getElementById("output");
const terminalBodyEl = document.getElementById("terminal-body");
const terminalInputEl = document.getElementById("terminal-input");
const terminalInputField = document.getElementById("terminal-input-field");
const codeSampleEl = document.getElementById("code-sample");
const sourceHighlightEl = document.getElementById("source-highlight");
const runBtn = document.getElementById("run");
const codegenBtn = document.getElementById("codegen");
const langEl = document.getElementById("lang");
const exampleEl = document.getElementById("example");
const codegenPanelEl = document.getElementById("codegen-panel");
const codegenOutputEl = document.getElementById("codegen-output");

const keywordList = [
  "Bir",
  "bir",
  "ya",
  "da",
  "olabilir",
  "var",
  "olamaz",
  "değilse",
  "yazdır",
  "diyelim",
  "olsun",
  "olarak",
  "yerleşik",
];

const keywordSet = new Set(keywordList);
const letterPattern = /\p{L}/u;

const jsKeywordList = [
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "default",
  "else",
  "export",
  "extends",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "let",
  "new",
  "return",
  "switch",
  "throw",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "await",
  "async",
  "of",
];

const jsLiteralList = ["true", "false", "null", "undefined"];
const jsKeywordSet = new Set(jsKeywordList);
const jsLiteralSet = new Set(jsLiteralList);

const examples = [
  { id: "selamlamak", file: "selamlamak.kip" },
  { id: "gün-örneği", file: "gün-örneği.kip" },
  { id: "fibonacci", file: "fibonacci.kip" },
  { id: "asal-sayılar", file: "asal-sayılar.kip" },
  { id: "bir-fazlası", file: "bir-fazlası.kip" },
  { id: "ikili-ağaç-araması", file: "ikili-ağaç-araması.kip" },
  { id: "dosya-io", file: "dosya-io.kip" },
];

sourceEl.value = `(bu tam-sayı listesini) bastırmak,\n  bu boşsa,\n    durmaktır,\n  ilkin devama ekiyse,\n    ilki yazıp,\n    devamı bastırmaktır.\n\n((1'in (2'nin boşa ekine) ekinin) tersini) bastır.`;

async function loadText(path) {
  const res = await fetch(path);
  if (!res.ok) {
    throw new Error(`Failed to load ${path}`);
  }
  return res.text();
}

function escapeHtml(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/\"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function highlightJsTokens(text) {
  const tokenPattern = /\b[A-Za-z_]\w*\b|\d+(?:\.\d+)?|[()[\]{}]/g;
  let result = "";
  let lastIndex = 0;
  let match;

  while ((match = tokenPattern.exec(text)) !== null) {
    const token = match[0];
    const start = match.index;
    const end = start + token.length;
    result += escapeHtml(text.slice(lastIndex, start));

    if (token === "(" || token === ")" || token === "[" || token === "]" || token === "{" || token === "}") {
      result += `<span class="kip-paren">${token}</span>`;
    } else if (/^\d/.test(token)) {
      result += `<span class="kip-literal">${escapeHtml(token)}</span>`;
    } else if (jsLiteralSet.has(token)) {
      result += `<span class="kip-literal">${escapeHtml(token)}</span>`;
    } else if (jsKeywordSet.has(token)) {
      result += `<span class="kip-keyword">${escapeHtml(token)}</span>`;
    } else {
      result += escapeHtml(token);
    }

    lastIndex = end;
  }

  result += escapeHtml(text.slice(lastIndex));
  return result;
}

function highlightJs(text) {
  let result = "";
  let lastIndex = 0;
  let i = 0;

  while (i < text.length) {
    const ch = text[i];
    const next = text[i + 1];

    if (ch === "/" && next === "/") {
      result += highlightJsTokens(text.slice(lastIndex, i));
      let j = i + 2;
      while (j < text.length && text[j] !== "\n") {
        j += 1;
      }
      result += `<span class="kip-comment">${escapeHtml(text.slice(i, j))}</span>`;
      i = j;
      lastIndex = i;
      continue;
    }

    if (ch === "/" && next === "*") {
      result += highlightJsTokens(text.slice(lastIndex, i));
      let j = i + 2;
      while (j < text.length) {
        if (text[j] === "*" && text[j + 1] === "/") {
          j += 2;
          break;
        }
        j += 1;
      }
      result += `<span class="kip-comment">${escapeHtml(text.slice(i, j))}</span>`;
      i = j;
      lastIndex = i;
      continue;
    }

    if (ch === "\"" || ch === "'" || ch === "`") {
      result += highlightJsTokens(text.slice(lastIndex, i));
      const quote = ch;
      let j = i + 1;
      while (j < text.length) {
        if (text[j] === "\\\\") {
          j += 2;
          continue;
        }
        if (text[j] === quote) {
          j += 1;
          break;
        }
        j += 1;
      }
      result += `<span class="kip-literal">${escapeHtml(text.slice(i, j))}</span>`;
      i = j;
      lastIndex = i;
      continue;
    }

    i += 1;
  }

  result += highlightJsTokens(text.slice(lastIndex));
  return result;
}

function highlightNonString(text) {
  const tokenPattern = /\d+(?:'?\p{L}+)?|\p{L}+(?:-\p{L}+)*|[(),.]/gu;
  const tokens = [];
  let match;

  while ((match = tokenPattern.exec(text)) !== null) {
    const token = match[0];
    const kind =
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

  const typeWordIndices = new Set();

  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].kind !== "word" || tokens[i].token !== "Bir") {
      continue;
    }
    for (let j = i + 1; j < tokens.length; j += 1) {
      if (tokens[j].kind === "word" && tokens[j].token === "ya") {
        for (let k = i + 1; k < j; k += 1) {
          if (tokens[k].kind === "word") {
            typeWordIndices.add(k);
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
              typeWordIndices.add(k);
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
    const wordIndices = [];
    for (let j = start; j < endIndex; j += 1) {
      if (tokens[j].kind === "word") {
        wordIndices.push(j);
      }
    }
    if (wordIndices.length < 2) {
      continue;
    }
    const lastWordIndex = wordIndices[wordIndices.length - 1];
    for (const index of wordIndices) {
      if (index !== lastWordIndex) {
        typeWordIndices.add(index);
      }
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
    const parenStack = [];
    let seenTopLevelToken = false;
    for (let j = defStart; j < i; j += 1) {
      const token = tokens[j];
      if (token.kind === "paren") {
        if (token.token === "(") {
          const eligible =
            commaIndex !== -1 &&
            j < commaIndex &&
            !seenTopLevelToken;
          parenStack.push({
            eligible,
            wordIndices: [],
            hasNumber: false,
          });
        } else if (parenStack.length) {
          const top = parenStack.pop();
          if (top.eligible && !top.hasNumber && top.wordIndices.length > 1) {
            for (let k = 1; k < top.wordIndices.length; k += 1) {
              typeWordIndices.add(top.wordIndices[k]);
            }
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
        continue;
      }
      if (token.kind === "word") {
        top.wordIndices.push(j);
      }
    }
    defStart = i + 1;
  }

  let result = "";
  let lastIndex = 0;
  for (let i = 0; i < tokens.length; i += 1) {
    const token = tokens[i];
    const { start, end } = token;
    result += escapeHtml(text.slice(lastIndex, start));

    if (token.kind === "paren") {
      result += `<span class="kip-paren">${token.token}</span>`;
    } else if (token.kind === "number") {
      const prev = start > 0 ? text[start - 1] : "";
      if (prev && letterPattern.test(prev)) {
        result += escapeHtml(token.token);
      } else {
        result += `<span class="kip-literal">${escapeHtml(token.token)}</span>`;
      }
    } else if (token.kind === "comma" || token.kind === "period") {
      result += `<span class="kip-keyword">${escapeHtml(token.token)}</span>`;
    } else if (token.kind === "word" && keywordSet.has(token.token)) {
      result += `<span class="kip-keyword">${escapeHtml(token.token)}</span>`;
    } else if (token.kind === "word" && typeWordIndices.has(i)) {
      result += `<span class="kip-type">${escapeHtml(token.token)}</span>`;
    } else {
      result += escapeHtml(token.token);
    }

    lastIndex = end;
  }

  result += escapeHtml(text.slice(lastIndex));
  return result;
}

function highlightKeywords(text) {
  let result = "";
  let lastIndex = 0;
  let i = 0;
  let commentDepth = 0;
  let commentStart = 0;
  let mode = "normal";

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
          const comment = text.slice(commentStart, i);
          result += `<span class="kip-comment">${escapeHtml(comment)}</span>`;
          lastIndex = i;
          mode = "normal";
        }
        continue;
      }
      i += 1;
      continue;
    }

    if (text[i] === "(" && text[i + 1] === "*") {
      result += highlightNonString(text.slice(lastIndex, i));
      commentDepth = 1;
      commentStart = i;
      mode = "comment";
      i += 2;
      continue;
    }

    if (text[i] === "\"") {
      result += highlightNonString(text.slice(lastIndex, i));
      let j = i + 1;
      while (j < text.length) {
        if (text[j] === "\\\\") {
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
      const literal = text.slice(i, end);
      result += `<span class="kip-literal">${escapeHtml(literal)}</span>`;
      i = end;
      lastIndex = i;
      continue;
    }

    i += 1;
  }

  if (mode === "comment") {
    const comment = text.slice(commentStart);
    result += `<span class="kip-comment">${escapeHtml(comment)}</span>`;
    return result;
  }

  result += highlightNonString(text.slice(lastIndex));
  return result;
}

function syncHighlight() {
  if (sourceHighlightEl) {
    sourceHighlightEl.innerHTML = `${highlightKeywords(sourceEl.value)}\n`;
  }
}

function highlightCodeSample() {
  if (!codeSampleEl) {
    return;
  }
  codeSampleEl.innerHTML = highlightKeywords(codeSampleEl.textContent || "");
}

function clearTerminal() {
  outputEl.textContent = "";
  hideTerminalInput();
}

function appendTerminalLine(line) {
  outputEl.textContent += `${line}\n`;
  if (terminalBodyEl) {
    terminalBodyEl.scrollTop = terminalBodyEl.scrollHeight;
  }
}

function showTerminalInput() {
  if (!terminalInputEl || !terminalInputField) {
    return;
  }
  terminalInputEl.classList.remove("hidden");
  terminalInputField.value = "";
  terminalInputField.focus();
  if (terminalBodyEl) {
    terminalBodyEl.scrollTop = terminalBodyEl.scrollHeight;
  }
}

function hideTerminalInput() {
  if (!terminalInputEl || !terminalInputField) {
    return;
  }
  terminalInputEl.classList.add("hidden");
  terminalInputField.value = "";
}

let activeWorker = null;
let inputSignalView = null;
let inputBufferView = null;
let pendingInput = false;
let interactiveSupported = true;
let activeMode = null;
let codegenLines = [];

function terminateWorker() {
  if (activeWorker) {
    activeWorker.terminate();
    activeWorker = null;
  }
  inputSignalView = null;
  inputBufferView = null;
  pendingInput = false;
}

function createInputBuffers() {
  if (typeof SharedArrayBuffer === "undefined" || !crossOriginIsolated) {
    interactiveSupported = false;
    return { signal: null, buffer: null };
  }
  const signal = new SharedArrayBuffer(8);
  const buffer = new SharedArrayBuffer(65536);
  inputSignalView = new Int32Array(signal);
  inputBufferView = new Uint8Array(buffer);
  return { signal, buffer };
}

function handleWorkerMessage(event) {
  const { type, line, error } = event.data || {};
  switch (type) {
    case "stdout":
      if (activeMode === "codegen") {
        codegenLines.push(line ?? "");
        if (codegenOutputEl) {
          codegenOutputEl.innerHTML = highlightJs(codegenLines.join("\n"));
        }
      } else {
        appendTerminalLine(line ?? "");
      }
      break;
    case "stderr":
      if (activeMode === "codegen") {
        codegenLines.push(line ?? "");
        if (codegenOutputEl) {
          codegenOutputEl.innerHTML = highlightJs(codegenLines.join("\n"));
        }
      } else {
        appendTerminalLine(line ?? "");
      }
      break;
    case "stdin-request":
      if (activeMode === "codegen") {
        break;
      }
      if (!interactiveSupported) {
        appendTerminalLine("(stdin unavailable)");
        break;
      }
      pendingInput = true;
      showTerminalInput();
      break;
    case "exit":
      pendingInput = false;
      hideTerminalInput();
      runBtn.disabled = false;
      if (codegenBtn) {
        codegenBtn.disabled = false;
      }
      break;
    case "error":
      if (activeMode === "codegen") {
        codegenLines.push(error ?? "Unknown error");
        if (codegenOutputEl) {
          codegenOutputEl.innerHTML = highlightJs(codegenLines.join("\n"));
        }
      } else {
        appendTerminalLine(error ?? "Unknown error");
      }
      pendingInput = false;
      hideTerminalInput();
      runBtn.disabled = false;
      if (codegenBtn) {
        codegenBtn.disabled = false;
      }
      break;
    default:
      break;
  }
}

function sendInput(value) {
  if (!pendingInput || !inputSignalView || !inputBufferView) {
    return;
  }
  const encoder = new TextEncoder();
  const bytes = encoder.encode(`${value}\n`);
  const limit = Math.min(bytes.length, inputBufferView.length);
  inputBufferView.fill(0);
  inputBufferView.set(bytes.slice(0, limit));
  inputSignalView[1] = limit;
  Atomics.store(inputSignalView, 0, 2);
  Atomics.notify(inputSignalView, 0, 1);
  pendingInput = false;
}

async function runKip() {
  runBtn.disabled = true;
  if (codegenBtn) {
    codegenBtn.disabled = true;
  }
  clearTerminal();
  terminateWorker();
  activeMode = "run";
  interactiveSupported = true;

  const { signal, buffer } = createInputBuffers();
  if (!interactiveSupported) {
    appendTerminalLine("Interactive input requires cross-origin isolation (COOP/COEP).");
    appendTerminalLine("Running without stdin support.");
  }
  const worker = new Worker(workerUrl, { type: "module" });
  activeWorker = worker;
  worker.addEventListener("message", handleWorkerMessage);
  worker.addEventListener("error", (event) => {
    appendTerminalLine(String(event.message || event.error || event));
    runBtn.disabled = false;
  });

  const args = ["kip-playground", "--exec", "/main.kip", "--lang", langEl.value];
  worker.postMessage({
    type: "run",
    source: sourceEl.value,
    args,
    signal,
    buffer,
  });
}

function runCodegen() {
  if (!codegenOutputEl || !codegenPanelEl) {
    return;
  }
  runBtn.disabled = true;
  codegenBtn.disabled = true;
  terminateWorker();
  activeMode = "codegen";
  codegenLines = [];
  codegenOutputEl.textContent = "";
  codegenPanelEl.classList.remove("hidden");

  const worker = new Worker(workerUrl, { type: "module" });
  activeWorker = worker;
  worker.addEventListener("message", handleWorkerMessage);
  worker.addEventListener("error", (event) => {
    codegenLines.push(String(event.message || event.error || event));
    codegenOutputEl.innerHTML = highlightJs(codegenLines.join("\n"));
    runBtn.disabled = false;
    codegenBtn.disabled = false;
  });

  const args = ["kip-playground", "--codegen", "js", "/main.kip", "--lang", langEl.value];
  worker.postMessage({
    type: "run",
    source: sourceEl.value,
    args,
  });
}

function formatExampleLabel(example) {
  return example.id;
}

function buildExampleOptions() {
  if (!exampleEl) {
    return;
  }
  exampleEl.innerHTML = "";
  exampleEl.appendChild(new Option("Custom", "__custom__"));
  for (const example of examples) {
    exampleEl.appendChild(new Option(formatExampleLabel(example), example.id));
  }
  exampleEl.value = "__custom__";
}

async function loadExample(exampleId) {
  const example = examples.find((entry) => entry.id === exampleId);
  if (!example) {
    return;
  }
  const source = await loadText(`./assets/examples/${example.file}`);
  sourceEl.value = source;
  syncHighlight();
}

syncHighlight();
highlightCodeSample();
sourceEl.addEventListener("input", syncHighlight);
sourceEl.addEventListener("scroll", () => {
  if (sourceHighlightEl) {
    sourceHighlightEl.scrollTop = sourceEl.scrollTop;
    sourceHighlightEl.scrollLeft = sourceEl.scrollLeft;
  }
});

if (exampleEl) {
  buildExampleOptions();
  exampleEl.addEventListener("change", async (event) => {
    const value = event.target.value;
    if (value === "__custom__") {
      return;
    }
    try {
      await loadExample(value);
    } catch (err) {
      appendTerminalLine(String(err));
    }
  });
}

if (terminalInputField) {
  terminalInputField.addEventListener("keydown", (event) => {
    if (event.key !== "Enter") {
      return;
    }
    event.preventDefault();
    const value = terminalInputField.value;
    hideTerminalInput();
    appendTerminalLine(`› ${value}`);
    sendInput(value);
  });
}

runBtn.addEventListener("click", runKip);
if (codegenBtn) {
  codegenBtn.addEventListener("click", runCodegen);
}
