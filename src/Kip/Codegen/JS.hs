{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | JavaScript-ish code generator for Kip AST.
module Kip.Codegen.JS
  ( codegenProgram
  , codegenStmts
  , codegenStmt
  , codegenExp
  ) where

import Data.Char (isLetter)
import Data.List (intercalate, partition)
import Data.Text (Text)
import qualified Data.Text as T

import Kip.AST

-- | Codegen a list of statements into a JS-like program.
-- Order: primitives, then async IIFE containing:
--   - function definitions (hoisted)
--   - overload wrappers (capture hoisted functions)
--   - expression statements (executed)
codegenProgram :: [Stmt Ann] -> Text
codegenProgram stmts =
  let -- Separate function definitions from other statements
      (funcDefs, otherStmts) = partition isFunctionDef stmts
      -- Function definitions first (they hoist anyway)
      funcCode = T.intercalate "\n\n" (map codegenStmt funcDefs)
      -- Expression statements last
      exprCode = T.intercalate "\n\n" (map codegenStmt otherStmts)
      -- All user code inside async IIFE with wrappers after functions
      wrapped = T.unlines
        [ "const __kip_run = async () => {"
        , funcCode
        , ""
        , jsOverloadWrappers
        , ""
        , exprCode
        , "};"
        , "await __kip_run();"
        , "__kip_close_stdin();"
        ]
  in jsPrimitives <> "\n\n" <> wrapped

-- | Check if a statement is a function definition (including types).
isFunctionDef :: Stmt Ann -> Bool
isFunctionDef stmt =
  case stmt of
    Function {} -> True
    Defn {} -> True
    NewType {} -> True
    PrimFunc {} -> True
    PrimType {} -> True
    Load {} -> True
    ExpStmt {} -> False

-- | JavaScript implementations of Kip primitives.
-- Uses 'var' so user code can override with 'const'.
-- Note: Boolean constructors are defined by the library's doğruluk type,
-- so we use a helper to get the correct constructor format at runtime.
-- Primitives that have library overloads (ters, birleşim, uzunluk, toplam)
-- are stored with __kip_ prefix and wrapped at the end.
-- The code is async-capable to support interactive browser I/O.
-- When KIP_RANDOM_SEED is set, the RNG mirrors the runtime LCG.
jsPrimitives :: Text
jsPrimitives = T.unlines
  [ "// Kip → JavaScript (async/await for interactive browser support)"
  , ""
  , "// Node.js modules for I/O (lazy loaded)"
  , "var __kip_fs = null;"
  , "var __kip_readline = null;"
  , "var __kip_stdin_queue = [];"
  , "var __kip_stdin_waiters = [];"
  , "var __kip_stdin_closed = false;"
  , "var __kip_stdin_mode = null;"
  , "var __kip_is_browser = (typeof window !== 'undefined');"
  , "var __kip_require = null;"
  , "var __kip_random_seed = null;"
  , "if (!__kip_is_browser && typeof process !== 'undefined' && process.versions && process.versions.node) {"
  , "  const { createRequire } = await import('module');"
  , "  __kip_require = createRequire(import.meta.url);"
  , "}"
  , "if (__kip_is_browser) {"
  , "  if (typeof window.__kip_write !== 'function') {"
  , "    window.__kip_write = (x) => console.log(x);"
  , "  }"
  , "  if (typeof window.__kip_read_line !== 'function') {"
  , "    window.__kip_read_line = async () => {"
  , "      var v = prompt('Input:');"
  , "      return v === null ? '' : v;"
  , "    };"
  , "  }"
  , "}"
  , "if (!__kip_is_browser && typeof process !== 'undefined' && process.env && process.env.KIP_RANDOM_SEED) {"
  , "  const seed = Number(process.env.KIP_RANDOM_SEED);"
  , "  if (!Number.isNaN(seed)) {"
  , "    __kip_random_seed = seed >>> 0;"
  , "  }"
  , "}"
  , "if (__kip_is_browser && typeof window.__kip_random_seed === 'number') {"
  , "  __kip_random_seed = window.__kip_random_seed >>> 0;"
  , "}"
  , "var __kip_rand = () => {"
  , "  if (__kip_random_seed === null) return Math.floor(Math.random() * 4294967296);"
  , "  __kip_random_seed = (Math.imul(__kip_random_seed, 1664525) + 1013904223) >>> 0;"
  , "  return __kip_random_seed;"
  , "};"
  , ""
  , "// Initialize stdin buffer for line-by-line reading (Node.js only)"
  , "var __kip_init_stdin = () => {"
  , "  if (typeof process === 'undefined' || !process.stdin) return;"
  , "  if (__kip_stdin_mode !== null) return;"
  , "  if (!__kip_require) return;"
  , "  if (process.stdin.isTTY === false) {"
  , "    __kip_stdin_mode = 'pipe';"
  , "    __kip_fs = __kip_fs || __kip_require('fs');"
  , "    try {"
  , "      __kip_stdin_queue = __kip_fs.readFileSync(0, 'utf8').split('\\n');"
  , "    } catch (e) {"
  , "      __kip_stdin_queue = [];"
  , "    }"
  , "    __kip_stdin_closed = true;"
  , "    return;"
  , "  }"
  , "  __kip_stdin_mode = 'tty';"
  , "  if (__kip_readline === null) {"
  , "    var readline = __kip_require('readline');"
  , "    __kip_readline = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });"
  , "    __kip_readline.on('line', (line) => {"
  , "      if (__kip_stdin_waiters.length > 0) {"
  , "        __kip_stdin_waiters.shift()(line);"
  , "      } else {"
  , "        __kip_stdin_queue.push(line);"
  , "      }"
  , "    });"
  , "    __kip_readline.on('close', () => {"
  , "      __kip_stdin_closed = true;"
  , "      while (__kip_stdin_waiters.length > 0) {"
  , "        __kip_stdin_waiters.shift()('');"
  , "      }"
  , "    });"
  , "  }"
  , "};"
  , "var __kip_close_stdin = () => {"
  , "  if (__kip_readline && __kip_stdin_mode === 'tty') {"
  , "    __kip_readline.close();"
  , "    __kip_readline = null;"
  , "  }"
  , "};"
  , ""
  , "// Helper to create a tagged value (works whether constructor is function or object)"
  , "var __kip_bool = (tag) => typeof window !== 'undefined' && typeof window[tag] === 'function' ? window[tag]() : { tag, args: [] };"
  , "var __kip_true = () => typeof doğru === 'function' ? doğru() : doğru;"
  , "var __kip_false = () => typeof yanlış === 'function' ? yanlış() : yanlış;"
  , "var __kip_some = (x) => typeof varlık === 'function' ? varlık(x) : { tag: 'varlık', args: [x] };"
  , "var __kip_none = () => typeof yokluk === 'function' ? yokluk() : { tag: 'yokluk', args: [] };"
  , "var __kip_float = (x) => ({ __kip_float: true, value: x });"
  , "var __kip_is_float = (x) => typeof x === 'object' && x && x.__kip_float === true;"
  , "var __kip_num = (x) => __kip_is_float(x) ? x.value : x;"
  , ""
  , "// Primitive boolean constructors (will be overridden by library)"
  , "var doğru = { tag: \"doğru\", args: [] };"
  , "var yanlış = { tag: \"yanlış\", args: [] };"
  , ""
  , "// Option type constructors (will be overridden by library if defined)"
  , "var varlık = (...args) => ({ tag: \"varlık\", args });"
  , "var yokluk = (...args) => ({ tag: \"yokluk\", args });"
  , ""
  , "// Unit type (will be overridden by library if defined)"
  , "var bitimlik = (...args) => ({ tag: \"bitimlik\", args });"
  , ""
  , "// Primitive functions for strings/numbers (may be overloaded by library for other types)"
  , "var __kip_prim_ters = (s) => s.split('').reverse().join('');"
  , "var __kip_prim_birleşim = (a, b) => __kip_num(a) + __kip_num(b);"
  , "var __kip_prim_uzunluk = (s) => s.length;"
  , "var __kip_prim_toplam = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) + __kip_num(b)) : (__kip_num(a) + __kip_num(b));"
  , ""
  , "// I/O primitives - async to support browser interactivity"
  , "var __kip_prim_oku_stdin = async () => {"
  , "  // Check for browser runtime at call time"
  , "  if (__kip_is_browser && typeof window.__kip_read_line === 'function') {"
  , "    return await window.__kip_read_line();"
  , "  }"
  , "  // Node.js fallback"
  , "  __kip_init_stdin();"
  , "  if (__kip_stdin_queue.length > 0) {"
  , "    return __kip_stdin_queue.shift();"
  , "  }"
  , "  if (__kip_stdin_closed) {"
  , "    return '';"
  , "  }"
  , "  return await new Promise((resolve) => {"
  , "    __kip_stdin_waiters.push(resolve);"
  , "  });"
  , "};"
  , "var __kip_prim_oku_dosya = (path) => {"
  , "  if (!__kip_require) return __kip_none();"
  , "  __kip_fs = __kip_fs || __kip_require('fs');"
  , "  try {"
  , "    return __kip_some(__kip_fs.readFileSync(path, 'utf8'));"
  , "  } catch (e) {"
  , "    return __kip_none();"
  , "  }"
  , "};"
  , "var __kip_prim_yaz_dosya = (path, content) => {"
  , "  if (!__kip_require) return __kip_false();"
  , "  __kip_fs = __kip_fs || __kip_require('fs');"
  , "  try {"
  , "    __kip_fs.writeFileSync(path, content);"
  , "    return __kip_true();"
  , "  } catch (e) {"
  , "    return __kip_false();"
  , "  }"
  , "};"
  , ""
  , "// Primitive functions (can be overridden)"
  , "var yaz = (x) => {"
  , "  if (__kip_is_browser && typeof window.__kip_write === 'function') {"
  , "    window.__kip_write(__kip_is_float(x) ? x.value : x);"
  , "  } else {"
  , "    console.log(__kip_is_float(x) ? x.value : x);"
  , "  }"
  , "  return typeof bitimlik === 'function' ? bitimlik() : bitimlik;"
  , "};"
  , "var çarpım = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) * __kip_num(b)) : (__kip_num(a) * __kip_num(b));"
  , "var fark = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) - __kip_num(b)) : (__kip_num(a) - __kip_num(b));"
  , "var bölüm = (a, b) => {"
  , "  var av = __kip_num(a);"
  , "  var bv = __kip_num(b);"
  , "  if (bv === 0) return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(0) : 0;"
  , "  return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(av / bv) : Math.trunc(av / bv);"
  , "};"
  , "var kalan = (a, b) => {"
  , "  var av = __kip_num(a);"
  , "  var bv = __kip_num(b);"
  , "  if (bv === 0) return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(0) : 0;"
  , "  return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(av % bv) : (av % bv);"
  , "};"
  , "var sayı_çek = (a, b) => {"
  , "  var lo = Math.min(a, b);"
  , "  var hi = Math.max(a, b);"
  , "  var range = hi - lo + 1;"
  , "  return lo + (__kip_rand() % range);"
  , "};"
  , "var eşitlik = (a, b) => __kip_num(a) === __kip_num(b) ? __kip_true() : __kip_false();"
  , "var küçüklük = (a, b) => __kip_num(a) < __kip_num(b) ? __kip_true() : __kip_false();"
  , "var küçük_eşitlik = (a, b) => __kip_num(a) <= __kip_num(b) ? __kip_true() : __kip_false();"
  , "var büyüklük = (a, b) => __kip_num(a) > __kip_num(b) ? __kip_true() : __kip_false();"
  , "var büyük_eşitlik = (a, b) => __kip_num(a) >= __kip_num(b) ? __kip_true() : __kip_false();"
  , "var dizge_hal = (n) => String(__kip_num(n));"
  , "var tam_sayı_hal = (s) => { const n = parseInt(s, 10); return isNaN(n) ? __kip_none() : __kip_some(n); };"
  , "var ondalık_sayı_hal = (s) => { const n = parseFloat(s); return isNaN(n) ? __kip_none() : __kip_some(__kip_float(n)); };"
  ]

-- | Wrappers for overloaded functions that dispatch based on argument type.
-- These are emitted at the END of the output, after all library code.
-- They capture the library implementations (if any) and create unified functions.
-- I/O functions are async to support interactive browser input.
jsOverloadWrappers :: Text
jsOverloadWrappers = T.unlines
  [ "// Overload wrappers - dispatch to library or primitive based on type"
  , "var __kip_lib_ters = typeof ters === 'function' ? ters : null;"
  , "var __kip_lib_birleşim = typeof birleşim === 'function' ? birleşim : null;"
  , "var __kip_lib_uzunluk = typeof uzunluk === 'function' ? uzunluk : null;"
  , "var __kip_lib_toplam = typeof toplam === 'function' ? toplam : null;"
  , "var __kip_lib_yaz = typeof yaz === 'function' ? yaz : null;"
  , ""
  , "var ters = async (x) => {"
  , "  if (typeof x === 'string') return __kip_prim_ters(x);"
  , "  if (__kip_lib_ters) return await __kip_lib_ters(x);"
  , "  throw new Error('ters: unsupported type');"
  , "};"
  , ""
  , "var birleşim = async (a, b) => {"
  , "  if (typeof a === 'string' || typeof a === 'number') return __kip_prim_birleşim(a, b);"
  , "  if (__kip_lib_birleşim) return await __kip_lib_birleşim(a, b);"
  , "  throw new Error('birleşim: unsupported type');"
  , "};"
  , ""
  , "var uzunluk = async (x) => {"
  , "  if (typeof x === 'string') return __kip_prim_uzunluk(x);"
  , "  if (__kip_lib_uzunluk) return await __kip_lib_uzunluk(x);"
  , "  throw new Error('uzunluk: unsupported type');"
  , "};"
  , ""
  , "var toplam = async (...args) => {"
  , "  if (args.length === 2 && typeof args[0] === 'number') return __kip_prim_toplam(args[0], args[1]);"
  , "  if (args.length === 2 && (__kip_is_float(args[0]) || __kip_is_float(args[1]))) return __kip_prim_toplam(args[0], args[1]);"
  , "  if (args.length === 1 && __kip_lib_toplam) return await __kip_lib_toplam(args[0]);"
  , "  if (__kip_lib_toplam) return await __kip_lib_toplam(...args);"
  , "  return __kip_prim_toplam(...args);"
  , "};"
  , ""
  , "// I/O wrappers - async for interactive browser support"
  , "var oku = async (...args) => {"
  , "  if (args.length === 0) return await __kip_prim_oku_stdin();"
  , "  if (args.length === 1 && typeof args[0] === 'string') return __kip_prim_oku_dosya(args[0]);"
  , "  throw new Error('oku: unsupported arguments');"
  , "};"
  , ""
  , "var yaz = (...args) => {"
  , "  if (args.length === 1) {"
  , "    var val = __kip_is_float(args[0]) ? args[0].value : args[0];"
  , "    if (__kip_is_browser && typeof window.__kip_write === 'function') {"
  , "      window.__kip_write(val);"
  , "    } else {"
  , "      console.log(val);"
  , "    }"
  , "    return typeof bitimlik === 'function' ? bitimlik() : bitimlik;"
  , "  }"
  , "  if (args.length === 2 && typeof args[0] === 'string') {"
  , "    return __kip_prim_yaz_dosya(args[0], args[1]);"
  , "  }"
  , "  if (__kip_lib_yaz) return __kip_lib_yaz(...args);"
  , "  throw new Error('yaz: unsupported arguments');"
  , "};"
  ]

-- | Codegen a list of statements (no prelude).
codegenStmts :: [Stmt Ann] -> Text
codegenStmts = T.intercalate "\n\n" . map codegenStmt

-- | Codegen a single statement into JS-like code.
codegenStmt :: Stmt Ann -> Text
codegenStmt stmt =
  case stmt of
    Defn name _ exp' ->
      "const " <> toJsIdent name <> " = " <> codegenExp exp' <> ";"
    Function name args _ clauses _ ->
      renderFunction name args clauses
    PrimFunc name args _ _ ->
      "// primitive function " <> identText name <> "(" <> renderArgNames args <> ")"
    Load name ->
      "// load " <> identText name
    NewType name _ ctors ->
      renderNewType name ctors
    PrimType name ->
      "// primitive type " <> identText name
    ExpStmt exp' ->
      codegenExp exp' <> ";"

-- | Codegen an expression into a JS-like expression string.
codegenExp :: Exp Ann -> Text
codegenExp exp' =
  case exp' of
    Var {varName, varCandidates} ->
      case varCandidates of
        ((ident, _):_) -> toJsIdent ident
        [] -> toJsIdent varName
    StrLit {lit} ->
      renderString lit
    IntLit {intVal} ->
      T.pack (show intVal)
    FloatLit {floatVal} ->
      "__kip_float(" <> T.pack (show floatVal) <> ")"
    App {fn, args} ->
      renderCall fn args
    Bind {bindName, bindExp} ->
      renderIife
        [ "const " <> toJsIdent bindName <> " = " <> codegenExp bindExp <> ";"
        , "return " <> toJsIdent bindName <> ";"
        ]
    Seq {first, second} ->
      renderIife
        (renderExpAsStmt first ++ ["return " <> codegenExp second <> ";"])
    Match {scrutinee, clauses} ->
      renderMatch scrutinee clauses
    Let {body} ->
      codegenExp body

renderFunction :: Identifier -> [Arg Ann] -> [Clause Ann] -> Text
renderFunction name args clauses =
  let argsText = renderArgNames args
      bodyLines =
        case clauses of
          [Clause (PWildcard _) body] ->
            ["return " <> codegenExp body <> ";"]
          _ ->
            let arg0 = case args of
                         [] -> "__arg0"
                         ((argName, _) : _) -> toJsIdent argName
            in ("const __scrut = " <> arg0 <> ";")
               : renderClauseChain "__scrut" clauses
  in
    T.unlines
      [ "async function " <> toJsIdent name <> "(" <> argsText <> ") {"
      , indent 2 (T.unlines bodyLines)
      , "}"
      ]

renderClauseChain :: Text -> [Clause Ann] -> [Text]
renderClauseChain = renderClauseIfChain

-- | Render an if/else chain for ordered clause matching.
renderClauseIfChain :: Text -> [Clause Ann] -> [Text]
renderClauseIfChain scrutinee =
  go True
  where
    go _ [] =
      ["throw new Error(\"No match\");"]
    go isFirst (Clause pat body : rest) =
      let (cond, binds) = renderPatMatchCond scrutinee pat
          bodyLines = binds ++ ["return " <> codegenExp body <> ";"]
          header =
            if cond == ""
              then if isFirst then "{" else "else {"
              else (if isFirst then "if (" else "else if (") <> cond <> ") {"
          block =
            [ header
            , indent 2 (T.unlines bodyLines)
            , "}"
            ]
      in if cond == ""
           then block
           else block ++ go False rest

-- | Render pattern bindings for nested patterns, avoiding duplicate JS declarations.
renderPatternBindings :: Text -> [Pat Ann] -> Int -> ([Text], Int)
renderPatternBindings scrutinee pats startIdx =
  let (binds, idx, _) = foldl collect ([], startIdx, []) pats
  in (binds, idx)
  where
    collect (acc, idx, seen) pat =
      let (binds, nextIdx, seen') = renderPatBinding scrutinee idx seen pat
      in (acc ++ binds, nextIdx, seen')

    renderPatBinding scrut idx seen pat =
      case pat of
        PWildcard _ -> ([], idx + 1, seen)
        PVar n _ ->
          let name = toJsIdent n
              argAccess = scrut <> ".args[" <> T.pack (show idx) <> "]"
          in if name `elem` seen
               then ([], idx + 1, seen)
               else ([ "const " <> name <> " = " <> argAccess <> ";" ], idx + 1, name : seen)
        PCtor _ subPats ->
          let argAccess = scrut <> ".args[" <> T.pack (show idx) <> "]"
              (subBinds, _, seen') = renderPatternBindingsWithSeen argAccess subPats 0 seen
          in (subBinds, idx + 1, seen')

    renderPatternBindingsWithSeen scrut pats idx seen =
      foldl collectWithSeen ([], idx, seen) pats
      where
        collectWithSeen (acc, ix, seenAcc) p =
          let (binds, nextIx, seen') = renderPatBinding scrut ix seenAcc p
          in (acc ++ binds, nextIx, seen')

renderMatch :: Exp Ann -> [Clause Ann] -> Text
renderMatch scrutinee clauses =
  renderIife $
    ("const __scrut = " <> codegenExp scrutinee <> ";")
      : renderMatchClauses "__scrut" clauses

renderMatchClauses :: Text -> [Clause Ann] -> [Text]
renderMatchClauses = renderClauseIfChain

renderPatMatch :: Text -> Pat Ann -> (Text, [Text])
renderPatMatch = renderPatMatchCond

-- | Render pattern condition and bindings for nested patterns.
renderPatMatchCond :: Text -> Pat Ann -> (Text, [Text])
renderPatMatchCond scrutinee pat =
  case pat of
    PWildcard _ -> ("", [])
    PVar n _ -> ("", ["const " <> toJsIdent n <> " = " <> scrutinee <> ";"])
    PCtor ctor pats ->
      let cond = renderPatCond scrutinee pat
          (binds, _) = renderPatternBindings scrutinee pats 0
      in (cond, binds)

-- | Render a boolean condition for a pattern.
renderPatCond :: Text -> Pat Ann -> Text
renderPatCond scrutinee pat =
  case pat of
    PWildcard _ -> "true"
    PVar _ _ -> "true"
    PCtor ctor pats ->
      let headCond = scrutinee <> ".tag === " <> renderString (normalizeCtorName ctor)
          argConds =
            [ cond
            | (p, idx) <- zip pats [0 ..]
            , let cond = renderPatCond (scrutinee <> ".args[" <> T.pack (show idx) <> "]") p
            , cond /= "true"
            ]
      in T.intercalate " && " (headCond : argConds)

-- | Normalize a constructor name by stripping Turkish suffixes.
-- Handles conditional (-sa/-se with apostrophe) and possessive (-ı/-i/-u/-ü) forms.
normalizeCtorName :: Identifier -> Text
normalizeCtorName (ns, name) =
  let base = stripTurkishSuffixes name
  in case ns of
       [] -> base
       _  -> T.intercalate "_" (map (T.filter (/= ' ')) ns ++ [base])

-- | Strip common Turkish suffixes from a word.
-- This is a conservative heuristic that handles:
-- 1. Conditional suffix with apostrophe ('sa, 'se, etc.)
-- 2. Possessive suffix after soft-g (ğı, ği, ğu, ğü → k)
stripTurkishSuffixes :: Text -> Text
stripTurkishSuffixes txt =
  let -- Strip conditional suffix with apostrophe ('sa, 'se, etc.)
      withoutCond = case T.breakOn "'" txt of
                      (pref, suf) | T.length suf > 1 -> pref
                      _ -> txt
      -- Strip possessive suffix only after soft-g (ğ + vowel → k)
      withoutPoss = stripSoftGPossessive withoutCond
  in withoutPoss

-- | Strip Turkish possessive suffix only when preceded by soft-g.
-- Converts ğı, ği, ğu, ğü back to k.
stripSoftGPossessive :: Text -> Text
stripSoftGPossessive txt =
  case T.unsnoc txt of
    Just (pref, c)
      | c `elem` ("ıiuü" :: String) ->
          -- Only strip if preceded by soft-g
          case T.unsnoc pref of
            Just (pref', 'ğ') -> pref' <> "k"  -- Convert ğ back to k
            _ -> txt  -- Keep original if not after soft-g
    _ -> txt

renderNewType :: Identifier -> [Ctor Ann] -> Text
renderNewType name ctors =
  let ctorLines =
        [ renderCtor ctorName args
        | ((ctorName, _), args) <- ctors
        ]
      ctorSig =
        T.intercalate " | "
          [ identText ctorName <> "(" <> T.replicate (length args) "_" <> ")"
          | ((ctorName, _), args) <- ctors
          ]
      -- For single-constructor types with no args, also alias the type name
      -- to the constructor (e.g., bitim = bitimlik for unit types)
      typeAlias = case ctors of
        [((ctorName, _), [])] | identText name /= identText ctorName ->
          ["var " <> toJsIdent name <> " = " <> toJsIdent ctorName <> ";"]
        _ -> []
  in
    T.unlines $
      ("/* type " <> identText name <> " = " <> ctorSig <> " */")
        : ctorLines ++ typeAlias

-- | Render a single constructor.
-- Zero-argument constructors are defined as objects (values).
-- Constructors with arguments are defined as functions.
-- Uses toJsIdent for both JS variable name and tag to ensure consistency.
renderCtor :: Identifier -> [a] -> Text
renderCtor ctorName args =
  let jsName = toJsIdent ctorName
  in case args of
    [] -> "var " <> jsName <> " = { tag: "
            <> renderString jsName <> ", args: [] };"
    _ -> "var " <> jsName <> " = (...args) => ({ tag: "
            <> renderString jsName <> ", args });"

renderCall :: Exp Ann -> [Exp Ann] -> Text
renderCall fn args =
  let fnText =
        case fn of
          Var {} -> codegenExp fn
          _ -> "(" <> codegenExp fn <> ")"
  in "(await " <> fnText <> "(" <> T.intercalate ", " (map codegenExp args) <> "))"

renderExpAsStmt :: Exp Ann -> [Text]
renderExpAsStmt exp' =
  case exp' of
    Bind {bindName, bindExp} ->
      ["const " <> toJsIdent bindName <> " = " <> codegenExp bindExp <> ";"]
    _ ->
      [codegenExp exp' <> ";"]

renderIife :: [Text] -> Text
renderIife lines' =
  T.unlines
    [ "(await (async () => {"
    , indent 2 (T.unlines lines')
    , "})())"
    ]

renderArgNames :: [Arg Ann] -> Text
renderArgNames args =
  T.intercalate ", " (map (toJsIdent . fst) args)

identText :: Identifier -> Text
identText (_, name) = name

toJsIdent :: Identifier -> Text
toJsIdent ident =
  let raw = baseIdent ident
      sanitized = T.map replaceDash raw
      cleaned = T.filter (/= '\'') sanitized
      prefixed =
        case T.uncons cleaned of
          Nothing -> "_"
          Just (c, _) ->
            if isIdentStart c then cleaned else "_" <> cleaned
      safe =
        if prefixed `elem` jsReserved
          then "_" <> prefixed
          else prefixed
  in safe
  where
    baseIdent (ns, name) =
      let cleanName = T.filter (/= ' ') name
      in case ns of
           [] -> cleanName
           _  -> T.intercalate "_" (map (T.filter (/= ' ')) ns ++ [cleanName])
    replaceDash c = if c == '-' then '_' else c
    isIdentStart c = isLetter c || c == '_' || c == '$'

jsReserved :: [Text]
jsReserved =
  [ "break", "case", "catch", "class", "const", "continue", "debugger"
  , "default", "delete", "do", "else", "export", "extends", "false"
  , "finally", "for", "function", "if", "import", "in", "instanceof"
  , "new", "null", "return", "super", "switch", "this", "throw"
  , "true", "try", "typeof", "var", "void", "while", "with", "yield"
  , "let", "enum", "await", "implements", "interface", "package"
  , "private", "protected", "public", "static", "undefined"
  ]

renderString :: Text -> Text
renderString txt =
  "\"" <> T.concatMap escapeChar txt <> "\""

escapeChar :: Char -> Text
escapeChar c =
  case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> T.singleton c

indent :: Int -> Text -> Text
indent n =
  T.unlines . map (T.replicate n " " <>) . filter (not . T.null) . T.lines
