// Kip → JavaScript (async/await for interactive browser support)

// Node.js modules for I/O (lazy loaded)
var __kip_fs = null;
var __kip_readline = null;
var __kip_stdin_queue = [];
var __kip_stdin_waiters = [];
var __kip_stdin_closed = false;
var __kip_stdin_mode = null;
var __kip_is_browser = (typeof window !== 'undefined');
var __kip_require = null;
if (!__kip_is_browser && typeof process !== 'undefined' && process.versions && process.versions.node) {
  const { createRequire } = await import('module');
  __kip_require = createRequire(import.meta.url);
}
if (__kip_is_browser) {
  if (typeof window.__kip_write !== 'function') {
    window.__kip_write = (x) => console.log(x);
  }
  if (typeof window.__kip_read_line !== 'function') {
    window.__kip_read_line = async () => {
      var v = prompt('Input:');
      return v === null ? '' : v;
    };
  }
}

// Initialize stdin buffer for line-by-line reading (Node.js only)
var __kip_init_stdin = () => {
  if (typeof process === 'undefined' || !process.stdin) return;
  if (__kip_stdin_mode !== null) return;
  if (!__kip_require) return;
  if (process.stdin.isTTY === false) {
    __kip_stdin_mode = 'pipe';
    __kip_fs = __kip_fs || __kip_require('fs');
    try {
      __kip_stdin_queue = __kip_fs.readFileSync(0, 'utf8').split('\n');
    } catch (e) {
      __kip_stdin_queue = [];
    }
    __kip_stdin_closed = true;
    return;
  }
  __kip_stdin_mode = 'tty';
  if (__kip_readline === null) {
    var readline = __kip_require('readline');
    __kip_readline = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
    __kip_readline.on('line', (line) => {
      if (__kip_stdin_waiters.length > 0) {
        __kip_stdin_waiters.shift()(line);
      } else {
        __kip_stdin_queue.push(line);
      }
    });
    __kip_readline.on('close', () => {
      __kip_stdin_closed = true;
      while (__kip_stdin_waiters.length > 0) {
        __kip_stdin_waiters.shift()('');
      }
    });
  }
};
var __kip_close_stdin = () => {
  if (__kip_readline && __kip_stdin_mode === 'tty') {
    __kip_readline.close();
    __kip_readline = null;
  }
};

// Helper to create a tagged value (works whether constructor is function or object)
var __kip_bool = (tag) => typeof window !== 'undefined' && typeof window[tag] === 'function' ? window[tag]() : { tag, args: [] };
var __kip_true = () => typeof doğru === 'function' ? doğru() : doğru;
var __kip_false = () => typeof yanlış === 'function' ? yanlış() : yanlış;
var __kip_some = (x) => typeof varlık === 'function' ? varlık(x) : { tag: 'varlık', args: [x] };
var __kip_none = () => typeof yokluk === 'function' ? yokluk() : { tag: 'yokluk', args: [] };

// Primitive boolean constructors (will be overridden by library)
var doğru = { tag: "doğru", args: [] };
var yanlış = { tag: "yanlış", args: [] };

// Option type constructors (will be overridden by library if defined)
var varlık = (...args) => ({ tag: "varlık", args });
var yokluk = (...args) => ({ tag: "yokluk", args });

// Unit type (will be overridden by library if defined)
var bitimlik = (...args) => ({ tag: "bitimlik", args });

// Primitive functions for strings/numbers (may be overloaded by library for other types)
var __kip_prim_ters = (s) => s.split('').reverse().join('');
var __kip_prim_birleşim = (a, b) => a + b;
var __kip_prim_uzunluk = (s) => s.length;
var __kip_prim_toplam = (a, b) => a + b;

// I/O primitives - async to support browser interactivity
var __kip_prim_oku_stdin = async () => {
  // Check for browser runtime at call time
  if (__kip_is_browser && typeof window.__kip_read_line === 'function') {
    return await window.__kip_read_line();
  }
  // Node.js fallback
  __kip_init_stdin();
  if (__kip_stdin_queue.length > 0) {
    return __kip_stdin_queue.shift();
  }
  if (__kip_stdin_closed) {
    return '';
  }
  return await new Promise((resolve) => {
    __kip_stdin_waiters.push(resolve);
  });
};
var __kip_prim_oku_dosya = (path) => {
  if (!__kip_require) return __kip_none();
  __kip_fs = __kip_fs || __kip_require('fs');
  try {
    return __kip_some(__kip_fs.readFileSync(path, 'utf8'));
  } catch (e) {
    return __kip_none();
  }
};
var __kip_prim_yaz_dosya = (path, content) => {
  if (!__kip_require) return __kip_false();
  __kip_fs = __kip_fs || __kip_require('fs');
  try {
    __kip_fs.writeFileSync(path, content);
    return __kip_true();
  } catch (e) {
    return __kip_false();
  }
};

// Primitive functions (can be overridden)
var yaz = (x) => {
  if (__kip_is_browser && typeof window.__kip_write === 'function') {
    window.__kip_write(x);
  } else {
    console.log(x);
  }
  return typeof bitimlik === 'function' ? bitimlik() : bitimlik;
};
var çarpım = (a, b) => a * b;
var fark = (a, b) => a - b;
var eşitlik = (a, b) => a === b ? __kip_true() : __kip_false();
var küçüklük = (a, b) => a < b ? __kip_true() : __kip_false();
var küçük_eşitlik = (a, b) => a <= b ? __kip_true() : __kip_false();
var büyüklük = (a, b) => a > b ? __kip_true() : __kip_false();
var büyük_eşitlik = (a, b) => a >= b ? __kip_true() : __kip_false();
var dizge_hal = (n) => String(n);
var tam_sayı_hal = (s) => { const n = parseInt(s, 10); return isNaN(n) ? __kip_none() : __kip_some(n); };


const __kip_run = async () => {
async function selamla() {
  return (await (async () => {
    const isim = (await oku());
    return (await yaz((await birleşim("Merhaba ", isim))));
  })())
  ;

}


// Overload wrappers - dispatch to library or primitive based on type
var __kip_lib_ters = typeof ters === 'function' ? ters : null;
var __kip_lib_birleşim = typeof birleşim === 'function' ? birleşim : null;
var __kip_lib_uzunluk = typeof uzunluk === 'function' ? uzunluk : null;
var __kip_lib_toplam = typeof toplam === 'function' ? toplam : null;
var __kip_lib_yaz = typeof yaz === 'function' ? yaz : null;

var ters = async (x) => {
  if (typeof x === 'string') return __kip_prim_ters(x);
  if (__kip_lib_ters) return await __kip_lib_ters(x);
  throw new Error('ters: unsupported type');
};

var birleşim = async (a, b) => {
  if (typeof a === 'string' || typeof a === 'number') return __kip_prim_birleşim(a, b);
  if (__kip_lib_birleşim) return await __kip_lib_birleşim(a, b);
  throw new Error('birleşim: unsupported type');
};

var uzunluk = async (x) => {
  if (typeof x === 'string') return __kip_prim_uzunluk(x);
  if (__kip_lib_uzunluk) return await __kip_lib_uzunluk(x);
  throw new Error('uzunluk: unsupported type');
};

var toplam = async (...args) => {
  if (args.length === 2 && typeof args[0] === 'number') return __kip_prim_toplam(args[0], args[1]);
  if (args.length === 1 && __kip_lib_toplam) return await __kip_lib_toplam(args[0]);
  if (__kip_lib_toplam) return await __kip_lib_toplam(...args);
  return __kip_prim_toplam(...args);
};

// I/O wrappers - async for interactive browser support
var oku = async (...args) => {
  if (args.length === 0) return await __kip_prim_oku_stdin();
  if (args.length === 1 && typeof args[0] === 'string') return __kip_prim_oku_dosya(args[0]);
  throw new Error('oku: unsupported arguments');
};

var yaz = (...args) => {
  if (args.length === 1) {
    if (__kip_is_browser && typeof window.__kip_write === 'function') {
      window.__kip_write(args[0]);
    } else {
      console.log(args[0]);
    }
    return typeof bitimlik === 'function' ? bitimlik() : bitimlik;
  }
  if (args.length === 2 && typeof args[0] === 'string') {
    return __kip_prim_yaz_dosya(args[0], args[1]);
  }
  if (__kip_lib_yaz) return __kip_lib_yaz(...args);
  throw new Error('yaz: unsupported arguments');
};


(await selamla());
};
await __kip_run();
__kip_close_stdin();

