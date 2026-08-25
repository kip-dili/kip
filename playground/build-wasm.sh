#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${ROOT_DIR}/playground/dist"

WASM_GHC="${WASM_GHC:-wasm32-wasi-ghc}"
WASM_GHC_PKG="${WASM_GHC_PKG:-wasm32-wasi-ghc-pkg}"
WASM_CC="${WASM_CC:-wasm32-wasi-clang}"
WASM_CABAL="${WASM_CABAL:-wasm32-wasi-cabal}"
ZLIB_WASM_PREFIX="${ZLIB_WASM_PREFIX:-${ROOT_DIR}/playground/.foma-wasm/zlib-wasm}"
WASM_OPT="${WASM_OPT:-wasm-opt}"

if [[ -z "${FOMA_WASM_PREFIX:-}" ]]; then
  echo "FOMA_WASM_PREFIX is not set."
  echo "Set it to the prefix containing include/fomalib.h and lib/libfoma.a."
  exit 1
fi

mkdir -p "${DIST_DIR}"

GHC_OPT_FLAGS="-O2 -split-sections -optl=-Wl,--gc-sections"
WASM_OPT_FLAGS="-O3"

pushd "${ROOT_DIR}" >/dev/null

# Build the reactor executable so the browser worker can reuse one WASM instance
# and call the exported `kip_run` function across multiple playground runs.
"${WASM_CABAL}" build kip-playground-reactor \
  --with-compiler="${WASM_GHC}" \
  --with-hc-pkg="${WASM_GHC_PKG}" \
  --with-gcc="${WASM_CC}" \
  --ghc-options="${GHC_OPT_FLAGS}" \
  --extra-include-dirs="${FOMA_WASM_PREFIX}/include" \
  --extra-include-dirs="${ZLIB_WASM_PREFIX}/include" \
  --extra-lib-dirs="${FOMA_WASM_PREFIX}/lib" \
  --extra-lib-dirs="${ZLIB_WASM_PREFIX}/lib"

BIN_PATH="$(cabal list-bin kip-playground-reactor --with-compiler="${WASM_GHC}")"
cp "${BIN_PATH}" "${DIST_DIR}/kip-playground.wasm"

popd >/dev/null

for asset in index.html playground.js style.css logo.png kip-worker.js coi-serviceworker.js; do
  cp "${ROOT_DIR}/playground/${asset}" "${DIST_DIR}/${asset}"
done
mkdir -p "${DIST_DIR}/quiz"
cp "${ROOT_DIR}/playground/quiz/index.html" "${DIST_DIR}/quiz/index.html"
rm -f "${DIST_DIR}/quiz.html"

if command -v "${WASM_OPT}" >/dev/null 2>&1; then
  "${WASM_OPT}" ${WASM_OPT_FLAGS} -o "${DIST_DIR}/kip-playground.wasm" "${DIST_DIR}/kip-playground.wasm"
else
  echo "Note: ${WASM_OPT} not found; skipping post-link WASM optimization."
fi

"${ROOT_DIR}/playground/build-assets.sh"
"${ROOT_DIR}/playground/build-iz-wasm.sh"

echo "WASM build complete: ${DIST_DIR}/kip-playground.wasm"
