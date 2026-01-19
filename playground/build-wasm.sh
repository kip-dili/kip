#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${ROOT_DIR}/playground/dist"

WASM_GHC="${WASM_GHC:-wasm32-wasi-ghc}"
WASM_GHC_PKG="${WASM_GHC_PKG:-wasm32-wasi-ghc-pkg}"
WASM_CC="${WASM_CC:-wasm32-wasi-clang}"
WASM_CABAL="${WASM_CABAL:-wasm32-wasi-cabal}"
ZLIB_WASM_PREFIX="${ZLIB_WASM_PREFIX:-${ROOT_DIR}/playground/.foma-wasm/zlib-wasm}"

if [[ -z "${FOMA_WASM_PREFIX:-}" ]]; then
  echo "FOMA_WASM_PREFIX is not set."
  echo "Set it to the prefix containing include/fomalib.h and lib/libfoma.a."
  exit 1
fi

mkdir -p "${DIST_DIR}"

pushd "${ROOT_DIR}" >/dev/null

"${WASM_CABAL}" build kip-playground \
  --with-compiler="${WASM_GHC}" \
  --with-hc-pkg="${WASM_GHC_PKG}" \
  --with-gcc="${WASM_CC}" \
  --extra-include-dirs="${FOMA_WASM_PREFIX}/include" \
  --extra-include-dirs="${ZLIB_WASM_PREFIX}/include" \
  --extra-lib-dirs="${FOMA_WASM_PREFIX}/lib" \
  --extra-lib-dirs="${ZLIB_WASM_PREFIX}/lib"

BIN_PATH="$(cabal list-bin kip-playground --with-compiler="${WASM_GHC}")"
cp "${BIN_PATH}" "${DIST_DIR}/kip-playground.wasm"

popd >/dev/null

cp "${ROOT_DIR}/playground/index.html" "${DIST_DIR}/index.html"
cp "${ROOT_DIR}/playground/playground.js" "${DIST_DIR}/playground.js"
cp "${ROOT_DIR}/playground/style.css" "${DIST_DIR}/style.css"
cp "${ROOT_DIR}/playground/logo.png" "${DIST_DIR}/logo.png"

"${ROOT_DIR}/playground/build-assets.sh"

echo "WASM build complete: ${DIST_DIR}/kip-playground.wasm"
