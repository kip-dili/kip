#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_ROOT="${BUILD_ROOT:-${ROOT_DIR}/playground/.foma-wasm}"
FOMA_REPO="${FOMA_REPO:-https://github.com/mhulden/foma.git}"
FOMA_REF="${FOMA_REF:-master}"
FOMA_PREFIX="${FOMA_WASM_PREFIX:-${ROOT_DIR}/playground/foma-wasm}"
ZLIB_REPO="${ZLIB_REPO:-https://github.com/madler/zlib.git}"
ZLIB_PREFIX="${ZLIB_WASM_PREFIX:-${BUILD_ROOT}/zlib-wasm}"

if [[ -z "${WASI_SDK_PATH:-}" ]]; then
  echo "WASI_SDK_PATH is not set (should point to the wasi-sdk root)."
  exit 1
fi

WASI_CC="${WASI_CC:-${WASI_SDK_PATH}/bin/clang}"
WASI_AR="${WASI_AR:-${WASI_SDK_PATH}/bin/llvm-ar}"
WASI_RANLIB="${WASI_RANLIB:-${WASI_SDK_PATH}/bin/llvm-ranlib}"

SRC_DIR="${BUILD_ROOT}/src"
FOMA_SRC_DIR="${SRC_DIR}/foma"
BUILD_DIR="${BUILD_ROOT}/build"
ZLIB_SRC_DIR="${BUILD_ROOT}/zlib-src"
ZLIB_BUILD_DIR="${BUILD_ROOT}/zlib-build"
HOST_BISON="${BISON_EXECUTABLE:-}"

mkdir -p "${BUILD_ROOT}"

if [[ ! -d "${SRC_DIR}" ]]; then
  git clone "${FOMA_REPO}" "${SRC_DIR}"
fi

pushd "${SRC_DIR}" >/dev/null
git fetch --all --tags
git checkout "${FOMA_REF}"
popd >/dev/null

if [[ ! -d "${ZLIB_SRC_DIR}" ]]; then
  git clone "${ZLIB_REPO}" "${ZLIB_SRC_DIR}"
fi

FOMA_CMAKE="${FOMA_SRC_DIR}/CMakeLists.txt"
if [[ -f "${FOMA_CMAKE}" ]] && ! grep -q "KIP_WASI_NO_SHARED" "${FOMA_CMAKE}"; then
  python3 - "${FOMA_CMAKE}" <<'PY'
from pathlib import Path
import sys

cmake_path = Path(sys.argv[1])
lines = cmake_path.read_text().splitlines()

patched = False
for idx, line in enumerate(lines):
    if "add_library" in line and "foma-shared" in line and "SHARED" in line:
        lines[idx] = line.replace("SHARED", "STATIC")
        patched = True
        break

if not patched:
    raise SystemExit("Expected foma-shared SHARED target not found for patching.")

cmake_path.write_text("\n".join(lines) + "\n")
PY
fi

rm -rf "${BUILD_DIR}" "${ZLIB_BUILD_DIR}"
mkdir -p "${BUILD_DIR}" "${ZLIB_BUILD_DIR}"

cmake -S "${ZLIB_SRC_DIR}" -B "${ZLIB_BUILD_DIR}" \
  -DCMAKE_SYSTEM_NAME=Generic \
  -DCMAKE_C_COMPILER="${WASI_CC}" \
  -DCMAKE_AR="${WASI_AR}" \
  -DCMAKE_RANLIB="${WASI_RANLIB}" \
  -DCMAKE_C_FLAGS="--target=wasm32-wasi -O2 -fPIC" \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
  -DZLIB_BUILD_SHARED=OFF \
  -DZLIB_BUILD_TESTING=OFF \
  -DCMAKE_INSTALL_PREFIX="${ZLIB_PREFIX}"

cmake --build "${ZLIB_BUILD_DIR}"
cmake --install "${ZLIB_BUILD_DIR}"

if [[ -z "${HOST_BISON}" ]]; then
  HOST_BISON="$(command -v bison || true)"
fi

if [[ -z "${HOST_BISON}" ]]; then
  echo "Bison is required to build Foma."
  echo "Install bison or set BISON_EXECUTABLE to its path."
  exit 1
fi

cmake -S "${FOMA_SRC_DIR}" -B "${BUILD_DIR}" \
  -DCMAKE_SYSTEM_NAME=Generic \
  -DCMAKE_C_COMPILER="${WASI_CC}" \
  -DCMAKE_AR="${WASI_AR}" \
  -DCMAKE_RANLIB="${WASI_RANLIB}" \
  -DCMAKE_C_FLAGS="--target=wasm32-wasi -O2" \
  -DGETOPT_INCLUDE="${WASI_SDK_PATH}/share/wasi-sysroot/include" \
  -DREADLINE_INCLUDE_DIRS="${WASI_SDK_PATH}/share/wasi-sysroot/include" \
  -DREADLINE_LIBRARY_DIRS="" \
  -DREADLINE_LIBRARIES="" \
  -DZLIB_INCLUDE="${ZLIB_PREFIX}/include" \
  -DZLIB_LIBRARIES="${ZLIB_PREFIX}/lib/libz.a" \
  -DBISON_EXECUTABLE="${HOST_BISON}" \
  -DKIP_WASI_NO_SHARED=ON

cmake --build "${BUILD_DIR}" --target foma-static

mkdir -p "${FOMA_PREFIX}/lib" "${FOMA_PREFIX}/include"
if [[ -f "${BUILD_DIR}/libfoma.a" ]]; then
  cp "${BUILD_DIR}/libfoma.a" "${FOMA_PREFIX}/lib/libfoma.a"
else
  cp "${BUILD_DIR}/libfoma-static.a" "${FOMA_PREFIX}/lib/libfoma.a"
fi
cp "${FOMA_SRC_DIR}/fomalib.h" "${FOMA_PREFIX}/include/fomalib.h"
cp "${FOMA_SRC_DIR}/fomalibconf.h" "${FOMA_PREFIX}/include/fomalibconf.h"

echo "Built Foma for WASI at: ${FOMA_PREFIX}"
echo "export FOMA_WASM_PREFIX=${FOMA_PREFIX}"
