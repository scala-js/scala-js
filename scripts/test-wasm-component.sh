#!/usr/bin/env bash
# Links examples/wasm-component as a core Wasm module (ModuleKind.WasmModule),
# turns it into a Wasm component with wasm-tools, and runs it with wasmtime.
#
# Usage: scripts/test-wasm-component.sh [scala-binary-version]  (default: 2.12)
# Requires sbt, wasm-tools and wasmtime on the PATH.

set -euo pipefail

SCALA_BINARY_VERSION="${1:-2.12}"
PROJECT="wasmComponentExample${SCALA_BINARY_VERSION/./_}"
EXAMPLE_DIR="examples/wasm-component"
CORE_WASM="$EXAMPLE_DIR/.$SCALA_BINARY_VERSION/target/scala-$SCALA_BINARY_VERSION/wasm-component-fastopt/main.wasm"
OUT_DIR="$EXAMPLE_DIR/.$SCALA_BINARY_VERSION/target/component"
WASMTIME_FLAGS=(-W gc=y,function-references=y,exceptions=y)

sbt "$PROJECT/fastLinkJS"

mkdir -p "$OUT_DIR"
wasm-tools component embed "$EXAMPLE_DIR/wit" "$CORE_WASM" \
  --world wasm-component -o "$OUT_DIR/embedded.wasm"
wasm-tools component new "$OUT_DIR/embedded.wasm" -o "$OUT_DIR/component.wasm"
wasm-tools validate "$OUT_DIR/component.wasm"
wasm-tools component wit "$OUT_DIR/component.wasm"

# wasi:cli/run: exits with a non-zero status if `run` returns `err`
wasmtime run "${WASMTIME_FLAGS[@]}" "$OUT_DIR/component.wasm"

check_invoke() {
  local call="$1"
  local expected="$2"
  local actual
  actual="$(wasmtime run "${WASMTIME_FLAGS[@]}" --invoke "$call" "$OUT_DIR/component.wasm")"
  if [ "$actual" != "$expected" ]; then
    echo "$call: expected $expected but got $actual" >&2
    exit 1
  fi
  echo "$call = $actual"
}

check_invoke 'fib(10)' '55'
check_invoke 'fib(90)' '2880067194370816120'
check_invoke "sum(true, -3, 65535, -100, 1000000000000, 1.5, 0.25, 'A')" '1000000065499.75'
