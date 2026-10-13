# Wasm component from a `WasmModule`

This example links a Scala.js program with `ModuleKind.WasmModule`, then turns
the core Wasm module into a [Wasm component](https://component-model.bytecodealliance.org/)
with `wasm-tools`, and runs it with `wasmtime`.

```sh
scripts/test-wasm-component.sh        # Scala 2.12
scripts/test-wasm-component.sh 2.13   # Scala 2.13
```

The script needs `sbt`, [`wasm-tools`](https://github.com/bytecodealliance/wasm-tools)
and [`wasmtime`](https://github.com/bytecodealliance/wasmtime) on the `PATH`.
`wasmtime` needs `-W gc=y,function-references=y,exceptions=y`.

## Why this works

The Canonical ABI flattens each WIT function to a core Wasm function.
If a function only uses scalar WIT types, has at most 16 flattened parameters
and at most 1 flattened result, the core signature uses only `i32`, `i64`,
`f32` and `f64`, and no linear memory is involved.
`@WasmImport` and `@WasmExport` produce exactly these core functions:

| WIT type                                      | Core type | Scala type |
|-----------------------------------------------|-----------|------------|
| `bool`, `s8`, `u8`, `s16`, `u16`, `s32`, `u32`, `char` | `i32`     | `Int`      |
| `s64`, `u64`                                  | `i64`     | `Long`     |
| `f32`                                         | `f32`     | `Float`    |
| `f64`                                         | `f64`     | `Double`   |
| `enum`, `flags` (up to 32), `result` without payloads | `i32` | `Int` |

Unsigned values arrive as their two's-complement bit pattern: use
`Integer.toUnsignedLong`, `Long.toUnsignedString`, etc.

The names follow the conventions of `wasm-tools component new`:

* import: `@WasmImport("<package>/<interface>@<version>", "<function>")`
* export: `@WasmExport("<package>/<interface>@<version>#<function>")`

## What does not work

Anything that needs the Canonical ABI with linear memory:

* `string`, `list<T>`, records, tuples, variants with payloads, `option<T>`,
  `result` with payloads, resources;
* functions with more than 16 flattened parameters, or more than 1 flattened
  result (the Canonical ABI then passes them through linear memory).

A `WasmModule` has no linear memory and no `cabi_realloc` export.
Full Component Model support, with the Canonical ABI in the linker backend, is
being developed in [scala-wasm](https://github.com/scala-wasm/scala-wasm).
