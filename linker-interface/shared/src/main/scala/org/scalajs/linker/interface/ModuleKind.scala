/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.interface

/** Kind of module structure emitted for the Scala.js output. */
abstract class ModuleKind private ()

object ModuleKind {

  /** All the available module kinds.
   *
   *  They are listed in decreasing order of "importance", as judged by
   *  whoever maintains the back-ends.
   */
  val All: List[ModuleKind] = List(
      NoModule,
      ESModule,
      CommonJSModule,
      WasmModule)

  /** No module structure.
   *
   *  With this module kind, exports are stored on the global object.
   *
   *  Imports are not supported.
   */
  case object NoModule extends ModuleKind

  /** An ECMAScript 2015 module.
   *
   *  Scala.js imports and exports directly map to `import` and `export`
   *  clauses in the ES module.
   */
  case object ESModule extends ModuleKind

  /** A CommonJS module (notably used by Node.js).
   *
   *  Imported modules are fetched with `require`. Exports go to the `exports`
   *  module-global variable.
   */
  case object CommonJSModule extends ModuleKind

  /** A WebAssembly module with Wasm import/export interop.
   *
   *  The resulting module has no JS interop, but can be run in any Wasm
   *  engine.
   *
   *  Since the Wasm module uses GC types, it cannot be used in multi-threaded
   *  setups. This is likely to be enforced by the engines, for safety reasons.
   *
   *  <h2>Experimental support for testing</h2>
   *
   *  Through scalajs-js-envs v1.7.0, Scala.js provides experimental support
   *  for testing `WasmModule`s using the regular testing infrastructure of
   *  build tools. No special setup is required.
   *
   *  However, the contract between the produced `.wasm` file and the `JSEnv`
   *  API is not yet set in stone. It is possible that a future version of
   *  scalajs-js-envs will break the contract, requiring a matching update of
   *  Scala.js core (to be precise, its scalajs-test-bridge artifact).
   *
   *  Unless you manually depend on newer versions of scalajs-js-envs, updates
   *  should be transparent.
   */
  case object WasmModule extends ModuleKind

  private[interface] implicit object ModuleKindFingerprint extends Fingerprint[ModuleKind] {

    override def fingerprint(moduleKind: ModuleKind): String = {
      moduleKind match {
        case NoModule       => "NoModule"
        case ESModule       => "ESModule"
        case CommonJSModule => "CommonJSModule"
        case WasmModule     => "WasmModule"
      }
    }
  }
}
