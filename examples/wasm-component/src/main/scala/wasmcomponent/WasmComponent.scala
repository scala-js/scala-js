/* Scala.js example code
 * Public domain
 */

package wasmcomponent

import scala.scalajs.wasm
import scala.scalajs.wasm.annotation._

/** Core module of a Wasm component, linked with `ModuleKind.WasmModule`.
 *
 *  The WIT world is in `wit/world.wit`. Every function of that world only uses
 *  scalar WIT types, and has at most 16 flattened parameters and at most 1
 *  flattened result. For such functions, the Canonical ABI is the identity on
 *  core Wasm values, so `@WasmImport` and `@WasmExport` already produce the
 *  core functions that `wasm-tools component new` expects. See `README.md`.
 */
object WasmComponent {
  // wasi:random/random@0.2.0: get-random-u64: func() -> u64
  @WasmImport("wasi:random/random@0.2.0", "get-random-u64")
  def getRandomU64(): Long = wasm.native

  /* wasi:cli/run@0.2.0: run: func() -> result
   * A `result` without payloads flattens to its i32 discriminant:
   * 0 is `ok`, 1 is `err`.
   */
  @WasmExport("wasi:cli/run@0.2.0#run")
  def run(): Int = {
    // Check fib(n + 2) == fib(n + 1) + fib(n) for a random n in [0, 90)
    val n = java.lang.Long.remainderUnsigned(getRandomU64(), 90L).toInt
    val ok = fib(n + 2) == fib(n + 1) + fib(n) &&
      fib(90) == 2880067194370816120L
    if (ok) 0 else 1
  }

  // scalajs:example/numbers: fib: func(n: u32) -> u64
  @WasmExport("scalajs:example/numbers#fib")
  def fib(n: Int): Long = {
    var a = 0L
    var b = 1L
    var i = 0
    while (i != n) {
      val next = a + b
      a = b
      b = next
      i += 1
    }
    a
  }

  /* scalajs:example/numbers:
   *   sum: func(a: bool, b: s8, c: u16, d: s32, e: s64, f: f32, g: f64,
   *       h: char) -> f64
   * bool, s8, u16 and char all flatten to i32, hence to Int.
   */
  @WasmExport("scalajs:example/numbers#sum")
  def sum(a: Int, b: Int, c: Int, d: Int, e: Long, f: Float, g: Double,
      h: Int): Double = {
    a.toDouble + b + c + d + e.toDouble + f + g + h
  }
}
