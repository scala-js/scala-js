package org.scalajs.testsuite.minwasminterop

import scala.scalajs.wasm.minimal.annotation._

object MinWasmInterop {
  @WasmExport("foo")
  def foo(x: Int): Int = x * 5

  @WasmExport("i8ArraySum")
  def i8ArraySum(xs: Array[Byte]): Int = xs.sum.toInt
}
