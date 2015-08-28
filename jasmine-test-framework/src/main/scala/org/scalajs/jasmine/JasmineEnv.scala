package org.scalajs.jasmine

import scala.scalajs.js

@js.native
trait JasmineEnv extends js.Object {
  def Clock: JasmineEnv.Clock = js.native
}

object JasmineEnv {
  @js.native
  trait Clock extends js.Object {
    def tick(time: Double): Unit = js.native
    def useMock(): Unit = js.native
  }
}
