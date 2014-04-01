package org.scalajs.jasmine

import scala.scalajs.js

trait JasmineEnv extends js.Object {
  def Clock: JasmineEnv.Clock
}

object JasmineEnv {
  trait Clock extends js.Object {
    def tick(time: Double): Unit
    def useMock(): Unit
  }
}
