package org.scalajs.jasmine

import scala.scalajs.js

trait JasmineContext extends js.Object {
  val currentSpec: Spec = ???
  val currentSuite: Suite = ???
}
