package org.scalajs.jasmine

import scala.scalajs.js

trait Result extends js.Object {
  def `type`: String = js.native
  val trace: js.Dynamic = js.native
}

trait ExpectationResult extends Result {
  def passed(): Boolean = js.native
  val message: String = js.native
}
