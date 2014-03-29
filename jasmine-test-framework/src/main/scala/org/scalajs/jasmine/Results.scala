package org.scalajs.jasmine

import scala.scalajs.js

trait Result extends js.Object {
  def `type`: String = ???
  val trace: js.Dynamic = ???
}

trait ExpectationResult extends Result {
  def passed(): Boolean = ???
  val message: String = ???
}
