package org.scalajs.jasmine

import scala.scalajs.js

trait Result extends js.Object {

  def `type`: js.String = ???
  val trace: js.Dynamic = ???
}

trait ExpectationResult extends Result {

  def passed(): js.Boolean = ???
  val message: js.String = ???
}
