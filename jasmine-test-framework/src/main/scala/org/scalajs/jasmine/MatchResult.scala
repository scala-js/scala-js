package org.scalajs.jasmine

import scala.scalajs.js

trait MatchResult extends js.Object {
  val actual: js.Any = ???
  var message: js.Function0[js.Array[String]] = ???
}