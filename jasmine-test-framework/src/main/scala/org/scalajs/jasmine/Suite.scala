package org.scalajs.jasmine

import scala.scalajs.js

trait Suite extends js.Object {

  def results(): SuiteResults = ???
  val description: js.String = ???
}
