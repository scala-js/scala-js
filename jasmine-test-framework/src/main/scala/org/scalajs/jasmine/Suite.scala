package org.scalajs.jasmine

import scala.scalajs.js

trait Suite extends js.Object {
  val id: String = ???
  val description: String = ???
  def results(): SuiteResults = ???
  def specs(): js.Array[Spec] = ???
}
