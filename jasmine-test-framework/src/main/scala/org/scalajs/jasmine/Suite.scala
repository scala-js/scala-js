package org.scalajs.jasmine

import scala.scalajs.js

trait Suite extends js.Object {
  def results(): SuiteResults = js.native
  val description: String = js.native
  def getFullName(): String = js.native
}
