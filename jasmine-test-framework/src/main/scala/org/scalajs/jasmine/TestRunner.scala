package org.scalajs.jasmine

import scala.scalajs.js

trait TestRunner extends js.Object {
  def suites(): js.Array[Suite] = ???
  def specs(): js.Array[Spec] = ???
  def topLevelSuites(): js.Array[Suite] = ???
}