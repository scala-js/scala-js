package org.scalajs.jasmine

import scala.scalajs.js

trait Spec extends js.Object {
  val id: String = ???
  val description: String = ???
  val suite: Suite = ???
  def results(): SpecResults = ???
  def addMatchers(matchers: js.Dictionary[js.ThisFunction1[MatchResult, js.Any, Boolean]]): Unit = ???
}
