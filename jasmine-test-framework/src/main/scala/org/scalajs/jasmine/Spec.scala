package org.scalajs.jasmine

import scala.scalajs.js

trait Spec extends js.Object {
  def results(): SpecResults = ???
  val description: String = ???
  val suite: Suite = ???
}
