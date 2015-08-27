package org.scalajs.jasmine

import scala.scalajs.js

@js.native
trait Spec extends js.Object {
  def results(): SpecResults = js.native
  val description: String = js.native
  val suite: Suite = js.native
}
