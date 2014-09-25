package org.scalajs.jasmine

import scala.scalajs.js

trait SpecResults extends js.Object {
  def passed(): Boolean = js.native
  def getItems(): js.Array[Result] = js.native
}
