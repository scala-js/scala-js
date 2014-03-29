package org.scalajs.jasmine

import scala.scalajs.js

trait SpecResults extends js.Object {
  def passed(): Boolean = ???
  def getItems(): js.Array[Result] = ???
}
