package scala.scalajs.js

import annotation.{JSExport, JSExportDescendentObjects}

@JSExportDescendentObjects
trait JSApp {
  @JSExport
  def main(): Unit
}
