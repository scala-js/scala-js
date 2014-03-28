package scala.scalajs.js

import annotation.{ JSExport, JSExportDescendentObjects }

@JSExportDescendentObjects
trait JSApp {
  @JSExport
  def main(): Unit

  // for discovery by sbt:
  @JSExport
  final def main(args: scala.Array[java.lang.String]): Unit = main()
}
