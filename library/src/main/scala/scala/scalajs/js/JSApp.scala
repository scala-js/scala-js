package scala.scalajs.js

import annotation.{JSExport, JSExportDescendentObjects}

/** Base class for top-level, entry point main objects.
 *
 *  Objects inheriting from [[JSApp]] are automatically exported to JavaScript
 *  under their fully qualified name, and their [[main]] method as well.
 *
 *  [[JSApp]] is typically used to mark the entry point of a Scala.js
 *  application. As such, the sbt plugin also recognizes top-level objects
 *  extending [[JSApp]]. It allows to run their [[main]] method with `sbt run`,
 *  and can also generate a tiny JavaScript launcher snippet executing the
 *  [[main]] method of one specific [[JSApp]] object.
 */
@JSExportDescendentObjects
trait JSApp {
  @JSExport
  def main(): Unit
}
