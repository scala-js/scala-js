package scala.scalajs.js

/** Base class for top-level, entry point main objects.
 *
 *  [[JSApp]] is typically used to mark the entry point of a Scala.js
 *  application. As such, the sbt plugin also recognizes top-level objects
 *  extending [[JSApp]]. It allows to run their [[main]] method with `sbt run`.
 *
 *  To execute the [[main]] method immediately when your Scala.js file is
 *  loaded, use the `scalaJSUseMainModuleInitializer` setting in the sbt plugin.
 */
trait JSApp {
  def main(): Unit
}
