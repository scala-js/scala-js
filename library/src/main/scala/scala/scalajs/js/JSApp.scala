package scala.scalajs.js

/** Base class for top-level, entry point main objects (softly deprecated).
 *
 *  In Scala.js 1.x, `js.JSApp` will disappear. It is currently "softly"
 *  deprecated: it is not recommended to use it in new code, but it does not
 *  cause a deprecation warning (yet). Prefer using a standard main method (see
 *  below).
 *
 *  [[JSApp]] is typically used to mark the entry point of a Scala.js
 *  application. As such, the sbt plugin also recognizes top-level objects
 *  extending [[JSApp]]. It allows to run their [[main]] method with `sbt run`.
 *
 *  To execute the [[main]] method immediately when your Scala.js file is
 *  loaded, use the `scalaJSUseMainModuleInitializer` setting in the sbt plugin.
 *
 *  Starting with Scala.js 0.6.18, the sbt plugin can also recognize "standard"
 *  `main` methods of the form
 *  {{{
 *  def main(args: Array[String]): Unit = ...
 *  }}}
 *  in objects, even if they do not extend `JSApp`. Such main methods are
 *  cross-platform, and should be preferred over extending `JSApp` in new code.
 */
trait JSApp {
  def main(): Unit
}
