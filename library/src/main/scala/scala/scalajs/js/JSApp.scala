package scala.scalajs.js

import annotation.{JSExport, JSExportDescendentObjects}

/** Base class for top-level, entry point main objects (softly deprecated).
 *
 *  In Scala.js 1.x, `js.JSApp` will disappear. It is currently "softly"
 *  deprecated: it is not recommended to use it in new code, but it does not
 *  cause a deprecation warning (yet). Prefer using a standard main method (see
 *  below).
 *
 *  Objects inheriting from [[JSApp]] are automatically exported to JavaScript
 *  under their fully qualified name, and their [[main]] method as well.
 *
 *  [[JSApp]] is typically used to mark the entry point of a Scala.js
 *  application. As such, the sbt plugin also recognizes top-level objects
 *  extending [[JSApp]]. It allows to run their [[main]] method with `sbt run`,
 *  and can also generate a tiny JavaScript launcher snippet executing the
 *  [[main]] method of one specific [[JSApp]] object.
 *
 *  Starting with Scala.js 0.6.18, the sbt plugin can also recognize "standard"
 *  `main` methods of the form
 *  {{{
 *  def main(args: Array[String]): Unit = ...
 *  }}}
 *  in objects, even if they do not extend `JSApp`. Such main methods are
 *  cross-platform, and should be preferred over extending `JSApp` in new code.
 *  Note however that:
 *
 *  - the sbt plugin cannot create a launcher snippet for such objects, and
 *  - these objects are not automatically exported to JavaScript.
 */
@JSExportDescendentObjects
trait JSApp {
  @JSExport
  def main(): Unit
}
