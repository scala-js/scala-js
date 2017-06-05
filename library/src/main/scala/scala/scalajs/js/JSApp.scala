package scala.scalajs.js

/** Base class for top-level, entry point main objects.
 *
 *  In Scala.js 0.6.x, an top-level object had to extend `js.JSApp` to be
 *  recognized by the sbt plugin as a "main" object, to be executed with `run`.
 *  Starting with Scala.js 1.0.0, any object with a standard main method of the
 *  form
 *  {{{
 *  def main(args: Array[String]): Unit = ???
 *  }}}
 *  will be recognized by the sbt plugin, just like for a JVM project.
 *
 *  [[JSApp]] is therefore deprecated, and should not be used anymore.
 */
@deprecated(
    "Extending js.JSApp is not necessary anymore for an object to be " +
    "recognized by the Scala.js sbt plugin. " +
    "Use a normal object with a `def main(args: Array[String]): Unit` " +
    "instead, which will also be cross-platform. " +
    "Note that js.JSApp objects and their main() method are not " +
    "automatically exported to JavaScript either; explicitly export your " +
    "object if you relied on this behavior.",
    "1.0.0")
trait JSApp {
  def main(args: scala.Array[String]): Unit = main()

  def main(): Unit
}
