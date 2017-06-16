package scala.scalajs.js

/** Base class for top-level, entry point main objects.
 *
 *  Before Scala.js 0.6.18, a top-level object had to extend `js.JSApp` to be
 *  recognized by the sbt plugin as a "main" object, to be executed with `run`.
 *  Starting with Scala.js 0.6.18, any object with a standard main method of
 *  the form
 *  {{{
 *  def main(args: Array[String]): Unit = ???
 *  }}}
 *  will be recognized by the sbt plugin, just like for a JVM project.
 *
 *  In order for the `main` method to be considered by the sbt plugin, set
 *  {{{
 *  scalaJSUseMainModuleInitializer := true
 *  }}}
 *  in your build.
 *
 *  [[JSApp]] is therefore deprecated, and should not be used anymore. It will
 *  disappear before 1.0.0 final.
 *
 *  Also note that an object extending [[JSApp]] is not exported to JavaScript
 *  anymore, nor is its `main()` method. Explicitly export the object and/or
 *  its main method if necessary.
 */
@deprecated(
    "Extending js.JSApp is not necessary anymore for an object to be " +
    "recognized by the Scala.js sbt plugin. " +
    "Use a normal object with a `def main(args: Array[String]): Unit` " +
    "instead, which will also be cross-platform. " +
    "Note that js.JSApp objects and their main() method are not " +
    "automatically exported to JavaScript anymore either; explicitly export " +
    "your object if you relied on this behavior.",
    "1.0.0-M1")
trait JSApp {
  def main(args: scala.Array[String]): Unit = main()

  def main(): Unit
}
