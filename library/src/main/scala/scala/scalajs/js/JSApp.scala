/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs.js

import annotation.{JSExport, JSExportDescendentObjects}

/** Old-style base class for top-level, entry point main objects.
 *
 *  [[JSApp]] provides two services to an `object Foo` that extends it. These
 *  two services are replaced by two different features, starting with Scala.js
 *  0.6.18.
 *
 *  == Discoverability by sbt as main object ==
 *
 *  Since Scala.js 0.6.18, the sbt plugin can recognize "standard" `main`
 *  methods of the form
 *  {{{
 *  def main(args: Array[String]): Unit = ...
 *  }}}
 *  in objects, even if they do not extend `JSApp`. Use such a main method to
 *  replace [[JSApp]] in the context of discoverability by sbt.
 *
 *  To enable it as main method, make sure you also set
 *  {{{
 *  scalaJSUseMainModuleInitializer := true
 *  }}}
 *  in your project settings.
 *
 *  == Automatic export to JavaScript ==
 *
 *  Given
 *  {{{
 *  package bar
 *
 *  object Foo extends js.JSApp {
 *    def main(): Unit = println("Hello world!")
 *  }
 *  }}}
 *  the object `Foo` and its `main` method are automatically exported such that
 *  JavaScript code can call
 *  {{{
 *  bar.Foo().main();
 *  }}}
 *
 *  To achieve exactly the same behavior without [[JSApp]], define `Foo` as
 *  {{{
 *  package bar
 *
 *  object Foo {
 *    @JSExportTopLevel("bar.Foo")
 *    protected def getInstance(): this.type = this
 *
 *    @JSExport
 *    def main(): Unit = println("Hello world!")
 *  }
 *  }}}
 *
 *  Alternatively, you can define it as
 *  {{{
 *  package bar
 *
 *  object Foo {
 *    @JSExportTopLevel("bar.Foo.main")
 *    def main(): Unit = println("Hello world!")
 *  }
 *  }}}
 *  but in that case, the JavaScript code will have to be changed to
 *  {{{
 *  bar.Foo.main()
 *  }}}
 */
@deprecated("Consult the Scaladoc of js.JSApp for migration tips.", "0.6.20")
@JSExportDescendentObjects
trait JSApp {
  @JSExport
  def main(): Unit
}
