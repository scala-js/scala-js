/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js.annotation

/** Exports the given method to JavaScript with named parameters.
 *
 *  It can then be called like this:
 *  {{{
 *  obj.foo({
 *    param1: value1
 *    param2: value2
 *    param7: value3
 *  });
 *  }}}
 *
 *  Note that named exports don't support overloading. Therefore the
 *  following will fail:
 *  {{{
 *  class A {
 *    @JSExportNamed
 *    def a(foo: Int) = foo + 1
 *    @JSExportNamed
 *    def a(bar: String) = "Hello " + bar
 *  }
 *  }}}
 *
 *  As of Scala.js 0.6.11, `@JSExportNamed` is deprecated without direct
 *  replacement (see [[https://github.com/scala-js/scala-js/issues/2442]]).
 *  You should take a single parameter of a JS type and decompose it yourself.
 *  For example, instead of
 *  {{{
 *  class A {
 *    @JSExportNamed
 *    def foo(a: Int, b: String, c: Boolean = false): Unit = {
 *      // do something with a, b, c
 *    }
 *  }
 *  }}}
 *  you should write:
 *  {{{
 *  @ScalaJSDefined
 *  trait FooOptions extends js.Object {
 *    val a: Int
 *    val b: String
 *    val c: js.UndefOr[Boolean]
 *  }
 *
 *  class A {
 *    @JSExport
 *    def foo(options: FooOptions): Unit = {
 *      val a = options.a
 *      val b = options.b
 *      val c = options.c.getOrElse(false)
 *      // do something with a, b, c
 *    }
 *  }
 *  }}}
 *
 *  @see [[http://www.scala-js.org/doc/export-to-javascript.html Export Scala.js APIs to JavaScript]]
 */
@deprecated(
    "Use @JSExport with an explicit option bag instead. " +
    "See the Scaladoc for more details.",
    "0.6.11")
class JSExportNamed extends scala.annotation.StaticAnnotation {
  def this(name: String) = this()
}
