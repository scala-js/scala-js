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
 *  @see [[http://www.scala-js.org/doc/export-to-javascript.html Export Scala.js APIs to JavaScript]]
 */
class JSExportNamed extends scala.annotation.StaticAnnotation {
  def this(name: String) = this()
}
