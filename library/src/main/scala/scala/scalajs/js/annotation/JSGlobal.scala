/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js.annotation

/** Marks the annotated class or object as being a member of the JavaScript
 *  global scope.
 *
 *  The annotated class/object must also be annotated with `@js.native`, and
 *  therefore extend [[scala.scalajs.js.Any js.Any]].
 *
 *  Given:
 *  {{{
 *  @js.native
 *  @JSGlobal
 *  class Foo extends js.Object
 *
 *  @js.native
 *  @JSGlobal("Foobar")
 *  object Bar extends js.Object
 *
 *  @js.native
 *  @JSGlobal("Lib.Babar")
 *  class Babar extends js.Object
 *  }}}
 *
 *  The following mappings apply (`global` denotes the global scope):
 *
 *  {{{
 *  Scala.js                | JavaScript
 *  ------------------------+------------------
 *  new Foo()               | new global.Foo()
 *  Bar                     | global.Foobar
 *  js.constructorOf[Babar] | global.Lib.Babar
 *  }}}
 *
 *  @see [[http://www.scala-js.org/doc/calling-javascript.html Calling JavaScript from Scala.js]]
 */
class JSGlobal extends scala.annotation.StaticAnnotation {
  def this(name: String) = this()
}
