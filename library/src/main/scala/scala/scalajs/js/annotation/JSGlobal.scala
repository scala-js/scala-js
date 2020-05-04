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

package scala.scalajs.js.annotation

import scala.annotation.meta._

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
@field @getter @setter
class JSGlobal extends scala.annotation.StaticAnnotation {
  def this(name: String) = this()
}
