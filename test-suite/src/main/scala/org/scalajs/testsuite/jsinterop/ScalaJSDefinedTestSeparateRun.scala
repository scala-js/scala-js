/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Elements of ScalaJSDefinedTest in `src/test/` to be compiled in a
 *  separate compiler run, to test separate compilation.
 */
object ScalaJSDefinedTestSeparateRun {

  @ScalaJSDefined
  class SimpleParentClass extends js.Object {
    def foo(x: Int): Int = x + 1
  }

  @ScalaJSDefined
  class SimpleChildClass extends SimpleParentClass {
    override def foo(x: Int): Int = x + 3
  }

  @ScalaJSDefined
  trait SimpleTrait extends js.Any {
    def foo(x: Int): Int
  }

}
