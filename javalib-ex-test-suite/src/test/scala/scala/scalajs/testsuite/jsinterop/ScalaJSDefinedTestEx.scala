/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.jsinterop

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Additional tests for Scala.js-defined JS classes that have to be in a
 *  separate codebase than testSuite to be meaningful.
 *
 *  If moved to testSuite, those tests "fail to fail" due to mass effects
 *  produced by the immensity of the testSuite codebase.
 */
class ScalaJSDefinedTestEx {

  @Test def constructor_property_on_the_prototype_issue_1963(): Unit = {
    @ScalaJSDefined
    class ParentClass extends js.Object

    @ScalaJSDefined
    class ChildClass extends ParentClass

    val child = new ChildClass().asInstanceOf[js.Dynamic]
    assertSame(js.constructorOf[ChildClass], child.constructor)
  }
}
