/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.jsinterop

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Additional tests for Scala.js-defined JS classes that have to be in a
 *  separate codebase than testSuite to be meaningful.
 *
 *  If moved to testSuite, those tests "fail to fail" due to mass effects
 *  produced by the immensity of the testSuite codebase.
 */
object ScalaJSDefinedTestEx extends JasmineTest {

  describe("Scala.js-defined JS classes ex") {

    it("constructor property on the prototype - #1963") {
      @ScalaJSDefined
      class ParentClass extends js.Object

      @ScalaJSDefined
      class ChildClass extends ParentClass

      val child = new ChildClass().asInstanceOf[js.Dynamic]
      expect(child.constructor).toBe(js.constructorOf[ChildClass])
    }

  }
}
