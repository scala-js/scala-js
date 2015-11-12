/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js

/** Additional tests for java.lang.Object that have to be in a separate
 *  codebase than testSuite to be meaningful.
 *
 *  If moved to testSuite, those tests "fail to fail" due to mass effects
 *  produced by the immensity of the testSuite codebase.
 */
object ObjectTestEx extends JasmineTest {

  describe("java.lang.Object ex") {

    it("clone() - #2010") {
      class NotCloneable extends Object {
        override def clone(): NotCloneable =
          super.clone().asInstanceOf[NotCloneable]
      }

      expect(() => new NotCloneable().clone()).toThrow

      class SomeCloneable(val x: Int) extends Object with Cloneable {
        override def clone(): SomeCloneable =
          super.clone().asInstanceOf[SomeCloneable]

        @noinline def y(): Int = x + 3
      }

      val o = new SomeCloneable(5)
      val o2 = o.clone()
      expect(o2.asInstanceOf[js.Any]).not.toBe(o.asInstanceOf[js.Any])
      expect(o2.getClass.asInstanceOf[js.Any]).toBe(classOf[SomeCloneable].asInstanceOf[js.Any])
      expect(o2.x).toEqual(5)
      expect(o2.y()).toEqual(8)
    }

  }
}
