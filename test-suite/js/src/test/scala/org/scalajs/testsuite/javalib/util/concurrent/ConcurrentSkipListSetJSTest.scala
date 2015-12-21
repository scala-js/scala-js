/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent

import java.util.concurrent.ConcurrentSkipListSet

import org.junit.Assert._

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.AssertThrows._

import scala.language.implicitConversions

object ConcurrentSkipListSetJSTest extends JasmineTest {
  describe("ConcurrentSkipListSetTest") {
    when("compliant-asinstanceofs").
    it("should throw exception on non comparable objects") {
      case class TestObj(num: Int)

      val csls = new ConcurrentSkipListSet[TestObj]()

      assertEquals(0, csls.size())
      expectThrows(classOf[ClassCastException], csls.add(TestObj(111)))
      assertTrue(csls.comparator eq null)
    }
  }
}
