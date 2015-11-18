/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert._
import org.scalajs.jasminetest.JasmineTest

/** Tests the compiler re-patching of native longs to
 *  scala.scalajs.runtime.Long
 *  see org.scalajs.testsuite.jsinterop.RuntimeLongTest
 *  for a test of the implementation itself
 */
class LongJSTest {
  @Test def `should_convert_to_js.Any`(): Unit = {
    val x = 5: js.Any
    assertTrue((5L: js.Any) == x)
  }
}

object LongJSTest extends JasmineTest {

  describe("JavaScript 64-bit long compatibility") {
    when("compliant-asinstanceofs").
    it("should correctly implement asInstanceOf Longs (negative)") {
      val dyn: Any = 5L

      expect(() => dyn.asInstanceOf[Int]).toThrow
    }
  }

}
