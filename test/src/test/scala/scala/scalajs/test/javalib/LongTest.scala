/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import java.lang.Long

/**
 * tests the implementation of the java standard library Long
 * requires jsinterop/LongTest to work to make sense
 */
object LongTest extends JasmineTest {

  describe("java.lang.Long") {
    it("should implement bitCount") {
      expect(Long.bitCount(0L)).toEqual(0)
      expect(Long.bitCount(35763829229342837L)).toEqual(26)
      expect(Long.bitCount(-350003829229342837L)).toEqual(32)
    }
  }

}
