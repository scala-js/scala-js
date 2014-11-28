/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import org.scalajs.jasminetest.JasmineTest

object RangesTest extends JasmineTest {

  describe("Collection ranges") {

    it("Iterable.range should not emit dce warnings - #650") {
      Iterable.range(1, 10)
    }

    it("Iterable.range and simple range should be equal") {
      // Mostly to exercise more methods of ranges for dce warnings
      expect(Iterable.range(0, 10).toList == (0 until 10).toList).toBeTruthy
    }

  }

}
