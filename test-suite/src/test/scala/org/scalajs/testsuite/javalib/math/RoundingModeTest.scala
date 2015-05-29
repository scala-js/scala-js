/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/RoundingModeTest.java
 */

package org.scalajs.testsuite.javalib.math

import org.scalajs.jasminetest.JasmineTest
import java.math.RoundingMode

object RoundingModeTest extends JasmineTest {

  describe("RoundingModeTest") {

    it("testValues") {

      val values = RoundingMode.values
      expect(values.size).toEqual(8)

      expect(RoundingMode.UP.ordinal).toEqual(values(0).ordinal)
      expect(RoundingMode.DOWN.ordinal).toEqual(values(1).ordinal)
      expect(RoundingMode.CEILING.ordinal).toEqual(values(2).ordinal)
      expect(RoundingMode.FLOOR.ordinal).toEqual(values(3).ordinal)
      expect(RoundingMode.HALF_UP.ordinal).toEqual(values(4).ordinal)
      expect(RoundingMode.HALF_DOWN.ordinal).toEqual(values(5).ordinal)
      expect(RoundingMode.HALF_EVEN.ordinal).toEqual(values(6).ordinal)
      expect(RoundingMode.UNNECESSARY.ordinal).toEqual(values(7).ordinal)

      val rmUP = RoundingMode.UP
      expect(rmUP.toString).toEqual("UP")
    }
  }
}
