// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/RoundingModeTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.RoundingMode

import org.junit.Test
import org.junit.Assert._

class RoundingModeTest {

  @Test def testValues(): Unit = {

    val values = RoundingMode.values
    assertEquals(8, values.size)

    assertEquals(values(0).ordinal, RoundingMode.UP.ordinal)
    assertEquals(values(1).ordinal, RoundingMode.DOWN.ordinal)
    assertEquals(values(2).ordinal, RoundingMode.CEILING.ordinal)
    assertEquals(values(3).ordinal, RoundingMode.FLOOR.ordinal)
    assertEquals(values(4).ordinal, RoundingMode.HALF_UP.ordinal)
    assertEquals(values(5).ordinal, RoundingMode.HALF_DOWN.ordinal)
    assertEquals(values(6).ordinal, RoundingMode.HALF_EVEN.ordinal)
    assertEquals(values(7).ordinal, RoundingMode.UNNECESSARY.ordinal)

    val rmUP = RoundingMode.UP
    assertEquals("UP", rmUP.toString)
  }
}
