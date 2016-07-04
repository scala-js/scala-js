/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform

/* General note on the way these tests are written:
 * We leverage the constant folding applied by the Scala compiler to write
 * sound tests. We always perform the same operation, on the same operands,
 * once in a way constant folding understands, and once in a way it doesn't.
 * Since constant folding is performed on the JVM, we know it has the right
 * semantics.
 */

class IntTest {
  import IntTest._

  @Test def `should_support_unary_minus`(): Unit = {
    def test(a: Int, expected: Int): Unit =
      assertEquals(expected, -a)

    test(56, -56)
    test(0, 0)
    test(-36, 36)

    test(MaxVal, -MaxVal)
    test(MinVal, -MinVal)
    test(-MaxVal, MaxVal)
    test(AlmostMinVal, -AlmostMinVal)
    test(AlmostMaxVal, -AlmostMaxVal)
  }

  @Test def `should_support_plus`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a + b)

    test(56, 654, 56 + 654)
    test(0, 25, 0 + 25)
    test(-36, 13, -36 + 13)

    test(MaxVal, 1, MaxVal + 1)
    test(MinVal, -1, MinVal - 1)
    test(MaxVal, MinVal, MaxVal + MinVal)
    test(AlmostMinVal, -100, AlmostMinVal - 100)
    test(AlmostMaxVal, 123, AlmostMaxVal + 123)
  }

  @Test def `should_support_minus`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a - b)

    test(56, 654, 56 - 654)
    test(0, 25, 0 - 25)
    test(-36, 13, -36 - 13)

    test(MaxVal, -1, MaxVal + 1)
    test(MinVal, 1, MinVal - 1)
    test(MaxVal, MinVal, MaxVal - MinVal)
    test(AlmostMinVal, 100, AlmostMinVal - 100)
    test(AlmostMaxVal, -123, AlmostMaxVal + 123)
  }

  @Test def `should_support_times`(): Unit = {
    @inline def test(a: Int, b: Int, expected: Int): Unit = {
      @noinline def hideFromOptimizer(x: Int): Int = x

      assertEquals(expected, a * b)
      assertEquals(expected, hideFromOptimizer(a) * b)
      assertEquals(expected, a * hideFromOptimizer(b))
      assertEquals(expected, hideFromOptimizer(a) * hideFromOptimizer(b))
    }

    test(56, 654, 56 * 654)
    test(0, 25, 0 * 25)
    test(-36, 13, -36 * 13)
    test(-5, -6, -5 * -6)

    test(MinVal, 1, MinVal * 1)
    test(MinVal, -1, MinVal * -1)
    test(MaxVal, 1, MaxVal * 1)
    test(MaxVal, -1, MaxVal * -1)

    test(MaxVal, MinVal, MaxVal * MinVal)
    test(MaxVal, MaxVal, MaxVal * MaxVal)
    test(MinVal, MaxVal, MinVal * MaxVal)
    test(MinVal, MinVal, MinVal * MinVal)

    test(AlmostMaxVal, 2, AlmostMaxVal * 2)
    test(AlmostMaxVal, 5, AlmostMaxVal * 5)
    test(AlmostMaxVal, -7, AlmostMaxVal * -7)
    test(AlmostMaxVal, -14, AlmostMaxVal * -14)
    test(AlmostMinVal, 100, AlmostMinVal * 100)
    test(AlmostMaxVal, -123, AlmostMaxVal * -123)

    // Random tests
    test(239172, 35717, -47428268)
    test(5, -2, -10)
    test(-1, -18457781, 18457781)
    test(755, -150845, -113887975)
    test(-2, -7688353, 15376706)
    test(-40, -1203115611, 879984184)
    test(-565859, -1498055, 1579346933)
    test(-15014, 1728312, -179072592)
    test(-14207764, -7738417, -1265340716)
    test(-1811162, -3, 5433486)
    test(-4784147, -157, 751111079)
    test(39312, -146385, -1459719824)
    test(0, -912, 0)
    test(-3859449, -89501, 1827161269)
    test(-386078, -5993, -1981201842)
    test(-61, -180184, 10991224)
    test(17285, 111570408, 54186376)
    test(363, 1727234, 626985942)
    test(-19, -25, 475)
    test(-6205, 3781567, -1989786755)
    test(-20630158, -2689, -360079986)
    test(-154, -4045, 622930)
    test(2083, 3, 6249)
    test(-112639, 2601236, -942845676)
    test(6, 1180501, 7083006)
    test(198232281, -84740529, 227574007)
    test(-5663851, -48985, -1729133005)
    test(-16, -1, 16)
    test(1036879855, -4578474, -1320724662)
    test(32717, 1435, 46948895)
    test(1508, -87, -131196)
    test(11579, -256851389, -1964864399)
    test(3, 32054941, 96164823)
    test(-490220367, -13, 2077897475)
    test(-4382268, -5294, 1724890312)
    test(-833003443, 4, 962953524)
    test(-6653, -11272, 74992616)
    test(-80, -12860, 1028800)
    test(339085308, 7, -1921370140)
    test(0, 14697, 0)
    test(9, 435192333, -378236299)
    test(25002923, 596076303, 1719763973)
    test(97786, 5883, 575275038)
    test(-19116, 24, -458784)
    test(-1, -97231130, 97231130)
    test(9, 62947, 566523)
    test(390, -78455341, -532811918)
    test(-3865508, 2824049, 1422864540)
    test(11937525, 1, 11937525)
    test(49387685, -792695, -744058035)

    // Random power of 2 tests
    test(-91975, 2, -183950)
    test(2, -91975, -183950)
    test(8061, 2048, 16508928)
    test(2048, 8061, 16508928)
    test(10292783, 32, 329369056)
    test(32, 10292783, 329369056)
    test(47407, 65536, -1188102144)
    test(65536, 47407, -1188102144)
    test(-7712808, 262144, 1063256064)
    test(262144, -7712808, 1063256064)
    test(-1120, 512, -573440)
    test(512, -1120, -573440)
    test(482292, 65536, 1542717440)
    test(65536, 482292, 1542717440)
    test(-8, 512, -4096)
    test(512, -8, -4096)
    test(0, 2, 0)
    test(2, 0, 0)
    test(-1, 16, -16)
    test(16, -1, -16)
    test(205384, 16777216, 1207959552)
    test(16777216, 205384, 1207959552)
    test(-80290, 2048, -164433920)
    test(2048, -80290, -164433920)
    test(-12, 16777216, -201326592)
    test(16777216, -12, -201326592)
    test(1, 4194304, 4194304)
    test(4194304, 1, 4194304)
    test(693299, 32768, 1243185152)
    test(32768, 693299, 1243185152)
    test(-3601067, 32, -115234144)
    test(32, -3601067, -115234144)
    test(-2093, 16, -33488)
    test(16, -2093, -33488)
    test(24, 65536, 1572864)
    test(65536, 24, 1572864)
    test(1037462, 4, 4149848)
    test(4, 1037462, 4149848)
    test(-141227812, 1048576, -1916796928)
    test(1048576, -141227812, -1916796928)
    test(-1, 2048, -2048)
    test(2048, -1, -2048)
    test(-449116, 16384, 1231618048)
    test(16384, -449116, 1231618048)
    test(-1, 524288, -524288)
    test(524288, -1, -524288)
    test(286, 67108864, 2013265920)
    test(67108864, 286, 2013265920)
    test(-5, 4, -20)
    test(4, -5, -20)
    test(-4, 8, -32)
    test(8, -4, -32)
    test(4, 134217728, 536870912)
    test(134217728, 4, 536870912)
    test(-253751, 512, -129920512)
    test(512, -253751, -129920512)
    test(1065624891, 268435456, -1342177280)
    test(268435456, 1065624891, -1342177280)
    test(86, 134217728, -1342177280)
    test(134217728, 86, -1342177280)
    test(-1249574471, 16777216, -1191182336)
    test(16777216, -1249574471, -1191182336)
    test(-424071148, 262144, -1068498944)
    test(262144, -424071148, -1068498944)
    test(15164009, 8388608, 880803840)
    test(8388608, 15164009, 880803840)
    test(-2, 131072, -262144)
    test(131072, -2, -262144)
    test(-2, 536870912, -1073741824)
    test(536870912, -2, -1073741824)
    test(-419474, 262144, 1706557440)
    test(262144, -419474, 1706557440)
    test(529136, 262144, 1270874112)
    test(262144, 529136, 1270874112)
    test(112, 65536, 7340032)
    test(65536, 112, 7340032)
    test(7600891, 2097152, 1600126976)
    test(2097152, 7600891, 1600126976)
    test(-28, 536870912, -2147483648)
    test(536870912, -28, -2147483648)
    test(-1, 2, -2)
    test(2, -1, -2)
    test(-1, 524288, -524288)
    test(524288, -1, -524288)
    test(9, 2048, 18432)
    test(2048, 9, 18432)
    test(1517, 536870912, -1610612736)
    test(536870912, 1517, -1610612736)
    test(-9838638, 131072, -1079771136)
    test(131072, -9838638, -1079771136)
    test(-169, 4, -676)
    test(4, -169, -676)
    test(30234488, 8388608, -1140850688)
    test(8388608, 30234488, -1140850688)
    test(-23, 33554432, -771751936)
    test(33554432, -23, -771751936)
    test(16365585, 256, -105377536)
    test(256, 16365585, -105377536)
    test(20668, 536870912, -2147483648)
    test(536870912, 20668, -2147483648)
  }

  @Test def `should_support_division`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a / b)

    test(654, 56, 654 / 56)
    test(0, 25, 0 / 25)
    test(-36, 13, -36 / 13)
    test(-55, -6, -55 / -6)

    test(MinVal, 1, MinVal / 1)
    test(MinVal, -1, MinVal / -1)
    test(MaxVal, 1, MaxVal / 1)
    test(MaxVal, -1, MaxVal / -1)

    test(MaxVal, MinVal, MaxVal / MinVal)
    test(MaxVal, MaxVal, MaxVal / MaxVal)
    test(MinVal, MaxVal, MinVal / MaxVal)
    test(MinVal, MinVal, MinVal / MinVal)

    test(AlmostMaxVal, 2, AlmostMaxVal / 2)
    test(AlmostMaxVal, 5, AlmostMaxVal / 5)
    test(AlmostMaxVal, -7, AlmostMaxVal / -7)
    test(AlmostMaxVal, -14, AlmostMaxVal / -14)
    test(AlmostMinVal, 100, AlmostMinVal / 100)
    test(AlmostMaxVal, -123, AlmostMaxVal / -123)
  }

  @Test def `percent_should_never_produce_a_negative_0_#1984`(): Unit = {
    @noinline def value: Int = -8
    assertTrue((value % 8).asInstanceOf[java.lang.Integer].equals(0))
  }

  @Test def `should_support_shift_left`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a << b)

    test(0, 5, 0 << 5)
    test(1, 5, 1 << 5)
    test(13, 4, 13 << 4)
    test(-35, 5, -35 << 5)
    test(345, 0, 345 << 0)

    test(MinVal, 0, MinVal << 0)
    test(MaxVal, 0, MaxVal << 0)
    test(MinVal, 1, MinVal << 1)
    test(MaxVal, 1, MaxVal << 1)
  }

  @Test def `should_support_shift_right`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a >> b)

    test(0, 5, 0 >> 5)
    test(32, 5, 32 >> 5)
    test(31, 4, 31 >> 4)
    test(-355, 5, -355 >> 5)
    test(345, 0, 345 >> 0)

    test(MinVal, 0, MinVal >> 0)
    test(MaxVal, 0, MaxVal >> 0)
    test(MinVal, 1, MinVal >> 1)
    test(MaxVal, 1, MaxVal >> 1)
  }

  @Test def `should_support_shift_right_sign_extend`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a >>> b)

    test(0, 5, 0 >>> 5)
    test(32, 5, 32 >>> 5)
    test(31, 4, 31 >>> 4)
    test(-355, 5, -355 >>> 5)
    test(345, 0, 345 >>> 0)

    test(MinVal, 0, MinVal >>> 0)
    test(MaxVal, 0, MaxVal >>> 0)
    test(MinVal, 1, MinVal >>> 1)
    test(MaxVal, 1, MaxVal >>> 1)
  }

  private def scalacCorrectlyHandlesIntShiftLong: Boolean = {
    import Platform.scalaVersion
    !(scalaVersion.startsWith("2.10.") || scalaVersion.startsWith("2.11.") ||
        scalaVersion.startsWith("2.12.0-M4"))
  }

  @Test def intShiftLeftLongConstantFolded(): Unit = {
    assumeTrue("scalac must correctly handle int shift long",
        scalacCorrectlyHandlesIntShiftLong)

    assert(0x01030507 << 36L == 271601776)
    val r = 0x01030507 << 36L
    assert(r == 271601776)
  }

  @Test def intShiftLeftLongAtRuntime(): Unit = {
    assumeTrue("On the JVM, scalac must correctly handle int shift long",
        !Platform.executingInJVM || scalacCorrectlyHandlesIntShiftLong)

    var x: Int = 0x01030507
    var y: Long = 36L
    assert(x << y == 271601776)
    val r = x << y
    assert(r == 271601776)
  }

  @Test def intShiftLogicalRightLongConstantFolded(): Unit = {
    assumeTrue("scalac must correctly handle int shift long",
        scalacCorrectlyHandlesIntShiftLong)

    assert(0x90503010 >>> 36L == 151323393)
    val r = 0x90503010 >>> 36L
    assert(r == 151323393)
  }

  @Test def intShiftLogicalRightLongAtRuntime(): Unit = {
    assumeTrue("On the JVM, scalac must correctly handle int shift long",
        !Platform.executingInJVM || scalacCorrectlyHandlesIntShiftLong)

    var x: Int = 0x90503010
    var y: Long = 36L
    assert(x >>> y == 151323393)
    val r = x >>> y
    assert(r == 151323393)
  }

  @Test def intShiftArithmeticRightLongConstantFolded(): Unit = {
    assumeTrue("scalac must correctly handle int shift long",
        scalacCorrectlyHandlesIntShiftLong)

    assert(0x90503010 >> 36L == -117112063)
    val r = 0x90503010 >> 36L
    assert(r == -117112063)
  }

  @Test def intShiftArithmeticRightLongAtRuntime(): Unit = {
    assumeTrue("On the JVM, scalac must correctly handle int shift long",
        !Platform.executingInJVM || scalacCorrectlyHandlesIntShiftLong)

    var x: Int = 0x90503010
    var y: Long = 36L
    assert(x >> y == -117112063)
    val r = x >> y
    assert(r == -117112063)
  }
}

object IntTest {

  // final val without type ascription to make sure these are constant-folded
  final val MinVal = Int.MinValue
  final val MaxVal = Int.MaxValue
  final val AlmostMinVal = Int.MinValue + 43
  final val AlmostMaxVal = Int.MaxValue - 36
}
