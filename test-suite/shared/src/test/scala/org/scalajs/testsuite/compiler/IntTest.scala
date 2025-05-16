/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

/* General note on the way these tests are written:
 * We leverage the constant folding applied by the Scala compiler to write
 * sound tests. We always perform the same operation, on the same operands,
 * once in a way constant folding understands, and once in a way it doesn't.
 * Since constant folding is performed on the JVM, we know it has the right
 * semantics.
 */

class IntTest {
  import IntTest._

  @noinline def hideFromOptimizer(x: Int): Int = x

  @Test def unaryMinus(): Unit = {
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

  @Test def plus(): Unit = {
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

  @Test def minus(): Unit = {
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

  @Test def times(): Unit = {
    @inline def test(a: Int, b: Int, expected: Int): Unit = {
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

  @Test def division(): Unit = {
    @inline def test(a: Int, b: Int, expected: Int): Unit = {
      assertEquals(expected, a / b)
      assertEquals(expected, hideFromOptimizer(a) / b)
      assertEquals(expected, a / hideFromOptimizer(b))
      assertEquals(expected, hideFromOptimizer(a) / hideFromOptimizer(b))
    }

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

  @Test def divisionByZero(): Unit = {
    @noinline def divNoInline(x: Int, y: Int): Int = x / y

    @inline def divInline(x: Int, y: Int): Int = x / y

    @inline def test(x: Int): Unit = {
      assertThrows(classOf[ArithmeticException], x / 0)
      assertThrows(classOf[ArithmeticException], divNoInline(x, 0))
      assertThrows(classOf[ArithmeticException], divInline(x, 0))
    }

    test(0)
    test(1)
    test(43)
    test(-3)

    // Eligible for constant-folding by scalac itself
    assertThrows(classOf[ArithmeticException], 5 / 0)
  }

  @Test def modulo(): Unit = {
    @inline def test(a: Int, b: Int, expected: Int): Unit = {
      assertEquals(expected, a % b)
      assertEquals(expected, hideFromOptimizer(a) % b)
      assertEquals(expected, a % hideFromOptimizer(b))
      assertEquals(expected, hideFromOptimizer(a) % hideFromOptimizer(b))
    }

    test(0, 53423, 0)
    test(0, -4321, 0)
    test(42, 100, 42)
    test(-42, 100, -42)
    test(42, -100, 42)
    test(-42, -100, -42)
    test(54321, 1, 0)
    test(12345, -1, 0)

    test(Int.MinValue, -1, 0)
    test(Int.MinValue, 1, 0)
    test(Int.MinValue, Int.MinValue, 0)
    test(Int.MaxValue, Int.MinValue, Int.MaxValue)

    test(888911840, 398464245, 91983350)
    test(-634705906, 1150127857, -634705906)
    test(-1467962174, -233333970, -67958354)
    test(-696524351, 1221802275, -696524351)
    test(192062123, -1389202096, 192062123)
    test(-2208176, -1199770929, -2208176)
    test(775070956, -1351562695, 775070956)
    test(1410025853, 634082193, 141861467)
    test(1483138107, 545129863, 392878381)
    test(-1750603509, -2001668676, -1750603509)
    test(-270330575, 1100500745, -270330575)
    test(1557209065, 639211870, 278785325)
    test(297563540, 1672453705, 297563540)
    test(-883430936, -305148158, -273134620)
    test(247095188, 1930104566, 247095188)
    test(215451192, 281252383, 215451192)
    test(-1638861962, -1884346174, -1638861962)
    test(-849285004, -1937370114, -849285004)
    test(917797205, -275505304, 91281293)
    test(2049327999, -204847985, 848149)
    test(1174749612, -1273766477, 1174749612)
    test(1189124914, 1440011421, 1189124914)
    test(1369891803, -739431590, 630460213)
    test(-1577016723, 413149709, -337567596)
    test(755666685, 866271370, 755666685)
    test(-1782130290, -1489343133, -292787157)
    test(816445033, 538200830, 278244203)
    test(-1414153870, 1421982787, -1414153870)
    test(-527770684, 934868397, -527770684)
    test(970171664, -1074068330, 970171664)
    test(1169129213, 909981312, 259147901)
    test(549895888, 1424255773, 549895888)
    test(39288225, 1676833161, 39288225)
    test(2103415794, 1433396342, 670019452)
    test(-1335123667, 1253612082, -81511585)
    test(1167175365, 183823784, 64232661)
    test(-218820366, -453768428, -218820366)
    test(1057222901, 979724663, 77498238)
    test(1447649376, 562648203, 322352970)
    test(-1707094850, -1747773256, -1707094850)
    test(1306320298, 1011113251, 295207047)
    test(876405764, -1894502292, 876405764)
    test(545913031, 2065126651, 545913031)
    test(1260153312, -1226838929, 33314383)
    test(-1661253454, 1525786722, -135466732)
    test(-111268491, 890896413, -111268491)
    test(-1759700274, 177094529, -165849513)
    test(-1800706765, 676167605, -448371555)
    test(-1062816870, -1277427373, -1062816870)
    test(-1459251939, 1050101679, -409150260)
    test(-435377952, -839304773, -435377952)
    test(814184788, 468237189, 345947599)
    test(762986966, 1152461206, 762986966)
    test(1903384632, -1627287802, 276096830)
    test(-520543732, 1588064688, -520543732)
    test(-1818967095, -177683924, -42127855)
    test(1344112404, -1559563028, 1344112404)
    test(1799414498, -1577351623, 222062875)
    test(-712914966, -138071199, -22558971)
    test(-797669144, -239001431, -80664851)
    test(1692265734, 334357639, 20477539)
    test(1924004536, 1660310311, 263694225)
    test(-2070801297, -212345714, -159689871)
    test(2135202456, 250533809, 130931984)
    test(1837231905, 1239875653, 597356252)
    test(212607908, 958539849, 212607908)
    test(1708259480, -597507370, 513244740)
    test(211788294, -1703494233, 211788294)
    test(-1226812265, 643864664, -582947601)
    test(129117064, 24772648, 5253824)
    test(-1817485301, -1220089446, -597395855)
    test(1050041972, 1323407722, 1050041972)
    test(1894216159, 550525839, 242638642)
    test(1512888749, -58954615, 39023374)
    test(67943709, 251158410, 67943709)
    test(-170907253, 259754549, -170907253)
    test(1817679841, 1962917348, 1817679841)
    test(1393475223, 56342366, 41258439)
    test(-1446764231, 732553836, -714210395)
    test(-1177352752, -1540441526, -1177352752)
    test(1730351975, 1070202005, 660149970)
    test(-245860610, -1447047935, -245860610)
    test(171297294, -175021127, 171297294)
    test(-1683938370, -1283002924, -400935446)
    test(-1784042737, 1248954832, -535087905)
    test(-1192964387, -641693195, -551271192)
    test(-1785435356, 1381533640, -403901716)
    test(1742945724, 1082065950, 660879774)
    test(-1980310021, -1734212561, -246097460)
    test(-45415630, 1220878976, -45415630)
    test(1026872019, -781642578, 245229441)
    test(-857269030, -246749459, -117020653)
    test(42248076, -376251144, 42248076)
    test(2095799967, 7118872, 2851599)
    test(-1108120389, -1226123877, -1108120389)
    test(-86291288, -2049867490, -86291288)
    test(-1611534696, 2039632573, -1611534696)
    test(430855640, -969487425, 430855640)
    test(-1501301311, 306307416, -276071647)
    test(-1272583978, -1973216746, -1272583978)
  }

  @Test def moduloByZero(): Unit = {
    @noinline def modNoInline(x: Int, y: Int): Int = x % y

    @inline def modInline(x: Int, y: Int): Int = x % y

    @inline def test(x: Int): Unit = {
      assertThrows(classOf[ArithmeticException], x % 0)
      assertThrows(classOf[ArithmeticException], modNoInline(x, 0))
      assertThrows(classOf[ArithmeticException], modInline(x, 0))
    }

    test(0)
    test(1)
    test(43)
    test(-3)

    // Eligible for constant-folding by scalac itself
    assertThrows(classOf[ArithmeticException], 5 % 0)
  }

  @Test def remainderNegative0_Issue1984(): Unit = {
    @noinline def value: Int = -8
    assertEquals(0, value % 8)
  }

  @Test def shiftLeft(): Unit = {
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

  @Test def shiftRight(): Unit = {
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

  @Test def shiftRightSignExtend(): Unit = {
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

  @Test def intShiftLeftLongConstantFolded(): Unit = {
    assert(0x01030507 << 36L == 271601776)
    val r = 0x01030507 << 36L
    assert(r == 271601776)
  }

  @Test def intShiftLeftLongAtRuntime(): Unit = {
    var x: Int = 0x01030507
    var y: Long = 36L
    assert(x << y == 271601776)
    val r = x << y
    assert(r == 271601776)
  }

  @Test def intShiftLogicalRightLongConstantFolded(): Unit = {
    assert(0x90503010 >>> 36L == 151323393)
    val r = 0x90503010 >>> 36L
    assert(r == 151323393)
  }

  @Test def intShiftLogicalRightLongAtRuntime(): Unit = {
    var x: Int = 0x90503010
    var y: Long = 36L
    assert(x >>> y == 151323393)
    val r = x >>> y
    assert(r == 151323393)
  }

  @Test def intShiftArithmeticRightLongConstantFolded(): Unit = {
    assert(0x90503010 >> 36L == -117112063)
    val r = 0x90503010 >> 36L
    assert(r == -117112063)
  }

  @Test def intShiftArithmeticRightLongAtRuntime(): Unit = {
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
