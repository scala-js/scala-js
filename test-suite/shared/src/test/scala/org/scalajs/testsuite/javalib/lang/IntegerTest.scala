/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class IntegerTest {

  // Explicitly define these as `var`'s to avoid any compile-time constant folding
  val MaxValue: Int = Int.MaxValue
  val MinValue: Int = Int.MinValue

  @Test def `reverseBytes`(): Unit = {
    assertEquals(0xefbeadde, Integer.reverseBytes(0xdeadbeef))
  }

  @Test def rotateLeft(): Unit = {
    assertEquals(0x689cd401, Integer.rotateLeft(0x689cd401, 0))
    assertEquals(0xd139a802, Integer.rotateLeft(0x689cd401, 1))
    assertEquals(0x9cd40168, Integer.rotateLeft(0x689cd401, 8))
    assertEquals(0x9a802d13, Integer.rotateLeft(0x689cd401, 13))
    assertEquals(0x689cd401, Integer.rotateLeft(0x689cd401, 32))
    assertEquals(0xd139a802, Integer.rotateLeft(0x689cd401, 33))
    assertEquals(0xe6a00b44, Integer.rotateLeft(0x689cd401, 43))
    assertEquals(0xb44e6a00, Integer.rotateLeft(0x689cd401, -1))
    assertEquals(0x89cd4016, Integer.rotateLeft(0x689cd401, -28))
    assertEquals(0x2d139a8, Integer.rotateLeft(0x689cd401, -39))
  }

  @Test def rotateRight(): Unit = {
    assertEquals(0x689cd401, Integer.rotateRight(0x689cd401, 0))
    assertEquals(0xb44e6a00, Integer.rotateRight(0x689cd401, 1))
    assertEquals(0x1689cd4, Integer.rotateRight(0x689cd401, 8))
    assertEquals(0xa00b44e6, Integer.rotateRight(0x689cd401, 13))
    assertEquals(0x689cd401, Integer.rotateRight(0x689cd401, 32))
    assertEquals(0xb44e6a00, Integer.rotateRight(0x689cd401, 33))
    assertEquals(0x802d139a, Integer.rotateRight(0x689cd401, 43))
    assertEquals(0xd139a802, Integer.rotateRight(0x689cd401, -1))
    assertEquals(0x1689cd40, Integer.rotateRight(0x689cd401, -28))
    assertEquals(0x4e6a00b4, Integer.rotateRight(0x689cd401, -39))
  }

  @Test def bitCount(): Unit = {
    assertEquals(0, Integer.bitCount(0))
    assertEquals(1, Integer.bitCount(1))
    assertEquals(1, Integer.bitCount(2))
    assertEquals(2, Integer.bitCount(3))
    assertEquals(31, Integer.bitCount(Int.MaxValue))
    assertEquals(1, Integer.bitCount(Int.MinValue))
    assertEquals(2, Integer.bitCount(Int.MinValue + 1))
    assertEquals(32, Integer.bitCount(-1))
    assertEquals(31, Integer.bitCount(-2))

    assertEquals(18, Integer.bitCount(-155937076))
    assertEquals(12, Integer.bitCount(830524462))
    assertEquals(17, Integer.bitCount(-1468950275))
    assertEquals(22, Integer.bitCount(1878189982))
    assertEquals(16, Integer.bitCount(1369853111))
    assertEquals(16, Integer.bitCount(993872011))
    assertEquals(17, Integer.bitCount(-419203945))
    assertEquals(18, Integer.bitCount(-1529972891))
    assertEquals(15, Integer.bitCount(-560981166))
    assertEquals(19, Integer.bitCount(-1083297551))
    assertEquals(19, Integer.bitCount(-1513915437))
    assertEquals(19, Integer.bitCount(-774144288))
    assertEquals(13, Integer.bitCount(1617041908))
    assertEquals(15, Integer.bitCount(-799619923))
    assertEquals(11, Integer.bitCount(1630552297))
    assertEquals(15, Integer.bitCount(1893565724))
    assertEquals(20, Integer.bitCount(-167512165))
    assertEquals(17, Integer.bitCount(-1226735856))
    assertEquals(13, Integer.bitCount(-1602623352))
    assertEquals(14, Integer.bitCount(73385109))
    assertEquals(17, Integer.bitCount(1843595740))
    assertEquals(16, Integer.bitCount(-2005160623))
    assertEquals(21, Integer.bitCount(-220474394))
    assertEquals(14, Integer.bitCount(-1474261577))
    assertEquals(18, Integer.bitCount(-2015504620))
    assertEquals(20, Integer.bitCount(1450835633))
    assertEquals(13, Integer.bitCount(-1877059561))
    assertEquals(16, Integer.bitCount(-864957023))
    assertEquals(17, Integer.bitCount(-1423863837))
    assertEquals(16, Integer.bitCount(661877472))
    assertEquals(14, Integer.bitCount(-1297344862))
    assertEquals(20, Integer.bitCount(-1084965589))
    assertEquals(20, Integer.bitCount(-169792549))
    assertEquals(19, Integer.bitCount(-1175303521))
    assertEquals(14, Integer.bitCount(-2075407535))
    assertEquals(21, Integer.bitCount(-34407382))
    assertEquals(18, Integer.bitCount(-686482061))
    assertEquals(18, Integer.bitCount(-1280254298))
    assertEquals(14, Integer.bitCount(-1236753591))
    assertEquals(20, Integer.bitCount(-629695246))
    assertEquals(15, Integer.bitCount(-1041379007))
    assertEquals(12, Integer.bitCount(1133674695))
    assertEquals(17, Integer.bitCount(-673156775))
    assertEquals(15, Integer.bitCount(1634657308))
    assertEquals(15, Integer.bitCount(-1634571160))
    assertEquals(17, Integer.bitCount(-1394160814))
    assertEquals(13, Integer.bitCount(57693078))
    assertEquals(15, Integer.bitCount(788250760))
    assertEquals(19, Integer.bitCount(-1217888690))
    assertEquals(12, Integer.bitCount(-1568144709))
    assertEquals(16, Integer.bitCount(827170343))
    assertEquals(21, Integer.bitCount(-341950555))
    assertEquals(14, Integer.bitCount(1287166354))
    assertEquals(19, Integer.bitCount(-1639223942))
    assertEquals(17, Integer.bitCount(532815708))
    assertEquals(17, Integer.bitCount(-768179729))
    assertEquals(15, Integer.bitCount(760154173))
    assertEquals(15, Integer.bitCount(2000995890))
    assertEquals(10, Integer.bitCount(1468010757))
    assertEquals(17, Integer.bitCount(295957433))
    assertEquals(17, Integer.bitCount(-1594421450))
    assertEquals(16, Integer.bitCount(-1110692843))
    assertEquals(10, Integer.bitCount(72567553))
    assertEquals(14, Integer.bitCount(1008258604))
    assertEquals(18, Integer.bitCount(1017279739))
    assertEquals(14, Integer.bitCount(-649579130))
    assertEquals(12, Integer.bitCount(-1743090924))
    assertEquals(15, Integer.bitCount(-1321851761))
    assertEquals(21, Integer.bitCount(1995849614))
    assertEquals(19, Integer.bitCount(1874069759))
    assertEquals(18, Integer.bitCount(57468414))
    assertEquals(13, Integer.bitCount(-159055416))
    assertEquals(15, Integer.bitCount(-770646612))
    assertEquals(15, Integer.bitCount(1274257460))
    assertEquals(17, Integer.bitCount(-1728268856))
    assertEquals(14, Integer.bitCount(-131769823))
    assertEquals(18, Integer.bitCount(1810706244))
    assertEquals(14, Integer.bitCount(881236344))
    assertEquals(11, Integer.bitCount(-536176288))
    assertEquals(20, Integer.bitCount(-371993265))
    assertEquals(13, Integer.bitCount(-1257692889))
    assertEquals(11, Integer.bitCount(38550368))
    assertEquals(14, Integer.bitCount(-196060824))
    assertEquals(18, Integer.bitCount(-218909520))
    assertEquals(21, Integer.bitCount(-735195141))
    assertEquals(16, Integer.bitCount(-1122922843))
    assertEquals(19, Integer.bitCount(-269171126))
    assertEquals(18, Integer.bitCount(2002409940))
    assertEquals(18, Integer.bitCount(-106797451))
    assertEquals(17, Integer.bitCount(-1412648370))
    assertEquals(20, Integer.bitCount(-342432881))
    assertEquals(20, Integer.bitCount(-294768321))
    assertEquals(14, Integer.bitCount(586296006))
    assertEquals(19, Integer.bitCount(-1627992562))
    assertEquals(17, Integer.bitCount(-1567624079))
    assertEquals(13, Integer.bitCount(453182827))
    assertEquals(16, Integer.bitCount(-704549035))
    assertEquals(15, Integer.bitCount(1722304234))
    assertEquals(19, Integer.bitCount(-747553362))
    assertEquals(18, Integer.bitCount(-1535508973))
  }

  @Test def numberOfLeadingZeros(): Unit = {
    /* The optimizer can *constant-fold* Integer.numberOfLeadingZeros,
     * so if we want to actually test anything happening at runtime, we have
     * to prevent the optimizer to see the connection between the actual
     * value of i, hence testNoInline. We also test the constant-folding
     * logic with testInline.
     */

    @inline def testInline(i: Int, expected: Int): Unit =
      assertEquals(expected, Integer.numberOfLeadingZeros(i))

    @noinline def testNoInline(i: Int, expected: Int): Unit =
      testInline(i, expected)

    @inline def test(i: Int, expected: Int): Unit = {
      testInline(i, expected)
      testNoInline(i, expected)
    }

    test(0, 32)
    test(1, 31)
    test(5, 29)
    test(-1, 0)

    test(454050, 13)
    test(5623, 19)
    test(31, 27)
    test(9903091, 8)
    test(1692, 21)
    test(2, 30)
    test(1109670822, 1)
    test(3453, 20)
    test(38, 26)
    test(5, 29)
    test(4966, 19)
    test(1, 31)
    test(11552, 18)
    test(3, 30)
    test(7973478, 9)
    test(29285, 17)
    test(286646607, 3)
    test(226, 24)
    test(3934789, 10)
    test(661375551, 2)
    test(96414310, 5)
    test(983679, 12)
    test(1277, 21)
    test(168316711, 4)
    test(23440, 17)
    test(4609, 19)
    test(1757, 21)
    test(307973987, 3)
    test(3260121, 10)
    test(52257, 16)
    test(402, 23)
    test(1046482241, 2)
    test(12785, 18)
    test(4, 29)
    test(422, 23)
    test(19, 27)
    test(2991, 20)
    test(2036, 21)
    test(13, 28)
    test(38, 26)
    test(101, 25)
    test(85138295, 5)
    test(13225, 18)
    test(7768, 19)
    test(7630814, 9)
    test(226, 24)
    test(177422164, 4)
    test(32, 26)
    test(407, 23)
    test(860691653, 2)
    test(34, 26)
    test(25786, 17)
    test(55778847, 6)
    test(1439057, 11)
    test(671565896, 2)
    test(603, 22)
    test(741, 22)
    test(1459118, 11)
    test(78, 25)
    test(177510, 14)
    test(926370, 12)
    test(685733055, 2)
    test(217, 24)
    test(523651, 13)
    test(3, 30)
    test(8, 28)
    test(-1253180290, 0)
    test(134956860, 4)
    test(1255403863, 1)
    test(30, 27)
    test(351465, 13)
    test(1281021, 11)
    test(12073178, 8)
    test(1034035, 12)
    test(978759, 12)
    test(63, 26)
    test(49700708, 6)
    test(31, 27)
    test(20, 27)
    test(6686565, 9)
    test(31, 27)
    test(621614, 12)
    test(81224072, 5)
    test(2687, 20)
    test(19236, 17)
    test(129429186, 5)
    test(4, 29)
    test(978, 22)
    test(24137647, 7)
    test(150728, 14)
    test(3825, 20)
    test(34, 26)
    test(100111471, 5)
    test(92028, 15)
    test(-1198731278, 0)
    test(250395, 14)
    test(2753, 20)
    test(491505965, 3)
    test(30716590, 7)
    test(213241, 14)
  }

  @Test def numberOfTrailingZeros(): Unit = {
    assertEquals(32, Integer.numberOfTrailingZeros(0))
    assertEquals(0, Integer.numberOfTrailingZeros(1))
    assertEquals(2, Integer.numberOfTrailingZeros(12))
    assertEquals(3, Integer.numberOfTrailingZeros(1024 + 64 + 8))
    assertEquals(0, Integer.numberOfTrailingZeros(-1))
    assertEquals(31, Integer.numberOfTrailingZeros(Int.MinValue))
    assertEquals(0, Integer.numberOfTrailingZeros(Int.MaxValue))

    assertEquals(29, Integer.numberOfTrailingZeros(1610612736))
    assertEquals(29, Integer.numberOfTrailingZeros(-1610612736))
    assertEquals(26, Integer.numberOfTrailingZeros(1409286144))
    assertEquals(31, Integer.numberOfTrailingZeros(-2147483648))
    assertEquals(27, Integer.numberOfTrailingZeros(1207959552))
    assertEquals(13, Integer.numberOfTrailingZeros(-536928256))
    assertEquals(2, Integer.numberOfTrailingZeros(-1096256668))
    assertEquals(20, Integer.numberOfTrailingZeros(-923795456))
    assertEquals(29, Integer.numberOfTrailingZeros(-1610612736))
    assertEquals(19, Integer.numberOfTrailingZeros(-2077753344))
    assertEquals(23, Integer.numberOfTrailingZeros(-1602224128))
    assertEquals(4, Integer.numberOfTrailingZeros(-136456432))
    assertEquals(10, Integer.numberOfTrailingZeros(1711399936))
    assertEquals(21, Integer.numberOfTrailingZeros(467664896))
    assertEquals(22, Integer.numberOfTrailingZeros(1270874112))
    assertEquals(16, Integer.numberOfTrailingZeros(-1163198464))
    assertEquals(8, Integer.numberOfTrailingZeros(-1830050048))
    assertEquals(4, Integer.numberOfTrailingZeros(929614128))
    assertEquals(24, Integer.numberOfTrailingZeros(-83886080))
    assertEquals(29, Integer.numberOfTrailingZeros(-1610612736))
    assertEquals(31, Integer.numberOfTrailingZeros(-2147483648))
    assertEquals(15, Integer.numberOfTrailingZeros(-1549762560))
    assertEquals(7, Integer.numberOfTrailingZeros(1810143104))
    assertEquals(21, Integer.numberOfTrailingZeros(1398800384))
    assertEquals(31, Integer.numberOfTrailingZeros(-2147483648))
    assertEquals(16, Integer.numberOfTrailingZeros(-81723392))
    assertEquals(2, Integer.numberOfTrailingZeros(1061913644))
    assertEquals(7, Integer.numberOfTrailingZeros(-1964724608))
    assertEquals(20, Integer.numberOfTrailingZeros(1995440128))
    assertEquals(26, Integer.numberOfTrailingZeros(-335544320))
    assertEquals(0, Integer.numberOfTrailingZeros(-1193917187))
    assertEquals(18, Integer.numberOfTrailingZeros(1714683904))
    assertEquals(31, Integer.numberOfTrailingZeros(-2147483648))
    assertEquals(14, Integer.numberOfTrailingZeros(1003175936))
    assertEquals(31, Integer.numberOfTrailingZeros(-2147483648))
    assertEquals(29, Integer.numberOfTrailingZeros(1610612736))
    assertEquals(26, Integer.numberOfTrailingZeros(-1946157056))
    assertEquals(17, Integer.numberOfTrailingZeros(-1907228672))
    assertEquals(28, Integer.numberOfTrailingZeros(-268435456))
    assertEquals(9, Integer.numberOfTrailingZeros(-1535911424))
    assertEquals(7, Integer.numberOfTrailingZeros(523888512))
    assertEquals(5, Integer.numberOfTrailingZeros(358260320))
    assertEquals(23, Integer.numberOfTrailingZeros(981467136))
    assertEquals(23, Integer.numberOfTrailingZeros(1669332992))
    assertEquals(27, Integer.numberOfTrailingZeros(-1207959552))
    assertEquals(29, Integer.numberOfTrailingZeros(536870912))
    assertEquals(18, Integer.numberOfTrailingZeros(-748945408))
    assertEquals(14, Integer.numberOfTrailingZeros(1289338880))
    assertEquals(18, Integer.numberOfTrailingZeros(-82051072))
    assertEquals(31, Integer.numberOfTrailingZeros(-2147483648))
    assertEquals(12, Integer.numberOfTrailingZeros(-675172352))
    assertEquals(15, Integer.numberOfTrailingZeros(77234176))
    assertEquals(0, Integer.numberOfTrailingZeros(-276596511))
    assertEquals(5, Integer.numberOfTrailingZeros(222966496))
    assertEquals(6, Integer.numberOfTrailingZeros(-1981260992))
    assertEquals(14, Integer.numberOfTrailingZeros(1689927680))
    assertEquals(31, Integer.numberOfTrailingZeros(-2147483648))
    assertEquals(8, Integer.numberOfTrailingZeros(-1499141888))
    assertEquals(3, Integer.numberOfTrailingZeros(1441572168))
    assertEquals(30, Integer.numberOfTrailingZeros(-1073741824))
    assertEquals(10, Integer.numberOfTrailingZeros(-1175759872))
    assertEquals(19, Integer.numberOfTrailingZeros(781713408))
    assertEquals(30, Integer.numberOfTrailingZeros(1073741824))
    assertEquals(7, Integer.numberOfTrailingZeros(117264512))
    assertEquals(1, Integer.numberOfTrailingZeros(-808865046))
    assertEquals(23, Integer.numberOfTrailingZeros(-394264576))
    assertEquals(26, Integer.numberOfTrailingZeros(-1946157056))
    assertEquals(23, Integer.numberOfTrailingZeros(-1619001344))
    assertEquals(16, Integer.numberOfTrailingZeros(-1236860928))
    assertEquals(5, Integer.numberOfTrailingZeros(-134639968))
    assertEquals(26, Integer.numberOfTrailingZeros(-1946157056))
    assertEquals(28, Integer.numberOfTrailingZeros(-1342177280))
    assertEquals(29, Integer.numberOfTrailingZeros(1610612736))
    assertEquals(28, Integer.numberOfTrailingZeros(1879048192))
    assertEquals(30, Integer.numberOfTrailingZeros(-1073741824))
    assertEquals(21, Integer.numberOfTrailingZeros(1994391552))
    assertEquals(24, Integer.numberOfTrailingZeros(-1862270976))
    assertEquals(18, Integer.numberOfTrailingZeros(-2055471104))
    assertEquals(20, Integer.numberOfTrailingZeros(349175808))
    assertEquals(16, Integer.numberOfTrailingZeros(-1247477760))
    assertEquals(12, Integer.numberOfTrailingZeros(-1874415616))
    assertEquals(16, Integer.numberOfTrailingZeros(1336606720))
    assertEquals(0, Integer.numberOfTrailingZeros(565581745))
    assertEquals(31, Integer.numberOfTrailingZeros(-2147483648))
    assertEquals(23, Integer.numberOfTrailingZeros(562036736))
    assertEquals(3, Integer.numberOfTrailingZeros(1372687704))
    assertEquals(15, Integer.numberOfTrailingZeros(1147437056))
    assertEquals(10, Integer.numberOfTrailingZeros(941499392))
    assertEquals(9, Integer.numberOfTrailingZeros(938859008))
    assertEquals(2, Integer.numberOfTrailingZeros(1987222364))
    assertEquals(24, Integer.numberOfTrailingZeros(251658240))
    assertEquals(3, Integer.numberOfTrailingZeros(1598124120))
    assertEquals(14, Integer.numberOfTrailingZeros(-1177763840))
    assertEquals(28, Integer.numberOfTrailingZeros(1879048192))
    assertEquals(27, Integer.numberOfTrailingZeros(-939524096))
    assertEquals(21, Integer.numberOfTrailingZeros(-622854144))
    assertEquals(25, Integer.numberOfTrailingZeros(-1577058304))
    assertEquals(16, Integer.numberOfTrailingZeros(1751711744))
    assertEquals(24, Integer.numberOfTrailingZeros(-1392508928))
    assertEquals(19, Integer.numberOfTrailingZeros(303562752))
  }

  @Test def toBinaryString(): Unit = {
    assertEquals("11111111111111111111111111111111", Integer.toBinaryString(-1))
    assertEquals("11111111111111111101100011101111", Integer.toBinaryString(-10001))
    assertEquals("10000000000000000000000000000000", Integer.toBinaryString(MinValue))
    assertEquals("1111111111111111111111111111111", Integer.toBinaryString(MaxValue))
  }

  @Test def toHexString(): Unit = {
    assertEquals("ffffffff", Integer.toHexString(-1))
    assertEquals("ffffd8ef", Integer.toHexString(-10001))
    assertEquals("80000000", Integer.toHexString(MinValue))
    assertEquals("8007613e", Integer.toHexString(-2147000002))
    assertEquals("7fffffff", Integer.toHexString(MaxValue))
  }

  @Test def toOctalString(): Unit = {
    assertEquals("37777777777", Integer.toOctalString(-1))
    assertEquals("37777754357", Integer.toOctalString(-10001))
    assertEquals("20000000000", Integer.toOctalString(MinValue))
    assertEquals("17777777777", Integer.toOctalString(MaxValue))
  }

  @Test def compareTo(): Unit = {
    def compare(x: Int, y: Int): Int =
      new Integer(x).compareTo(new Integer(y))

    assertTrue(compare(0, 5) < 0)
    assertTrue(compare(10, 9) > 0)
    assertTrue(compare(-2, -1) < 0)
    assertEquals(0, compare(3, 3))
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0, 5) < 0)
    assertTrue(compare(10, 9) > 0)
    assertTrue(compare(-2, -1) < 0)
    assertEquals(0, compare(3, 3))
  }

  @Test def should_parse_strings(): Unit = {
    def test(s: String, v: Int, radix: Int = 10): Unit = {
      assertEquals(v, Integer.parseInt(s, radix))
      assertEquals(v, Integer.valueOf(s, radix).intValue())
      if (radix == 10)
        assertEquals(v, new Integer(s).intValue())
    }

    test("0", 0)
    test("5", 5)
    test("127", 127)
    test("-100", -100)
    test("30000", 30000)
    test("-90000", -90000)
    test("Kona", 411787, 27)
    if (!executingInJVMOnJDK6)
      test("+42", 42)
    test("-0", 0)
    test("-FF", -255, 16)
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    def test(s: String, radix: Int = 10): Unit =
      expectThrows(classOf[NumberFormatException], Integer.parseInt(s, radix))

    test("abc")
    test("5a")
    test("2147483648")
    test("99", 8)
    test("-")
    test("")
  }

  @Test def should_parse_strings_in_base_16(): Unit = {
    def test(s: String, v: Int): Unit = {
      assertEquals(v, Integer.parseInt(s, 16))
      assertEquals(v, Integer.valueOf(s, 16).intValue())
    }

    test("0", 0x0)
    test("5", 0x5)
    test("ff", 0xff)
    test("-24", -0x24)
    test("30000", 0x30000)
    test("-90000", -0x90000)
  }

  @Test def highestOneBit(): Unit = {
    /* Spec ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
     */
    assertEquals(0, Integer.highestOneBit(0))
    assertEquals(Integer.MIN_VALUE, Integer.highestOneBit(-1))
    assertEquals(Integer.MIN_VALUE, Integer.highestOneBit(-256))
    assertEquals(1, Integer.highestOneBit(1))
    assertEquals(0x80, Integer.highestOneBit(0x88))
    assertEquals(0x40000000, Integer.highestOneBit(Int.MaxValue))
    assertEquals(Int.MinValue, Integer.highestOneBit(Int.MinValue))
  }

  @Test def lowestOneBit(): Unit = {
    assertEquals(0, Integer.lowestOneBit(0))
    assertEquals(1, Integer.lowestOneBit(-1))
    assertEquals(256, Integer.lowestOneBit(-256))
    assertEquals(4, Integer.lowestOneBit(12))
    assertEquals(0x8, Integer.lowestOneBit(0x88))
    assertEquals(1, Integer.lowestOneBit(Int.MaxValue))
    assertEquals(Int.MinValue, Integer.lowestOneBit(Int.MinValue))
  }

  @Test def toString_without_radix(): Unit = {
    /* Spec ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
     */
    assertEquals("12345", new Integer(12345).toString)
    assertEquals("-12345", new Integer("-12345").toString)
    assertEquals("-80765", Integer.toString(-80765))
    assertEquals("2147483647", Integer.toString(Int.MaxValue))
    assertEquals("-2147483647", Integer.toString(-Int.MaxValue))
    assertEquals("-2147483648", Integer.toString(Int.MinValue))
    assertEquals("0", Integer.toString(0))
  }

  @Test def toString_with_radix(): Unit = {
    /* Spec ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
     */
    assertEquals("17777777777", Integer.toString(2147483647, 8))
    assertEquals("7fffffff", Integer.toString(2147483647, 16))
    assertEquals("1111111111111111111111111111111", Integer.toString(2147483647, 2))
    assertEquals("2147483647", Integer.toString(2147483647, 10))
    assertEquals("-17777777777", Integer.toString(-2147483647, 8))
    assertEquals("-7fffffff", Integer.toString(-2147483647, 16))
    assertEquals("-1111111111111111111111111111111", Integer.toString(-2147483647, 2))
    assertEquals("-2147483647", Integer.toString(-2147483647, 10))
    assertEquals("-20000000000", Integer.toString(-2147483648, 8))
    assertEquals("-80000000", Integer.toString(-2147483648, 16))
    assertEquals("-10000000000000000000000000000000", Integer.toString(-2147483648, 2))
    assertEquals("-2147483648", Integer.toString(-2147483648, 10))
  }
}
