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

class IntegerTestOnJDK8 {

  // Explicitly define these as `var`'s to avoid any compile-time constant folding
  val MaxValue: Int = Int.MaxValue
  val MinValue: Int = Int.MinValue

  @Test def should_parse_uInt_strings(): Unit = {
    def test(s: String, v: Int, radix: Int = 10): Unit = {
      assertEquals(v, Integer.parseUnsignedInt(s, radix))
    }

    test("0", 0)
    test("5", 5)
    test("127", 127)
    test("+100", 100)
    test("30000", 30000)
    test("Kona", 411787, 27)
    test("+42", 42)
    test("+0", 0)
    test("FF", 255, 16)
    test("4000000000", 0xEE6B2800)
    test("4294967295", 0xFFFFFFFF)
  }

  @Test def should_reject_invalid_uInt_strings_when_parsing(): Unit = {
    def test(s: String, radix: Int = 10): Unit =
      expectThrows(classOf[NumberFormatException], Integer.parseUnsignedInt(s, radix))

    test("abc")
    test("5a")
    test("99", 8)
    test("4294967296")
    test("-30000")
    test("+")
    test("-")
    test("-0")
    test("0.0")
  }

  @Test def should_parse_strings_in_base_16(): Unit = {
    def test(s: String, v: Int): Unit = {
      assertEquals(v, Integer.parseUnsignedInt(s, 16))
    }

    test("0", 0x0)
    test("5", 0x5)
    test("ff", 0xff)
    test("24", 0x24)
    test("30000", 0x30000)
    test("90000", 0x90000)
    test("EE6B2800", 0xEE6B2800)
    test("FFFFFFFF", 0xFFFFFFFF)
  }

  @Test def should_provide_compareUnsigned(): Unit = {
    def compare(x: Int, y: Int): Int =
      Integer.compareUnsigned(x, y)

    assertTrue(compare(0, 5) < 0)
    assertTrue(compare(10, 9) > 0)
    assertEquals(0, compare(3, 3))
    assertEquals(0, compare(0xFFFFFFFF, 0xFFFFFFFF))
    assertTrue(compare(0xEE6B2800, 0xFFFFFFFF) < 0)
    assertTrue(compare(0xFFFFFFFF, 0xEE6B2800) > 0)
    assertTrue(compare(0xEE6B2800, 3) > 0)
    assertTrue(compare(3, 0xEE6B2800) < 0)
  }

  @Test def should_provide_toUnsignedLong(): Unit = {
    def test(x: Int, y: Long): Unit =
      assertEquals(y, Integer.toUnsignedLong(x))

    test(0, 0L)
    test(5, 5L)
    test(43345, 43345L)
    test(0xEE6B2800, 0xEE6B2800L)
    test(0xFFFFFFFF, 0xFFFFFFFFL)
  }

  @Test def should_provide_divideUnsigned(): Unit = {
    def test(dividend: Int, divisor: Int, result: Int): Unit =
      assertEquals(result, Integer.divideUnsigned(dividend, divisor))

    test(1, 1, 1)
    test(4, 2, 2)
    test(3, 2, 1)
    test(0xFFFFFFFF, 7, 613566756)
    test(0xFFFFFFFF, 0xEE6B2800, 1)
    test(0xEE6B2800, 2, 2000000000)
  }

  @Test def should_provide_remainderUnsigned(): Unit = {
    def test(dividend: Int, divisor: Int, result: Int): Unit =
      assertEquals(result, Integer.remainderUnsigned(dividend, divisor))

    test(1, 1, 0)
    test(4, 2, 0)
    test(3, 2, 1)
    test(0xFFFFFFFF, 7, 3)
    test(0xFFFFFFFF, 0xEE6B2800, 294967295)
    test(0xEE6B2800, 2, 0)
  }

  @Test def should_provide_toUnsignedString_without_radix(): Unit = {
    assertEquals("0", Integer.toUnsignedString(0))
    assertEquals("12345", Integer.toUnsignedString(12345))
    assertEquals("242134", Integer.toUnsignedString(242134))
    assertEquals("2147483647", Integer.toUnsignedString(Integer.MAX_VALUE))
    assertEquals("4294967295", Integer.toUnsignedString(0xFFFFFFFF))
    assertEquals("4000000000", Integer.toUnsignedString(0xEE6B2800))
  }

  @Test def should_provide_toUnsignedString_with_radix(): Unit = {
    assertEquals("17777777777", Integer.toUnsignedString(2147483647, 8))
    assertEquals("7fffffff", Integer.toUnsignedString(2147483647, 16))
    assertEquals("1111111111111111111111111111111",
        Integer.toUnsignedString(2147483647, 2))
    assertEquals("2147483647", Integer.toUnsignedString(2147483647, 10))
    assertEquals("ffffffff", Integer.toUnsignedString(0xFFFFFFFF, 16))
    assertEquals("4294967295", Integer.toUnsignedString(0xFFFFFFFF, 10))
    assertEquals("ee6b2800", Integer.toUnsignedString(0xEE6B2800, 16))
    assertEquals("4000000000", Integer.toUnsignedString(0xEE6B2800, 10))
  }

  @Test def should_provide_hashCode_as_a_static_function(): Unit = {
    for (i <- -256 to 256)
      assertEquals(i.hashCode(), Integer.hashCode(i))
    assertEquals(Int.MaxValue.hashCode, Integer.hashCode(Int.MaxValue))
    assertEquals(Int.MinValue.hashCode, Integer.hashCode(Int.MinValue))
  }

  @Test def should_provide_sum_as_a_static_function(): Unit = {
    // 20 ramdomly generated cases
    assertEquals(-486527733, Integer.sum(1456847510, -1943375243))
    assertEquals(-1777185932, Integer.sum(-1675020769, -102165163))
    assertEquals(-382453283, Integer.sum(-492132773, 109679490))
    assertEquals(-2145997235, Integer.sum(-894160208, -1251837027))
    assertEquals(-166873150, Integer.sum(-1194861016, 1027987866))
    assertEquals(167480017, Integer.sum(-1898001389, 2065481406))
    assertEquals(673489472, Integer.sum(-311003114, 984492586))
    assertEquals(-1388364075, Integer.sum(-295074587, -1093289488))
    assertEquals(2111094009, Integer.sum(2022415614, 88678395))
    assertEquals(-1328656780, Integer.sum(-245624037, -1083032743))
    assertEquals(636897760, Integer.sum(-1075180485, 1712078245))
    assertEquals(820269321, Integer.sum(-1177939094, 1998208415))
    assertEquals(-1050613003, Integer.sum(-1682860108, 632247105))
    assertEquals(529249703, Integer.sum(1738870504, -1209620801))
    assertEquals(-1577064582, Integer.sum(1763433497, 954469217))
    assertEquals(1134712592, Integer.sum(1576449779, -441737187))
    assertEquals(1853525167, Integer.sum(2067118443, -213593276))
    assertEquals(-1850714324, Integer.sum(-1087866031, -762848293))
    assertEquals(-1545149944, Integer.sum(2107199426, 642617926))
    assertEquals(1312700933, Integer.sum(-928260456, -2054005907))
  }

  @Test def should_provide_max_as_a_static_function(): Unit = {
    // 20 ramdomly generated cases
    assertEquals(-270277483, Integer.max(-1790671798, -270277483))
    assertEquals(1571368144, Integer.max(1571368144, -695891091))
    assertEquals(-488353138, Integer.max(-488353138, -1038365399))
    assertEquals(-1299154858, Integer.max(-1299154858, -1746941781))
    assertEquals(-415165707, Integer.max(-1330811400, -415165707))
    assertEquals(-222101245, Integer.max(-222101245, -1612799352))
    assertEquals(6223768, Integer.max(6223768, -251871910))
    assertEquals(289107587, Integer.max(-1807128180, 289107587))
    assertEquals(1419004964, Integer.max(1419004964, 1391551452))
    assertEquals(1407516948, Integer.max(770531115, 1407516948))
    assertEquals(127943959, Integer.max(-1353241025, 127943959))
    assertEquals(1079220095, Integer.max(1079220095, -715415624))
    assertEquals(-451651341, Integer.max(-1758211842, -451651341))
    assertEquals(-719501136, Integer.max(-719501136, -720273331))
    assertEquals(136611495, Integer.max(136611495, 82825750))
    assertEquals(-572096554, Integer.max(-572096554, -1266456161))
    assertEquals(247666619, Integer.max(247666619, -599014758))
    assertEquals(979958171, Integer.max(979958171, -773699262))
    assertEquals(915015222, Integer.max(915015222, -895428609))
    assertEquals(1214239393, Integer.max(-2023661282, 1214239393))
  }

  @Test def should_provide_min_as_a_static_function(): Unit = {
    // 20 ramdomly generated cases
    assertEquals(-1360305565, Integer.min(1070612756, -1360305565))
    assertEquals(-1185998566, Integer.min(-1185998566, -943883433))
    assertEquals(-1767105808, Integer.min(-741471209, -1767105808))
    assertEquals(-586878137, Integer.min(-586878137, 1591634109))
    assertEquals(-1366663787, Integer.min(1017257927, -1366663787))
    assertEquals(-1769768449, Integer.min(-1769768449, -1206771005))
    assertEquals(-516274758, Integer.min(-516274758, 125028855))
    assertEquals(450306051, Integer.min(1929097253, 450306051))
    assertEquals(-2141159510, Integer.min(1232270613, -2141159510))
    assertEquals(456228627, Integer.min(1466133314, 456228627))
    assertEquals(-1549637221, Integer.min(1643492178, -1549637221))
    assertEquals(535997424, Integer.min(535997424, 1403224346))
    assertEquals(-1441182511, Integer.min(1100365123, -1441182511))
    assertEquals(-778397275, Integer.min(1752406139, -778397275))
    assertEquals(-1083524011, Integer.min(-1083524011, 906792532))
    assertEquals(-674955836, Integer.min(-674955836, 100476859))
    assertEquals(-33102740, Integer.min(702254105, -33102740))
    assertEquals(-1266058648, Integer.min(-1266058648, 1907502126))
    assertEquals(-1750379520, Integer.min(-1750379520, 1293903630))
    assertEquals(-641887949, Integer.min(-335824862, -641887949))
  }
}
