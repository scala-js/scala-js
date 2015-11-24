/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import java.lang.{Long => JLong}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

/** Tests the implementation of the java standard library Long
 *  requires jsinterop/LongTest to work to make sense
 */
class LongTest {

  @Test def reverseBytes(): Unit = {
    assertEquals(0x14ff01d49c68abf5L, JLong.reverseBytes(0xf5ab689cd401ff14L))
  }

  @Test def rotateLeft(): Unit = {
    assertEquals(0xf5ab689cd401ff14L, JLong.rotateLeft(0xf5ab689cd401ff14L, 0))
    assertEquals(0xeb56d139a803fe29L, JLong.rotateLeft(0xf5ab689cd401ff14L, 1))
    assertEquals(0xab689cd401ff14f5L, JLong.rotateLeft(0xf5ab689cd401ff14L, 8))
    assertEquals(0x6d139a803fe29eb5L, JLong.rotateLeft(0xf5ab689cd401ff14L, 13))
    assertEquals(0xf5ab689cd401ff14L, JLong.rotateLeft(0xf5ab689cd401ff14L, 64))
    assertEquals(0xeb56d139a803fe29L, JLong.rotateLeft(0xf5ab689cd401ff14L, 65))
    assertEquals(0x689cd401ff14f5abL, JLong.rotateLeft(0xf5ab689cd401ff14L, 80))
    assertEquals(0x7ad5b44e6a00ff8aL, JLong.rotateLeft(0xf5ab689cd401ff14L, -1))
    assertEquals(0xab689cd401ff14f5L, JLong.rotateLeft(0xf5ab689cd401ff14L, -56))
    assertEquals(0x53d6ada2735007fcL, JLong.rotateLeft(0xf5ab689cd401ff14L, -70))
  }

  @Test def rotateRight(): Unit = {
    assertEquals(0xf5ab689cd401ff14L, JLong.rotateRight(0xf5ab689cd401ff14L, 0))
    assertEquals(0x7ad5b44e6a00ff8aL, JLong.rotateRight(0xf5ab689cd401ff14L, 1))
    assertEquals(0x14f5ab689cd401ffL, JLong.rotateRight(0xf5ab689cd401ff14L, 8))
    assertEquals(0xf8a7ad5b44e6a00fL, JLong.rotateRight(0xf5ab689cd401ff14L, 13))
    assertEquals(0xf5ab689cd401ff14L, JLong.rotateRight(0xf5ab689cd401ff14L, 64))
    assertEquals(0x7ad5b44e6a00ff8aL, JLong.rotateRight(0xf5ab689cd401ff14L, 65))
    assertEquals(0xff14f5ab689cd401L, JLong.rotateRight(0xf5ab689cd401ff14L, 80))
    assertEquals(0xeb56d139a803fe29L, JLong.rotateRight(0xf5ab689cd401ff14L, -1))
    assertEquals(0x14f5ab689cd401ffL, JLong.rotateRight(0xf5ab689cd401ff14L, -56))
    assertEquals(0x6ada2735007fc53dL, JLong.rotateRight(0xf5ab689cd401ff14L, -70))
  }

  @Test def bitCount(): Unit = {
    assertEquals(0, JLong.bitCount(0L))
    assertEquals(26, JLong.bitCount(35763829229342837L))
    assertEquals(32, JLong.bitCount(-350003829229342837L))
  }

  @Test def compareTo(): Unit = {
    def compare(x: Long, y: Long): Int =
      new JLong(x).compareTo(new JLong(y))

    assertTrue(compare(0L, 5L) < 0)
    assertTrue(compare(10L, 9L) > 0)
    assertTrue(compare(-2L, -1L) < 0)
    assertEquals(0, compare(3L, 3L))
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0L, 5L) < 0)
    assertTrue(compare(10L, 9L) > 0)
    assertTrue(compare(-2L, -1L) < 0)
    assertEquals(0, compare(3L, 3L))
  }

  @Test def should_parse_strings(): Unit = {
    def test(s: String, v: Long): Unit = {
      assertEquals(v, JLong.parseLong(s))
      assertEquals(v, JLong.valueOf(s).longValue())
      assertEquals(v, new JLong(s).longValue())
    }

    test("0", 0L)
    test("5", 5L)
    test("127", 127L)
    test("-100", -100L)
    test("30000", 30000L)
    test("-90000", -90000L)
    test("4", 4L)
    test("-4", -4L)
    test("4000000000", 4000000000L)
    test("-18014398509482040", -18014398509482040L)
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    def test(s: String): Unit =
      expectThrows(classOf[NumberFormatException], JLong.parseLong(s))

    test("abc")
    test("asdf")
    test("")
  }

  @Test def should_parse_strings_in_base_16(): Unit = {
    def test(s: String, v: Long): Unit = {
      assertEquals(v, JLong.parseLong(s, 16))
      assertEquals(v, JLong.valueOf(s, 16).longValue())
    }

    test("0", 0x0L)
    test("5", 0x5L)
    test("ff", 0xffL)
    test("-24", -0x24L)
    test("30000", 0x30000L)
    test("-90000", -0x90000L)
    test("bfc94973", 3217639795L)
    test("bfc949733", 51482236723L)
  }

  @Test def should_parse_strings_in_bases_2_to_36(): Unit = {
    def test(radix: Int, s: String, v: Long): Unit = {
      assertEquals(v, JLong.parseLong(s, radix))
      assertEquals(v, JLong.valueOf(s, radix).longValue())
    }

    def genTestValue(i: Int): Long = {
      val result = Long.MaxValue / (1L << i)
      if (i > 63) -result
      else result
    }

    for {
      radix <- 2 to 36
      i <- 0 until 128
    } {
      val n = genTestValue(i)
      test(radix, JLong.toString(n, radix), n)
    }
  }

  @Test def should_reject_parsing_strings_when_base_less_than_2_or_base_larger_than_36(): Unit = {
    def test(s: String, radix: Int): Unit = {
      expectThrows(classOf[NumberFormatException], JLong.parseLong(s, radix))
      expectThrows(classOf[NumberFormatException], JLong.valueOf(s, radix).longValue())
    }

    List[Int](-10, -5, 0, 1, 37, 38, 50, 100).foreach(test("5", _))
  }

  @Test def toString_without_radix(): Unit = {
    assertEquals("2147483647", Int.MaxValue.toLong.toString)
    assertEquals("-50", (-50L).toString)
    assertEquals("-1000000000", (-1000000000L).toString)
    assertEquals("2147483648", (Int.MaxValue.toLong+1L).toString)
    assertEquals("-2147483648", Int.MinValue.toLong.toString)

    /* Ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/JLongTest.java
     */
    assertEquals("89000000005", new JLong(89000000005L).toString)
    assertEquals("-9223372036854775808", new JLong(JLong.MIN_VALUE).toString)
    assertEquals("9223372036854775807", new JLong(JLong.MAX_VALUE).toString)
    assertEquals("-80765", JLong.toString(-80765L))
    assertEquals("80765", JLong.toString(80765L))
    assertEquals("-2147483648", JLong.toString(Integer.MIN_VALUE.toLong))
    assertEquals("2147483647", JLong.toString(Integer.MAX_VALUE.toLong))
    assertEquals("-89000000005", JLong.toString(-89000000005L))
    assertEquals("89000000005", JLong.toString(89000000005L))
    assertEquals("-9223372036854775808", JLong.toString(JLong.MIN_VALUE))
    assertEquals("9223372036854775807", JLong.toString(JLong.MAX_VALUE))
  }

  @Test def toString_with_radix(): Unit = {
    /* Ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/JLongTest.java
     */
    assertEquals("100000000", JLong.toString(100000000L, 10))
    assertEquals("77777777777", JLong.toString(8589934591L, 8))
    assertEquals("fffffffff", JLong.toString(68719476735L, 16))
    assertEquals("1111111111111111111111111111111111111111111", JLong.toString(8796093022207L, 2))
    assertEquals("-9223372036854775808", JLong.toString(0x8000000000000000L, 10))
    assertEquals("9223372036854775807", JLong.toString(0x7fffffffffffffffL, 10))
    assertEquals("-8000000000000000", JLong.toString(0x8000000000000000L, 16))
    assertEquals("7fffffffffffffff", JLong.toString(0x7fffffffffffffffL, 16))
  }

  @Test def highestOneBit(): Unit = {
    assertEquals(0L, JLong.highestOneBit(0L))
    assertEquals(Long.MinValue, JLong.highestOneBit(-1L))
    assertEquals(Long.MinValue, JLong.highestOneBit(-256L))
    assertEquals(1L, JLong.highestOneBit(1L))
    assertEquals(0x80L, JLong.highestOneBit(0x88L))
    assertEquals(0x4000000000000000L, JLong.highestOneBit(Long.MaxValue))
    assertEquals(Long.MinValue, JLong.highestOneBit(Long.MinValue))
    assertEquals(0x20000000000L, JLong.highestOneBit(0x32100012300L))
  }

  @Test def lowestOneBit(): Unit = {
    assertEquals(0L, JLong.lowestOneBit(0L))
    assertEquals(1L, JLong.lowestOneBit(-1L))
    assertEquals(256L, JLong.lowestOneBit(-256L))
    assertEquals(4L, JLong.lowestOneBit(12L))
    assertEquals(0x8L, JLong.lowestOneBit(0x88L))
    assertEquals(1L, JLong.lowestOneBit(Long.MaxValue))
    assertEquals(Long.MinValue, JLong.lowestOneBit(Long.MinValue))
    assertEquals(0x100L, JLong.lowestOneBit(0x32100012300L))
  }

  @Test def toBinaryString(): Unit = {
    assertEquals("0", JLong.toBinaryString(0L))
    assertEquals("1111111111111111111111111111111111111111111111111111111111111111",
        JLong.toBinaryString(-1L))
    assertEquals("11011001100101111010101100110", JLong.toBinaryString(456324454L))
    assertEquals("1111111111111111111111111111111111100100110011010000101010011010",
        JLong.toBinaryString(-456324454L))
    assertEquals("10110011101001110011110011111111111101001111101",
        JLong.toBinaryString(98765432158845L))
    assertEquals("1111111111111111110100101110100101011001100101101001000111001100",
        JLong.toBinaryString(-49575304457780L))
    assertEquals("1000000000000000000000000000000000000000000000000000000000000000",
        JLong.toBinaryString(Long.MinValue))
    assertEquals("111111111111111111111111111111111111111111111111111111111111111",
        JLong.toBinaryString(Long.MaxValue))
  }

  @Test def toHexString(): Unit = {
    assertEquals("0", JLong.toHexString(0L))
    assertEquals("ffffffffffffffff", JLong.toHexString(-1L))
    assertEquals("1b32f566", JLong.toHexString(456324454L))
    assertEquals("ffffffffe4cd0a9a", JLong.toHexString(-456324454L))
    assertEquals("59d39e7ffa7d", JLong.toHexString(98765432158845L))
    assertEquals("ffffd2e9599691cc", JLong.toHexString(-49575304457780L))
    assertEquals("8000000000000000", JLong.toHexString(Long.MinValue))
    assertEquals("7fffffffffffffff", JLong.toHexString(Long.MaxValue))
  }

  @Test def toOctalString(): Unit = {
    assertEquals("0", JLong.toOctalString(0L))
    assertEquals("1777777777777777777777", JLong.toOctalString(-1L))
    assertEquals("3314572546", JLong.toOctalString(456324454L))
    assertEquals("1777777777774463205232", JLong.toOctalString(-456324454L))
    assertEquals("2635163637775175", JLong.toOctalString(98765432158845L))
    assertEquals("1777776456453145510714", JLong.toOctalString(-49575304457780L))
    assertEquals("1000000000000000000000", JLong.toOctalString(Long.MinValue))
    assertEquals("777777777777777777777", JLong.toOctalString(Long.MaxValue))
  }

  @Test def numberOfLeadingZeros(): Unit = {
    assertEquals(0, JLong.numberOfLeadingZeros(0x9876543210abcdefL))
    assertEquals(6, JLong.numberOfLeadingZeros(0x272d130652a160fL))
    assertEquals(61, JLong.numberOfLeadingZeros(0x4L))
    assertEquals(13, JLong.numberOfLeadingZeros(0x645d32476a42aL))
    assertEquals(31, JLong.numberOfLeadingZeros(0x19b8ed092L))
    assertEquals(8, JLong.numberOfLeadingZeros(0xdc2d80fe481e77L))
    assertEquals(2, JLong.numberOfLeadingZeros(0x3af189a5d0dfae26L))
    assertEquals(23, JLong.numberOfLeadingZeros(0x151dc269439L))
    assertEquals(9, JLong.numberOfLeadingZeros(0x60e7be653be060L))
    assertEquals(52, JLong.numberOfLeadingZeros(0xe39L))
    assertEquals(61, JLong.numberOfLeadingZeros(0x6L))
    assertEquals(37, JLong.numberOfLeadingZeros(0x7ea26e0L))
    assertEquals(12, JLong.numberOfLeadingZeros(0x882fb98ec313bL))
    assertEquals(11, JLong.numberOfLeadingZeros(0x136efd8f1beebaL))
    assertEquals(64, JLong.numberOfLeadingZeros(0x0L))
    assertEquals(58, JLong.numberOfLeadingZeros(0x3aL))
    assertEquals(4, JLong.numberOfLeadingZeros(0xc3c7ecf1e25f4b4L))
    assertEquals(57, JLong.numberOfLeadingZeros(0x48L))
    assertEquals(21, JLong.numberOfLeadingZeros(0x63c51c723a8L))
    assertEquals(50, JLong.numberOfLeadingZeros(0x2742L))
    assertEquals(39, JLong.numberOfLeadingZeros(0x10630c7L))
  }

  @Test def numberOfTrailingZeros(): Unit = {
    assertEquals(52, JLong.numberOfTrailingZeros(0xff10000000000000L))
    assertEquals(53, JLong.numberOfTrailingZeros(0xff20000000000000L))
    assertEquals(54, JLong.numberOfTrailingZeros(0xff40000000000000L))
    assertEquals(55, JLong.numberOfTrailingZeros(0xff80000000000000L))

    assertEquals(40, JLong.numberOfTrailingZeros(0x0000010000000000L))
    assertEquals(41, JLong.numberOfTrailingZeros(0x0000020000000000L))
    assertEquals(42, JLong.numberOfTrailingZeros(0x0000040000000000L))
    assertEquals(43, JLong.numberOfTrailingZeros(0x0000080000000000L))

    assertEquals(16, JLong.numberOfTrailingZeros(0x0000000000010000L))
    assertEquals(17, JLong.numberOfTrailingZeros(0x0000000000020000L))
    assertEquals(18, JLong.numberOfTrailingZeros(0x0000000000040000L))
    assertEquals(19, JLong.numberOfTrailingZeros(0x0000000000080000L))
  }

  @Test def signum(): Unit = {
    //check a few ints
    assertEquals(-1, JLong.signum(-11))
    assertEquals(-1, JLong.signum(-1))
    assertEquals(0, JLong.signum(0))
    assertEquals(1, JLong.signum(1))
    assertEquals(1, JLong.signum(11))

    //check a few longs
    assertEquals(-1, JLong.signum(Long.MinValue))
    assertEquals(-1, JLong.signum(-98765432158845L))
    assertEquals(-1, JLong.signum(-49575304457780L))
    assertEquals(-1, JLong.signum(-11L))
    assertEquals(-1, JLong.signum(-1L))
    assertEquals(0, JLong.signum(0L))
    assertEquals(1, JLong.signum(1L))
    assertEquals(1, JLong.signum(11L))
    assertEquals(1, JLong.signum(49575304457780L))
    assertEquals(1, JLong.signum(98765432158845L))
    assertEquals(1, JLong.signum(Long.MaxValue))
  }
}
