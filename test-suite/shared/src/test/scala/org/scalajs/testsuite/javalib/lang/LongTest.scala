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
    assertEquals(0x780176af73b18fc7L, JLong.reverseBytes(0xc78fb173af760178L))
    assertEquals(0x361d65c5b948d4d6L, JLong.reverseBytes(0xd6d448b9c5651d36L))
    assertEquals(0xf7a1ef821b8f4864L, JLong.reverseBytes(0x64488f1b82efa1f7L))
    assertEquals(0xddc509a6de8e44c5L, JLong.reverseBytes(0xc5448edea609c5ddL))
    assertEquals(0x288824701c0a5355L, JLong.reverseBytes(0x55530a1c70248828L))
    assertEquals(0x8bf3fed839300267L, JLong.reverseBytes(0x67023039d8fef38bL))
    assertEquals(0x44c9ade0187f253eL, JLong.reverseBytes(0x3e257f18e0adc944L))
    assertEquals(0x79a345f33753c217L, JLong.reverseBytes(0x17c25337f345a379L))
    assertEquals(0x52934dd800c7e57eL, JLong.reverseBytes(0x7ee5c700d84d9352L))
    assertEquals(0xc60434cd3ee0d783L, JLong.reverseBytes(0x83d7e03ecd3404c6L))
    assertEquals(0xe35901c3015a16a0L, JLong.reverseBytes(0xa0165a01c30159e3L))
    assertEquals(0x3b6967d70dbed537L, JLong.reverseBytes(0x37d5be0dd767693bL))
    assertEquals(0xc64eec7456b1d217L, JLong.reverseBytes(0x17d2b15674ec4ec6L))
    assertEquals(0xcd82e5c594a61174L, JLong.reverseBytes(0x7411a694c5e582cdL))
    assertEquals(0xd5f85c6a11888b2aL, JLong.reverseBytes(0x2a8b88116a5cf8d5L))
    assertEquals(0xa8c6597640b32446L, JLong.reverseBytes(0x4624b3407659c6a8L))
    assertEquals(0x61aa72555ec08d2dL, JLong.reverseBytes(0x2d8dc05e5572aa61L))
    assertEquals(0xd506860a916bb2d5L, JLong.reverseBytes(0xd5b26b910a8606d5L))
    assertEquals(0xa5b42bd78ecda8f1L, JLong.reverseBytes(0xf1a8cd8ed72bb4a5L))
    assertEquals(0xe7cb5958d4f41bfaL, JLong.reverseBytes(0xfa1bf4d45859cbe7L))
  }

  @Test def reverse(): Unit = {
    assertEquals(0x28ff802b3916d5afL, JLong.reverse(0xf5ab689cd401ff14L))
    assertEquals(0x0000000000000001L, JLong.reverse(0x8000000000000000L))
    assertEquals(0xc72a7066e1350f40L, JLong.reverse(0x02f0ac87660e54e3L))
    assertEquals(0x4455b80802f29f69L, JLong.reverse(0x96f94f40101daa22L))
    assertEquals(0xf8174394b3aaf2e6L, JLong.reverse(0x674f55cd29c2e81fL))
    assertEquals(0x9c6f15743242b439L, JLong.reverse(0x9c2d424c2ea8f639L))
    assertEquals(0x8ef01f0551b80ab0L, JLong.reverse(0x0d501d8aa0f80f71L))
    assertEquals(0xaeb088220ac8cf31L, JLong.reverse(0x8cf3135044110d75L))
    assertEquals(0x247e286155da22c9L, JLong.reverse(0x93445baa86147e24L))
    assertEquals(0xa45c9f296362d10cL, JLong.reverse(0x308b46c694f93a25L))
    assertEquals(0x16ca602244d14b85L, JLong.reverse(0xa1d28b2244065368L))
    assertEquals(0x878acb85b7fe40c5L, JLong.reverse(0xa3027feda1d351e1L))
    assertEquals(0xb8d996565b3f28d8L, JLong.reverse(0x1b14fcda6a699b1dL))
    assertEquals(0xc84e6c88148a6a27L, JLong.reverse(0xe456512811367213L))
    assertEquals(0x6bac67a3e7a7450cL, JLong.reverse(0x30a2e5e7c5e635d6L))
    assertEquals(0x2f17be6d95d551a5L, JLong.reverse(0xa58aaba9b67de8f4L))
    assertEquals(0x9ed3868d46155aaeL, JLong.reverse(0x755aa862b161cb79L))
    assertEquals(0x2de28f4e6b239166L, JLong.reverse(0x6689c4d672f147b4L))
    assertEquals(0x1528d4f46d89c9c9L, JLong.reverse(0x939391b62f2b14a8L))
    assertEquals(0xaca0de3697c073a4L, JLong.reverse(0x25ce03e96c7b0535L))
    assertEquals(0xe44211dee49bac56L, JLong.reverse(0x6a35d9277b884227L))
    assertEquals(0x438dc899d9c4de36L, JLong.reverse(0x6c7b239b9913b1c2L))
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

    test("\u19d9\u0f24\u0c6f\u1c47\ua623\u19d9\u0f24\u0c6f\u1c47\ua623",
        9497394973L)
    test("\u19d0" * 50 + "\u19d9\u0f24\u0c6f\u1c47\ua623\u19d9\u0f24\u0c6f\u1c47\ua623",
        9497394973L)
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

    test("\uff22\uff26\uff23\u19d9\u0f24\u0c6f\u1c47\ua623", 3217639795L)
    test("\uff42\uff46\uff43\u19d9\u0f24\u0c6f\u1c47\ua6233", 51482236723L)
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


  @Test def should_decode_decimal_string(): Unit = {
    assertEquals(89000000005L, JLong.decode("89000000005"))
    assertEquals(89000000005L, JLong.decode("+89000000005"))
    assertEquals(-89000000005L, JLong.decode("-89000000005"))
  }

  @Test def should_decode_octal_string_with_leading_0(): Unit = {
    assertEquals(456324454L, JLong.decode("03314572546"))
    assertEquals(456324454L, JLong.decode("+03314572546"))
    assertEquals(-456324454L, JLong.decode("-03314572546"))
  }

  @Test def should_decode_hexadeciaml_string_with_hexadecimal_specifier(): Unit = {
    assertEquals(98765432158845L, JLong.decode("0x59d39e7ffa7d"))
    assertEquals(98765432158845L, JLong.decode("+0x59d39e7ffa7d"))
    assertEquals(-49575304457780L, JLong.decode("-0x2d16a6696e34"))

    assertEquals(98765432158845L, JLong.decode("0X59d39e7ffa7d"))
    assertEquals(98765432158845L, JLong.decode("+0X59d39e7ffa7d"))
    assertEquals(-49575304457780L, JLong.decode("-0X2d16a6696e34"))

    assertEquals(98765432158845L, JLong.decode("#59d39e7ffa7d"))
    assertEquals(98765432158845L, JLong.decode("+#59d39e7ffa7d"))
    assertEquals(-49575304457780L, JLong.decode("-#2d16a6696e34"))
  }

  @Test def should_decode_min_and_max(): Unit = {
    assertEquals(JLong.MIN_VALUE, JLong.decode(s"-01000000000000000000000"))
    assertEquals(JLong.MIN_VALUE, JLong.decode(s"-0x8000000000000000"))
    assertEquals(JLong.MIN_VALUE, JLong.decode(s"-9223372036854775808"))

    assertEquals(JLong.MAX_VALUE, JLong.decode(s"0777777777777777777777"))
    assertEquals(JLong.MAX_VALUE, JLong.decode(s"0x7fffffffffffffff"))
    assertEquals(JLong.MAX_VALUE, JLong.decode(s"9223372036854775807"))
  }

  @Test def should_reject_strings_containing_other_than_numbers_when_decoding(): Unit = {
    // underscore delimitters
    assertThrows(classOf[NumberFormatException], JLong.decode("0_88_88"))
    assertThrows(classOf[NumberFormatException], JLong.decode("0xFF_FF"))

    // whitespaces
    assertThrows(classOf[NumberFormatException], JLong.decode(" 088"))
    assertThrows(classOf[NumberFormatException], JLong.decode("0xFF "))

    // signs after radix specifier
    assertThrows(classOf[NumberFormatException], JLong.decode("0+8888"))
    assertThrows(classOf[NumberFormatException], JLong.decode("0x-FF"))
  }

  @Test def should_reject_when_decoding_out_of_range(): Unit = {
    assertThrows(classOf[NumberFormatException], JLong.decode(s"9223372036854775808"))
    assertThrows(classOf[NumberFormatException], JLong.decode(s"-9223372036854775809"))
    assertThrows(classOf[NumberFormatException], JLong.decode(s"0x8000000000000000"))
    assertThrows(classOf[NumberFormatException], JLong.decode(s"-0x8000000000000001"))
  }
}
