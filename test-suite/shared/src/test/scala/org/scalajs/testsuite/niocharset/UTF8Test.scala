/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niocharset

import java.nio._
import java.nio.charset._

import org.junit.Test
import org.junit.Assert._

import BaseCharsetTest._

import org.scalajs.testsuite.utils.Platform.executingInJVM

class UTF8Test extends BaseCharsetTest(Charset.forName("UTF-8")) {
  @Test def decode(): Unit = {
    def OutSeq(elems: OutPart[CharBuffer]*): Seq[OutPart[CharBuffer]] =
      Seq[OutPart[CharBuffer]](elems: _*)

    // 1-byte characters
    testDecode(bb"42 6f 6e 6a 6f 75 72")(cb"Bonjour")

    // 2-byte characters
    testDecode(bb"47 72 c3 bc c3 9f 20 47 6f 74 74")(cb"Grüß Gott")
    testDecode(bb"ce 9a ce b1 ce bb ce b7 ce bc ce ad cf 81 ce b1")(cb"Καλημέρα")
    testDecode(bb"d8 b5 d8 a8 d8 a7 d8 ad 20 d8 a7 d9 84 d8 ae d9 8a d8 b1")(cb"صباح الخير")

    // 3-byte characters
    testDecode(bb"e3 81 93 e3 82 93 e3 81 ab e3 81 a1 e3 81 af")(cb"こんにちは")
    testDecode(bb"d0 94 d0 be d0 b1 d1 80 d1 8b d0 b9 20 d0 b4 d0 b5 d0 bd d1 8c")(cb"Добрый день")
    testDecode(bb"e4 bd a0 e5 a5 bd")(cb"你好")

    // 4-byte characters
    testDecode(bb"f0 9d 93 97 f0 9d 93 ae f0 9d 93 b5 f0 9d 93 b5 f0 9d 93 b8")(
      cb"\ud835\udcd7\ud835\udcee\ud835\udcf5\ud835\udcf5\ud835\udcf8")

    testDecode(bb"")(cb"")

    // First sequence of a certain length
    testDecode(bb"00")(cb"\u0000")
    testDecode(bb"c2 80")(cb"\u0080")
    testDecode(bb"e0 a0 80")(cb"\u0800")
    testDecode(bb"f0 90 80 80")(cb"\ud800\udc00")

    // Last sequence of a certain length
    testDecode(bb"7f")(cb"\u007f")
    testDecode(bb"df bf")(cb"\u07ff")
    testDecode(bb"ef bf bf")(cb"\uffff")
    testDecode(bb"f4 8f bf bf")(cb"\udbff\udfff")

    // Other boundary conditions
    testDecode(bb"ed 9f bf")(cb"\ud7ff")
    testDecode(bb"ee 80 80")(cb"\ue000")
    testDecode(bb"ef bf bd")(cb"\ufffd")

    // Here begin the sequences with at least one error

    // Code point too big
    if (!executingInJVM) {
      testDecode(bb"f4 90 80 80")(Malformed(4))
      testDecode(bb"41 f4 90 80 80 42")(cb"A", Malformed(4), cb"B")
    }

    // Unexpected continuation bytes (each is reported separately)
    testDecode(bb"80")(Malformed(1))
    testDecode(bb"bf")(Malformed(1))
    testDecode(bb"80 80")(Malformed(1), Malformed(1))
    testDecode(bb"80 80 80")(Malformed(1), Malformed(1), Malformed(1))
    testDecode(bb"80 80 80 80")(Malformed(1), Malformed(1), Malformed(1), Malformed(1))
    testDecode(bb"80 80 80 80 80")(Malformed(1), Malformed(1), Malformed(1), Malformed(1), Malformed(1))
    testDecode(bb"41 80 80 42 80 43")(cb"A", Malformed(1), Malformed(1), cb"B", Malformed(1), cb"C")

    // Lonely start characters, separated by spaces
    if (!executingInJVM) {
      testDecode(bb"${(0xc0 to 0xf4).flatMap(c => Seq(c, 32))}")(
        (0xc0 to 0xf4).flatMap(i => OutSeq(Malformed(1), cb" ")): _*)
    }

    // Sequences with some continuation bytes missing
    testDecode(bb"c2")(Malformed(1))
    testDecode(bb"e0")(Malformed(1))
    testDecode(bb"e0 a0")(Malformed(2))
    testDecode(bb"f0")(Malformed(1))
    testDecode(bb"f0 90")(Malformed(2))
    testDecode(bb"f0 90 80")(Malformed(3))
    // at the end of the buffer - #1537
    testDecode(bb"c0")(Malformed(1))
    if (!executingInJVM)
      testDecode(bb"e1 41")(Malformed(1), cb"A")
    testDecode(bb"e1 80 42")(Malformed(2), cb"B")
    // and all of them concatenated
    testDecode(bb"c2  e0  e0 a0  f0  f0 90  f0 90 80")(
      Seq(1, 1, 2, 1, 2, 3).map(Malformed(_)): _*)
    // and with normal sequences interspersed
    testDecode(bb"c2 41 e0 41 e0 a0 41 f0 41 f0 90 41 f0 90 80 41")(
      Seq(1, 1, 2, 1, 2, 3).flatMap(l => Seq[OutPart[CharBuffer]](Malformed(l), cb"A")): _*)

    // Impossible bytes
    testDecode(bb"fe")(Malformed(1))
    testDecode(bb"ff")(Malformed(1))
    testDecode(bb"fe fe ff ff")(Malformed(1), Malformed(1), Malformed(1), Malformed(1))
    // Old 5-byte and 6-byte starts
    if (!executingInJVM) {
      testDecode(bb"f8 80 80 80 af")(
          Malformed(1), Malformed(1), Malformed(1), Malformed(1), Malformed(1))
      testDecode(bb"fc 80 80 80 80 af")(
          Malformed(1), Malformed(1), Malformed(1), Malformed(1), Malformed(1), Malformed(1))

      // Overlong sequences (encoded with more bytes than necessary)
      // Overlong '/'
      testDecode(bb"c0 af")(Malformed(2))
      testDecode(bb"e0 80 af")(Malformed(3))
      testDecode(bb"f0 80 80 af")(Malformed(4))
      // Maximum overlong sequences
      testDecode(bb"c1 bf")(Malformed(2))
      testDecode(bb"e0 9f bf")(Malformed(3))
      testDecode(bb"f0 8f bf bf")(Malformed(4))
      // Overlong NUL
      testDecode(bb"c0 80")(Malformed(2))
      testDecode(bb"e0 80 80")(Malformed(3))
      testDecode(bb"f0 80 80 80")(Malformed(4))

      // Single UTF-16 surrogates
      testDecode(bb"ed a0 80")(Malformed(3))
      testDecode(bb"ed ad bf")(Malformed(3))
      testDecode(bb"ed ae 80")(Malformed(3))
      testDecode(bb"ed af bf")(Malformed(3))
      testDecode(bb"ed b0 80")(Malformed(3))
      testDecode(bb"ed be 80")(Malformed(3))
      testDecode(bb"ed bf bf")(Malformed(3))

      // Paired UTF-16 surrogates
      testDecode(bb"ed a0 80 ed b0 80")(Malformed(3), Malformed(3))
      testDecode(bb"ed a0 80 ed bf bf")(Malformed(3), Malformed(3))
      testDecode(bb"ed ad bf ed b0 80")(Malformed(3), Malformed(3))
      testDecode(bb"ed ad bf ed bf bf")(Malformed(3), Malformed(3))
      testDecode(bb"ed ae 80 ed b0 80")(Malformed(3), Malformed(3))
      testDecode(bb"ed ae 80 ed bf bf")(Malformed(3), Malformed(3))
      testDecode(bb"ed af bf ed b0 80")(Malformed(3), Malformed(3))
      testDecode(bb"ed af bf ed bf bf")(Malformed(3), Malformed(3))
    }
  }

  @Test def encode(): Unit = {
    def OutSeq(elems: OutPart[ByteBuffer]*): Seq[OutPart[ByteBuffer]] =
      Seq[OutPart[ByteBuffer]](elems: _*)

    // 1-byte characters
    testEncode(cb"Bonjour")(bb"42 6f 6e 6a 6f 75 72")

    // 2-byte characters
    testEncode(cb"Grüß Gott")(bb"47 72 c3 bc c3 9f 20 47 6f 74 74")
    testEncode(cb"Καλημέρα")(bb"ce 9a ce b1 ce bb ce b7 ce bc ce ad cf 81 ce b1")
    testEncode(cb"صباح الخير")(bb"d8 b5 d8 a8 d8 a7 d8 ad 20 d8 a7 d9 84 d8 ae d9 8a d8 b1")

    // 3-byte characters
    testEncode(cb"こんにちは")(bb"e3 81 93 e3 82 93 e3 81 ab e3 81 a1 e3 81 af")
    testEncode(cb"Добрый день")(bb"d0 94 d0 be d0 b1 d1 80 d1 8b d0 b9 20 d0 b4 d0 b5 d0 bd d1 8c")
    testEncode(cb"你好")(bb"e4 bd a0 e5 a5 bd")

    // 4-byte characters
    testEncode(cb"\ud835\udcd7\ud835\udcee\ud835\udcf5\ud835\udcf5\ud835\udcf8")(
        bb"f0 9d 93 97 f0 9d 93 ae f0 9d 93 b5 f0 9d 93 b5 f0 9d 93 b8")

    testEncode(cb"")(bb"")

    // First sequence of a certain length
    testEncode(cb"\u0000")(bb"00")
    testEncode(cb"\u0080")(bb"c2 80")
    testEncode(cb"\u0800")(bb"e0 a0 80")
    testEncode(cb"\ud800\udc00")(bb"f0 90 80 80")

    // Last sequence of a certain length
    testEncode(cb"\u007f")(bb"7f")
    testEncode(cb"\u07ff")(bb"df bf")
    testEncode(cb"\uffff")(bb"ef bf bf")
    testEncode(cb"\udbff\udfff")(bb"f4 8f bf bf")

    // Other boundary conditions
    testEncode(cb"\ud7ff")(bb"ed 9f bf")
    testEncode(cb"\ue000")(bb"ee 80 80")
    testEncode(cb"\ufffd")(bb"ef bf bd")

    // Here begin the sequences with at least one error

    // Single UTF-16 surrogates
    testEncode(cb"\ud800")(Malformed(1))
    testEncode(cb"\udaff")(Malformed(1))
    testEncode(cb"\udb80")(Malformed(1))
    testEncode(cb"\udbff")(Malformed(1))
    testEncode(cb"\udc00")(Malformed(1))
    testEncode(cb"\udf80")(Malformed(1))
    testEncode(cb"\udfff")(Malformed(1))

    // High UTF-16 surrogates not followed by low surrogates
    testEncode(cb"\ud800A")(Malformed(1), bb"41")
    testEncode(cb"\ud800\ud800")(Malformed(1), Malformed(1))
    testEncode(cb"\ud800\ud835\udcd7")(Malformed(1), bb"f0 9d 93 97")
    testEncode(cb"\udbffA")(Malformed(1), bb"41")
    testEncode(cb"\udbff\udb8f")(Malformed(1), Malformed(1))
    testEncode(cb"\udbff\ud835\udcd7")(Malformed(1), bb"f0 9d 93 97")
  }

  @Test def isLegalReplacement(): Unit = {
    val encoder = charset.newEncoder

    // The good ones

    assertTrue(encoder.isLegalReplacement(Array(0x00.toByte)))
    assertTrue(encoder.isLegalReplacement(Array(0x41.toByte)))
    assertTrue(encoder.isLegalReplacement(Array('?'.toByte)))
    assertTrue(encoder.isLegalReplacement(Array(0x7f.toByte)))

    assertTrue(encoder.isLegalReplacement(Array(0xc2.toByte, 0x80.toByte)))
    assertTrue(encoder.isLegalReplacement(Array(0xdf.toByte, 0xbf.toByte)))

    assertTrue(encoder.isLegalReplacement(
        Array(0xe0.toByte, 0xa0.toByte, 0x80.toByte)))
    assertTrue(encoder.isLegalReplacement(
        Array(0xef.toByte, 0xbf.toByte, 0xbf.toByte)))

    assertTrue(encoder.isLegalReplacement(
        Array(0xf0.toByte, 0x90.toByte, 0x80.toByte, 0x80.toByte)))
    assertTrue(encoder.isLegalReplacement(
        Array(0xf4.toByte, 0x8f.toByte, 0xbf.toByte, 0xbf.toByte)))

    // The bad ones

    assertFalse(encoder.isLegalReplacement(Array(0x80.toByte)))
    assertFalse(encoder.isLegalReplacement(Array(0xbf.toByte)))
    assertFalse(encoder.isLegalReplacement(Array(0xc2.toByte)))
    assertFalse(encoder.isLegalReplacement(Array(0xdf.toByte)))
    assertFalse(encoder.isLegalReplacement(Array(0xe0.toByte)))
    assertFalse(encoder.isLegalReplacement(Array(0xef.toByte)))
    assertFalse(encoder.isLegalReplacement(Array(0xf4.toByte)))

    assertFalse(encoder.isLegalReplacement(Array(0xe0.toByte, 0x80.toByte)))
  }
}
