/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niocharset

import java.nio.charset._

import org.junit.Test
import org.junit.Assert._

import BaseCharsetTest._

class USASCIITest extends BaseCharsetTest(Charset.forName("US-ASCII")) {
  @Test def decode(): Unit = {
    // Simple tests

    testDecode(bb"48 65 6c 6c 6f")(cb"Hello")
    testDecode(bb"42 6f 6e 6a 6f 75 72")(cb"Bonjour")

    testDecode(bb"00 01 0a 10 20")(cb"\u0000\u0001\u000a\u0010 ")
    testDecode(bb"7f 7f")(cb"\u007f\u007f")

    // Bit 7 on is an error - #2156
    testDecode(bb"48 e5 ec 6c ef")(cb"H", Malformed(1), Malformed(1), cb"l", Malformed(1))
    testDecode(bb"80 ff")(Malformed(1), Malformed(1))
  }

  @Test def encode(): Unit = {
    // Simple tests

    testEncode(cb"Hello")(bb"48 65 6c 6c 6f")
    testEncode(cb"Bonjour")(bb"42 6f 6e 6a 6f 75 72")

    testEncode(cb"\u0000\u0001\u000a\u0010 ")(bb"00 01 0a 10 20")
    testEncode(cb"\u007f\u007f")(bb"7f 7f")

    // Unmappable characters

    testEncode(cb"é")(Unmappable(1))
    testEncode(cb"\u0080")(Unmappable(1))
    testEncode(cb"\u00ff")(Unmappable(1))
    testEncode(cb"\u0100")(Unmappable(1))
    testEncode(cb"\u07ff")(Unmappable(1))
    testEncode(cb"\ue000")(Unmappable(1))
    testEncode(cb"\uffff")(Unmappable(1))
    testEncode(cb"\ud835\udcd7")(Unmappable(2))

    testEncode(cb"éA")(Unmappable(1), bb"41")
    testEncode(cb"\u0080A")(Unmappable(1), bb"41")
    testEncode(cb"\u00ffA")(Unmappable(1), bb"41")
    testEncode(cb"\u0100A")(Unmappable(1), bb"41")
    testEncode(cb"\u07ffA")(Unmappable(1), bb"41")
    testEncode(cb"\ue000A")(Unmappable(1), bb"41")
    testEncode(cb"\uffffA")(Unmappable(1), bb"41")
    testEncode(cb"\ud835\udcd7A")(Unmappable(2), bb"41")

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
    testEncode(cb"\ud800\ud835\udcd7")(Malformed(1), Unmappable(2))
    testEncode(cb"\udbffA")(Malformed(1), bb"41")
    testEncode(cb"\udbff\udb8f")(Malformed(1), Malformed(1))
    testEncode(cb"\udbff\ud835\udcd7")(Malformed(1), Unmappable(2))
  }

  @Test def isLegalReplacement(): Unit = {
    val encoder = charset.newEncoder
    assertTrue(encoder.isLegalReplacement(Array(0x00.toByte)))
    assertTrue(encoder.isLegalReplacement(Array(0x41.toByte)))
    assertTrue(encoder.isLegalReplacement(Array('?'.toByte)))

    // Bit 7 on is an error - #2156
    assertFalse(encoder.isLegalReplacement(Array(0x80.toByte)))
    assertFalse(encoder.isLegalReplacement(Array(0xff.toByte)))
  }
}
