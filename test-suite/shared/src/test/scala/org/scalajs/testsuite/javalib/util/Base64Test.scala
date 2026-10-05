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

package org.scalajs.testsuite.javalib.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException, InputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.util.Base64
import java.util.Base64.{Decoder, Encoder}

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform

class Base64Test {
  import Base64Test._

  // --------------------------------------------------------------------------
  // ENCODERS
  // --------------------------------------------------------------------------

  @Test def encodeToString(): Unit = {
    for (entry <- testEntries) {
      assertEquals(entry.configName, entry.encoded, entry.encoder.encodeToString(entry.bytes))
    }
  }

  @Test def encodeOneArray(): Unit = {
    for (entry <- testEntries) {
      val enc = entry.encoder.encode(entry.bytes)
      assertEquals(entry.configName, entry.encoded, new String(enc, ISO_8859_1))
    }
  }

  @Test def encodeTwoArrays(): Unit = {
    for (entry <- testEntries) {
      val dst = new Array[Byte](entry.encoded.length + 10) // array too big on purpose
      val written = entry.encoder.encode(entry.bytes, dst)
      assertEquals(entry.configName, entry.encoded.length, written)
      val content = dst.slice(0, written)
      val rlt = new String(content, ISO_8859_1)
      assertEquals(entry.configName, entry.encoded, rlt)
    }
  }

  @Test def encodeTwoArraysThrowsWithTooSmallDestination(): Unit = {
    val in = "Man"
    val dst = new Array[Byte](3) // too small
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getEncoder.encode(in.getBytes, dst)
    })
  }

  @Test def encodeByteBuffer(): Unit = {
    for (entry <- testEntries) {
      val result1 = entry.encoder.encode(ByteBuffer.wrap(entry.bytes))
      assertEquals(entry.configName, entry.encoded, new String(result1.array(), ISO_8859_1))

      val bb = ByteBuffer.allocate(entry.bytes.length + 2)
      bb.position(2)
      bb.mark()
      bb.put(entry.bytes)
      bb.reset()
      val result2 = entry.encoder.encode(bb)
      assertEquals(entry.configName, entry.encoded, new String(result2.array(), ISO_8859_1))
    }
  }

  @Test def encodeOutputStream(): Unit = {
    for (entry <- testEntries) {
      val baos = new ByteArrayOutputStream()
      val out = entry.encoder.wrap(baos)
      out.write(entry.bytes(0))
      out.write(entry.bytes, 1, entry.bytes.length - 1)
      out.close()
      val result = new String(baos.toByteArray, ISO_8859_1)
      assertEquals(entry.configName, entry.encoded, result)
    }
  }

  @Test def encodeOutputStreamFailsOnJVM(): Unit = {
    assumeFalse("JDK bug JDK-8176379", Platform.executingInJVM)

    // The `1` below will create a buggy encoder on the JVM
    val encoder = Base64.getMimeEncoder(1, Array('@'))
    val input = "Man"
    val expected = "TWFu"
    val ba = new ByteArrayOutputStream()
    val out = encoder.wrap(ba)
    out.write(input.getBytes)
    out.close()
    val result = new String(ba.toByteArray)
    assertEquals("outputstream should be initialized correctly",
        expected, result)
  }

  @Test def encodeOutputStreamTooMuch(): Unit = {
    val enc = Base64.getEncoder
    val out = enc.wrap(new ByteArrayOutputStream())
    assertThrows(classOf[IndexOutOfBoundsException], {
      out.write(Array.empty[Byte], 0, 5)
    })
  }

  @Test def testIllegalLineSeparator(): Unit = {
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getMimeEncoder(8, Array[Byte]('A'))
    })
  }

  // --------------------------------------------------------------------------
  // DECODERS
  // --------------------------------------------------------------------------

  @Test def decodeFromString(): Unit = {
    for (entry <- testEntries) {
      assertArrayEquals(entry.configName, entry.bytes, entry.decoder.decode(entry.encoded))
    }
  }

  @Test def decodeFromArray(): Unit = {
    for (entry <- testEntries) {
      val encodedBytes = entry.encoded.getBytes(ISO_8859_1)
      val result = entry.decoder.decode(encodedBytes)
      assertEquals(entry.configName, entry.text, new String(result, ISO_8859_1))
    }
  }

  @Test def decodeFromArrayToDest(): Unit = {
    for (entry <- testEntries) {
      val dst = new Array[Byte](entry.bytes.length)
      val encInBytes = entry.encoded.getBytes(ISO_8859_1)
      val dec = entry.decoder.decode(encInBytes, dst)
      assertEquals(entry.configName, entry.bytes.length, dec)
      assertArrayEquals(entry.configName, entry.bytes, dst)
    }
  }

  @Test def decodeFromByteBuffer(): Unit = {
    for (entry <- testEntries) {
      val bb = ByteBuffer.wrap(entry.encoded.getBytes(ISO_8859_1))
      val decoded = entry.decoder.decode(bb)
      val array = new Array[Byte](decoded.limit)
      decoded.get(array)
      assertArrayEquals(entry.configName, entry.bytes, array)
    }
  }

  @Test def decodeToArrayTooSmall(): Unit = {
    val encoded = "TWFu"
    val dst = new Array[Byte](2) // too small
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getDecoder.decode(encoded.getBytes, dst)
    })
  }

  @Test def decodeIllegalCharacter(): Unit = {
    val encoded = "TWE*"
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getDecoder.decode(encoded)
    })
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getUrlDecoder.decode(encoded)
    })

    assertEquals("MIME encoder should allow illegals",
        "Ma", new String(Base64.getMimeDecoder.decode(encoded), ISO_8859_1))
  }

  @Test def decodeIllegalLength(): Unit = {
    val encoded = "TWFuu"
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getDecoder.decode(encoded)
    })
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getUrlDecoder.decode(encoded)
    })
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getMimeDecoder.decode(encoded)
    })
  }

  @Test def decodeIllegalPadding(): Unit = {
    assumeFalse("JDK bug JDK-8176043", Platform.executingInJVM)

    val encoded = "TQ=*"
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getDecoder.decode(encoded)
    })
    assertThrows(classOf[IllegalArgumentException], {
      Base64.getUrlDecoder.decode(encoded)
    })

    assertEquals("MIME encoder should allow illegal paddings",
        "M", new String(Base64.getMimeDecoder.decode(encoded), ISO_8859_1))
  }

  @Test def decodeInputStream(): Unit = {
    for (entry <- testEntries) {
      val byteInstream = new ByteArrayInputStream(entry.encoded.getBytes(ISO_8859_1))
      val instream = entry.decoder.wrap(byteInstream)
      val read = new Array[Byte](entry.text.length)
      instream.read(read)
      while (instream.read() != -1) {} // read padding
      instream.close()
      assertEquals(entry.configName, entry.text, new String(read, ISO_8859_1))
    }
  }

  @Test def decodeIllegalsInputStream(): Unit = {
    val encoded = "TQ=*"
    assertThrows(classOf[IOException], {
      decodeInputStream(Base64.getDecoder(), encoded)
    })
    assertThrows(classOf[IOException], {
      decodeInputStream(Base64.getUrlDecoder(), encoded)
    })
    assertThrows(classOf[IOException], {
      decodeInputStream(Base64.getMimeDecoder(), "TWFu", Array('a'))
    })
    assertThrows(classOf[IOException], {
      decodeInputStream(Base64.getDecoder(), "TWFu", Array(0.toByte))
    })
  }

  @Test def decodeIllegalsInputStreamIllegalPadding(): Unit = {
    assumeFalse("JDK bug JDK-8176043", Platform.executingInJVM)

    val encoded = "TQ=*"
    assertEquals("mime decoder should allow illegal paddings",
        "M", decodeInputStream(Base64.getMimeDecoder(), encoded))
  }

  @Test def decodeBufferWithJustPaddingNonMime(): Unit = {
    for (decoder <- Seq(Base64.getDecoder, Base64.getUrlDecoder)) {
      // Should pass for empty and throw IllegalArgumentException for = and ==
      val bb = decoder.decode(ByteBuffer.allocate(0))
      assertEquals(0, bb.limit())
      for (input <- Seq("=", "==")) {
        assertThrows(classOf[IllegalArgumentException], {
          decoder.decode(ByteBuffer.wrap(input.getBytes))
        })
      }
    }
  }

  @Test def decodeBufferWithJustPaddingMime(): Unit = {
    assumeFalse("JDK bug JDK-8176043", Platform.executingInJVM)

    for (input <- Seq("", "=", "==")) {
      val bb = Base64.getMimeDecoder.decode(ByteBuffer.wrap(input.getBytes))
      assertEquals(0, bb.limit())
    }
  }

  @Test def decodeInputStreamFirstEOF(): Unit = {
    val emptyInputStream = new InputStream {
      override def read(): Int = -1
    }
    assertEquals(-1, Base64.getDecoder.wrap(emptyInputStream).read())
  }

  private def decodeInputStream(decoder: Decoder, input: String,
      dangling: Array[Byte] = Array.empty): String = {
    val bytes = input.getBytes ++ dangling
    val stream = decoder.wrap(new ByteArrayInputStream(bytes))
    val tmp = new Array[Byte](bytes.length)
    val read = stream.read(tmp)
    new String(tmp, 0, read)
  }

}

object Base64Test {

  private val input: Array[Byte] = {
    val text = {
      "Base64 is a group of similar binary-to-text encoding schemes that " +
      "represent binary data in an ASCII string format by translating it " +
      "into a radix-64 representation"
    }
    text.toArray.map(_.toByte)
  }

  private final case class TestEntry(
      configName: String,
      encoder: Encoder,
      decoder: Decoder,
      text: String,
      encoded: String
  ) {
    val bytes: Array[Byte] = text.toArray.map(_.toByte)
  }

  type Config = (String, Encoder, Decoder)

  private val basicPadding: Config =
    ("basic, padding", Base64.getEncoder(), Base64.getDecoder())

  private val basicNoPadding: Config =
    ("basic, padding", Base64.getEncoder().withoutPadding(), Base64.getDecoder())

  private val urlPadding: Config =
    ("url, padding", Base64.getUrlEncoder(), Base64.getUrlDecoder())

  private val urlNoPadding: Config =
    ("url, padding", Base64.getUrlEncoder().withoutPadding(), Base64.getUrlDecoder())

  private val mimePadding: Config =
    ("mime, padding", Base64.getMimeEncoder(), Base64.getMimeDecoder())

  private val mimeNoPadding: Config =
    ("mime, padding", Base64.getMimeEncoder().withoutPadding(), Base64.getMimeDecoder())

  private def mimePaddingMake(lineLength: Int, delimiters: String): Config = {
    val encoder =
      Base64.getMimeEncoder(lineLength, delimiters.toArray.map(_.toByte))
    (s"mime, padding, $lineLength, $delimiters", encoder, Base64.getMimeDecoder())
  }

  private def mimeNoPaddingMake(lineLength: Int, delimiters: String): Config = {
    val encoder =
      Base64.getMimeEncoder(lineLength, delimiters.toArray.map(_.toByte)).withoutPadding()
    (s"mime, no padding, $lineLength, $delimiters", encoder, Base64.getMimeDecoder())
  }

  private def e(config: (String, Encoder, Decoder), text: String, encoded: String): TestEntry =
    TestEntry(config._1, config._2, config._3, text, encoded)

  // scalafmt: { maxColumn = 1000 }
  private lazy val testEntries: Array[TestEntry] = Array(
    e(basicPadding, "B", "Qg=="),
    e(basicPadding, "Ba", "QmE="),
    e(basicPadding, "Bas", "QmFz"),
    e(basicPadding, "Base", "QmFzZQ=="),
    e(basicPadding, "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(basicPadding, "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(urlPadding, "B", "Qg=="),
    e(urlPadding, "Ba", "QmE="),
    e(urlPadding, "Bas", "QmFz"),
    e(urlPadding, "Base", "QmFzZQ=="),
    e(urlPadding, "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(urlPadding, "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePadding, "B", "Qg=="),
    e(mimePadding, "Ba", "QmE="),
    e(mimePadding, "Bas", "QmFz"),
    e(mimePadding, "Base", "QmFzZQ=="),
    e(mimePadding, "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePadding, "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hl\r\nbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQg\r\nYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(basicNoPadding, "B", "Qg"),
    e(basicNoPadding, "Ba", "QmE"),
    e(basicNoPadding, "Bas", "QmFz"),
    e(basicNoPadding, "Base", "QmFzZQ"),
    e(basicNoPadding, "Base64 is ", "QmFzZTY0IGlzIA"),
    e(basicNoPadding, "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(urlNoPadding, "B", "Qg"),
    e(urlNoPadding, "Ba", "QmE"),
    e(urlNoPadding, "Bas", "QmFz"),
    e(urlNoPadding, "Base", "QmFzZQ"),
    e(urlNoPadding, "Base64 is ", "QmFzZTY0IGlzIA"),
    e(urlNoPadding, "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPadding, "B", "Qg"),
    e(mimeNoPadding, "Ba", "QmE"),
    e(mimeNoPadding, "Bas", "QmFz"),
    e(mimeNoPadding, "Base", "QmFzZQ"),
    e(mimeNoPadding, "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPadding, "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hl\r\nbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQg\r\nYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(-1, ""), "B", "Qg=="),
    e(mimePaddingMake(-1, ""), "Ba", "QmE="),
    e(mimePaddingMake(-1, ""), "Bas", "QmFz"),
    e(mimePaddingMake(-1, ""), "Base", "QmFzZQ=="),
    e(mimePaddingMake(-1, ""), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(-1, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(0, ""), "B", "Qg=="),
    e(mimePaddingMake(0, ""), "Ba", "QmE="),
    e(mimePaddingMake(0, ""), "Bas", "QmFz"),
    e(mimePaddingMake(0, ""), "Base", "QmFzZQ=="),
    e(mimePaddingMake(0, ""), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(0, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(4, ""), "B", "Qg=="),
    e(mimePaddingMake(4, ""), "Ba", "QmE="),
    e(mimePaddingMake(4, ""), "Bas", "QmFz"),
    e(mimePaddingMake(4, ""), "Base", "QmFzZQ=="),
    e(mimePaddingMake(4, ""), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(4, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(5, ""), "B", "Qg=="),
    e(mimePaddingMake(5, ""), "Ba", "QmE="),
    e(mimePaddingMake(5, ""), "Bas", "QmFz"),
    e(mimePaddingMake(5, ""), "Base", "QmFzZQ=="),
    e(mimePaddingMake(5, ""), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(5, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(9, ""), "B", "Qg=="),
    e(mimePaddingMake(9, ""), "Ba", "QmE="),
    e(mimePaddingMake(9, ""), "Bas", "QmFz"),
    e(mimePaddingMake(9, ""), "Base", "QmFzZQ=="),
    e(mimePaddingMake(9, ""), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(9, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(-1, "@"), "B", "Qg=="),
    e(mimePaddingMake(-1, "@"), "Ba", "QmE="),
    e(mimePaddingMake(-1, "@"), "Bas", "QmFz"),
    e(mimePaddingMake(-1, "@"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(-1, "@"), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(-1, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(0, "@"), "B", "Qg=="),
    e(mimePaddingMake(0, "@"), "Ba", "QmE="),
    e(mimePaddingMake(0, "@"), "Bas", "QmFz"),
    e(mimePaddingMake(0, "@"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(0, "@"), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(0, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(4, "@"), "B", "Qg=="),
    e(mimePaddingMake(4, "@"), "Ba", "QmE="),
    e(mimePaddingMake(4, "@"), "Bas", "QmFz"),
    e(mimePaddingMake(4, "@"), "Base", "QmFz@ZQ=="),
    e(mimePaddingMake(4, "@"), "Base64 is ", "QmFz@ZTY0@IGlz@IA=="),
    e(mimePaddingMake(4, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u"),
    e(mimePaddingMake(5, "@"), "B", "Qg=="),
    e(mimePaddingMake(5, "@"), "Ba", "QmE="),
    e(mimePaddingMake(5, "@"), "Bas", "QmFz"),
    e(mimePaddingMake(5, "@"), "Base", "QmFz@ZQ=="),
    e(mimePaddingMake(5, "@"), "Base64 is ", "QmFz@ZTY0@IGlz@IA=="),
    e(mimePaddingMake(5, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u"),
    e(mimePaddingMake(9, "@"), "B", "Qg=="),
    e(mimePaddingMake(9, "@"), "Ba", "QmE="),
    e(mimePaddingMake(9, "@"), "Bas", "QmFz"),
    e(mimePaddingMake(9, "@"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(9, "@"), "Base64 is ", "QmFzZTY0@IGlzIA=="),
    e(mimePaddingMake(9, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0@IGlzIGEg@Z3JvdXAg@b2Ygc2lt@aWxhciBi@aW5hcnkt@dG8tdGV4@dCBlbmNv@ZGluZyBz@Y2hlbWVz@IHRoYXQg@cmVwcmVz@ZW50IGJp@bmFyeSBk@YXRhIGlu@IGFuIEFT@Q0lJIHN0@cmluZyBm@b3JtYXQg@YnkgdHJh@bnNsYXRp@bmcgaXQg@aW50byBh@IHJhZGl4@LTY0IHJl@cHJlc2Vu@dGF0aW9u"),
    e(mimePaddingMake(-1, "@$"), "B", "Qg=="),
    e(mimePaddingMake(-1, "@$"), "Ba", "QmE="),
    e(mimePaddingMake(-1, "@$"), "Bas", "QmFz"),
    e(mimePaddingMake(-1, "@$"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(-1, "@$"), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(-1, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(0, "@$"), "B", "Qg=="),
    e(mimePaddingMake(0, "@$"), "Ba", "QmE="),
    e(mimePaddingMake(0, "@$"), "Bas", "QmFz"),
    e(mimePaddingMake(0, "@$"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(0, "@$"), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(0, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(4, "@$"), "B", "Qg=="),
    e(mimePaddingMake(4, "@$"), "Ba", "QmE="),
    e(mimePaddingMake(4, "@$"), "Bas", "QmFz"),
    e(mimePaddingMake(4, "@$"), "Base", "QmFz@$ZQ=="),
    e(mimePaddingMake(4, "@$"), "Base64 is ", "QmFz@$ZTY0@$IGlz@$IA=="),
    e(mimePaddingMake(4, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u"),
    e(mimePaddingMake(5, "@$"), "B", "Qg=="),
    e(mimePaddingMake(5, "@$"), "Ba", "QmE="),
    e(mimePaddingMake(5, "@$"), "Bas", "QmFz"),
    e(mimePaddingMake(5, "@$"), "Base", "QmFz@$ZQ=="),
    e(mimePaddingMake(5, "@$"), "Base64 is ", "QmFz@$ZTY0@$IGlz@$IA=="),
    e(mimePaddingMake(5, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u"),
    e(mimePaddingMake(9, "@$"), "B", "Qg=="),
    e(mimePaddingMake(9, "@$"), "Ba", "QmE="),
    e(mimePaddingMake(9, "@$"), "Bas", "QmFz"),
    e(mimePaddingMake(9, "@$"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(9, "@$"), "Base64 is ", "QmFzZTY0@$IGlzIA=="),
    e(mimePaddingMake(9, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0@$IGlzIGEg@$Z3JvdXAg@$b2Ygc2lt@$aWxhciBi@$aW5hcnkt@$dG8tdGV4@$dCBlbmNv@$ZGluZyBz@$Y2hlbWVz@$IHRoYXQg@$cmVwcmVz@$ZW50IGJp@$bmFyeSBk@$YXRhIGlu@$IGFuIEFT@$Q0lJIHN0@$cmluZyBm@$b3JtYXQg@$YnkgdHJh@$bnNsYXRp@$bmcgaXQg@$aW50byBh@$IHJhZGl4@$LTY0IHJl@$cHJlc2Vu@$dGF0aW9u"),
    e(mimePaddingMake(-1, "@$*"), "B", "Qg=="),
    e(mimePaddingMake(-1, "@$*"), "Ba", "QmE="),
    e(mimePaddingMake(-1, "@$*"), "Bas", "QmFz"),
    e(mimePaddingMake(-1, "@$*"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(-1, "@$*"), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(-1, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(0, "@$*"), "B", "Qg=="),
    e(mimePaddingMake(0, "@$*"), "Ba", "QmE="),
    e(mimePaddingMake(0, "@$*"), "Bas", "QmFz"),
    e(mimePaddingMake(0, "@$*"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(0, "@$*"), "Base64 is ", "QmFzZTY0IGlzIA=="),
    e(mimePaddingMake(0, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimePaddingMake(4, "@$*"), "B", "Qg=="),
    e(mimePaddingMake(4, "@$*"), "Ba", "QmE="),
    e(mimePaddingMake(4, "@$*"), "Bas", "QmFz"),
    e(mimePaddingMake(4, "@$*"), "Base", "QmFz@$*ZQ=="),
    e(mimePaddingMake(4, "@$*"), "Base64 is ", "QmFz@$*ZTY0@$*IGlz@$*IA=="),
    e(mimePaddingMake(4, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u"),
    e(mimePaddingMake(5, "@$*"), "B", "Qg=="),
    e(mimePaddingMake(5, "@$*"), "Ba", "QmE="),
    e(mimePaddingMake(5, "@$*"), "Bas", "QmFz"),
    e(mimePaddingMake(5, "@$*"), "Base", "QmFz@$*ZQ=="),
    e(mimePaddingMake(5, "@$*"), "Base64 is ", "QmFz@$*ZTY0@$*IGlz@$*IA=="),
    e(mimePaddingMake(5, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u"),
    e(mimePaddingMake(9, "@$*"), "B", "Qg=="),
    e(mimePaddingMake(9, "@$*"), "Ba", "QmE="),
    e(mimePaddingMake(9, "@$*"), "Bas", "QmFz"),
    e(mimePaddingMake(9, "@$*"), "Base", "QmFzZQ=="),
    e(mimePaddingMake(9, "@$*"), "Base64 is ", "QmFzZTY0@$*IGlzIA=="),
    e(mimePaddingMake(9, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0@$*IGlzIGEg@$*Z3JvdXAg@$*b2Ygc2lt@$*aWxhciBi@$*aW5hcnkt@$*dG8tdGV4@$*dCBlbmNv@$*ZGluZyBz@$*Y2hlbWVz@$*IHRoYXQg@$*cmVwcmVz@$*ZW50IGJp@$*bmFyeSBk@$*YXRhIGlu@$*IGFuIEFT@$*Q0lJIHN0@$*cmluZyBm@$*b3JtYXQg@$*YnkgdHJh@$*bnNsYXRp@$*bmcgaXQg@$*aW50byBh@$*IHJhZGl4@$*LTY0IHJl@$*cHJlc2Vu@$*dGF0aW9u"),
    e(mimeNoPaddingMake(-1, ""), "B", "Qg"),
    e(mimeNoPaddingMake(-1, ""), "Ba", "QmE"),
    e(mimeNoPaddingMake(-1, ""), "Bas", "QmFz"),
    e(mimeNoPaddingMake(-1, ""), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(-1, ""), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(-1, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(0, ""), "B", "Qg"),
    e(mimeNoPaddingMake(0, ""), "Ba", "QmE"),
    e(mimeNoPaddingMake(0, ""), "Bas", "QmFz"),
    e(mimeNoPaddingMake(0, ""), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(0, ""), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(0, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(4, ""), "B", "Qg"),
    e(mimeNoPaddingMake(4, ""), "Ba", "QmE"),
    e(mimeNoPaddingMake(4, ""), "Bas", "QmFz"),
    e(mimeNoPaddingMake(4, ""), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(4, ""), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(4, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(5, ""), "B", "Qg"),
    e(mimeNoPaddingMake(5, ""), "Ba", "QmE"),
    e(mimeNoPaddingMake(5, ""), "Bas", "QmFz"),
    e(mimeNoPaddingMake(5, ""), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(5, ""), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(5, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(9, ""), "B", "Qg"),
    e(mimeNoPaddingMake(9, ""), "Ba", "QmE"),
    e(mimeNoPaddingMake(9, ""), "Bas", "QmFz"),
    e(mimeNoPaddingMake(9, ""), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(9, ""), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(9, ""), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(-1, "@"), "B", "Qg"),
    e(mimeNoPaddingMake(-1, "@"), "Ba", "QmE"),
    e(mimeNoPaddingMake(-1, "@"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(-1, "@"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(-1, "@"), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(-1, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(0, "@"), "B", "Qg"),
    e(mimeNoPaddingMake(0, "@"), "Ba", "QmE"),
    e(mimeNoPaddingMake(0, "@"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(0, "@"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(0, "@"), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(0, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(4, "@"), "B", "Qg"),
    e(mimeNoPaddingMake(4, "@"), "Ba", "QmE"),
    e(mimeNoPaddingMake(4, "@"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(4, "@"), "Base", "QmFz@ZQ"),
    e(mimeNoPaddingMake(4, "@"), "Base64 is ", "QmFz@ZTY0@IGlz@IA"),
    e(mimeNoPaddingMake(4, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u"),
    e(mimeNoPaddingMake(5, "@"), "B", "Qg"),
    e(mimeNoPaddingMake(5, "@"), "Ba", "QmE"),
    e(mimeNoPaddingMake(5, "@"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(5, "@"), "Base", "QmFz@ZQ"),
    e(mimeNoPaddingMake(5, "@"), "Base64 is ", "QmFz@ZTY0@IGlz@IA"),
    e(mimeNoPaddingMake(5, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u"),
    e(mimeNoPaddingMake(9, "@"), "B", "Qg"),
    e(mimeNoPaddingMake(9, "@"), "Ba", "QmE"),
    e(mimeNoPaddingMake(9, "@"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(9, "@"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(9, "@"), "Base64 is ", "QmFzZTY0@IGlzIA"),
    e(mimeNoPaddingMake(9, "@"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0@IGlzIGEg@Z3JvdXAg@b2Ygc2lt@aWxhciBi@aW5hcnkt@dG8tdGV4@dCBlbmNv@ZGluZyBz@Y2hlbWVz@IHRoYXQg@cmVwcmVz@ZW50IGJp@bmFyeSBk@YXRhIGlu@IGFuIEFT@Q0lJIHN0@cmluZyBm@b3JtYXQg@YnkgdHJh@bnNsYXRp@bmcgaXQg@aW50byBh@IHJhZGl4@LTY0IHJl@cHJlc2Vu@dGF0aW9u"),
    e(mimeNoPaddingMake(-1, "@$"), "B", "Qg"),
    e(mimeNoPaddingMake(-1, "@$"), "Ba", "QmE"),
    e(mimeNoPaddingMake(-1, "@$"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(-1, "@$"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(-1, "@$"), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(-1, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(0, "@$"), "B", "Qg"),
    e(mimeNoPaddingMake(0, "@$"), "Ba", "QmE"),
    e(mimeNoPaddingMake(0, "@$"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(0, "@$"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(0, "@$"), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(0, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(4, "@$"), "B", "Qg"),
    e(mimeNoPaddingMake(4, "@$"), "Ba", "QmE"),
    e(mimeNoPaddingMake(4, "@$"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(4, "@$"), "Base", "QmFz@$ZQ"),
    e(mimeNoPaddingMake(4, "@$"), "Base64 is ", "QmFz@$ZTY0@$IGlz@$IA"),
    e(mimeNoPaddingMake(4, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u"),
    e(mimeNoPaddingMake(5, "@$"), "B", "Qg"),
    e(mimeNoPaddingMake(5, "@$"), "Ba", "QmE"),
    e(mimeNoPaddingMake(5, "@$"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(5, "@$"), "Base", "QmFz@$ZQ"),
    e(mimeNoPaddingMake(5, "@$"), "Base64 is ", "QmFz@$ZTY0@$IGlz@$IA"),
    e(mimeNoPaddingMake(5, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u"),
    e(mimeNoPaddingMake(9, "@$"), "B", "Qg"),
    e(mimeNoPaddingMake(9, "@$"), "Ba", "QmE"),
    e(mimeNoPaddingMake(9, "@$"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(9, "@$"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(9, "@$"), "Base64 is ", "QmFzZTY0@$IGlzIA"),
    e(mimeNoPaddingMake(9, "@$"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0@$IGlzIGEg@$Z3JvdXAg@$b2Ygc2lt@$aWxhciBi@$aW5hcnkt@$dG8tdGV4@$dCBlbmNv@$ZGluZyBz@$Y2hlbWVz@$IHRoYXQg@$cmVwcmVz@$ZW50IGJp@$bmFyeSBk@$YXRhIGlu@$IGFuIEFT@$Q0lJIHN0@$cmluZyBm@$b3JtYXQg@$YnkgdHJh@$bnNsYXRp@$bmcgaXQg@$aW50byBh@$IHJhZGl4@$LTY0IHJl@$cHJlc2Vu@$dGF0aW9u"),
    e(mimeNoPaddingMake(-1, "@$*"), "B", "Qg"),
    e(mimeNoPaddingMake(-1, "@$*"), "Ba", "QmE"),
    e(mimeNoPaddingMake(-1, "@$*"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(-1, "@$*"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(-1, "@$*"), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(-1, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(0, "@$*"), "B", "Qg"),
    e(mimeNoPaddingMake(0, "@$*"), "Ba", "QmE"),
    e(mimeNoPaddingMake(0, "@$*"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(0, "@$*"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(0, "@$*"), "Base64 is ", "QmFzZTY0IGlzIA"),
    e(mimeNoPaddingMake(0, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u"),
    e(mimeNoPaddingMake(4, "@$*"), "B", "Qg"),
    e(mimeNoPaddingMake(4, "@$*"), "Ba", "QmE"),
    e(mimeNoPaddingMake(4, "@$*"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(4, "@$*"), "Base", "QmFz@$*ZQ"),
    e(mimeNoPaddingMake(4, "@$*"), "Base64 is ", "QmFz@$*ZTY0@$*IGlz@$*IA"),
    e(mimeNoPaddingMake(4, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u"),
    e(mimeNoPaddingMake(5, "@$*"), "B", "Qg"),
    e(mimeNoPaddingMake(5, "@$*"), "Ba", "QmE"),
    e(mimeNoPaddingMake(5, "@$*"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(5, "@$*"), "Base", "QmFz@$*ZQ"),
    e(mimeNoPaddingMake(5, "@$*"), "Base64 is ", "QmFz@$*ZTY0@$*IGlz@$*IA"),
    e(mimeNoPaddingMake(5, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u"),
    e(mimeNoPaddingMake(9, "@$*"), "B", "Qg"),
    e(mimeNoPaddingMake(9, "@$*"), "Ba", "QmE"),
    e(mimeNoPaddingMake(9, "@$*"), "Bas", "QmFz"),
    e(mimeNoPaddingMake(9, "@$*"), "Base", "QmFzZQ"),
    e(mimeNoPaddingMake(9, "@$*"), "Base64 is ", "QmFzZTY0@$*IGlzIA"),
    e(mimeNoPaddingMake(9, "@$*"), "Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation", "QmFzZTY0@$*IGlzIGEg@$*Z3JvdXAg@$*b2Ygc2lt@$*aWxhciBi@$*aW5hcnkt@$*dG8tdGV4@$*dCBlbmNv@$*ZGluZyBz@$*Y2hlbWVz@$*IHRoYXQg@$*cmVwcmVz@$*ZW50IGJp@$*bmFyeSBk@$*YXRhIGlu@$*IGFuIEFT@$*Q0lJIHN0@$*cmluZyBm@$*b3JtYXQg@$*YnkgdHJh@$*bnNsYXRp@$*bmcgaXQg@$*aW50byBh@$*IHJhZGl4@$*LTY0IHJl@$*cHJlc2Vu@$*dGF0aW9u")
  )
  // scalafmt: {}
}
