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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.util.Base64
import java.util.Base64.Decoder

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform

class Base64Test {
  import Base64Test._

  // --------------------------------------------------------------------------
  // ENCODERS
  // --------------------------------------------------------------------------

  @Test def encodeToString(): Unit = {
    val results =
      for ((name, in, enc) <- encoders) yield (name -> enc.encodeToString(in))
    assertEquals("calculated count doesn't match computed count",
        encodedResults.length, results.size)
    for (((name, enc), exp) <- results.zip(encodedResults))
      assertEquals(s"encodeToString doesn't match for: $name", exp, enc)
  }

  @Test def encodeOneArray(): Unit = {
    val results =
      for ((name, in, enc) <- encoders) yield (name -> enc.encode(in))
    assertEquals("calculated count doesn't match computed count",
        encodedResults.length, results.size)
    for (((name, enc), exp) <- results.zip(encodedResults)) {
      assertEquals(s"encode array doesn't match for: $name",
          exp, new String(enc, ISO_8859_1))
    }
  }

  @Test def encodeTwoArrays(): Unit = {
    for (((name, in, enc), exp) <- encoders.zip(encodedResults)) {
      val dst = new Array[Byte](exp.length + 10) // array too big on purpose
      val written = enc.encode(in, dst)
      assertEquals(s"number of written bytes doesn't match for: $name",
          exp.length, written)
      val content = dst.slice(0, written)
      val rlt = new String(content, ISO_8859_1)
      assertEquals(s"encode array into array doesn't match for: $name",
          exp, rlt)
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
    for (((name, in, enc), exp) <- encoders.zip(encodedResults)) {
      val result1 = enc.encode(ByteBuffer.wrap(in))
      assertEquals(s"byte buffers don't match for: $name",
          exp, new String(result1.array(), ISO_8859_1))

      val bb = ByteBuffer.allocate(in.length + 2)
      bb.position(2)
      bb.mark()
      bb.put(in)
      bb.reset()
      val result2 = enc.encode(bb)
      assertEquals(s"byte buffers don't match for: $name",
          exp, new String(result2.array(), ISO_8859_1))
    }
  }

  @Test def encodeOutputStream(): Unit = {
    for (((name, in, enc), exp) <- encoders.zip(encodedResults)) {
      val baos = new ByteArrayOutputStream()
      val out = enc.wrap(baos)
      out.write(in(0))
      out.write(in, 1, in.length - 1)
      out.close()
      val result = new String(baos.toByteArray, ISO_8859_1)
      assertEquals(s"output stream result doesn't match for: $name",
          exp, result)
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
    assertEquals("encoded data count doesn't match input count",
        encoders.length, decodersAndInputs.length)
    for ((encoded, (decoder, in)) <- encodedResults.zip(decodersAndInputs)) {
      assertArrayEquals(s"decoded doesn't match expected $encoded",
          in.getBytes(ISO_8859_1), decoder.decode(encoded))
    }
  }

  @Test def decodeFromArray(): Unit = {
    for ((encoded, (decoder, in)) <- encodedResults.zip(decodersAndInputs)) {
      val encodedBytes = encoded.getBytes(ISO_8859_1)
      val result = decoder.decode(encodedBytes)
      assertEquals(s"decoded doesn't match expected for encoded $encoded",
          in, new String(result, ISO_8859_1))
    }
  }

  @Test def decodeFromArrayToDest(): Unit = {
    for ((encoded, (decoder, in)) <- encodedResults.zip(decodersAndInputs)) {
      val dst = new Array[Byte](in.length)
      val encInBytes = encoded.getBytes(ISO_8859_1)
      val dec = decoder.decode(encInBytes, dst)
      assertEquals("decoded count doesn't match expected", in.length, dec)
      assertArrayEquals("decoded array doesn't match expected",
          in.getBytes(ISO_8859_1), dst)
    }
  }

  @Test def decodeFromByteBuffer(): Unit = {
    for ((encoded, (decoder, in)) <- encodedResults.zip(decodersAndInputs)) {
      val bb = ByteBuffer.wrap(encoded.getBytes(ISO_8859_1))
      val decoded = decoder.decode(bb)
      val array = new Array[Byte](decoded.limit)
      decoded.get(array)
      assertArrayEquals("decoded byte buffer doesn't match expected",
          in.getBytes(ISO_8859_1), array)
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
    for ((encoded, (decoder, expected)) <- encodedResults.zip(decodersAndInputs)) {
      val byteInstream = new ByteArrayInputStream(encoded.getBytes(ISO_8859_1))
      val instream = decoder.wrap(byteInstream)
      val read = new Array[Byte](expected.length)
      instream.read(read)
      while (instream.read() != -1) {} // read padding
      instream.close()
      assertEquals("inputstream read value not as expected",
          expected, new String(read, ISO_8859_1))
    }
  }

  @Test def decodeIllegalsInputStream(): Unit = {
    val encoded = "TQ=*"
    assertThrows(classOf[IOException], {
      decodeInputStream(basic, encoded)
    })
    assertThrows(classOf[IOException], {
      decodeInputStream(url, encoded)
    })
    assertThrows(classOf[IOException], {
      decodeInputStream(mime, "TWFu", Array('a'))
    })
    assertThrows(classOf[IOException], {
      decodeInputStream(basic, "TWFu", Array(0.toByte))
    })
  }

  @Test def decodeIllegalsInputStreamIllegalPadding(): Unit = {
    assumeFalse("JDK bug JDK-8176043", Platform.executingInJVM)

    val encoded = "TQ=*"
    assertEquals("mime encoder should allow illegal paddings",
        "M", decodeInputStream(mime, encoded))
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
    text.getBytes(ISO_8859_1)
  }

  private val inputLengths = Seq(1, 2, 3, 4, 10, input.length)
  private val lineDelimChars = "@$*"
  private val lineLengths = Seq(-1, 0, 4, 5, 9)

  private val stdEncPadding = Seq(
      "basic, padding" -> Base64.getEncoder,
      "url, padding" -> Base64.getUrlEncoder,
      "mime, padding" -> Base64.getMimeEncoder
  )

  private val stdEncNoPadding = {
    for ((name, enc) <- stdEncPadding)
      yield name.replace("padding", "no padding") -> enc.withoutPadding()
  }

  private val lineDelimitersWithLineLengths: Seq[(String, Int)] = {
    for {
      i <- 0 to lineDelimChars.length
      l <- lineLengths
    } yield {
      lineDelimChars.take(i) -> l
    }
  }

  private val customEncPadding = {
    for ((delim, ll) <- lineDelimitersWithLineLengths) yield {
      (s"mime, padding, line length: $ll delimiters: $delim" ->
          Base64.getMimeEncoder(ll, delim.getBytes))
    }
  }

  private val customEncNoPadding = {
    for ((name, enc) <- customEncPadding)
      yield name.replace("padding", "no padding") -> enc.withoutPadding()
  }

  private val allEncoders =
    stdEncPadding ++ stdEncNoPadding ++ customEncPadding ++ customEncNoPadding

  private val encoders = {
    for {
      (name, enc) <- allEncoders
      length <- inputLengths
    } yield {
      (s"$name", input.take(length), enc)
    }
  }

  private lazy val decodersAndInputs = data.map(t => getDecoderAndInput(t._1))
  private lazy val encodedResults = data.map(_._2)

  private val basic = Base64.getDecoder
  private val url = Base64.getUrlDecoder

  private val mime = Base64.getMimeDecoder

  def getDecoderAndInput(text: String): (Decoder, String) = {
    val decoder = {
      if (text.contains("basic")) basic
      else if (text.contains("url")) url
      else if (text.contains("mime")) mime
      else throw new IllegalArgumentException(s"no decoder found in string $text")
    }
    val input = text.replaceAll(".*%(.*)%", "$1")
    decoder -> input
  }

  // scalastyle:off line.size.limit
  lazy val data = Array(
      "basic, padding %B%" -> "Qg==",
      "basic, padding %Ba%" -> "QmE=",
      "basic, padding %Bas%" -> "QmFz",
      "basic, padding %Base%" -> "QmFzZQ==",
      "basic, padding %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "basic, padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "url, padding %B%" -> "Qg==",
      "url, padding %Ba%" -> "QmE=",
      "url, padding %Bas%" -> "QmFz",
      "url, padding %Base%" -> "QmFzZQ==",
      "url, padding %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "url, padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding %B%" -> "Qg==",
      "mime, padding %Ba%" -> "QmE=",
      "mime, padding %Bas%" -> "QmFz",
      "mime, padding %Base%" -> "QmFzZQ==",
      "mime, padding %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hl\r\nbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQg\r\nYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "basic, no padding %B%" -> "Qg",
      "basic, no padding %Ba%" -> "QmE",
      "basic, no padding %Bas%" -> "QmFz",
      "basic, no padding %Base%" -> "QmFzZQ",
      "basic, no padding %Base64 is %" -> "QmFzZTY0IGlzIA",
      "basic, no padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "url, no padding %B%" -> "Qg",
      "url, no padding %Ba%" -> "QmE",
      "url, no padding %Bas%" -> "QmFz",
      "url, no padding %Base%" -> "QmFzZQ",
      "url, no padding %Base64 is %" -> "QmFzZTY0IGlzIA",
      "url, no padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding %B%" -> "Qg",
      "mime, no padding %Ba%" -> "QmE",
      "mime, no padding %Bas%" -> "QmFz",
      "mime, no padding %Base%" -> "QmFzZQ",
      "mime, no padding %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hl\r\nbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQg\r\nYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: -1 delimiters:  %B%" -> "Qg==",
      "mime, padding, line length: -1 delimiters:  %Ba%" -> "QmE=",
      "mime, padding, line length: -1 delimiters:  %Bas%" -> "QmFz",
      "mime, padding, line length: -1 delimiters:  %Base%" -> "QmFzZQ==",
      "mime, padding, line length: -1 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: -1 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 0 delimiters:  %B%" -> "Qg==",
      "mime, padding, line length: 0 delimiters:  %Ba%" -> "QmE=",
      "mime, padding, line length: 0 delimiters:  %Bas%" -> "QmFz",
      "mime, padding, line length: 0 delimiters:  %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 0 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: 0 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 4 delimiters:  %B%" -> "Qg==",
      "mime, padding, line length: 4 delimiters:  %Ba%" -> "QmE=",
      "mime, padding, line length: 4 delimiters:  %Bas%" -> "QmFz",
      "mime, padding, line length: 4 delimiters:  %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 4 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: 4 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 5 delimiters:  %B%" -> "Qg==",
      "mime, padding, line length: 5 delimiters:  %Ba%" -> "QmE=",
      "mime, padding, line length: 5 delimiters:  %Bas%" -> "QmFz",
      "mime, padding, line length: 5 delimiters:  %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 5 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: 5 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 9 delimiters:  %B%" -> "Qg==",
      "mime, padding, line length: 9 delimiters:  %Ba%" -> "QmE=",
      "mime, padding, line length: 9 delimiters:  %Bas%" -> "QmFz",
      "mime, padding, line length: 9 delimiters:  %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 9 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: 9 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: -1 delimiters: @ %B%" -> "Qg==",
      "mime, padding, line length: -1 delimiters: @ %Ba%" -> "QmE=",
      "mime, padding, line length: -1 delimiters: @ %Bas%" -> "QmFz",
      "mime, padding, line length: -1 delimiters: @ %Base%" -> "QmFzZQ==",
      "mime, padding, line length: -1 delimiters: @ %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: -1 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 0 delimiters: @ %B%" -> "Qg==",
      "mime, padding, line length: 0 delimiters: @ %Ba%" -> "QmE=",
      "mime, padding, line length: 0 delimiters: @ %Bas%" -> "QmFz",
      "mime, padding, line length: 0 delimiters: @ %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 0 delimiters: @ %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: 0 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 4 delimiters: @ %B%" -> "Qg==",
      "mime, padding, line length: 4 delimiters: @ %Ba%" -> "QmE=",
      "mime, padding, line length: 4 delimiters: @ %Bas%" -> "QmFz",
      "mime, padding, line length: 4 delimiters: @ %Base%" -> "QmFz@ZQ==",
      "mime, padding, line length: 4 delimiters: @ %Base64 is %" -> "QmFz@ZTY0@IGlz@IA==",
      "mime, padding, line length: 4 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u",
      "mime, padding, line length: 5 delimiters: @ %B%" -> "Qg==",
      "mime, padding, line length: 5 delimiters: @ %Ba%" -> "QmE=",
      "mime, padding, line length: 5 delimiters: @ %Bas%" -> "QmFz",
      "mime, padding, line length: 5 delimiters: @ %Base%" -> "QmFz@ZQ==",
      "mime, padding, line length: 5 delimiters: @ %Base64 is %" -> "QmFz@ZTY0@IGlz@IA==",
      "mime, padding, line length: 5 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u",
      "mime, padding, line length: 9 delimiters: @ %B%" -> "Qg==",
      "mime, padding, line length: 9 delimiters: @ %Ba%" -> "QmE=",
      "mime, padding, line length: 9 delimiters: @ %Bas%" -> "QmFz",
      "mime, padding, line length: 9 delimiters: @ %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 9 delimiters: @ %Base64 is %" -> "QmFzZTY0@IGlzIA==",
      "mime, padding, line length: 9 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0@IGlzIGEg@Z3JvdXAg@b2Ygc2lt@aWxhciBi@aW5hcnkt@dG8tdGV4@dCBlbmNv@ZGluZyBz@Y2hlbWVz@IHRoYXQg@cmVwcmVz@ZW50IGJp@bmFyeSBk@YXRhIGlu@IGFuIEFT@Q0lJIHN0@cmluZyBm@b3JtYXQg@YnkgdHJh@bnNsYXRp@bmcgaXQg@aW50byBh@IHJhZGl4@LTY0IHJl@cHJlc2Vu@dGF0aW9u",
      "mime, padding, line length: -1 delimiters: @$ %B%" -> "Qg==",
      "mime, padding, line length: -1 delimiters: @$ %Ba%" -> "QmE=",
      "mime, padding, line length: -1 delimiters: @$ %Bas%" -> "QmFz",
      "mime, padding, line length: -1 delimiters: @$ %Base%" -> "QmFzZQ==",
      "mime, padding, line length: -1 delimiters: @$ %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: -1 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 0 delimiters: @$ %B%" -> "Qg==",
      "mime, padding, line length: 0 delimiters: @$ %Ba%" -> "QmE=",
      "mime, padding, line length: 0 delimiters: @$ %Bas%" -> "QmFz",
      "mime, padding, line length: 0 delimiters: @$ %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 0 delimiters: @$ %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: 0 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 4 delimiters: @$ %B%" -> "Qg==",
      "mime, padding, line length: 4 delimiters: @$ %Ba%" -> "QmE=",
      "mime, padding, line length: 4 delimiters: @$ %Bas%" -> "QmFz",
      "mime, padding, line length: 4 delimiters: @$ %Base%" -> "QmFz@$ZQ==",
      "mime, padding, line length: 4 delimiters: @$ %Base64 is %" -> "QmFz@$ZTY0@$IGlz@$IA==",
      "mime, padding, line length: 4 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u",
      "mime, padding, line length: 5 delimiters: @$ %B%" -> "Qg==",
      "mime, padding, line length: 5 delimiters: @$ %Ba%" -> "QmE=",
      "mime, padding, line length: 5 delimiters: @$ %Bas%" -> "QmFz",
      "mime, padding, line length: 5 delimiters: @$ %Base%" -> "QmFz@$ZQ==",
      "mime, padding, line length: 5 delimiters: @$ %Base64 is %" -> "QmFz@$ZTY0@$IGlz@$IA==",
      "mime, padding, line length: 5 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u",
      "mime, padding, line length: 9 delimiters: @$ %B%" -> "Qg==",
      "mime, padding, line length: 9 delimiters: @$ %Ba%" -> "QmE=",
      "mime, padding, line length: 9 delimiters: @$ %Bas%" -> "QmFz",
      "mime, padding, line length: 9 delimiters: @$ %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 9 delimiters: @$ %Base64 is %" -> "QmFzZTY0@$IGlzIA==",
      "mime, padding, line length: 9 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0@$IGlzIGEg@$Z3JvdXAg@$b2Ygc2lt@$aWxhciBi@$aW5hcnkt@$dG8tdGV4@$dCBlbmNv@$ZGluZyBz@$Y2hlbWVz@$IHRoYXQg@$cmVwcmVz@$ZW50IGJp@$bmFyeSBk@$YXRhIGlu@$IGFuIEFT@$Q0lJIHN0@$cmluZyBm@$b3JtYXQg@$YnkgdHJh@$bnNsYXRp@$bmcgaXQg@$aW50byBh@$IHJhZGl4@$LTY0IHJl@$cHJlc2Vu@$dGF0aW9u",
      "mime, padding, line length: -1 delimiters: @$* %B%" -> "Qg==",
      "mime, padding, line length: -1 delimiters: @$* %Ba%" -> "QmE=",
      "mime, padding, line length: -1 delimiters: @$* %Bas%" -> "QmFz",
      "mime, padding, line length: -1 delimiters: @$* %Base%" -> "QmFzZQ==",
      "mime, padding, line length: -1 delimiters: @$* %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: -1 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 0 delimiters: @$* %B%" -> "Qg==",
      "mime, padding, line length: 0 delimiters: @$* %Ba%" -> "QmE=",
      "mime, padding, line length: 0 delimiters: @$* %Bas%" -> "QmFz",
      "mime, padding, line length: 0 delimiters: @$* %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 0 delimiters: @$* %Base64 is %" -> "QmFzZTY0IGlzIA==",
      "mime, padding, line length: 0 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, padding, line length: 4 delimiters: @$* %B%" -> "Qg==",
      "mime, padding, line length: 4 delimiters: @$* %Ba%" -> "QmE=",
      "mime, padding, line length: 4 delimiters: @$* %Bas%" -> "QmFz",
      "mime, padding, line length: 4 delimiters: @$* %Base%" -> "QmFz@$*ZQ==",
      "mime, padding, line length: 4 delimiters: @$* %Base64 is %" -> "QmFz@$*ZTY0@$*IGlz@$*IA==",
      "mime, padding, line length: 4 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u",
      "mime, padding, line length: 5 delimiters: @$* %B%" -> "Qg==",
      "mime, padding, line length: 5 delimiters: @$* %Ba%" -> "QmE=",
      "mime, padding, line length: 5 delimiters: @$* %Bas%" -> "QmFz",
      "mime, padding, line length: 5 delimiters: @$* %Base%" -> "QmFz@$*ZQ==",
      "mime, padding, line length: 5 delimiters: @$* %Base64 is %" -> "QmFz@$*ZTY0@$*IGlz@$*IA==",
      "mime, padding, line length: 5 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u",
      "mime, padding, line length: 9 delimiters: @$* %B%" -> "Qg==",
      "mime, padding, line length: 9 delimiters: @$* %Ba%" -> "QmE=",
      "mime, padding, line length: 9 delimiters: @$* %Bas%" -> "QmFz",
      "mime, padding, line length: 9 delimiters: @$* %Base%" -> "QmFzZQ==",
      "mime, padding, line length: 9 delimiters: @$* %Base64 is %" -> "QmFzZTY0@$*IGlzIA==",
      "mime, padding, line length: 9 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0@$*IGlzIGEg@$*Z3JvdXAg@$*b2Ygc2lt@$*aWxhciBi@$*aW5hcnkt@$*dG8tdGV4@$*dCBlbmNv@$*ZGluZyBz@$*Y2hlbWVz@$*IHRoYXQg@$*cmVwcmVz@$*ZW50IGJp@$*bmFyeSBk@$*YXRhIGlu@$*IGFuIEFT@$*Q0lJIHN0@$*cmluZyBm@$*b3JtYXQg@$*YnkgdHJh@$*bnNsYXRp@$*bmcgaXQg@$*aW50byBh@$*IHJhZGl4@$*LTY0IHJl@$*cHJlc2Vu@$*dGF0aW9u",
      "mime, no padding, line length: -1 delimiters:  %B%" -> "Qg",
      "mime, no padding, line length: -1 delimiters:  %Ba%" -> "QmE",
      "mime, no padding, line length: -1 delimiters:  %Bas%" -> "QmFz",
      "mime, no padding, line length: -1 delimiters:  %Base%" -> "QmFzZQ",
      "mime, no padding, line length: -1 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: -1 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 0 delimiters:  %B%" -> "Qg",
      "mime, no padding, line length: 0 delimiters:  %Ba%" -> "QmE",
      "mime, no padding, line length: 0 delimiters:  %Bas%" -> "QmFz",
      "mime, no padding, line length: 0 delimiters:  %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 0 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: 0 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 4 delimiters:  %B%" -> "Qg",
      "mime, no padding, line length: 4 delimiters:  %Ba%" -> "QmE",
      "mime, no padding, line length: 4 delimiters:  %Bas%" -> "QmFz",
      "mime, no padding, line length: 4 delimiters:  %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 4 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: 4 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 5 delimiters:  %B%" -> "Qg",
      "mime, no padding, line length: 5 delimiters:  %Ba%" -> "QmE",
      "mime, no padding, line length: 5 delimiters:  %Bas%" -> "QmFz",
      "mime, no padding, line length: 5 delimiters:  %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 5 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: 5 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 9 delimiters:  %B%" -> "Qg",
      "mime, no padding, line length: 9 delimiters:  %Ba%" -> "QmE",
      "mime, no padding, line length: 9 delimiters:  %Bas%" -> "QmFz",
      "mime, no padding, line length: 9 delimiters:  %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 9 delimiters:  %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: 9 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: -1 delimiters: @ %B%" -> "Qg",
      "mime, no padding, line length: -1 delimiters: @ %Ba%" -> "QmE",
      "mime, no padding, line length: -1 delimiters: @ %Bas%" -> "QmFz",
      "mime, no padding, line length: -1 delimiters: @ %Base%" -> "QmFzZQ",
      "mime, no padding, line length: -1 delimiters: @ %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: -1 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 0 delimiters: @ %B%" -> "Qg",
      "mime, no padding, line length: 0 delimiters: @ %Ba%" -> "QmE",
      "mime, no padding, line length: 0 delimiters: @ %Bas%" -> "QmFz",
      "mime, no padding, line length: 0 delimiters: @ %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 0 delimiters: @ %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: 0 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 4 delimiters: @ %B%" -> "Qg",
      "mime, no padding, line length: 4 delimiters: @ %Ba%" -> "QmE",
      "mime, no padding, line length: 4 delimiters: @ %Bas%" -> "QmFz",
      "mime, no padding, line length: 4 delimiters: @ %Base%" -> "QmFz@ZQ",
      "mime, no padding, line length: 4 delimiters: @ %Base64 is %" -> "QmFz@ZTY0@IGlz@IA",
      "mime, no padding, line length: 4 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u",
      "mime, no padding, line length: 5 delimiters: @ %B%" -> "Qg",
      "mime, no padding, line length: 5 delimiters: @ %Ba%" -> "QmE",
      "mime, no padding, line length: 5 delimiters: @ %Bas%" -> "QmFz",
      "mime, no padding, line length: 5 delimiters: @ %Base%" -> "QmFz@ZQ",
      "mime, no padding, line length: 5 delimiters: @ %Base64 is %" -> "QmFz@ZTY0@IGlz@IA",
      "mime, no padding, line length: 5 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u",
      "mime, no padding, line length: 9 delimiters: @ %B%" -> "Qg",
      "mime, no padding, line length: 9 delimiters: @ %Ba%" -> "QmE",
      "mime, no padding, line length: 9 delimiters: @ %Bas%" -> "QmFz",
      "mime, no padding, line length: 9 delimiters: @ %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 9 delimiters: @ %Base64 is %" -> "QmFzZTY0@IGlzIA",
      "mime, no padding, line length: 9 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0@IGlzIGEg@Z3JvdXAg@b2Ygc2lt@aWxhciBi@aW5hcnkt@dG8tdGV4@dCBlbmNv@ZGluZyBz@Y2hlbWVz@IHRoYXQg@cmVwcmVz@ZW50IGJp@bmFyeSBk@YXRhIGlu@IGFuIEFT@Q0lJIHN0@cmluZyBm@b3JtYXQg@YnkgdHJh@bnNsYXRp@bmcgaXQg@aW50byBh@IHJhZGl4@LTY0IHJl@cHJlc2Vu@dGF0aW9u",
      "mime, no padding, line length: -1 delimiters: @$ %B%" -> "Qg",
      "mime, no padding, line length: -1 delimiters: @$ %Ba%" -> "QmE",
      "mime, no padding, line length: -1 delimiters: @$ %Bas%" -> "QmFz",
      "mime, no padding, line length: -1 delimiters: @$ %Base%" -> "QmFzZQ",
      "mime, no padding, line length: -1 delimiters: @$ %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: -1 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 0 delimiters: @$ %B%" -> "Qg",
      "mime, no padding, line length: 0 delimiters: @$ %Ba%" -> "QmE",
      "mime, no padding, line length: 0 delimiters: @$ %Bas%" -> "QmFz",
      "mime, no padding, line length: 0 delimiters: @$ %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 0 delimiters: @$ %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: 0 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 4 delimiters: @$ %B%" -> "Qg",
      "mime, no padding, line length: 4 delimiters: @$ %Ba%" -> "QmE",
      "mime, no padding, line length: 4 delimiters: @$ %Bas%" -> "QmFz",
      "mime, no padding, line length: 4 delimiters: @$ %Base%" -> "QmFz@$ZQ",
      "mime, no padding, line length: 4 delimiters: @$ %Base64 is %" -> "QmFz@$ZTY0@$IGlz@$IA",
      "mime, no padding, line length: 4 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u",
      "mime, no padding, line length: 5 delimiters: @$ %B%" -> "Qg",
      "mime, no padding, line length: 5 delimiters: @$ %Ba%" -> "QmE",
      "mime, no padding, line length: 5 delimiters: @$ %Bas%" -> "QmFz",
      "mime, no padding, line length: 5 delimiters: @$ %Base%" -> "QmFz@$ZQ",
      "mime, no padding, line length: 5 delimiters: @$ %Base64 is %" -> "QmFz@$ZTY0@$IGlz@$IA",
      "mime, no padding, line length: 5 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u",
      "mime, no padding, line length: 9 delimiters: @$ %B%" -> "Qg",
      "mime, no padding, line length: 9 delimiters: @$ %Ba%" -> "QmE",
      "mime, no padding, line length: 9 delimiters: @$ %Bas%" -> "QmFz",
      "mime, no padding, line length: 9 delimiters: @$ %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 9 delimiters: @$ %Base64 is %" -> "QmFzZTY0@$IGlzIA",
      "mime, no padding, line length: 9 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0@$IGlzIGEg@$Z3JvdXAg@$b2Ygc2lt@$aWxhciBi@$aW5hcnkt@$dG8tdGV4@$dCBlbmNv@$ZGluZyBz@$Y2hlbWVz@$IHRoYXQg@$cmVwcmVz@$ZW50IGJp@$bmFyeSBk@$YXRhIGlu@$IGFuIEFT@$Q0lJIHN0@$cmluZyBm@$b3JtYXQg@$YnkgdHJh@$bnNsYXRp@$bmcgaXQg@$aW50byBh@$IHJhZGl4@$LTY0IHJl@$cHJlc2Vu@$dGF0aW9u",
      "mime, no padding, line length: -1 delimiters: @$* %B%" -> "Qg",
      "mime, no padding, line length: -1 delimiters: @$* %Ba%" -> "QmE",
      "mime, no padding, line length: -1 delimiters: @$* %Bas%" -> "QmFz",
      "mime, no padding, line length: -1 delimiters: @$* %Base%" -> "QmFzZQ",
      "mime, no padding, line length: -1 delimiters: @$* %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: -1 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 0 delimiters: @$* %B%" -> "Qg",
      "mime, no padding, line length: 0 delimiters: @$* %Ba%" -> "QmE",
      "mime, no padding, line length: 0 delimiters: @$* %Bas%" -> "QmFz",
      "mime, no padding, line length: 0 delimiters: @$* %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 0 delimiters: @$* %Base64 is %" -> "QmFzZTY0IGlzIA",
      "mime, no padding, line length: 0 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u",
      "mime, no padding, line length: 4 delimiters: @$* %B%" -> "Qg",
      "mime, no padding, line length: 4 delimiters: @$* %Ba%" -> "QmE",
      "mime, no padding, line length: 4 delimiters: @$* %Bas%" -> "QmFz",
      "mime, no padding, line length: 4 delimiters: @$* %Base%" -> "QmFz@$*ZQ",
      "mime, no padding, line length: 4 delimiters: @$* %Base64 is %" -> "QmFz@$*ZTY0@$*IGlz@$*IA",
      "mime, no padding, line length: 4 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u",
      "mime, no padding, line length: 5 delimiters: @$* %B%" -> "Qg",
      "mime, no padding, line length: 5 delimiters: @$* %Ba%" -> "QmE",
      "mime, no padding, line length: 5 delimiters: @$* %Bas%" -> "QmFz",
      "mime, no padding, line length: 5 delimiters: @$* %Base%" -> "QmFz@$*ZQ",
      "mime, no padding, line length: 5 delimiters: @$* %Base64 is %" -> "QmFz@$*ZTY0@$*IGlz@$*IA",
      "mime, no padding, line length: 5 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u",
      "mime, no padding, line length: 9 delimiters: @$* %B%" -> "Qg",
      "mime, no padding, line length: 9 delimiters: @$* %Ba%" -> "QmE",
      "mime, no padding, line length: 9 delimiters: @$* %Bas%" -> "QmFz",
      "mime, no padding, line length: 9 delimiters: @$* %Base%" -> "QmFzZQ",
      "mime, no padding, line length: 9 delimiters: @$* %Base64 is %" -> "QmFzZTY0@$*IGlzIA",
      "mime, no padding, line length: 9 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%" -> "QmFzZTY0@$*IGlzIGEg@$*Z3JvdXAg@$*b2Ygc2lt@$*aWxhciBi@$*aW5hcnkt@$*dG8tdGV4@$*dCBlbmNv@$*ZGluZyBz@$*Y2hlbWVz@$*IHRoYXQg@$*cmVwcmVz@$*ZW50IGJp@$*bmFyeSBk@$*YXRhIGlu@$*IGFuIEFT@$*Q0lJIHN0@$*cmluZyBm@$*b3JtYXQg@$*YnkgdHJh@$*bnNsYXRp@$*bmcgaXQg@$*aW50byBh@$*IHJhZGl4@$*LTY0IHJl@$*cHJlc2Vu@$*dGF0aW9u"
  )
  // scalastyle:on line.size.limit
}
