package org.scalajs.testsuite.javalib.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util.Base64
import java.util.Base64.Decoder

import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.utils.AssertThrows._

class Base64Test {
  import Base64Test._

  // --------------------------------------------------------------------------
  // ENCODERS
  // --------------------------------------------------------------------------

  @Test def encodeToString(): Unit = {
    val results = encoders.map{case (name, in, enc) => name -> enc.encodeToString(in)}
    assertEquals("calculated count doesn't match computed count", encodedResults.length, results.size)
    results.zip(encodedResults).foreach{ case ((name, enc), exp) =>
      assertEquals(s"encodeToString doesn't match for: $name", exp, enc)
    }
  }

  @Test def encodeOneArray(): Unit = {
    val results = encoders.map{case (name, in, enc) => name -> enc.encode(in)}
    assertEquals("calculated count doesn't match computed count", encodedResults.length, results.size)
    results.zip(encodedResults).foreach{ case ((name, enc), exp) =>
      assertEquals(s"encode array doesn't match for: $name", exp, new String(enc, StandardCharsets.ISO_8859_1))
    }
  }

  @Test def encodeTwoArrays(): Unit = {
    encoders.zip(encodedResults).foreach{case ((name, in, enc), exp) =>
      val dst = new Array[Byte](exp.length + 10) // array too big on purpose
      val written = enc.encode(in, dst)
      assertEquals(s"number of written bytes doesn't match for: $name", exp.length, written)
      val content = dst.slice(0, written)
      val rlt = new String(content, StandardCharsets.ISO_8859_1)
      assertEquals(s"encode array into array doesn't match for: $name", exp, rlt)
    }
  }

  @Test def encodeTwoArraysThrowsWithTooSmallDestination(): Unit = {
    val in = "Man"
    val dst = new Array[Byte](3) // too small
    assertThrows(classOf[IllegalArgumentException], Base64.getEncoder.encode(in.getBytes, dst))
  }

  @Test def encodeByteBuffer(): Unit = {
    encoders.zip(encodedResults).foreach{case ((name, in, enc), exp) =>
      var rlt = enc.encode(ByteBuffer.wrap(in))
      assertEquals(s"byte buffers don't match for: $name", exp, new String(rlt.array(), StandardCharsets.ISO_8859_1))
      val bb = ByteBuffer.allocate(in.length + 2)
      bb.position(2)
      bb.mark()
      bb.put(in)
      bb.reset()
      rlt = enc.encode(bb)
      assertEquals(s"byte buffers don't match for: $name", exp, new String(rlt.array(), StandardCharsets.ISO_8859_1))
    }
  }

  @Test def encodeOutputStream(): Unit = {
    encoders.zip(encodedResults).foreach { case ((name, in, enc), exp) =>
      val baos = new ByteArrayOutputStream()
      val out = enc.wrap(baos)
      out.write(in(0))
      out.write(in, 1, in.length - 1)
      out.close()
      val rlt = new String(baos.toByteArray, StandardCharsets.ISO_8859_1)
      assertEquals(s"output stream result doesn't match for: $name", exp, rlt)
    }
  }

  // @Test // Fails on the JVM (bug no bug id yet)
  def encodeOutputStreamFailsOnJVM(): Unit = {
    val encoder = Base64.getMimeEncoder(1, Array('@')) // the 1 will create a buggy encoder on the jvm
    val input = "Man"
    val expected = "TWFu"
    val ba = new ByteArrayOutputStream()
    val out = encoder.wrap(ba)
    out.write(input.getBytes)
    out.close()
    val result = new String(ba.toByteArray)
    assertEquals("outputstream should be initialized correctly", expected, result)
  }

  @Test def encodeOutputStreamTooMuch(): Unit = {
    val enc = Base64.getEncoder
    val out = enc.wrap(new ByteArrayOutputStream())
    assertThrows(classOf[IndexOutOfBoundsException], out.write(Array.empty[Byte], 0, 5))
  }

  @Test def testIllegalLineSeparator(): Unit = {
    assertThrows(classOf[IllegalArgumentException], Base64.getMimeEncoder(8, Array[Byte]('A')))
  }

  // --------------------------------------------------------------------------
  // DECODERS
  // --------------------------------------------------------------------------

  @Test def decodeFromString(): Unit = {
    assertEquals("encoded data count doesn't match input count", encoders.length, decodersAndInputs.length)
    encodedResults.zip(decodersAndInputs).foreach{case (encoded, (decoder, in)) =>
      assertArrayEquals("decoded doesn't match expected",
        in.getBytes(StandardCharsets.ISO_8859_1), decoder.decode(encoded))
    }
  }

  @Test def decodeFromArray(): Unit = {
    encodedResults.zip(decodersAndInputs).foreach{case (encoded, (decoder, in)) =>
      assertEquals("decoded doesn't match expected",
        in, new String(decoder.decode(encoded.getBytes(StandardCharsets.ISO_8859_1)), StandardCharsets.ISO_8859_1))
    }
  }

  @Test def decodeFromArrayToDest(): Unit = {
    encodedResults.zip(decodersAndInputs).foreach{case (encoded, (decoder, in)) =>
      val dst = new Array[Byte](in.length)
      val encInBytes = encoded.getBytes(StandardCharsets.ISO_8859_1)
      val dec = decoder.decode(encInBytes, dst)
      assertEquals("decoded count doesn't match expected", in.length, dec)
      assertArrayEquals("decoded array doesn't match expected", in.getBytes(StandardCharsets.ISO_8859_1), dst)
    }
  }

  @Test def decodeFromByteBuffer(): Unit = {
    encodedResults.zip(decodersAndInputs).foreach { case (encoded, (decoder, in)) =>
      val bb = ByteBuffer.wrap(encoded.getBytes(StandardCharsets.ISO_8859_1))
      val decoded = decoder.decode(bb)
      val array = new Array[Byte](decoded.limit)
      decoded.get(array)
      assertArrayEquals("decoded byte buffer doesn't match expected",
        in.getBytes(StandardCharsets.ISO_8859_1), array)
    }
  }

  @Test def decodeToArrayTooSmall(): Unit = {
    val encoded = "TWFu"
    val dst = new Array[Byte](2) // too small
    assertThrows(classOf[IllegalArgumentException], Base64.getDecoder.decode(encoded.getBytes, dst))
  }

  @Test def decodeIllegalCharacter(): Unit = {
    val encoded = "TWE*"
    assertThrows(classOf[IllegalArgumentException], Base64.getDecoder.decode(encoded))
    assertThrows(classOf[IllegalArgumentException], Base64.getUrlDecoder.decode(encoded))
    assertEquals("mime encoder should allow illegals", "Ma",
      new String(Base64.getMimeDecoder.decode(encoded), StandardCharsets.ISO_8859_1))
  }

  // @Test // This will fail on the JVM (bug JDK-8176043)
  def decodeIllegalPadding(): Unit = {
    val encoded = "TQ=*"
    assertThrows(classOf[IllegalArgumentException], Base64.getDecoder.decode(encoded))
    assertThrows(classOf[IllegalArgumentException], Base64.getUrlDecoder.decode(encoded))
    assertEquals("mime encoder should allow illegal paddings", "M",
      new String(Base64.getMimeDecoder.decode(encoded), StandardCharsets.ISO_8859_1))
  }

  @Test def decodeInputStream(): Unit = {
    encodedResults.zip(decodersAndInputs).foreach { case (encoded, (decoder, expected)) =>
      val byteInstream = new ByteArrayInputStream(encoded.getBytes(StandardCharsets.ISO_8859_1))
      val instream = decoder.wrap(byteInstream)
      val read = new Array[Byte](expected.length)
      instream.read(read)
      while(instream.read() != -1) () // read padding
      instream.close()
      assertEquals("inputstream read value not as expected", expected, new String(read, StandardCharsets.ISO_8859_1))
    }
  }

  @Test def decodeIllegalsInputStream(): Unit = {
    val encoded = "TQ=*"
    assertThrows(classOf[IOException], decodeInputStream(basic, encoded))
    assertThrows(classOf[IOException], decodeInputStream(url, encoded))
    // Fails on the JVM (same bug as above JDK-8176043)
    //  assertEquals("mime encoder should allow illegal paddings", "M", decodeInputStream(mime, encoded))
    assertThrows(classOf[IOException], decodeInputStream(mime, "TWFu", Array('a')))
    assertThrows(classOf[IOException], decodeInputStream(basic, "TWFu", Array(0.toByte)))
  }

  def decodeInputStream(decoder: Decoder, input: String, dangling: Array[Byte] = Array.empty): String = {
    val bytes = input.getBytes ++ dangling
    val stream = decoder.wrap(new ByteArrayInputStream(bytes))
    val tmp = new Array[Byte](bytes.length)
    val read = stream.read(tmp)
    new String(tmp, 0, read)
  }

}

object Base64Test {

  private val input: Array[Byte] =
    ("Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string " +
      "format by translating it into a radix-64 representation").getBytes(StandardCharsets.ISO_8859_1)

  private val inputLengths = Seq(1, 2, 3, 4, 10, input.length)
  private val lineDelimChars = "@$*"
  private val lineLengths = Seq(-1,0,4,5,9)

  private val stdEncPadding = Seq("basic, padding" -> Base64.getEncoder,
    "url, padding" -> Base64.getUrlEncoder,
    "mime, padding" -> Base64.getMimeEncoder)

  private val stdEncNoPadding = stdEncPadding.map{case (name, enc) =>
    name.replace("padding", "no padding") -> enc.withoutPadding()
  }

  private val lineDelimitersWithLineLengths: Seq[(String, Int)] = for {
    i <- 0 to lineDelimChars.length
    l <- lineLengths
  } yield lineDelimChars.take(i) -> l

  private val customEncPadding = lineDelimitersWithLineLengths.map{case (delim, ll) =>
    s"mime, padding, line length: $ll delimiters: $delim" -> Base64.getMimeEncoder(ll, delim.getBytes)
  }

  private val customEncNoPadding = customEncPadding.map{case (name, enc) =>
    name.replace("padding", "no padding") -> enc.withoutPadding()
  }

  private val allEncoders = stdEncPadding ++ stdEncNoPadding ++ customEncPadding ++ customEncNoPadding

  val encoders = for {
    (name, enc) <- allEncoders
    length <- inputLengths
  } yield (s"$name", input.take(length), enc)

  // drop the first empty elem
  lazy val inputAndResults = result.split("//").toSeq.drop(1).map(_.trim)
  lazy val inputs = inputAndResults.zipWithIndex.collect{case (e,i) if i%2 == 0 => e}
  lazy val encodedResults = inputAndResults.zipWithIndex.collect{case (e,i) if i%2 == 1 => e}

  private val basic = Base64.getDecoder
  private val url = Base64.getUrlDecoder
  private val mime = Base64.getMimeDecoder

  def getDecoderAndInput(text: String): (Decoder, String) = {
    val decoder =
      if (text.contains("basic")) basic
      else if (text.contains("url")) url
      else if (text.contains("mime")) mime
      else throw new IllegalArgumentException(s"no decoder found in string $text")
    val input = text.replaceAll(".*%(.*)%", "$1")
    decoder -> input
  }

  lazy val decodersAndInputs = inputs.map(getDecoderAndInput)

  val result =
    """
      |// basic, padding %B%//
      |Qg==
      |// basic, padding %Ba%//
      |QmE=
      |// basic, padding %Bas%//
      |QmFz
      |// basic, padding %Base%//
      |QmFzZQ==
      |// basic, padding %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// basic, padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// url, padding %B%//
      |Qg==
      |// url, padding %Ba%//
      |QmE=
      |// url, padding %Bas%//
      |QmFz
      |// url, padding %Base%//
      |QmFzZQ==
      |// url, padding %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// url, padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding %B%//
      |Qg==
      |// mime, padding %Ba%//
      |QmE=
      |// mime, padding %Bas%//
      |QmFz
      |// mime, padding %Base%//
      |QmFzZQ==
      |// mime, padding %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hl----
      |bWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQg----
      |YnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// basic, no padding %B%//
      |Qg
      |// basic, no padding %Ba%//
      |QmE
      |// basic, no padding %Bas%//
      |QmFz
      |// basic, no padding %Base%//
      |QmFzZQ
      |// basic, no padding %Base64 is %//
      |QmFzZTY0IGlzIA
      |// basic, no padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// url, no padding %B%//
      |Qg
      |// url, no padding %Ba%//
      |QmE
      |// url, no padding %Bas%//
      |QmFz
      |// url, no padding %Base%//
      |QmFzZQ
      |// url, no padding %Base64 is %//
      |QmFzZTY0IGlzIA
      |// url, no padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding %B%//
      |Qg
      |// mime, no padding %Ba%//
      |QmE
      |// mime, no padding %Bas%//
      |QmFz
      |// mime, no padding %Base%//
      |QmFzZQ
      |// mime, no padding %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hl----
      |bWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQg----
      |YnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: -1 delimiters:  %B%//
      |Qg==
      |// mime, padding, line length: -1 delimiters:  %Ba%//
      |QmE=
      |// mime, padding, line length: -1 delimiters:  %Bas%//
      |QmFz
      |// mime, padding, line length: -1 delimiters:  %Base%//
      |QmFzZQ==
      |// mime, padding, line length: -1 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: -1 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 0 delimiters:  %B%//
      |Qg==
      |// mime, padding, line length: 0 delimiters:  %Ba%//
      |QmE=
      |// mime, padding, line length: 0 delimiters:  %Bas%//
      |QmFz
      |// mime, padding, line length: 0 delimiters:  %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 0 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: 0 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 4 delimiters:  %B%//
      |Qg==
      |// mime, padding, line length: 4 delimiters:  %Ba%//
      |QmE=
      |// mime, padding, line length: 4 delimiters:  %Bas%//
      |QmFz
      |// mime, padding, line length: 4 delimiters:  %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 4 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: 4 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 5 delimiters:  %B%//
      |Qg==
      |// mime, padding, line length: 5 delimiters:  %Ba%//
      |QmE=
      |// mime, padding, line length: 5 delimiters:  %Bas%//
      |QmFz
      |// mime, padding, line length: 5 delimiters:  %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 5 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: 5 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 9 delimiters:  %B%//
      |Qg==
      |// mime, padding, line length: 9 delimiters:  %Ba%//
      |QmE=
      |// mime, padding, line length: 9 delimiters:  %Bas%//
      |QmFz
      |// mime, padding, line length: 9 delimiters:  %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 9 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: 9 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: -1 delimiters: @ %B%//
      |Qg==
      |// mime, padding, line length: -1 delimiters: @ %Ba%//
      |QmE=
      |// mime, padding, line length: -1 delimiters: @ %Bas%//
      |QmFz
      |// mime, padding, line length: -1 delimiters: @ %Base%//
      |QmFzZQ==
      |// mime, padding, line length: -1 delimiters: @ %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: -1 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 0 delimiters: @ %B%//
      |Qg==
      |// mime, padding, line length: 0 delimiters: @ %Ba%//
      |QmE=
      |// mime, padding, line length: 0 delimiters: @ %Bas%//
      |QmFz
      |// mime, padding, line length: 0 delimiters: @ %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 0 delimiters: @ %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: 0 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 4 delimiters: @ %B%//
      |Qg==
      |// mime, padding, line length: 4 delimiters: @ %Ba%//
      |QmE=
      |// mime, padding, line length: 4 delimiters: @ %Bas%//
      |QmFz
      |// mime, padding, line length: 4 delimiters: @ %Base%//
      |QmFz@ZQ==
      |// mime, padding, line length: 4 delimiters: @ %Base64 is %//
      |QmFz@ZTY0@IGlz@IA==
      |// mime, padding, line length: 4 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u
      |// mime, padding, line length: 5 delimiters: @ %B%//
      |Qg==
      |// mime, padding, line length: 5 delimiters: @ %Ba%//
      |QmE=
      |// mime, padding, line length: 5 delimiters: @ %Bas%//
      |QmFz
      |// mime, padding, line length: 5 delimiters: @ %Base%//
      |QmFz@ZQ==
      |// mime, padding, line length: 5 delimiters: @ %Base64 is %//
      |QmFz@ZTY0@IGlz@IA==
      |// mime, padding, line length: 5 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u
      |// mime, padding, line length: 9 delimiters: @ %B%//
      |Qg==
      |// mime, padding, line length: 9 delimiters: @ %Ba%//
      |QmE=
      |// mime, padding, line length: 9 delimiters: @ %Bas%//
      |QmFz
      |// mime, padding, line length: 9 delimiters: @ %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 9 delimiters: @ %Base64 is %//
      |QmFzZTY0@IGlzIA==
      |// mime, padding, line length: 9 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0@IGlzIGEg@Z3JvdXAg@b2Ygc2lt@aWxhciBi@aW5hcnkt@dG8tdGV4@dCBlbmNv@ZGluZyBz@Y2hlbWVz@IHRoYXQg@cmVwcmVz@ZW50IGJp@bmFyeSBk@YXRhIGlu@IGFuIEFT@Q0lJIHN0@cmluZyBm@b3JtYXQg@YnkgdHJh@bnNsYXRp@bmcgaXQg@aW50byBh@IHJhZGl4@LTY0IHJl@cHJlc2Vu@dGF0aW9u
      |// mime, padding, line length: -1 delimiters: @$ %B%//
      |Qg==
      |// mime, padding, line length: -1 delimiters: @$ %Ba%//
      |QmE=
      |// mime, padding, line length: -1 delimiters: @$ %Bas%//
      |QmFz
      |// mime, padding, line length: -1 delimiters: @$ %Base%//
      |QmFzZQ==
      |// mime, padding, line length: -1 delimiters: @$ %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: -1 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 0 delimiters: @$ %B%//
      |Qg==
      |// mime, padding, line length: 0 delimiters: @$ %Ba%//
      |QmE=
      |// mime, padding, line length: 0 delimiters: @$ %Bas%//
      |QmFz
      |// mime, padding, line length: 0 delimiters: @$ %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 0 delimiters: @$ %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: 0 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 4 delimiters: @$ %B%//
      |Qg==
      |// mime, padding, line length: 4 delimiters: @$ %Ba%//
      |QmE=
      |// mime, padding, line length: 4 delimiters: @$ %Bas%//
      |QmFz
      |// mime, padding, line length: 4 delimiters: @$ %Base%//
      |QmFz@$ZQ==
      |// mime, padding, line length: 4 delimiters: @$ %Base64 is %//
      |QmFz@$ZTY0@$IGlz@$IA==
      |// mime, padding, line length: 4 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u
      |// mime, padding, line length: 5 delimiters: @$ %B%//
      |Qg==
      |// mime, padding, line length: 5 delimiters: @$ %Ba%//
      |QmE=
      |// mime, padding, line length: 5 delimiters: @$ %Bas%//
      |QmFz
      |// mime, padding, line length: 5 delimiters: @$ %Base%//
      |QmFz@$ZQ==
      |// mime, padding, line length: 5 delimiters: @$ %Base64 is %//
      |QmFz@$ZTY0@$IGlz@$IA==
      |// mime, padding, line length: 5 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u
      |// mime, padding, line length: 9 delimiters: @$ %B%//
      |Qg==
      |// mime, padding, line length: 9 delimiters: @$ %Ba%//
      |QmE=
      |// mime, padding, line length: 9 delimiters: @$ %Bas%//
      |QmFz
      |// mime, padding, line length: 9 delimiters: @$ %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 9 delimiters: @$ %Base64 is %//
      |QmFzZTY0@$IGlzIA==
      |// mime, padding, line length: 9 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0@$IGlzIGEg@$Z3JvdXAg@$b2Ygc2lt@$aWxhciBi@$aW5hcnkt@$dG8tdGV4@$dCBlbmNv@$ZGluZyBz@$Y2hlbWVz@$IHRoYXQg@$cmVwcmVz@$ZW50IGJp@$bmFyeSBk@$YXRhIGlu@$IGFuIEFT@$Q0lJIHN0@$cmluZyBm@$b3JtYXQg@$YnkgdHJh@$bnNsYXRp@$bmcgaXQg@$aW50byBh@$IHJhZGl4@$LTY0IHJl@$cHJlc2Vu@$dGF0aW9u
      |// mime, padding, line length: -1 delimiters: @$* %B%//
      |Qg==
      |// mime, padding, line length: -1 delimiters: @$* %Ba%//
      |QmE=
      |// mime, padding, line length: -1 delimiters: @$* %Bas%//
      |QmFz
      |// mime, padding, line length: -1 delimiters: @$* %Base%//
      |QmFzZQ==
      |// mime, padding, line length: -1 delimiters: @$* %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: -1 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 0 delimiters: @$* %B%//
      |Qg==
      |// mime, padding, line length: 0 delimiters: @$* %Ba%//
      |QmE=
      |// mime, padding, line length: 0 delimiters: @$* %Bas%//
      |QmFz
      |// mime, padding, line length: 0 delimiters: @$* %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 0 delimiters: @$* %Base64 is %//
      |QmFzZTY0IGlzIA==
      |// mime, padding, line length: 0 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, padding, line length: 4 delimiters: @$* %B%//
      |Qg==
      |// mime, padding, line length: 4 delimiters: @$* %Ba%//
      |QmE=
      |// mime, padding, line length: 4 delimiters: @$* %Bas%//
      |QmFz
      |// mime, padding, line length: 4 delimiters: @$* %Base%//
      |QmFz@$*ZQ==
      |// mime, padding, line length: 4 delimiters: @$* %Base64 is %//
      |QmFz@$*ZTY0@$*IGlz@$*IA==
      |// mime, padding, line length: 4 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u
      |// mime, padding, line length: 5 delimiters: @$* %B%//
      |Qg==
      |// mime, padding, line length: 5 delimiters: @$* %Ba%//
      |QmE=
      |// mime, padding, line length: 5 delimiters: @$* %Bas%//
      |QmFz
      |// mime, padding, line length: 5 delimiters: @$* %Base%//
      |QmFz@$*ZQ==
      |// mime, padding, line length: 5 delimiters: @$* %Base64 is %//
      |QmFz@$*ZTY0@$*IGlz@$*IA==
      |// mime, padding, line length: 5 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u
      |// mime, padding, line length: 9 delimiters: @$* %B%//
      |Qg==
      |// mime, padding, line length: 9 delimiters: @$* %Ba%//
      |QmE=
      |// mime, padding, line length: 9 delimiters: @$* %Bas%//
      |QmFz
      |// mime, padding, line length: 9 delimiters: @$* %Base%//
      |QmFzZQ==
      |// mime, padding, line length: 9 delimiters: @$* %Base64 is %//
      |QmFzZTY0@$*IGlzIA==
      |// mime, padding, line length: 9 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0@$*IGlzIGEg@$*Z3JvdXAg@$*b2Ygc2lt@$*aWxhciBi@$*aW5hcnkt@$*dG8tdGV4@$*dCBlbmNv@$*ZGluZyBz@$*Y2hlbWVz@$*IHRoYXQg@$*cmVwcmVz@$*ZW50IGJp@$*bmFyeSBk@$*YXRhIGlu@$*IGFuIEFT@$*Q0lJIHN0@$*cmluZyBm@$*b3JtYXQg@$*YnkgdHJh@$*bnNsYXRp@$*bmcgaXQg@$*aW50byBh@$*IHJhZGl4@$*LTY0IHJl@$*cHJlc2Vu@$*dGF0aW9u
      |// mime, no padding, line length: -1 delimiters:  %B%//
      |Qg
      |// mime, no padding, line length: -1 delimiters:  %Ba%//
      |QmE
      |// mime, no padding, line length: -1 delimiters:  %Bas%//
      |QmFz
      |// mime, no padding, line length: -1 delimiters:  %Base%//
      |QmFzZQ
      |// mime, no padding, line length: -1 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: -1 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 0 delimiters:  %B%//
      |Qg
      |// mime, no padding, line length: 0 delimiters:  %Ba%//
      |QmE
      |// mime, no padding, line length: 0 delimiters:  %Bas%//
      |QmFz
      |// mime, no padding, line length: 0 delimiters:  %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 0 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: 0 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 4 delimiters:  %B%//
      |Qg
      |// mime, no padding, line length: 4 delimiters:  %Ba%//
      |QmE
      |// mime, no padding, line length: 4 delimiters:  %Bas%//
      |QmFz
      |// mime, no padding, line length: 4 delimiters:  %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 4 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: 4 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 5 delimiters:  %B%//
      |Qg
      |// mime, no padding, line length: 5 delimiters:  %Ba%//
      |QmE
      |// mime, no padding, line length: 5 delimiters:  %Bas%//
      |QmFz
      |// mime, no padding, line length: 5 delimiters:  %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 5 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: 5 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 9 delimiters:  %B%//
      |Qg
      |// mime, no padding, line length: 9 delimiters:  %Ba%//
      |QmE
      |// mime, no padding, line length: 9 delimiters:  %Bas%//
      |QmFz
      |// mime, no padding, line length: 9 delimiters:  %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 9 delimiters:  %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: 9 delimiters:  %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: -1 delimiters: @ %B%//
      |Qg
      |// mime, no padding, line length: -1 delimiters: @ %Ba%//
      |QmE
      |// mime, no padding, line length: -1 delimiters: @ %Bas%//
      |QmFz
      |// mime, no padding, line length: -1 delimiters: @ %Base%//
      |QmFzZQ
      |// mime, no padding, line length: -1 delimiters: @ %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: -1 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 0 delimiters: @ %B%//
      |Qg
      |// mime, no padding, line length: 0 delimiters: @ %Ba%//
      |QmE
      |// mime, no padding, line length: 0 delimiters: @ %Bas%//
      |QmFz
      |// mime, no padding, line length: 0 delimiters: @ %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 0 delimiters: @ %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: 0 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 4 delimiters: @ %B%//
      |Qg
      |// mime, no padding, line length: 4 delimiters: @ %Ba%//
      |QmE
      |// mime, no padding, line length: 4 delimiters: @ %Bas%//
      |QmFz
      |// mime, no padding, line length: 4 delimiters: @ %Base%//
      |QmFz@ZQ
      |// mime, no padding, line length: 4 delimiters: @ %Base64 is %//
      |QmFz@ZTY0@IGlz@IA
      |// mime, no padding, line length: 4 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u
      |// mime, no padding, line length: 5 delimiters: @ %B%//
      |Qg
      |// mime, no padding, line length: 5 delimiters: @ %Ba%//
      |QmE
      |// mime, no padding, line length: 5 delimiters: @ %Bas%//
      |QmFz
      |// mime, no padding, line length: 5 delimiters: @ %Base%//
      |QmFz@ZQ
      |// mime, no padding, line length: 5 delimiters: @ %Base64 is %//
      |QmFz@ZTY0@IGlz@IA
      |// mime, no padding, line length: 5 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@ZTY0@IGlz@IGEg@Z3Jv@dXAg@b2Yg@c2lt@aWxh@ciBi@aW5h@cnkt@dG8t@dGV4@dCBl@bmNv@ZGlu@ZyBz@Y2hl@bWVz@IHRo@YXQg@cmVw@cmVz@ZW50@IGJp@bmFy@eSBk@YXRh@IGlu@IGFu@IEFT@Q0lJ@IHN0@cmlu@ZyBm@b3Jt@YXQg@Ynkg@dHJh@bnNs@YXRp@bmcg@aXQg@aW50@byBh@IHJh@ZGl4@LTY0@IHJl@cHJl@c2Vu@dGF0@aW9u
      |// mime, no padding, line length: 9 delimiters: @ %B%//
      |Qg
      |// mime, no padding, line length: 9 delimiters: @ %Ba%//
      |QmE
      |// mime, no padding, line length: 9 delimiters: @ %Bas%//
      |QmFz
      |// mime, no padding, line length: 9 delimiters: @ %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 9 delimiters: @ %Base64 is %//
      |QmFzZTY0@IGlzIA
      |// mime, no padding, line length: 9 delimiters: @ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0@IGlzIGEg@Z3JvdXAg@b2Ygc2lt@aWxhciBi@aW5hcnkt@dG8tdGV4@dCBlbmNv@ZGluZyBz@Y2hlbWVz@IHRoYXQg@cmVwcmVz@ZW50IGJp@bmFyeSBk@YXRhIGlu@IGFuIEFT@Q0lJIHN0@cmluZyBm@b3JtYXQg@YnkgdHJh@bnNsYXRp@bmcgaXQg@aW50byBh@IHJhZGl4@LTY0IHJl@cHJlc2Vu@dGF0aW9u
      |// mime, no padding, line length: -1 delimiters: @$ %B%//
      |Qg
      |// mime, no padding, line length: -1 delimiters: @$ %Ba%//
      |QmE
      |// mime, no padding, line length: -1 delimiters: @$ %Bas%//
      |QmFz
      |// mime, no padding, line length: -1 delimiters: @$ %Base%//
      |QmFzZQ
      |// mime, no padding, line length: -1 delimiters: @$ %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: -1 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 0 delimiters: @$ %B%//
      |Qg
      |// mime, no padding, line length: 0 delimiters: @$ %Ba%//
      |QmE
      |// mime, no padding, line length: 0 delimiters: @$ %Bas%//
      |QmFz
      |// mime, no padding, line length: 0 delimiters: @$ %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 0 delimiters: @$ %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: 0 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 4 delimiters: @$ %B%//
      |Qg
      |// mime, no padding, line length: 4 delimiters: @$ %Ba%//
      |QmE
      |// mime, no padding, line length: 4 delimiters: @$ %Bas%//
      |QmFz
      |// mime, no padding, line length: 4 delimiters: @$ %Base%//
      |QmFz@$ZQ
      |// mime, no padding, line length: 4 delimiters: @$ %Base64 is %//
      |QmFz@$ZTY0@$IGlz@$IA
      |// mime, no padding, line length: 4 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u
      |// mime, no padding, line length: 5 delimiters: @$ %B%//
      |Qg
      |// mime, no padding, line length: 5 delimiters: @$ %Ba%//
      |QmE
      |// mime, no padding, line length: 5 delimiters: @$ %Bas%//
      |QmFz
      |// mime, no padding, line length: 5 delimiters: @$ %Base%//
      |QmFz@$ZQ
      |// mime, no padding, line length: 5 delimiters: @$ %Base64 is %//
      |QmFz@$ZTY0@$IGlz@$IA
      |// mime, no padding, line length: 5 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@$ZTY0@$IGlz@$IGEg@$Z3Jv@$dXAg@$b2Yg@$c2lt@$aWxh@$ciBi@$aW5h@$cnkt@$dG8t@$dGV4@$dCBl@$bmNv@$ZGlu@$ZyBz@$Y2hl@$bWVz@$IHRo@$YXQg@$cmVw@$cmVz@$ZW50@$IGJp@$bmFy@$eSBk@$YXRh@$IGlu@$IGFu@$IEFT@$Q0lJ@$IHN0@$cmlu@$ZyBm@$b3Jt@$YXQg@$Ynkg@$dHJh@$bnNs@$YXRp@$bmcg@$aXQg@$aW50@$byBh@$IHJh@$ZGl4@$LTY0@$IHJl@$cHJl@$c2Vu@$dGF0@$aW9u
      |// mime, no padding, line length: 9 delimiters: @$ %B%//
      |Qg
      |// mime, no padding, line length: 9 delimiters: @$ %Ba%//
      |QmE
      |// mime, no padding, line length: 9 delimiters: @$ %Bas%//
      |QmFz
      |// mime, no padding, line length: 9 delimiters: @$ %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 9 delimiters: @$ %Base64 is %//
      |QmFzZTY0@$IGlzIA
      |// mime, no padding, line length: 9 delimiters: @$ %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0@$IGlzIGEg@$Z3JvdXAg@$b2Ygc2lt@$aWxhciBi@$aW5hcnkt@$dG8tdGV4@$dCBlbmNv@$ZGluZyBz@$Y2hlbWVz@$IHRoYXQg@$cmVwcmVz@$ZW50IGJp@$bmFyeSBk@$YXRhIGlu@$IGFuIEFT@$Q0lJIHN0@$cmluZyBm@$b3JtYXQg@$YnkgdHJh@$bnNsYXRp@$bmcgaXQg@$aW50byBh@$IHJhZGl4@$LTY0IHJl@$cHJlc2Vu@$dGF0aW9u
      |// mime, no padding, line length: -1 delimiters: @$* %B%//
      |Qg
      |// mime, no padding, line length: -1 delimiters: @$* %Ba%//
      |QmE
      |// mime, no padding, line length: -1 delimiters: @$* %Bas%//
      |QmFz
      |// mime, no padding, line length: -1 delimiters: @$* %Base%//
      |QmFzZQ
      |// mime, no padding, line length: -1 delimiters: @$* %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: -1 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 0 delimiters: @$* %B%//
      |Qg
      |// mime, no padding, line length: 0 delimiters: @$* %Ba%//
      |QmE
      |// mime, no padding, line length: 0 delimiters: @$* %Bas%//
      |QmFz
      |// mime, no padding, line length: 0 delimiters: @$* %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 0 delimiters: @$* %Base64 is %//
      |QmFzZTY0IGlzIA
      |// mime, no padding, line length: 0 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBiaW5hcnktdG8tdGV4dCBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVzZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcgaXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9u
      |// mime, no padding, line length: 4 delimiters: @$* %B%//
      |Qg
      |// mime, no padding, line length: 4 delimiters: @$* %Ba%//
      |QmE
      |// mime, no padding, line length: 4 delimiters: @$* %Bas%//
      |QmFz
      |// mime, no padding, line length: 4 delimiters: @$* %Base%//
      |QmFz@$*ZQ
      |// mime, no padding, line length: 4 delimiters: @$* %Base64 is %//
      |QmFz@$*ZTY0@$*IGlz@$*IA
      |// mime, no padding, line length: 4 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u
      |// mime, no padding, line length: 5 delimiters: @$* %B%//
      |Qg
      |// mime, no padding, line length: 5 delimiters: @$* %Ba%//
      |QmE
      |// mime, no padding, line length: 5 delimiters: @$* %Bas%//
      |QmFz
      |// mime, no padding, line length: 5 delimiters: @$* %Base%//
      |QmFz@$*ZQ
      |// mime, no padding, line length: 5 delimiters: @$* %Base64 is %//
      |QmFz@$*ZTY0@$*IGlz@$*IA
      |// mime, no padding, line length: 5 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFz@$*ZTY0@$*IGlz@$*IGEg@$*Z3Jv@$*dXAg@$*b2Yg@$*c2lt@$*aWxh@$*ciBi@$*aW5h@$*cnkt@$*dG8t@$*dGV4@$*dCBl@$*bmNv@$*ZGlu@$*ZyBz@$*Y2hl@$*bWVz@$*IHRo@$*YXQg@$*cmVw@$*cmVz@$*ZW50@$*IGJp@$*bmFy@$*eSBk@$*YXRh@$*IGlu@$*IGFu@$*IEFT@$*Q0lJ@$*IHN0@$*cmlu@$*ZyBm@$*b3Jt@$*YXQg@$*Ynkg@$*dHJh@$*bnNs@$*YXRp@$*bmcg@$*aXQg@$*aW50@$*byBh@$*IHJh@$*ZGl4@$*LTY0@$*IHJl@$*cHJl@$*c2Vu@$*dGF0@$*aW9u
      |// mime, no padding, line length: 9 delimiters: @$* %B%//
      |Qg
      |// mime, no padding, line length: 9 delimiters: @$* %Ba%//
      |QmE
      |// mime, no padding, line length: 9 delimiters: @$* %Bas%//
      |QmFz
      |// mime, no padding, line length: 9 delimiters: @$* %Base%//
      |QmFzZQ
      |// mime, no padding, line length: 9 delimiters: @$* %Base64 is %//
      |QmFzZTY0@$*IGlzIA
      |// mime, no padding, line length: 9 delimiters: @$* %Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation%//
      |QmFzZTY0@$*IGlzIGEg@$*Z3JvdXAg@$*b2Ygc2lt@$*aWxhciBi@$*aW5hcnkt@$*dG8tdGV4@$*dCBlbmNv@$*ZGluZyBz@$*Y2hlbWVz@$*IHRoYXQg@$*cmVwcmVz@$*ZW50IGJp@$*bmFyeSBk@$*YXRhIGlu@$*IGFuIEFT@$*Q0lJIHN0@$*cmluZyBm@$*b3JtYXQg@$*YnkgdHJh@$*bnNsYXRp@$*bmcgaXQg@$*aW50byBh@$*IHJhZGl4@$*LTY0IHJl@$*cHJlc2Vu@$*dGF0aW9u
    """.stripMargin
      .replaceAll("----\\s*", "\r\n")
  /* That last replaceAll is to make sure that no matter where this file is edited the multiline mime encoded
  lines end in \r\n. The mime version of the encoder adds \r\n at the end of lines*/
}
