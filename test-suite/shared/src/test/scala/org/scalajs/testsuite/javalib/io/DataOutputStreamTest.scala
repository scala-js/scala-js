package org.scalajs.testsuite.javalib.io

import java.io._

import org.junit._
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

object DataOutputStreamTest {
  class DataOutputStreamWrittenAccess(out: OutputStream)
      extends DataOutputStream(out) {
    def getWritten(): Int = written
    def setWritten(v: Int): Unit = written = v
  }

  class CheckerOutputStream extends ByteArrayOutputStream {
    val dataOutputStream = new DataOutputStreamWrittenAccess(this)

    assertEquals(0, dataOutputStream.getWritten())

    /* Artificially skew `written` to make sure `size()` uses `written` as
     * its implementation.
     */
    dataOutputStream.setWritten(100)

    def check(expectedBytes: Int*): Unit = {
      val expectedWritten = 100 + expectedBytes.size
      assertEquals(expectedWritten, dataOutputStream.getWritten())
      assertEquals(expectedWritten, dataOutputStream.size())
      assertArrayEquals(expectedBytes.map(_.toByte).toArray, toByteArray())
    }
  }
}

class DataOutputStreamTest {
  import DataOutputStreamTest._

  private def newStream(): (DataOutputStream, CheckerOutputStream) = {
    val checker = new CheckerOutputStream
    (checker.dataOutputStream, checker)
  }

  @Test def construct(): Unit = {
    val (stream, checker) = newStream()
    checker.check()
  }

  @Test def writePrimitiveByte(): Unit = {
    val (stream, checker) = newStream()
    stream.write(42)
    checker.check(42)
  }

  @Test def writePrimitiveBytes(): Unit = {
    val (stream, checker) = newStream()
    stream.write(Array[Byte](5, 3, 1, 43, 6, 12, 2), 1, 4)
    checker.check(3, 1, 43, 6)
  }

  @Test def writeBoolean(): Unit = {
    val (stream, checker) = newStream()

    stream.writeBoolean(true)
    stream.writeBoolean(true)
    stream.writeBoolean(false)

    checker.check(1, 1, 0)
  }

  @Test def writeByte(): Unit = {
    val (stream, checker) = newStream()

    stream.writeByte(0)
    stream.writeByte(1)
    stream.writeByte(-15)
    stream.writeByte(125)
    stream.writeByte(53)

    checker.check(0, 1, -15, 125, 53)
  }

  @Test def writeShort(): Unit = {
    val (stream, checker) = newStream()

    stream.writeShort(453)
    stream.writeShort(-43)
    stream.writeShort(Short.MaxValue)
    stream.writeShort(6320)
    stream.writeShort(0)
    stream.writeShort(Short.MinValue)
    stream.writeShort(-346)
    stream.writeShort(34)

    checker.check(
        0x01, 0xc5,
        0xff, 0xd5,
        0x7f, 0xff,
        0x18, 0xb0,
        0x00, 0x00,
        0x80, 0x00,
        0xfe, 0xa6,
        0x00, 0x22
    )
  }

  @Test def writeChar(): Unit = {
    val (stream, checker) = newStream()

    for (c <- "Höllö Wărȴđ")
      stream.writeChar(c)

    checker.check(
        0x00, 0x48, // H
        0x00, 0xF6, // ö
        0x00, 0x6C, // l
        0x00, 0x6C, // l
        0x00, 0xF6, // ö
        0x00, 0x20, // [space]
        0x00, 0x57, // W
        0x01, 0x03, // ă
        0x00, 0x72, // r
        0x02, 0x34, // ȴ
        0x01, 0x11  // đ
    )
  }

  @Test def writeInt(): Unit = {
    val (stream, checker) = newStream()

    stream.writeInt(0)
    stream.writeInt(Int.MaxValue)
    stream.writeInt(-4)
    stream.writeInt(83)
    stream.writeInt(35234)
    stream.writeInt(-97539)
    stream.writeInt(Int.MinValue)
    stream.writeInt(1)

    checker.check(
        0x00, 0x00, 0x00, 0x00,
        0x7f, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xfc,
        0x00, 0x00, 0x00, 0x53,
        0x00, 0x00, 0x89, 0xa2,
        0xff, 0xfe, 0x82, 0xfd,
        0x80, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01
    )
  }

  @Test def writeLong(): Unit = {
    val (stream, checker) = newStream()

    stream.writeLong(546372873646234L)
    stream.writeLong(-32451234L)
    stream.writeLong(Long.MaxValue)
    stream.writeLong(0L)
    stream.writeLong(-1L)
    stream.writeLong(Long.MinValue)
    stream.writeLong(-34524L)
    stream.writeLong(47L)

    checker.check(
        0x00, 0x01, 0xf0, 0xec, 0x59, 0x0c, 0x70, 0x9a,
        0xff, 0xff, 0xff, 0xff, 0xfe, 0x10, 0xd5, 0x5e,
        0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x79, 0x24,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2f
    )
  }

  @Test def writeFloat(): Unit = {
    val (stream, checker) = newStream()

    stream.writeFloat(-1.0f)
    stream.writeFloat(4563.564f)
    stream.writeFloat(-0.0f)
    stream.writeFloat(0.0f)
    stream.writeFloat(Float.NaN)
    stream.writeFloat(Float.PositiveInfinity)
    stream.writeFloat(-0.002f)
    stream.writeFloat(Float.NegativeInfinity)

    checker.check(
        0xbf, 0x80, 0x00, 0x00,
        0x45, 0x8e, 0x9c, 0x83,
        0x80, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x7f, 0xc0, 0x00, 0x00,
        0x7f, 0x80, 0x00, 0x00,
        0xbb, 0x03, 0x12, 0x6f,
        0xff, 0x80, 0x00, 0x00
    )
  }

  @Test def writeDouble(): Unit = {
    val (stream, checker) = newStream()

    stream.writeDouble(0.7)
    stream.writeDouble(345672.067923)
    stream.writeDouble(-3472.0673)
    stream.writeDouble(Double.NaN)
    stream.writeDouble(Double.PositiveInfinity)
    stream.writeDouble(-7.0134674)
    stream.writeDouble(Double.NegativeInfinity)
    stream.writeDouble(0.0)

    checker.check(
        0x3f, 0xe6, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66,
        0x41, 0x15, 0x19, 0x20, 0x45, 0x8d, 0x9b, 0x5f,
        0xc0, 0xab, 0x20, 0x22, 0x75, 0x25, 0x46, 0x0b,
        0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xc0, 0x1c, 0x0d, 0xca, 0x65, 0xea, 0x3f, 0xa4,
        0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    )
  }

  @Test def writeBytesOfString(): Unit = {
    val (stream, checker) = newStream()

    stream.writeBytes("Höllö Wărȴđ")

    checker.check(
        0x48, // H
        0xF6, // ö
        0x6C, // l
        0x6C, // l
        0xF6, // ö
        0x20, // [space]
        0x57, // W
        0x03, // ă
        0x72, // r
        0x34, // ȴ
        0x11  // đ
    )
  }

  @Test def writeCharsOfString(): Unit = {
    val (stream, checker) = newStream()

    stream.writeChars("Höllö Wărȴđ")

    checker.check(
        0x00, 0x48, // H
        0x00, 0xF6, // ö
        0x00, 0x6C, // l
        0x00, 0x6C, // l
        0x00, 0xF6, // ö
        0x00, 0x20, // [space]
        0x00, 0x57, // W
        0x01, 0x03, // ă
        0x00, 0x72, // r
        0x02, 0x34, // ȴ
        0x01, 0x11  // đ
    )
  }

  @Test def writeUTF(): Unit = {
    val (stream, checker) = newStream()

    stream.writeUTF("Höllö Wărȴđ")
    stream.writeUTF("poo -> 💩")
    stream.writeUTF("愛")

    checker.check(
        0x00, 0x10, 0x48, 0xc3, 0xb6, 0x6c, 0x6c, 0xc3,
        0xb6, 0x20, 0x57, 0xc4, 0x83, 0x72, 0xc8, 0xb4,
        0xc4, 0x91, 0x00, 0x0d, 0x70, 0x6f, 0x6f, 0x20,
        0x2d, 0x3e, 0x20, 0xed, 0xa0, 0xbd, 0xed, 0xb2,
        0xa9, 0x00, 0x03, 0xe6, 0x84, 0x9b
    )
  }

  @Test def writeUTFTooLong(): Unit = {
    val (stream, checker) = newStream()

    var longString = "aaa"
    while (longString.length < 0x10000)
      longString = longString + longString

    assertThrows(classOf[UTFDataFormatException], {
      stream.writeUTF(longString)
    })
  }
}
