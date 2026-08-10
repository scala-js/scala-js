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

package org.scalajs.testsuite.javalib.io

import java.io._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform

class InputStreamTest extends CommonStreamsTests {

  def mkStream(seq: Seq[Int]): InputStream =
    new SeqInputStreamForTest(seq)

  /** InputStream that only ever reads max bytes at once */
  def chunkedStream(max: Int, seq: Seq[Int]): InputStream = new SeqInputStreamForTest(seq) {
    require(max > 0)

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      val newLen = Math.min(max, len)
      super.read(b, off, newLen)
    }
  }

  /** InputStream that only ever skips max bytes at once */
  def lowSkipStream(max: Int, seq: Seq[Int]): InputStream = new SeqInputStreamForTest(seq) {
    require(max > 0)

    override def skip(n: Long): Long =
      super.skip(Math.min(max.toLong, n).toInt)
  }

  def emptyStream(): InputStream = new InputStream {
    def read(): Int = -1
  }

  private def assertBytesEqual(expect: Seq[Int], got: Array[Byte]) =
    assertArrayEquals(expect.toArray.map(_.toByte), got)

  @Test def readArrayByte(): Unit = {
    val stream = mkStream(1 to 200)

    val buf = new Array[Byte](50)

    // Should read first 50 bytes (next: 51)
    assertEquals(50, stream.read(buf))
    assertArrayEquals((1 to 50).toArray.map(_.toByte), buf)

    // Should read another 20 (next: 71)
    assertEquals(20, stream.read(buf, 10, 20))
    assertArrayEquals(((1 to 10) ++ (51 to 70) ++ (31 to 50)).toArray.map(_.toByte), buf)

    // Test some Exception conditions
    assertThrows(classOf[IndexOutOfBoundsException], stream.read(buf, -1, 10))
    assertThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 0, -1))
    assertThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 10, 100))

    // Buffer should be unmodified
    assertArrayEquals(
        ((1 to 10) ++ (51 to 70) ++ (31 to 50)).toArray.map(_.toByte), buf)

    // Should read nothing (next: 71)
    assertEquals(0, stream.read(buf, 10, 0))
    assertArrayEquals(
        ((1 to 10) ++ (51 to 70) ++ (31 to 50)).toArray.map(_.toByte), buf)

    // Skip 40 bytes (next: 111)
    assertEquals(40L, stream.skip(40))

    // Read 50 bytes, should wrap (next: 161)
    assertEquals(50, stream.read(buf))
    assertArrayEquals(
        ((111 to 127) ++ (-128 to -96)).toArray.map(_.toByte), buf)

    // Read 45 bytes, should read 40 (next: EOF)
    assertEquals(40, stream.read(buf, 5, 45))
    assertArrayEquals(
        ((111 to 115) ++ (-95 to -56) ++ (-100 to -96)).toArray.map(_.toByte), buf)

    // Read 50 bytes, should read nothing
    assertEquals(-1, stream.read(buf))
    assertEquals(0, stream.read(buf, 0, 0))
    assertArrayEquals(
        ((111 to 115) ++ (-95 to -56) ++ (-100 to -96)).toArray.map(_.toByte), buf)
  }

  @Test def skip(): Unit = {
    val stream = mkStream(1 to 10)

    assertEquals(5L, stream.skip(5))
    assertEquals(6, stream.read())
    assertEquals(1L, stream.skip(1))
    assertEquals(8, stream.read())
    assertEquals(0L, stream.skip(-5))
    assertEquals(9, stream.read())
    assertEquals(0L, stream.skip(0))
    assertEquals(10, stream.read())
    assertEquals(0L, stream.skip(10))
  }

  @Test def readAllBytes(): Unit = {
    assertBytesEqual(0 until 100, chunkedStream(10, 0 until 100).readAllBytes())
    assertBytesEqual(0 until 4000, chunkedStream(100, 0 until 4000).readAllBytes())
    assertBytesEqual(Nil, emptyStream().readAllBytes())
  }

  @Test def readNBytes(): Unit = {
    assertBytesEqual(0 until 20, chunkedStream(10, 0 until 100).readNBytes(20))
    assertBytesEqual(0 until 100, chunkedStream(10, 0 until 100).readNBytes(200))
    assertBytesEqual(Nil, chunkedStream(10, 0 until 100).readNBytes(0))
    assertBytesEqual(Nil, emptyStream().readNBytes(1000))

    // test buffer growing
    assertBytesEqual(0 until 2000, chunkedStream(200, 0 until 2000).readNBytes(2000))
    assertBytesEqual(0 until 20000, chunkedStream(2000, 0 until 20000).readNBytes(20000))

    assertThrows(classOf[IllegalArgumentException], emptyStream().readNBytes(-1))
  }

  @Test def readNBytesBuf(): Unit = {
    val buf = new Array[Byte](30)

    chunkedStream(10, 0 until 100).readNBytes(buf, 2, 22)

    assertBytesEqual(Seq.fill(2)(0) ++ (0 until 22) ++ Seq.fill(6)(0), buf)
  }

  @Test def transferTo(): Unit = {
    val stream = chunkedStream(10, 0 until 100)
    val out = new ByteArrayOutputStream()
    stream.transferTo(out)

    assertBytesEqual(0 until 100, out.toByteArray())
  }

  @Test def transferToThrowsNPE(): Unit = {
    assumeTrue("assumed compliant NPEs", Platform.hasCompliantNullPointers)
    // nothing to write, should still throw.
    assertThrows(classOf[NullPointerException], emptyStream().transferTo(null))
  }

  @Test def nullInputStream(): Unit = {
    val stream = InputStream.nullInputStream()

    assertEquals(-1, stream.read())
    assertEquals(0, stream.skip(10))
    assertBytesEqual(Nil, stream.readAllBytes())

    stream.close()
    stream.close() // shouldn't throw

    assertThrows(classOf[IOException], stream.available())
    assertThrows(classOf[IOException], stream.read())
    assertThrows(classOf[IOException], stream.read(new Array[Byte](1))) // JDK doesn't throw if len == 0
    assertThrows(classOf[IOException], stream.read(new Array[Byte](1), 0, 1)) // JDK doesn't throw if len == 0
    assertThrows(classOf[IOException], stream.readAllBytes())
    assertThrows(classOf[IOException], stream.readNBytes(new Array[Byte](1), 0, 0))
    assertThrows(classOf[IOException], stream.readNBytes(0))
    assertThrows(classOf[IOException], stream.skip(1))
    assertThrows(classOf[IOException], stream.skip(0))
    assertThrows(classOf[IOException], stream.skipNBytes(0))
    assertThrows(classOf[IOException], stream.transferTo(new ByteArrayOutputStream))
  }

  @Test def skipNBytes(): Unit = {
    val stream = lowSkipStream(10, 0 until 100)

    assertBytesEqual(0 until 15, stream.readNBytes(15))

    stream.skipNBytes(25)

    assertBytesEqual(40 until 55, stream.readNBytes(15))

    stream.skipNBytes(45)

    assertBytesEqual(Nil, stream.readNBytes(20))
  }

  @Test def skipNBytesThrowsOnEOF(): Unit =
    assertThrows(classOf[EOFException], lowSkipStream(10, 0 until 11).skipNBytes(20))

  @Test def skipNBytesThrowsIfBadSkip(): Unit = {
    class BadSkipStream(skipResult: Long) extends InputStream {
      def read(): Int = 0
      override def skip(n: Long): Long = skipResult
    }

    assertThrows(classOf[IOException], new BadSkipStream(-1).skipNBytes(1))
    assertThrows(classOf[IOException], new BadSkipStream(2).skipNBytes(1))

    // Must not invoke skip if non-positive count
    new BadSkipStream(2).skipNBytes(0)
    new BadSkipStream(2).skipNBytes(-1)
  }
}
