/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.typedarray

import org.scalajs.testsuite.utils.Requires

import scala.language.implicitConversions

import java.io.InputStream

import scala.scalajs.js
import scala.scalajs.js.typedarray._

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

/** Tests for our implementation of java.io._ stream classes */
trait ArrayBufferInputStreamTest {

  def byteArray(a: TraversableOnce[Int]): Array[Byte] = {
    a.toArray.map(_.toByte)
  }

  def mkStream(seq: Seq[Int]): InputStream

  private val length = 50

  private def newStream: InputStream = mkStream(1 to length)

  @Test def read(): Unit = {
    val stream = newStream

    for (i <- 1 to length)
      assertEquals(i, stream.read())

    for (_ <- 1 to 5)
      assertEquals(-1, stream.read())
  }

  @Test def read_buf(): Unit = {
    val stream = newStream
    val buf = new Array[Byte](10)

    assertEquals(10, stream.read(buf))
    assertArrayEquals(byteArray(1 to 10), buf)

    assertEquals(35L, stream.skip(35))

    assertEquals(5, stream.read(buf))
    assertArrayEquals(byteArray((46 to 50) ++ (6 to 10)), buf)

    assertEquals(-1, stream.read(buf))
    assertEquals(-1, stream.read())
  }

  @Test def read_full_argument(): Unit = {
    val stream = newStream
    val buf = new Array[Byte](20)

    assertEquals(5, stream.read(buf, 10, 5))
    assertArrayEquals(byteArray(Seq.fill(10)(0) ++ (1 to 5) ++ Seq.fill(5)(0)), buf)

    assertEquals(20, stream.read(buf, 0, 20))
    assertArrayEquals(byteArray(6 to 25), buf)

    assertEquals(0, stream.read(buf, 10, 0))
    assertArrayEquals(byteArray(6 to 25), buf)

    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, -1, 0))
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 0, -1))
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 100, 0))
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 10, 100))
    assertArrayEquals(byteArray(6 to 25), buf)

    assertEquals(20L, stream.skip(20))

    assertEquals(5, stream.read(buf, 0, 10))
    assertArrayEquals(byteArray((46 to 50) ++ (11 to 25)), buf)

    assertEquals(-1, stream.read(buf, 0, 10))
    assertEquals(0, stream.read(buf, 0,  0))
    assertArrayEquals(byteArray((46 to 50) ++ (11 to 25)), buf)
  }

  @Test def available(): Unit = {
    val stream = newStream

    def mySkip(n: Int) = for (_ <- 1 to n) assertNotEquals(-1, stream.read())
    def check(n: Int) = assertEquals(n, stream.available)

    check(50)
    mySkip(5)
    check(45)
    assertEquals(10L, stream.skip(10))
    check(35)
    mySkip(30)
    check(5)
    assertEquals(5L, stream.skip(20))
    check(0)
  }

  @Test def skip(): Unit = {
    val stream = newStream

    assertEquals(7L, stream.skip(7))

    for (i <- 8 to 32)
      assertEquals(i, stream.read())

    assertEquals(0L, stream.skip(0))
    assertEquals(33, stream.read())
    assertEquals(0L, stream.skip(-4))
    assertEquals(34, stream.read())

    assertEquals(16L, stream.skip(30))
    assertEquals(0L, stream.skip(30))
  }

  @Test def markSupported(): Unit = {
    assertTrue(newStream.markSupported)
  }

  @Test def close(): Unit = {
    val stream = newStream

    for (i <- 1 to length) {
      stream.close()
      assertEquals(i, stream.read())
    }
  }

  @Test def mark_reset(): Unit = {
    val stream = newStream

    def read(range: Range) = for (i <- range) assertEquals(i, stream.read())

    read(1 to 10)
    stream.reset()  // mark must be 0 at creation
    read(1 to 5)
    stream.mark(length)
    read(6 to 22)
    stream.reset()
    read(6 to 20)
    stream.reset()
    read(6 to 25)
    stream.reset()
    assertEquals(40L, stream.skip(40))
    stream.mark(length)
    read(46 to 50)
    stream.reset()
    read(46 to 50)
    stream.mark(length)
    assertEquals(-1, stream.read())
    stream.reset()
    assertEquals(-1, stream.read())
  }

  @Test def should_return_positive_integers_when_calling_read(): Unit = {
    val stream = mkStream(Seq(-1, -2, -3))
    assertEquals(255, stream.read())
    assertEquals(254, stream.read())
    assertEquals(253, stream.read())
    assertEquals(-1, stream.read())
  }
}

object ArrayBufferInputStreamWithoutOffsetTest extends Requires.TypedArray

class ArrayBufferInputStreamWithoutOffsetTest extends ArrayBufferInputStreamTest {
  def mkStream(seq: Seq[Int]): InputStream = {
    import js.JSConverters._
    new ArrayBufferInputStream(new Int8Array(seq.toJSArray).buffer)
  }
}

object ArrayBufferInputStreamWithOffsetTest extends Requires.TypedArray

class ArrayBufferInputStreamWithOffsetTest extends ArrayBufferInputStreamTest {
  def mkStream(seq: Seq[Int]): InputStream = {
    import js.JSConverters._
    val off = 100
    val data = new Int8Array(seq.size + off)
    data.set(seq.toJSArray, off)

    new ArrayBufferInputStream(data.buffer, off, seq.size)
  }
}
