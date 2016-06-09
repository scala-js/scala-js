/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.io

import java.io._

import scala.language.implicitConversions

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

trait CommonStreamsTests {

  def mkStream(seq: Seq[Int]): InputStream

  private val length = 50
  private def newStream: InputStream = mkStream(1 to length)

  private implicit def seqToArray(seq: Seq[Int]): Array[Byte] =
    seq.toArray.map(_.toByte)

  @Test def should_provide_read()(): Unit = {
    val stream = newStream

    for (i <- 1 to length)
      assertEquals(i, stream.read())

    for (_ <- 1 to 5)
      assertEquals(-1, stream.read())
  }

  @Test def should_provide_read_from_buf(): Unit = {
    val stream = newStream
    val buf = new Array[Byte](10)

    assertEquals(10, stream.read(buf))
    assertArrayEquals(1 to 10, buf)

    assertEquals(35L, stream.skip(35))

    assertEquals(5, stream.read(buf))
    assertArrayEquals((46 to 50) ++ (6 to 10), buf)

    assertEquals(-1, stream.read(buf))
    assertEquals(-1, stream.read())
  }

  @Test def should_provide_full_argument_read(): Unit = {
    val stream = newStream
    val buf = new Array[Byte](20)

    assertEquals(5, stream.read(buf, 10, 5))
    assertArrayEquals(Seq.fill(10)(0) ++ (1 to 5) ++ Seq.fill(5)(0), buf)

    assertEquals(20, stream.read(buf, 0, 20))
    assertArrayEquals(6 to 25, buf)

    assertEquals(0, stream.read(buf, 10, 0))
    assertArrayEquals(6 to 25, buf)

    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, -1, 0))
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 0, -1))
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 100, 0))
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 10, 100))
    assertArrayEquals(6 to 25, buf)

    assertEquals(20L, stream.skip(20))

    assertEquals(5, stream.read(buf, 0, 10))
    assertArrayEquals((46 to 50) ++ (11 to 25), buf)

    assertEquals(-1, stream.read(buf, 0, 10))
    assertArrayEquals((46 to 50) ++ (11 to 25), buf)

  }

  @Test def should_provide_available(): Unit = {
    val stream = newStream

    def mySkip(n: Int) = for (_ <- 1 to n) assertNotEquals(stream.read(), -1)
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

  @Test def should_provide_skip(): Unit = {
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

  @Test def should_return_true_from_markSupported(): Unit = {
    assertTrue(newStream.markSupported)
  }

  @Test def should_provide_no_op_close(): Unit = {
    val stream = newStream

    for (i <- 1 to length) {
      stream.close()
      assertEquals(i, stream.read())
    }
  }

  @Test def should_provide_mark_and_reset(): Unit = {
    val stream = newStream

    def read(range: Range) = for (i <- range) assertEquals(i, stream.read())

    read(1 to 10)
    stream.reset() // mark must be 0 at creation
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
