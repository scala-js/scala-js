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

class InputStreamTest extends CommonStreamsTests {

  class DummyInputStream(val length: Int) extends InputStream {
    private var i: Int = 0
    def read(): Int = if (i < length) { i += 1; i } else -1
  }

  @Test def should_provide_a_default_implementation_of_read_to_an_array(): Unit = {
    val stream = new DummyInputStream(200)

    val buf = new Array[Byte](50)

    // Should read first 50 bytes (next: 51)
    assertEquals(50, stream.read(buf))
    assertArrayEquals((1 to 50).toArray.map(_.toByte), buf)

    // Should read another 20 (next: 71)
    assertEquals(20, stream.read(buf, 10, 20))
    assertArrayEquals(((1 to 10) ++ (51 to 70) ++ (31 to 50)).toArray.map(_.toByte), buf)

    // Test some Exception conditions
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, -1, 10))
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 0, -1))
    expectThrows(classOf[IndexOutOfBoundsException], stream.read(buf, 10, 100))

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

  @Test def should_provide_a_default_implementation_of_skip(): Unit = {
    val stream = new DummyInputStream(10)

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
}
