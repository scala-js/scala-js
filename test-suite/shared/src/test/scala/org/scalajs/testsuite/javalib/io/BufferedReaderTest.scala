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

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class BufferedReaderTest {

  val str = "line1\nline2\r\n\nline4\rline5"
  def newReader: BufferedReader = new BufferedReader(new StringReader(str), 3)

  @Test def close(): Unit = {
    class UnderlyingReader extends StringReader(str) {
      var closeCount: Int = 0

      override def close(): Unit = {
        closeCount += 1
        /* Do not call super.close(), to ensure IOExceptions come from
         * BufferedReader, and not the underlying reader.
         */
      }
    }

    val underlying = new UnderlyingReader
    val r = new BufferedReader(underlying)
    r.read()
    assertEquals(0, underlying.closeCount)
    r.close()
    assertEquals(1, underlying.closeCount)

    // close() actually prevents further use of the reader
    assertThrows(classOf[IOException], r.mark(1))
    assertThrows(classOf[IOException], r.read())
    assertThrows(classOf[IOException], r.read(new Array[Char](1), 0, 1))
    assertThrows(classOf[IOException], r.read(new Array[Char](1)))
    assertThrows(classOf[IOException], r.readLine())
    assertThrows(classOf[IOException], r.ready())
    assertThrows(classOf[IOException], r.reset())
    assertThrows(classOf[IOException], r.skip(1L))
    assertThrows(classOf[IllegalArgumentException], r.skip(-1L))

    // close() is idempotent
    r.close()
    assertEquals(1, underlying.closeCount)
  }

  @Test def read(): Unit = {
    val r = newReader

    for (c <- str) {
      assertEquals(c, r.read().toChar)
    }
    assertEquals(-1, r.read())
  }

  @Test def readArrayChar(): Unit = {
    var read = 0
    val r = newReader
    val buf = new Array[Char](15)

    // twice to force filling internal buffer
    for (_ <- 0 to 1) {
      val len = r.read(buf)
      assertTrue(len > 0)

      for (i <- 0 until len)
        assertEquals(str.charAt(i + read), buf(i))

      read += len
    }
  }

  @Test def readArrayCharIntInt(): Unit = {
    var read = 0
    val r = newReader
    val buf = new Array[Char](15)

    // twice to force filling internal buffer
    for (_ <- 0 to 1) {
      val len = r.read(buf, 1, 10)
      assertTrue(len > 0)
      assertTrue(len < 11)

      for (i <- 0 until len)
        assertEquals(str.charAt(i + read), buf(i + 1))

      read += len
    }
  }

  @Test def markAndReset(): Unit = {
    val r = newReader
    assertEquals('l': Int, r.read())

    // force moving and resizing buffer
    r.mark(10)

    for (i <- 0 until 10) {
      assertEquals(str.charAt(i + 1): Int, r.read())
    }

    r.reset()

    for (i <- 1 until str.length) {
      assertEquals(str.charAt(i): Int, r.read())
    }
  }

  @Test def readLine(): Unit = {
    val r = newReader

    assertEquals("line1", r.readLine())
    assertEquals("line2", r.readLine())
    assertEquals("", r.readLine())
    assertEquals("line4", r.readLine())
    assertEquals("line5", r.readLine())
    assertEquals(null, r.readLine())
  }

  @Test def readLineEmptyStream(): Unit = {
    val r = new BufferedReader(new StringReader(""))

    assertEquals(null, r.readLine())
  }

  @Test def readLineEmptyLinesOnly(): Unit = {
    val r = new BufferedReader(new StringReader("\n\r\n\r\r\n"), 1)

    for (_ <- 1 to 4)
      assertEquals("", r.readLine())

    assertEquals(null, r.readLine())
  }

  @Test def skipReturns0AfterReachingEnd(): Unit = {
    val r = newReader
    assertEquals(25, r.skip(100))
    assertEquals(-1, r.read())

    assertEquals(0, r.skip(100))
    assertEquals(-1, r.read())
  }

  @Test def markSupported(): Unit = {
    assertTrue(newReader.markSupported)
  }

  @Test def markThrowsWithNegativeLookahead(): Unit = {
    assertThrows(classOf[IllegalArgumentException], newReader.mark(-10))
  }
}
