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

class StringReaderTest {
  val str = "asdf"
  def newReader: StringReader = new StringReader(str)

  @Test def read()(): Unit = {
    val r = newReader

    for (c <- str) {
      assertEquals(c, r.read().toChar)
    }

    assertEquals(-1, r.read())
  }

  @Test def readArrayCharIntInt(): Unit = {
    val r = newReader
    val buf = new Array[Char](10)

    assertEquals(4, r.read(buf, 2, 8))
    assertArrayEquals(buf.map(_.toInt),
        Array[Int](0, 0, 'a', 's', 'd', 'f', 0, 0, 0, 0))
    assertEquals(-1, r.read(buf, 2, 8)) // #1560
  }

  @Test def readCharBuffer(): Unit = {
    val r = newReader
    val buf0 = java.nio.CharBuffer.allocate(25)
    buf0.position(3)
    val buf = buf0.slice()
    buf.position(4)
    buf.limit(14)

    assertEquals(4, r.read(buf))
    assertEquals(8, buf.position())
    buf.flip()
    assertArrayEquals(buf.toString().map(_.toInt).toArray,
        Array[Int](0, 0, 0, 0, 'a', 's', 'd', 'f'))
  }

  @Test def ready(): Unit = {
    val r = newReader

    for (c <- str) {
      assertTrue(r.ready())
      assertEquals(c, r.read().toChar)
    }

    assertTrue(r.ready())
    assertEquals(-1, r.read())

    r.close()
    assertThrows(classOf[IOException], r.ready())
  }

  @Test def markReset(): Unit = {
    val r = newReader
    r.mark(str.length)

    for (c <- str) {
      assertEquals(c, r.read().toChar)
    }
    assertEquals(-1, r.read())

    r.reset()

    for (c <- str) {
      assertEquals(c, r.read().toChar)
    }
    assertEquals(-1, r.read())
  }

  @Test def skip(): Unit = {
    val r = newReader

    assertEquals('a': Int, r.read())
    assertEquals(2, r.skip(2L).toInt)

    assertEquals('f': Int, r.read())
    assertEquals(-1, r.read())
  }

  @Test def close(): Unit = {
    val r = newReader

    r.close()
    assertThrows(classOf[IOException], r.read())
  }

  @Test def mark(): Unit =
    assertTrue(newReader.markSupported)

  @Test def markThrowsWithNegativeLookahead(): Unit =
    assertThrows(classOf[IllegalArgumentException], newReader.mark(-10))

  @Test def skipAcceptsNegativeLookaheadAsLookback(): Unit = {
    // StringReader.skip accepts negative lookahead
    val r = newReader
    assertEquals("already head", 0, r.skip(-1))
    assertEquals('a', r.read())

    assertEquals(1, r.skip(1))
    assertEquals('d', r.read())

    assertEquals(-2, r.skip(-2))
    assertEquals('s', r.read())
  }

  @Test def skipReturns0AfterReachingEnd(): Unit = {
    val r = newReader
    assertEquals(4, r.skip(100))
    assertEquals(-1, r.read())

    assertEquals(0, r.skip(-100))
    assertEquals(-1, r.read())
  }
}
