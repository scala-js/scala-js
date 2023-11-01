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

import scala.annotation.tailrec

import java.io._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class InputStreamReaderTest {

  @Test def readUTF8(): Unit = {

    val buf = Array[Byte](72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100,
        46, -29, -127, -109, -29, -126, -109, -29, -127, -85, -29, -127, -95,
        -29, -127, -81, -26, -105, -91, -26, -100, -84, -24, -86, -98, -29,
        -126, -110, -24, -86, -83, -29, -126, -127, -29, -127, -66, -29, -127,
        -103, -29, -127, -117, -29, -128, -126)

    val r = new InputStreamReader(new ByteArrayInputStream(buf))

    def expectRead(str: String): Unit = {
      val buf = new Array[Char](str.length)
      @tailrec
      def readAll(readSoFar: Int): Int = {
        if (readSoFar == buf.length) readSoFar
        else {
          val newlyRead = r.read(buf, readSoFar, buf.length - readSoFar)
          if (newlyRead == -1) readSoFar
          else readAll(readSoFar + newlyRead)
        }
      }
      assertEquals(str.length, readAll(0))
      assertEquals(str, new String(buf))
    }

    expectRead("Hello World.")
    expectRead("こんにちは")
    expectRead("日本語を読めますか。")
    assertEquals(-1, r.read())
  }

  @Test def readEOFThrows(): Unit = {
    val data = "Lorem ipsum".getBytes()
    val streamReader = new InputStreamReader(new ByteArrayInputStream(data))
    val bytes = new Array[Char](11)

    assertEquals(11, streamReader.read(bytes))
    // Do it twice to check for a regression where this used to throw
    assertEquals(-1, streamReader.read(bytes))
    assertEquals(-1, streamReader.read(bytes))
    assertThrows(classOf[IndexOutOfBoundsException],
        streamReader.read(bytes, 10, 3))
    assertEquals(0, streamReader.read(new Array[Char](0)))
  }

  @Test def skipReturns0AfterReachingEnd(): Unit = {
    val data = "Lorem ipsum".getBytes()
    val r = new InputStreamReader(new ByteArrayInputStream(data))
    assertTrue(r.skip(100) > 0)
    assertEquals(-1, r.read())

    assertEquals(0, r.skip(100))
    assertEquals(-1, r.read())
  }

  @Test def markThrowsNotSupported(): Unit = {
    val data = "Lorem ipsum".getBytes()
    val r = new InputStreamReader(new ByteArrayInputStream(data))
    assertThrows(classOf[IOException], r.mark(0))
  }
}
