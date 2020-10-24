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
import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.utils.AssertThrows._

class CharArrayReaderTest {
  private val hw: Array[Char] = Array('H', 'e', 'l', 'l', 'o', 'W', 'o', 'r', 'l', 'd')

  private def withClose[T <: AutoCloseable](closeable: T)(fn: T => Unit): Unit = {
    fn(closeable)

    if (closeable != null)
      closeable.close()
  }

  @Test def ctorCharArray(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      assertTrue("Failed to create reader", cr.ready())
    }
  }

  @Test def ctorCharArrayWithOffsetAndLength(): Unit = {
    // CharArrayReader for "Worl"
    withClose(new CharArrayReader(hw, 5, 4)) { cr =>
      assertTrue("Failed to create reader", cr.ready())
      val c = new Array[Char](10)
      // Doesn't read past length & starts at offset
      val bytesRead: Int = cr.read(c, 0, 10)
      assertEquals(4, bytesRead)
      assertArrayEquals("Worl".toCharArray, java.util.Arrays.copyOf(c, bytesRead))
      assertEquals(-1, cr.read())
      assertFalse(cr.ready())
    }
  }

  @Test def ctorCharArrayWithOffsetAndLargerLength(): Unit = {
    // CharArrayReader for "World"
    withClose(new CharArrayReader(hw, 5, 100)) { cr =>
      assertTrue("Failed to create reader", cr.ready())
      val c = new Array[Char](100)
      // Doesn't read past buffer length
      val bytesRead: Int = cr.read(c, 0, 100)
      assertEquals(5, bytesRead)
      assertArrayEquals("World".toCharArray, java.util.Arrays.copyOf(c, bytesRead))
      assertEquals(-1, cr.read())
      assertFalse(cr.ready())
    }
  }

  @Test def readThrowsIOExceptionAfterClose(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      cr.close()
      assertThrows(classOf[IOException], cr.read())
    }
  }

  @Test def markSupported(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      assertTrue("markSupported returned false", cr.markSupported)
    }
  }

  @Test def readChar(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      assertEquals("Read returned incorrect char", 'H', cr.read())
    }

    withClose(new CharArrayReader(Array[Char]('\u8765'))) { cr =>
      assertEquals("Incorrect double byte char", '\u8765', cr.read())
    }
  }

  @Test def readCharArray(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      val c = new Array[Char](11)
      cr.read(c, 1, 10)
      assertArrayEquals(Array(0.toChar) ++ hw, c)
    }
  }

  @Test def readyThrowsIOException(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      assertTrue("ready returned false", cr.ready())
      cr.skip(1000L)
      assertTrue("ready returned true", !cr.ready())
      cr.close()

      assertThrows(classOf[IOException], cr.ready())
    }

    withClose(new CharArrayReader(hw)) { cr =>
      cr.close()
      assertThrows(classOf[IOException], cr.ready())
    }
  }

  @Test def resetAfterMark(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      cr.skip(5L)
      // Mark current position
      cr.mark(100)
      assertEquals("Reset failed to return to marker position", 'W', cr.read())
      // Reset back  to 'W'
      cr.reset()
      assertEquals("Reset failed to return to marker position", 'W', cr.read())
    }
  }

  @Test def markLimitMinusOneIgnored(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      cr.skip(5L)
      // Mark current position
      cr.mark(-1)
      val c = new Array[Char](5)
      cr.read(c, 0, 5)
      assertArrayEquals("Mark read limit should be ignored", "World".toCharArray, c)

      // Reset back  to 'W'
      cr.reset()
      assertEquals("Reset failed to return to marker position", 'W', cr.read())
    }
  }

  @Test def skipNegative(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      assertEquals("Negative skip values should return zero", 0, cr.skip(-1L))
    }
  }

  @Test def skipZero(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      assertEquals("Zero skip value should return zero", 0, cr.skip(0L))
    }
  }

  @Test def skipReturnsArraySizeWhenGreater(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      assertEquals("Skip didn't return array size", 10L, cr.skip(1000L))
    }
  }

  @Test def resetFromOffsetReader(): Unit = {
    val data = "offsetHello world!".toCharArray
    val offsetLength = 6
    val length = data.length - offsetLength

    withClose(new CharArrayReader(data, offsetLength, length)) { cr =>
      cr.reset()
      for (i <- 0 until length) {
        assertEquals(data(offsetLength + i), cr.read().toChar)
      }
      // Verify EOF
      assertEquals(-1, cr.read())
    }
  }

  @Test def skip(): Unit = {
    withClose(new CharArrayReader(hw)) { cr =>
      val skipped = cr.skip(5L)
      assertEquals("Failed to skip correct number of chars", 5L, skipped)
      assertEquals("Skip skipped wrong chars", 'W', cr.read())
    }
  }
}
