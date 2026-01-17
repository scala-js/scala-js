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

import java.io.{CharArrayWriter, StringWriter}
import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class CharArrayWriterTest {
  private val hw: Array[Char] = Array('H', 'e', 'l', 'l', 'o', 'W', 'o', 'r', 'l', 'd')

  private def withClose[T <: AutoCloseable](closeable: T)(fn: T => Unit): Unit = {
    fn(closeable)

    if (closeable != null)
      closeable.close()
  }

  @Test def ctorSize: Unit = {
    withClose(new CharArrayWriter(90)) { cw =>
      assertEquals(0, cw.size)
    }
  }

  @Test def ctorSizeNegativeThrows(): Unit =
    assertThrows(classOf[IllegalArgumentException], new CharArrayWriter(-1))

  @Test def ctorSizeDefault(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      assertEquals(0, cw.size)
    }
  }

  @Test def close(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.close()
      cw.close() // no-op
      assertEquals(0, cw.size)
    }
  }

  @Test def flush(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.flush()
    }
  }

  @Test def reset(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 5, 5)
      cw.reset()
      cw.write("HelloWorld", 0, 5)

      assertArrayEquals("Hello".toCharArray, cw.toCharArray)
    }
  }

  @Test def size(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      assertEquals(0, cw.size)
      cw.write(hw, 5, 5)
      assertEquals(5, cw.size)
    }
  }

  @Test def toCharArray(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 0, 10)
      assertArrayEquals("HelloWorld".toCharArray, cw.toCharArray)
    }
  }

  @Test def testToString(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 5, 5)
      assertEquals("World", cw.toString)
    }
  }

  @Test def writeSubArrayToCharArray(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write(hw, 5, 5)
      assertEquals("World", cw.toString)
      assertArrayEquals("World".toCharArray, cw.toCharArray)
    }
  }

  @Test def throwsIndexOutOfBoundsException(): Unit = {
    withClose(new CharArrayWriter) { obj =>
      assertThrows(classOf[IndexOutOfBoundsException],
          obj.write(Array[Char]('0'), 0, -1))
    }
  }

  @Test def writeChar(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write('T')
      assertEquals("T", cw.toString)
      assertArrayEquals(Array('T'), cw.toCharArray)
    }
  }

  @Test def writeString(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 5, 5)
      assertEquals("World", cw.toString)
      assertArrayEquals("World".toCharArray, cw.toCharArray)
    }
  }

  @Test def writeToStringWriter(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 0, 10)
      withClose(new StringWriter) { sw =>
        cw.writeTo(sw)
        assertEquals("HelloWorld", sw.toString)
      }
    }
  }

  @Test def appendChar(): Unit = {
    withClose(new CharArrayWriter(10)) { cw =>
      val testChar = ' '
      cw.append(testChar)
      cw.flush()
      assertEquals(String.valueOf(testChar), cw.toString)
      assertArrayEquals(Array(testChar), cw.toCharArray)
    }
  }

  @Test def appendCharSequence(): Unit = {
    withClose(new CharArrayWriter(10)) { cw =>
      val testString: CharSequence = "My Test String"
      cw.append(testString)
      cw.flush()
      assertEquals("My Test String", cw.toString)
      assertArrayEquals("My Test String".toCharArray, cw.toCharArray)
    }
  }

  @Test def appendCharSequenceWithOffset(): Unit = {
    withClose(new CharArrayWriter(10)) { cw =>
      val testString: String = "My Test String"
      cw.append(testString, 1, 3)
      cw.flush()
      assertEquals(testString.substring(1, 3), cw.toString)
      assertArrayEquals(testString.substring(1, 3).toCharArray, cw.toCharArray)
    }
  }
}
