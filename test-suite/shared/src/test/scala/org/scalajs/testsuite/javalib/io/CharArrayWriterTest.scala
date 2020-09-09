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
import org.scalajs.testsuite.utils.AssertThrows._

class CharArrayWriterTest {
  private val hw: Array[Char] = Array('H', 'e', 'l', 'l', 'o', 'W', 'o', 'r', 'l', 'd')

  private def withClose[T <: AutoCloseable](closeable: T)(fn: T => Unit): Unit = {
    fn(closeable)

    if (closeable != null)
      closeable.close()
  }

  @Test def should_construct_with_correct_size_when_supplied(): Unit = {
    withClose(new CharArrayWriter(90)) { cw =>
      assertEquals(0, cw.size)
    }
  }

  @Test def should_construct_with_exception(): Unit = {
    assertThrows(classOf[IllegalArgumentException], new CharArrayWriter(-1))
  }

  @Test def should_construct_with_correct_size_default(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      assertEquals(0, cw.size)
    }
  }

  @Test def should_support_close(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.close()
      cw.close() // no-op
      assertEquals(0, cw.size)
    }
  }

  @Test def should_support_flush(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.flush()
    }
  }

  @Test def should_support_reset(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 5, 5)
      cw.reset()
      cw.write("HelloWorld", 0, 5)

      assertArrayEquals("Hello".toCharArray, cw.toCharArray)
    }
  }

  @Test def should_support_size(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      assertEquals(0, cw.size)
      cw.write(hw, 5, 5)
      assertEquals(5, cw.size)
    }
  }

  @Test def should_support_toCharArray(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 0, 10)
      assertArrayEquals("HelloWorld".toCharArray, cw.toCharArray)
    }
  }

  @Test def should_support_toString(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 5, 5)
      assertEquals("World", cw.toString)
    }
  }

  @Test def should_support_write_char_array(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write(hw, 5, 5)
      assertEquals("World", cw.toString)
      assertArrayEquals("World".toCharArray, cw.toCharArray)
    }
  }

  @Test def write_should_throw_IndexOutOfBoundsException(): Unit = {
    withClose(new CharArrayWriter) { obj =>
      assertThrows(classOf[IndexOutOfBoundsException],
          obj.write(Array[Char]('0'), 0, -1))
    }
  }

  @Test def should_support_write_char(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write('T')
      assertEquals("T", cw.toString)
      assertArrayEquals(Array('T'), cw.toCharArray)
    }
  }

  @Test def write_should_support_write_string(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 5, 5)
      assertEquals("World", cw.toString)
      assertArrayEquals("World".toCharArray, cw.toCharArray)
    }
  }

  @Test def should_support_writeTo_StringWriter(): Unit = {
    withClose(new CharArrayWriter) { cw =>
      cw.write("HelloWorld", 0, 10)
      withClose(new StringWriter) { sw =>
        cw.writeTo(sw)
        assertEquals("HelloWorld", sw.toString)
      }
    }
  }

  @Test def should_support_append_char(): Unit = {
    withClose(new CharArrayWriter(10)) { cw =>
      val testChar = ' '
      cw.append(testChar)
      cw.flush()
      assertEquals(String.valueOf(testChar), cw.toString)
      assertArrayEquals(Array(testChar), cw.toCharArray)
    }
  }

  @Test def should_support_append_CharSequence(): Unit = {
    withClose(new CharArrayWriter(10)) { cw =>
      val testString: CharSequence = "My Test String"
      cw.append(testString)
      cw.flush()
      assertEquals("My Test String", cw.toString)
      assertArrayEquals("My Test String".toCharArray, cw.toCharArray)
    }
  }

  @Test def should_support_append_CharSequence_with_offset(): Unit = {
    withClose(new CharArrayWriter(10)) { cw =>
      val testString: String = "My Test String"
      cw.append(testString, 1, 3)
      cw.flush()
      assertEquals(testString.substring(1, 3), cw.toString)
      assertArrayEquals(testString.substring(1, 3).toCharArray, cw.toCharArray)
    }
  }
}
