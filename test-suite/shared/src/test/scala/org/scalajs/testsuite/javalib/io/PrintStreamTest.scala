/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.io

import java.io._

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class PrintStreamTest {
  private def newPrintStream(
      autoFlush: Boolean = false): (MockPrintStream, MockByteArrayOutputStream) = {
    val bos = new MockByteArrayOutputStream
    val ps = new MockPrintStream(bos, autoFlush)
    (ps, bos)
  }

  @Test def flush(): Unit = {
    val (ps, bos) = newPrintStream()
    ps.print("hello")
    assertFalse(bos.flushed)
    ps.flush()
    assertTrue(bos.flushed)
  }

  @Test def close(): Unit = {
    val (ps, bos) = newPrintStream()
    ps.write(Array[Byte](1))
    assertFalse(bos.flushed)

    ps.close()
    if (!executingInJVM)
      assertTrue(bos.flushed)
    assertTrue(bos.closed)
    assertFalse(ps.checkError())

    // can double-close without error
    ps.close()
    assertFalse(ps.checkError())
    ps.clearError()

    // when closed, other operations cause error
    def expectCausesError(body: => Unit): Unit = {
      body
      assertTrue(ps.checkError())
      ps.clearError()
    }
    expectCausesError(ps.print("never printed"))
    expectCausesError(ps.write(Array[Byte]('a', 'b')))
    expectCausesError(ps.append("hello", 1, 3))
    expectCausesError(ps.flush())

    // at the end of it all, bos is still what it was when it was closed
    assertArrayEquals(Array[Byte](1), bos.toByteArray)
  }

  @Test def write_pass_the_bytes_through(): Unit = {
    def test(body: PrintStream => Unit, expected: Array[Int],
        testFlushed: Boolean = false): Unit = {
      val (ps, bos) = newPrintStream(autoFlush = true)
      body(ps)
      if (testFlushed)
        assertTrue(bos.flushed)
      assertFalse(ps.checkError())
      assertArrayEquals(expected.map(_.toByte), bos.toByteArray)
    }

    test(_.write('a'), Array('a'))
    test(_.write('\n'), Array('\n'), testFlushed = true)
    test(_.write(Array[Byte]('A', '\n')), Array('A', '\n'), testFlushed = true)
    test(_.write(Array[Byte]('A', 'B', '\n', 'C'), 1, 2),
        Array('B', '\n'), testFlushed = true)

    test(_.write('é'.toByte), Array('é'))
    test(_.write(Array[Byte]('é'.toByte, 'à'.toByte)), Array('é', 'à'))
  }

  @Test def print(): Unit = {
    def test(body: PrintStream => Unit, expected: String,
        testFlushed: Boolean = false): Unit = {
      val (ps, bos) = newPrintStream(autoFlush = true)
      body(ps)
      if (testFlushed)
        assertTrue(bos.flushed)
      assertFalse(ps.checkError())
      assertEquals(expected, bos.toString())
    }

    test(_.print(true), "true")
    test(_.print('Z'), "Z")
    test(_.print('\n'), "\n", testFlushed = true)
    test(_.print(5), "5")
    test(_.print(1234567891011L), "1234567891011")
    test(_.print(1.5f), "1.5")
    test(_.print(Math.PI), "3.141592653589793")
    test(_.print(Array('A', '\n')), "A\n", testFlushed = true)
    test(_.print("hello\n"), "hello\n", testFlushed = true)
    test(_.print(null: String), "null")
    test(_.print((1, 2)), "(1,2)")
    test(_.print(null: AnyRef), "null")
  }

  @Test def print_encodes_in_UTF_8(): Unit = {
    def test(body: PrintStream => Unit, expected: Array[Int]): Unit = {
      val (ps, bos) = newPrintStream(autoFlush = false)
      body(ps)
      assertFalse(ps.checkError())
      assertArrayEquals(expected.map(_.toByte), bos.toByteArray)
    }

    test(_.print('é'), Array(0xc3, 0xa9))
    test(_.print("こんにちは"), Array(
        0xe3, 0x81, 0x93, 0xe3, 0x82, 0x93, 0xe3, 0x81, 0xab, 0xe3, 0x81, 0xa1, 0xe3, 0x81, 0xaf))
    test(_.print("ημέρ"), Array(0xce, 0xb7, 0xce, 0xbc, 0xce, 0xad, 0xcf, 0x81))

    test(_.print("\ud83d\udca9"), Array(0xf0, 0x9f, 0x92, 0xa9))
    test(_.print("b\ud83d\udca9c"), Array('b', 0xf0, 0x9f, 0x92, 0xa9, 'c'))

    test({ osw => osw.print("ab\ud83d"); osw.print('\udca9') },
        Array('a', 'b', 0xf0, 0x9f, 0x92, 0xa9))

    test({ osw => osw.print("ab\ud83d"); osw.print("\udca9cd") },
        Array('a', 'b', 0xf0, 0x9f, 0x92, 0xa9, 'c', 'd'))

    // Start of malformed sequences

    test(_.print("\ud83da"), Array('?', 'a'))
    test(_.print("\udca9"), Array('?'))

    test({ osw => osw.print('\ud83d'); osw.print('a') }, Array('?', 'a'))

    test({ osw => osw.print("ab\ud83d"); osw.print("\ud83d") },
        Array('a', 'b', '?'))

    test({ osw => osw.print("ab\ud83d"); osw.print("\ud83dc") },
        Array('a', 'b', '?', '?', 'c'))

    test({ osw => osw.print('\ud83d'); osw.close() }, Array('?'))

    test({ osw => osw.print("ab\ud83d"); osw.close() }, Array('a', 'b', '?'))
  }

  @Test def println_forwards_and_flushes_when_autoFlush_is_true(): Unit = {
    testPrintlnForwards(_.println(), "\n", autoFlush = true)
    testPrintlnForwards(_.println(true), "true\n", autoFlush = true)
    testPrintlnForwards(_.println('Z'), "Z\n", autoFlush = true)
    testPrintlnForwards(_.println('\n'), "\n\n", autoFlush = true)
    testPrintlnForwards(_.println(5), "5\n", autoFlush = true)
    testPrintlnForwards(_.println(1234567891011L), "1234567891011\n", autoFlush = true)
    testPrintlnForwards(_.println(1.5f), "1.5\n", autoFlush = true)
    testPrintlnForwards(_.println(Math.PI), "3.141592653589793\n", autoFlush = true)
    testPrintlnForwards(_.println(Array('A', '\n')), "A\n\n", autoFlush = true)
    testPrintlnForwards(_.println("hello\n"), "hello\n\n", autoFlush = true)
    testPrintlnForwards(_.println(null: String), "null\n", autoFlush = true)
    testPrintlnForwards(_.println((1, 2)), "(1,2)\n", autoFlush = true)
    testPrintlnForwards(_.println(null: AnyRef), "null\n", autoFlush = true)
  }

  @Test def println_forwards_does_not_flush_when_autoFlush_is_false(): Unit = {
    testPrintlnForwards(_.println(), "\n", autoFlush = false)
    testPrintlnForwards(_.println(true), "true\n", autoFlush = false)
    testPrintlnForwards(_.println('Z'), "Z\n", autoFlush = false)
    testPrintlnForwards(_.println('\n'), "\n\n", autoFlush = false)
    testPrintlnForwards(_.println(5), "5\n", autoFlush = false)
    testPrintlnForwards(_.println(1234567891011L), "1234567891011\n", autoFlush = false)
    testPrintlnForwards(_.println(1.5f), "1.5\n", autoFlush = false)
    testPrintlnForwards(_.println(Math.PI), "3.141592653589793\n", autoFlush = false)
    testPrintlnForwards(_.println(Array('A', '\n')), "A\n\n", autoFlush = false)
    testPrintlnForwards(_.println("hello\n"), "hello\n\n", autoFlush = false)
    testPrintlnForwards(_.println(null: String), "null\n", autoFlush = false)
    testPrintlnForwards(_.println((1, 2)), "(1,2)\n", autoFlush = false)
    testPrintlnForwards(_.println(null: AnyRef), "null\n", autoFlush = false)
  }

  private def testPrintlnForwards(body: PrintStream => Unit, expected: String, autoFlush: Boolean): Unit = {
    val (ps, bos) = newPrintStream(autoFlush = autoFlush)
    body(ps)
    if (autoFlush) assertTrue(bos.flushed)
    else           assertFalse(bos.flushed)
    assertFalse(ps.checkError())
    assertEquals(expected, bos.toString())
  }

  @Test def printf_format_which_flushes_when_autoFlush_is_true(): Unit = {
    testPrintfFormat(_.printf("%04d", Int.box(5)), "0005", autoFlush = true)
    testPrintfFormat(_.format("%.5f", Double.box(Math.PI)), "3.14159", autoFlush = true)
  }
  @Test def printf_format_which_flushes_when_autoFlush_is_false(): Unit = {
    testPrintfFormat(_.printf("%04d", Int.box(5)), "0005", autoFlush = false)
    testPrintfFormat(_.format("%.5f", Double.box(Math.PI)), "3.14159", autoFlush = false)
  }

  private def testPrintfFormat(body: PrintStream => Unit, expected: String,
      autoFlush: Boolean): Unit = {
    val (ps, bos) = newPrintStream(autoFlush = autoFlush)
    body(ps)
    if (autoFlush) assertTrue(bos.flushed)
    else           assertFalse(bos.flushed)
    assertFalse(ps.checkError())
    assertEquals(expected, bos.toString())
  }

  @Test def append(): Unit = {
    def test(body: PrintStream => Unit, expected: String,
        testFlushed: Boolean = false): Unit = {
      val (ps, bos) = newPrintStream(autoFlush = true)
      body(ps)
      if (testFlushed)
        assertTrue(bos.flushed)
      assertFalse(ps.checkError())
      assertEquals(expected, bos.toString())
    }

    test(_.append("hello\n"), "hello\n", testFlushed = true)
    test(_.append(null: CharSequence), "null")
    test(_.append("hello\nworld", 3, 6), "lo\n", testFlushed = true)
    test(_.append(null: CharSequence, 1, 2), "u")
    test(_.append('A'), "A")
    test(_.append('\n'), "\n", testFlushed = true)
  }

  @Test def traps_all_IOException_and_updates_checkError(): Unit = {
    def test(body: PrintStream => Unit): Unit = {
      val (ps, bos) = newPrintStream()
      bos.throwing = true
      body(ps)
      assertTrue(ps.checkError())
    }

    test(_.flush())
    test(_.close())

    test(_.write('Z'))
    test(_.write(Array[Byte]('A', 'B')))
    test(_.write(Array[Byte]('A', 'B'), 1, 1))

    test(_.print(true))
    test(_.print('Z'))
    test(_.print('\n'))
    test(_.print(5))
    test(_.print(1234567891011L))
    test(_.print(1.5f))
    test(_.print(Math.PI))
    test(_.print(Array('A', '\n')))
    test(_.print("hello\n"))
    test(_.print(null: String))
    test(_.print((1, 2)))
    test(_.print(null: AnyRef))

    test(_.println())
    test(_.println(true))
    test(_.println('Z'))
    test(_.println('\n'))
    test(_.println(5))
    test(_.println(1234567891011L))
    test(_.println(1.5f))
    test(_.println(Math.PI))
    test(_.println(Array('A', '\n')))
    test(_.println("hello\n"))
    test(_.println(null: String))
    test(_.println((1, 2)))
    test(_.println(null: AnyRef))

    test(_.append("hello\n"))
    test(_.append(null: CharSequence))
    test(_.append("hello\nworld", 3, 6))
    test(_.append(null: CharSequence, 1, 2))
    test(_.append('A'))
    test(_.append('\n'))
  }

  @Test def write_short_circuits_pending_high_surrogates_in_print(): Unit = {
    val (ps, bos) = newPrintStream()
    ps.print('A')
    assertArrayEquals(Array[Byte]('A'), bos.toByteArray)
    ps.print('\ud83d')
    assertArrayEquals(Array[Byte]('A'), bos.toByteArray)
    ps.flush()
    assertArrayEquals(Array[Byte]('A'), bos.toByteArray)
    ps.write('Z')
    assertArrayEquals(Array[Byte]('A', 'Z'), bos.toByteArray)
    ps.print('\udca9')
    assertArrayEquals(Array[Byte]('A', 'Z', -16, -97, -110, -87), bos.toByteArray)
  }

  /** A PrintStream that exposes various hooks for testing purposes. */
  private class MockPrintStream(out: OutputStream,
      autoFlush: Boolean) extends PrintStream(out, autoFlush) {
    def this(out: OutputStream) = this(out, false)

    override def clearError(): Unit = super.clearError()
  }

}
