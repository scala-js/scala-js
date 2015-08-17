/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.io

import java.io._

import scala.scalajs.js
import js.JSConverters._
import org.scalajs.jasminetest.JasmineTest

object PrintStreamTest extends JasmineTest {
  private def newPrintStream(
      autoFlush: Boolean = false): (MockPrintStream, MockByteArrayOutputStream) = {
    val bos = new MockByteArrayOutputStream
    val ps = new MockPrintStream(bos, autoFlush)
    (ps, bos)
  }

  describe("java.io.PrintStream") {
    it("flush") {
      val (ps, bos) = newPrintStream()
      ps.print("hello")
      expect(bos.flushed).toBeFalsy
      ps.flush()
      expect(bos.flushed).toBeTruthy
    }

    it("close") {
      val (ps, bos) = newPrintStream()
      ps.write(Array[Byte](1))
      expect(bos.flushed).toBeFalsy

      ps.close()
      expect(bos.flushed).toBeTruthy
      expect(bos.closed).toBeTruthy
      expect(ps.checkError()).toBeFalsy

      // can double-close without error
      ps.close()
      expect(ps.checkError()).toBeFalsy
      ps.clearError()

      // when closed, other operations cause error
      def expectCausesError(body: => Unit): Unit = {
        body
        expect(ps.checkError()).toBeTruthy
        ps.clearError()
      }
      expectCausesError(ps.print("never printed"))
      expectCausesError(ps.write(Array[Byte]('a', 'b')))
      expectCausesError(ps.append("hello", 1, 3))
      expectCausesError(ps.flush())

      // at the end of it all, bos is still what it was when it was closed
      expect(bos.toByteArray.toJSArray).toEqual(js.Array(1))
    }

    it("write, pass the bytes through") {
      def test(body: PrintStream => Unit, expected: js.Array[Int],
          testFlushed: Boolean = false): Unit = {
        val (ps, bos) = newPrintStream(autoFlush = true)
        body(ps)
        if (testFlushed)
          expect(bos.flushed).toBeTruthy
        expect(ps.checkError()).toBeFalsy
        expect(bos.toByteArray.toJSArray).toEqual(expected.map(_.toByte))
      }

      test(_.write('a'), js.Array('a'))
      test(_.write('\n'), js.Array('\n'), testFlushed = true)
      test(_.write(Array[Byte]('A', '\n')),
          js.Array('A', '\n'), testFlushed = true)
      test(_.write(Array[Byte]('A', 'B', '\n', 'C'), 1, 2),
          js.Array('B', '\n'), testFlushed = true)

      test(_.write('é'.toByte), js.Array('é'))
      test(_.write(Array[Byte]('é'.toByte, 'à'.toByte)), js.Array('é', 'à'))
    }

    it("print") {
      def test(body: PrintStream => Unit, expected: String,
          testFlushed: Boolean = false): Unit = {
        val (ps, bos) = newPrintStream(autoFlush = true)
        body(ps)
        if (testFlushed)
          expect(bos.flushed).toBeTruthy
        expect(ps.checkError()).toBeFalsy
        expect(bos.toString()).toBe(expected)
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

    it("print encodes in UTF-8") {
      def test(body: PrintStream => Unit, expected: js.Array[Int]): Unit = {
        val (ps, bos) = newPrintStream(autoFlush = false)
        body(ps)
        expect(ps.checkError()).toBeFalsy
        expect(bos.toByteArray.toJSArray).toEqual(expected.map(_.toByte))
      }

      test(_.print('é'), js.Array(0xc3, 0xa9))
      test(_.print("こんにちは"), js.Array(
          0xe3, 0x81, 0x93, 0xe3, 0x82, 0x93, 0xe3, 0x81, 0xab, 0xe3, 0x81, 0xa1, 0xe3, 0x81, 0xaf))
      test(_.print("ημέρ"), js.Array(
          0xce, 0xb7, 0xce, 0xbc, 0xce, 0xad, 0xcf, 0x81))

      test(_.print("\ud83d\udca9"), js.Array(0xf0, 0x9f, 0x92, 0xa9))
      test(_.print("b\ud83d\udca9c"), js.Array('b', 0xf0, 0x9f, 0x92, 0xa9, 'c'))

      test({ osw => osw.print("ab\ud83d"); osw.print('\udca9') },
          js.Array('a', 'b', 0xf0, 0x9f, 0x92, 0xa9))

      test({ osw => osw.print("ab\ud83d"); osw.print("\udca9cd") },
          js.Array('a', 'b', 0xf0, 0x9f, 0x92, 0xa9, 'c', 'd'))

      // Start of malformed sequences

      test(_.print("\ud83da"), js.Array('?', 'a'))
      test(_.print("\udca9"), js.Array('?'))

      test({ osw => osw.print('\ud83d'); osw.print('a') },
          js.Array('?', 'a'))

      test({ osw => osw.print("ab\ud83d"); osw.print("\ud83d") },
          js.Array('a', 'b', '?'))

      test({ osw => osw.print("ab\ud83d"); osw.print("\ud83dc") },
          js.Array('a', 'b', '?', '?', 'c'))

      test({ osw => osw.print('\ud83d'); osw.close() },
          js.Array('?'))

      test({ osw => osw.print("ab\ud83d"); osw.close() },
          js.Array('a', 'b', '?'))
    }

    for (autoFlush <- Seq(true, false)) {
      val title =
        if (autoFlush) "println forwards and flushes when autoFlush is true"
        else           "println forwards, does not flush when autoFlush is false"
      it(title) {
        def test(body: PrintStream => Unit, expected: String): Unit = {
          val (ps, bos) = newPrintStream(autoFlush = autoFlush)
          body(ps)
          if (autoFlush) expect(bos.flushed).toBeTruthy
          else           expect(bos.flushed).toBeFalsy
          expect(ps.checkError()).toBeFalsy
          expect(bos.toString()).toBe(expected)
        }

        test(_.println(), "\n")
        test(_.println(true), "true\n")
        test(_.println('Z'), "Z\n")
        test(_.println('\n'), "\n\n")
        test(_.println(5), "5\n")
        test(_.println(1234567891011L), "1234567891011\n")
        test(_.println(1.5f), "1.5\n")
        test(_.println(Math.PI), "3.141592653589793\n")
        test(_.println(Array('A', '\n')), "A\n\n")
        test(_.println("hello\n"), "hello\n\n")
        test(_.println(null: String), "null\n")
        test(_.println((1, 2)), "(1,2)\n")
        test(_.println(null: AnyRef), "null\n")
      }
    }

    for (autoFlush <- Seq(true, false)) {
      val title =
        if (autoFlush) "printf/format, which flushes when autoFlush is true"
        else           "printf/format, does not flush when autoFlush is false"
      it(title) {
        def test(body: PrintStream => Unit, expected: String): Unit = {
          val (ps, bos) = newPrintStream(autoFlush = autoFlush)
          body(ps)
          if (autoFlush) expect(bos.flushed).toBeTruthy
          else           expect(bos.flushed).toBeFalsy
          expect(ps.checkError()).toBeFalsy
          expect(bos.toString()).toBe(expected)
        }

        test(_.printf("%04d", Int.box(5)), "0005")
        test(_.format("%.5f", Double.box(Math.PI)), "3.14159")
      }
    }

    it("append") {
      def test(body: PrintStream => Unit, expected: String,
          testFlushed: Boolean = false): Unit = {
        val (ps, bos) = newPrintStream(autoFlush = true)
        body(ps)
        if (testFlushed)
          expect(bos.flushed).toBeTruthy
        expect(ps.checkError()).toBeFalsy
        expect(bos.toString()).toBe(expected)
      }

      test(_.append("hello\n"), "hello\n", testFlushed = true)
      test(_.append(null: CharSequence), "null")
      test(_.append("hello\nworld", 3, 6), "lo\n", testFlushed = true)
      test(_.append(null: CharSequence, 1, 2), "u")
      test(_.append('A'), "A")
      test(_.append('\n'), "\n", testFlushed = true)
    }

    it("traps all IOException and updates checkError") {
      def test(body: PrintStream => Unit): Unit = {
        val (ps, bos) = newPrintStream()
        bos.throwing = true
        body(ps)
        expect(ps.checkError()).toBeTruthy
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

    it("write short-circuits pending high surrogates in print") {
      val (ps, bos) = newPrintStream()
      ps.print('A')
      expect(bos.toByteArray.toJSArray).toEqual(js.Array[Byte]('A'))
      ps.print('\ud83d')
      expect(bos.toByteArray.toJSArray).toEqual(js.Array[Byte]('A'))
      ps.flush()
      expect(bos.toByteArray.toJSArray).toEqual(js.Array[Byte]('A'))
      ps.write('Z')
      expect(bos.toByteArray.toJSArray).toEqual(js.Array[Byte]('A', 'Z'))
      ps.print('\udca9')
      expect(bos.toByteArray.toJSArray).toEqual(js.Array[Byte](
          'A', 'Z', -16, -97, -110, -87))
    }
  }

  /** A PrintStream that exposes various hooks for testing purposes. */
  private class MockPrintStream(out: OutputStream,
      autoFlush: Boolean) extends PrintStream(out, autoFlush) {
    def this(out: OutputStream) = this(out, false)

    override def clearError(): Unit = super.clearError()
  }

}
