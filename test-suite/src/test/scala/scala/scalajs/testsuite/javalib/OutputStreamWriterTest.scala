/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.javalib

import java.io._

import scala.scalajs.js
import js.JSConverters._
import org.scalajs.jasminetest.JasmineTest

object OutputStreamWriterTest extends JasmineTest {
  private def newOSWriter(): (OutputStreamWriter, MockByteArrayOutputStream) = {
    val bos = new MockByteArrayOutputStream
    val osw = new OutputStreamWriter(bos)
    (osw, bos)
  }

  describe("java.io.OutputStreamWriter") {
    it("flush") {
      val (osw, bos) = newOSWriter()
      bos.write(1)
      osw.write("ABC")
      expect(bos.flushed).toBeFalsy
      osw.flush()
      expect(bos.flushed).toBeTruthy
    }

    it("close") {
      val (osw, bos) = newOSWriter()
      bos.write(1)
      osw.write("ABC")
      expect(bos.flushed).toBeFalsy

      osw.close()
      expect(bos.flushed).toBeTruthy
      expect(bos.closed).toBeTruthy

      // can double-close without error
      osw.close()

      // when closed, other operations cause error
      expect(() => osw.write('A')).toThrow
      expect(() => osw.write("never printed")).toThrow
      expect(() => osw.write(Array('a', 'b'))).toThrow
      expect(() => osw.append("hello", 1, 3)).toThrow
      expect(() => osw.flush()).toThrow

      // at the end of it all, bos is still what it was when it was closed
      expect(bos.toByteArray().toJSArray).toEqual(js.Array(1, 65, 66, 67))
    }

    def testW(body: OutputStreamWriter => Unit,
        expected: js.Array[Int], alreadyFlushed: Boolean = false): Unit = {
      val (osw, bos) = newOSWriter()
      body(osw)
      if (!alreadyFlushed) {
        expect(bos.size).toEqual(0) // write() methods should buffer
        osw.flush()
      }
      expect(bos.flushed).toBeTruthy
      expect(bos.toByteArray.toJSArray).toEqual(expected.map(_.toByte))
    }

    it("write(), ASCII repertoire") {
      // Pure ASCII
      testW(_.write('\n'), js.Array('\n'))
      testW(_.write("hello\n"), js.Array('h', 'e', 'l', 'l', 'o', '\n'))
      testW(_.write("hello\nworld", 3, 4), js.Array('l', 'o', '\n', 'w'))
      testW(_.write(Array('A', '\n')), js.Array('A', '\n'))
      testW(_.write(Array('A', 'B', '\n', 'C'), 1, 2), js.Array('B', '\n'))
    }

    it("write(), Unicode repertoire without surrogates") {
      testW(_.write('é'), js.Array(0xc3, 0xa9))
      testW(_.write("こんにちは"), js.Array(
          0xe3, 0x81, 0x93, 0xe3, 0x82, 0x93, 0xe3, 0x81, 0xab, 0xe3, 0x81, 0xa1, 0xe3, 0x81, 0xaf))
      testW(_.write("Καλημέρα", 3, 4), js.Array(
          0xce, 0xb7, 0xce, 0xbc, 0xce, 0xad, 0xcf, 0x81))
    }

    it("write(), surrogate pairs") {
      testW(_.write("\ud83d\udca9"), js.Array(0xf0, 0x9f, 0x92, 0xa9))
      testW(_.write("ab\ud83d\udca9cd", 1, 3), js.Array('b', 0xf0, 0x9f, 0x92, 0xa9))
    }

    it("write(), surrogate pairs spread across multiple writes") {
      testW({ osw => osw.write('\ud83d'); osw.write('\udca9') },
          js.Array(0xf0, 0x9f, 0x92, 0xa9))

      testW({ osw => osw.write('\ud83d'); osw.flush(); osw.write('\udca9') },
          js.Array(0xf0, 0x9f, 0x92, 0xa9))

      testW({ osw => osw.write("ab\ud83d"); osw.write('\udca9') },
          js.Array('a', 'b', 0xf0, 0x9f, 0x92, 0xa9))

      testW({ osw => osw.write("ab\ud83d"); osw.write("\udca9cd") },
          js.Array('a', 'b', 0xf0, 0x9f, 0x92, 0xa9, 'c', 'd'))

      testW({ osw => osw.write("ab\ud83dzz", 1, 2); osw.write("ww\udca9cd", 2, 2) },
          js.Array('b', 0xf0, 0x9f, 0x92, 0xa9, 'c'))
    }

    it("write(), malformed surrogates") {
      testW(_.write("\ud83da"), js.Array('?', 'a'))
      testW(_.write("\udca9"), js.Array('?'))
    }

    it("write(), malformed surrogates spread across multiple writes") {
      testW({ osw => osw.write('\ud83d'); osw.write('a') },
          js.Array('?', 'a'))

      testW({ osw => osw.write("ab\ud83d"); osw.write("\ud83d") },
          js.Array('a', 'b', '?'))

      testW({ osw => osw.write("ab\ud83d"); osw.write("\ud83dc") },
          js.Array('a', 'b', '?', '?', 'c'))
    }

    it("write(), malformed surrogates at end of input") {
      testW({ osw => osw.write('\ud83d'); osw.close() },
          js.Array('?'), alreadyFlushed = true)

      testW({ osw => osw.write("ab\ud83d"); osw.close() },
          js.Array('a', 'b', '?'), alreadyFlushed = true)
    }

  }

}
