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

import org.scalajs.testsuite.utils.AssertThrows._

class OutputStreamWriterTest {
  private def newOSWriter(): (OutputStreamWriter, MockByteArrayOutputStream) = {
    val bos = new MockByteArrayOutputStream
    val osw = new OutputStreamWriter(bos)
    (osw, bos)
  }

  @Test def flush(): Unit = {
    val (osw, bos) = newOSWriter()
    bos.write(1)
    osw.write("ABC")
    assertFalse(bos.flushed)

    osw.flush()
    assertTrue(bos.flushed)
    assertArrayEquals(Array[Byte](1, 'A', 'B', 'C'), bos.toByteArray)
  }

  @Test def close(): Unit = {
    val (osw, bos) = newOSWriter()
    bos.write(1)
    osw.write("ABC")
    assertFalse(bos.flushed)

    osw.close()
    assertTrue(bos.closed)
    assertArrayEquals(Array[Byte](1, 'A', 'B', 'C'), bos.toByteArray)

    // can double-close without error
    osw.close()

    // when closed, other operations cause error
    assertThrows(classOf[IOException], osw.write('A'))
    assertThrows(classOf[IOException], osw.write("never printed"))
    assertThrows(classOf[IOException], osw.write(Array('a', 'b')))
    assertThrows(classOf[IOException], osw.append("hello", 1, 3))
    assertThrows(classOf[IOException], osw.flush())

    // at the end of it all, bos is still what it was when it was closed
    assertArrayEquals(Array[Byte](1, 65, 66, 67), bos.toByteArray())
  }

  def testW(body: OutputStreamWriter => Unit,
      expected: Array[Int], alreadyFlushed: Boolean = false): Unit = {
    val (osw, bos) = newOSWriter()
    body(osw)
    if (!alreadyFlushed) {
      assertEquals(0, bos.size) // write() methods should buffer
      osw.flush()
    }
    assertArrayEquals(expected.map(_.toByte), bos.toByteArray)
  }

  @Test def writeASCIIRepertoire(): Unit = {
    // Pure ASCII
    testW(_.write('\n'), Array('\n'))
    testW(_.write("hello\n"), Array('h', 'e', 'l', 'l', 'o', '\n'))
    testW(_.write("hello\nworld", 3, 4), Array('l', 'o', '\n', 'w'))
    testW(_.write(Array('A', '\n')), Array('A', '\n'))
    testW(_.write(Array('A', 'B', '\n', 'C'), 1, 2), Array('B', '\n'))
  }

  @Test def writeUnicodeRepertoireWithoutSurrogates(): Unit = {
    testW(_.write('é'), Array(0xc3, 0xa9))
    testW(_.write("こんにちは"), Array(
        0xe3, 0x81, 0x93, 0xe3, 0x82, 0x93, 0xe3, 0x81, 0xab, 0xe3, 0x81, 0xa1, 0xe3, 0x81, 0xaf))
    testW(_.write("Καλημέρα", 3, 4), Array(
        0xce, 0xb7, 0xce, 0xbc, 0xce, 0xad, 0xcf, 0x81))
  }

  @Test def writeSurrogatePairs(): Unit = {
    testW(_.write("\ud83d\udca9"), Array(0xf0, 0x9f, 0x92, 0xa9))
    testW(_.write("ab\ud83d\udca9cd", 1, 3), Array('b', 0xf0, 0x9f, 0x92, 0xa9))
  }

  @Test def writeSurrogatePairsSpreadAcrossMultipleWrites(): Unit = {
    testW({ osw => osw.write('\ud83d'); osw.write('\udca9') },
        Array(0xf0, 0x9f, 0x92, 0xa9))

    testW({ osw => osw.write('\ud83d'); osw.flush(); osw.write('\udca9') },
        Array(0xf0, 0x9f, 0x92, 0xa9))

    testW({ osw => osw.write("ab\ud83d"); osw.write('\udca9') },
        Array('a', 'b', 0xf0, 0x9f, 0x92, 0xa9))

    testW({ osw => osw.write("ab\ud83d"); osw.write("\udca9cd") },
        Array('a', 'b', 0xf0, 0x9f, 0x92, 0xa9, 'c', 'd'))

    testW({ osw => osw.write("ab\ud83dzz", 1, 2); osw.write("ww\udca9cd", 2, 2) },
        Array('b', 0xf0, 0x9f, 0x92, 0xa9, 'c'))
  }

  @Test def writeMalformedSurrogates(): Unit = {
    testW(_.write("\ud83da"), Array('?', 'a'))
    testW(_.write("\udca9"), Array('?'))
  }

  @Test def writeMalformedSurrogatesSpreadAcrossMultipleWrites(): Unit = {
    testW({ osw => osw.write('\ud83d'); osw.write('a') },
        Array('?', 'a'))

    testW({ osw => osw.write("ab\ud83d"); osw.write("\ud83d") },
        Array('a', 'b', '?'))

    testW({ osw => osw.write("ab\ud83d"); osw.write("\ud83dc") },
        Array('a', 'b', '?', '?', 'c'))
  }

  @Test def writeMalformedSurrogatesAtEndOfInput(): Unit = {
    testW({ osw => osw.write('\ud83d'); osw.close() },
        Array('?'), alreadyFlushed = true)

    testW({ osw => osw.write("ab\ud83d"); osw.close() },
        Array('a', 'b', '?'), alreadyFlushed = true)
  }

  @Test def constructorThrowUnsupportedEncodingExceptionIfUnsupportedCharsetNameGiven(): Unit = {
    val ex = assertThrows(classOf[UnsupportedEncodingException],
      new OutputStreamWriter(new ByteArrayOutputStream(), "UNSUPPORTED-CHARSET"))
    assertTrue("Cause should be null since constructor does not accept cause",
      ex.getCause == null)
  }
}
