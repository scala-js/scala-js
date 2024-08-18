// Ported from Scala-native

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

class BufferedWriterTest {

  @Test def creatingBufferedWriterWithBufferSizeZeroThrowsException(): Unit = {
    val writer = new OutputStreamWriter(new ByteArrayOutputStream)
    assertThrows(
      classOf[IllegalArgumentException],
      new BufferedWriter(writer, -1)
    )
  }

  @Test def canWriteSmallChunksToBufferedWriter(): Unit = {
    val stringWriter = new StringWriter()
    val writer = new BufferedWriter(stringWriter)
    val string = "Hello, world!"
    writer.write(string)
    assertTrue(stringWriter.toString == "")
    writer.flush()
    assertTrue(stringWriter.toString == string)
  }

  @Test def canWriteChunkLargerThanBufferSizeToBufferedWriter(): Unit = {
    val stringWriter = new StringWriter()
    val writer = new BufferedWriter(stringWriter, 1)
    val string = "Hello, world!"
    writer.write(string)
    assertTrue(stringWriter.toString == string)
    writer.flush()
    assertTrue(stringWriter.toString == string)
  }

  @Test def canWriteInMultiplePartsToBufferedWriter(): Unit = {
    val stringWriter = new StringWriter()
    val writer = new BufferedWriter(stringWriter, 5)
    writer.write("Hell")
    assertTrue(stringWriter.toString == "")
    writer.write("o,")
    assertTrue(stringWriter.toString == "Hello,")
    writer.write(" w")
    assertTrue(stringWriter.toString == "Hello,")
    writer.flush()
    assertTrue(stringWriter.toString == "Hello, w")
    writer.write("orld!")
    assertTrue(stringWriter.toString == "Hello, world!")
  }

  @Test def writeNegativeLengthWritesNothing(): Unit = {
    val stringWriter = new StringWriter()
    val writer = new BufferedWriter(stringWriter)
    val string = "Hello, world!"
    writer.write(string, 0, -1)
    writer.flush()
    assertTrue(stringWriter.toString == "")
  }

  @Test def writeNegativeOffsetPlusLengthWritesNothing(): Unit = {
    val stringWriter = new StringWriter()
    val writer = new BufferedWriter(stringWriter)
    val string = "Hello, world!"
    writer.write(string, -2, 1)
    writer.flush()
    assertTrue(stringWriter.toString == "")
  }

  @Test def closedWritersThrow(): Unit = {
    val stream = new ByteArrayOutputStream
    val writer = new BufferedWriter(new OutputStreamWriter(stream))
    writer.close()
    assertThrows(classOf[IOException], writer.write("Hello, world!"))
  }

  @Test def closingTwiceIsHarmless(): Unit = {
    val stream = new ByteArrayOutputStream
    val writer = new BufferedWriter(new OutputStreamWriter(stream))
    writer.close()
    writer.close()
  }

  @Test def canWriteNewLineSeparator(): Unit = {
    val stringWriter = new StringWriter()
    val writer = new BufferedWriter(stringWriter)
    writer.write("Hello")
    writer.newLine()
    writer.write("world!")
    writer.flush()
    val expectedResult = "Hello" + System.lineSeparator() + "world!"
    assertTrue(stringWriter.toString == expectedResult)
  }
}
