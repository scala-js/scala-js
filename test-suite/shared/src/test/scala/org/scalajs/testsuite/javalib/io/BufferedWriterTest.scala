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
    writer.flush()
    assertTrue(stringWriter.toString == string)
  }

  @Test def canWriteChunkLargerThanBufferSizeToBufferedWriter(): Unit = {
    val stringWriter = new StringWriter()
    val writer = new BufferedWriter(stringWriter, 1)
    val string = "Hello, world!"
    writer.write(string)
    writer.flush()
    assertTrue(stringWriter.toString == string)
  }

  @Test def closingTwiceIsHarmless(): Unit = {
    val stream = new ByteArrayOutputStream
    val writer = new BufferedWriter(new OutputStreamWriter(stream))
    writer.close()
    writer.close()
  }
}
