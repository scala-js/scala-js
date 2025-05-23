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
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform

class InputStreamTestOnJDK17 {
  /** InputStream that only ever skips max bytes at once */
  def lowSkipStream(max: Int, seq: Seq[Int]): InputStream = new SeqInputStreamForTest(seq) {
    require(max > 0)

    override def skip(n: Long): Long =
      super.skip(Math.min(max.toLong, n).toInt)
  }

  private def assertBytesEqual(expect: Seq[Int], got: Array[Byte]) =
    assertArrayEquals(expect.toArray.map(_.toByte), got)

  @Test def skipNBytes(): Unit = {
    val stream = lowSkipStream(10, 0 until 100)

    assertBytesEqual(0 until 15, stream.readNBytes(15))

    stream.skipNBytes(25)

    assertBytesEqual(40 until 55, stream.readNBytes(15))

    stream.skipNBytes(45)

    assertBytesEqual(Nil, stream.readNBytes(20))
  }

  @Test def skipNBytesThrowsOnEOF(): Unit = {
    assertThrows(classOf[EOFException], lowSkipStream(10, 0 until 11).skipNBytes(20))
  }

  @Test def skipNBytesThrowsIfBadSkip(): Unit = {
    class BadSkipStream(skipResult: Long) extends InputStream {
      def read(): Int = 0
      override def skip(n: Long): Long = skipResult
    }

    assertThrows(classOf[IOException], new BadSkipStream(-1).skipNBytes(1))
    assertThrows(classOf[IOException], new BadSkipStream(2).skipNBytes(1))

    // Must not invoke skip if non-positive count
    new BadSkipStream(2).skipNBytes(0)
    new BadSkipStream(2).skipNBytes(-1)
  }

  @Test def nullInputStream(): Unit = {
    val stream = InputStream.nullInputStream()

    stream.close()

    assertThrows(classOf[IOException], stream.skipNBytes(0))
  }
}
