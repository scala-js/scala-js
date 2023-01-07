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

class InputStreamTestOnJDK11 {
  /** InputStream that only ever reads max bytes at once */
  def chunkedStream(max: Int, seq: Seq[Int]): InputStream = new SeqInputStreamForTest(seq) {
    require(max > 0)

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      val newLen = Math.min(max, len)
      super.read(b, off, newLen)
    }
  }

  def emptyStream(): InputStream = new InputStream {
    def read(): Int = -1
  }

  private def assertBytesEqual(expect: Seq[Int], got: Array[Byte]) =
    assertArrayEquals(expect.toArray.map(_.toByte), got)

  @Test def readAllBytes(): Unit = {
    assertBytesEqual(0 until 100, chunkedStream(10, 0 until 100).readAllBytes())
    assertBytesEqual(Nil, emptyStream().readAllBytes())
  }

  @Test def readNBytes(): Unit = {
    assertBytesEqual(0 until 20, chunkedStream(10, 0 until 100).readNBytes(20))
    assertBytesEqual(0 until 100, chunkedStream(10, 0 until 100).readNBytes(200))
    assertBytesEqual(Nil, chunkedStream(10, 0 until 100).readNBytes(0))
    assertBytesEqual(Nil, emptyStream().readNBytes(1000))

    // test buffer growing
    assertBytesEqual(0 until 2000, chunkedStream(200, 0 until 2000).readNBytes(2000))

    assertThrows(classOf[IllegalArgumentException], emptyStream().readNBytes(-1))
  }

  @Test def readNBytesBuf(): Unit = {
    val buf = new Array[Byte](30)

    chunkedStream(10, 0 until 100).readNBytes(buf, 2, 22)

    assertBytesEqual(Seq.fill(2)(0) ++ (0 until 22) ++ Seq.fill(6)(0), buf)
  }

  @Test def transferTo(): Unit = {
    val stream = chunkedStream(10, 0 until 100)
    val out = new ByteArrayOutputStream()
    stream.transferTo(out)

    assertBytesEqual(0 until 100, out.toByteArray())
  }

  @Test def transferToThrowsNPE(): Unit = {
    assumeTrue("assumed compliant NPEs", Platform.hasCompliantNullPointers)
    // nothing to write, should still throw.
    assertThrows(classOf[NullPointerException], emptyStream().transferTo(null))
  }
}
