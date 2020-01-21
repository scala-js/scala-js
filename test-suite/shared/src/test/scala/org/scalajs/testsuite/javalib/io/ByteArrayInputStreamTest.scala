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

class ByteArrayInputStreamTest extends CommonStreamsTests {
  def mkStream(seq: Seq[Int]): InputStream = {
    new ByteArrayInputStream(seq.map(_.toByte).toArray)
  }

  @Test
  def readWithZeroLengthReturnsMinus1AtEOF_issue_3913(): Unit = {
    /* Contrary to the spec in the base class InputStream, read(_, _, 0) must
     * return -1 if the stream is at the end of the buffer, instead of 0.
     */
    val stream = new ByteArrayInputStream(new Array(5))
    val buf = new Array[Byte](10)
    assertEquals(5, stream.read(buf, 0, 5))
    assertEquals(-1, stream.read(buf, 0, 0))
  }
}
