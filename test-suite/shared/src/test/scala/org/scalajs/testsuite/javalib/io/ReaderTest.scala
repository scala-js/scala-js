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

/** Tests for our implementation of java.io._ reader classes */
class ReaderTest {
  object MyReader extends java.io.Reader {
    def read(dbuf: Array[Char], off: Int, len: Int): Int = {
      java.util.Arrays.fill(dbuf, off, off + len, 'A')
      len
    }

    def close(): Unit = ()
  }

  @Test def skipIntIfPossible(): Unit = {
    assertEquals(42, MyReader.skip(42))
    assertEquals(10000, MyReader.skip(10000)) // more than the 8192 batch size
  }
}
