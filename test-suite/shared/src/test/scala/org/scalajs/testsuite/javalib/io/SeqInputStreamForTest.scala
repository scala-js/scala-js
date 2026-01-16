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

class SeqInputStreamForTest(seq: Seq[Int]) extends InputStream {
  private var i: Int = 0
  private var m: Int = 0

  def read(): Int = {
    if (i < seq.length) {
      val e = seq(i)
      i += 1
      e & 0xff
    } else {
      -1
    }
  }

  override def available(): Int = seq.length - i

  override def mark(readlimit: Int): Unit = m = i

  override def reset(): Unit = i = m

  override def markSupported(): Boolean = true
}
