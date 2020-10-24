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

package org.scalajs.testsuite.scalalib

import scala.reflect.NameTransformer

import org.junit.Test
import org.junit.Assert._

class NameTransformerTest {

  @Test def decode_Issue1602(): Unit = {
    /* Mostly to make sure it links.
     * We trust the Scala implementation for correctness. And if it isn't,
     * well, behaving the same as Scala is the correct thing do for us
     * anyway.
     */
    assertEquals("+", NameTransformer.decode("$plus"))
    assertEquals("ab+", NameTransformer.decode("ab$plus"))
    assertEquals("-", NameTransformer.decode("$minus"))
    assertEquals("+x-y", NameTransformer.decode("$plusx$minusy"))
    assertEquals("+-", NameTransformer.decode("$plus$minus"))
  }
}
