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

package org.scalajs.testsuite.javalib.util.function

import org.junit.Assert._
import org.junit.Test

import java.util.function._

class LongFunctionTest {
  @Test def testApply(): Unit = {
    val f = new LongFunction[Seq[Long]] {
      override def apply(value: Long): Seq[Long] = List.fill(value.toInt)(value)
    }
    assertEquals(f.apply(1L), Seq(1L))
    assertEquals(f.apply(3L), Seq(3L, 3L, 3L))
  }
}
