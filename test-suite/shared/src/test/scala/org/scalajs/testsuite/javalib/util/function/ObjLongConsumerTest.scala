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

class ObjLongConsumerTest {
  @Test def accept(): Unit = {
    // side-effects
    var current: String = ""

    val op = new ObjLongConsumer[String] {
      override def accept(left: String, right: Long): Unit = current += s"$left $right "
    }
    op.accept("First", 2L)
    op.accept("Second", 3L)
    assertEquals(current, "First 2 Second 3 ")
  }
}
