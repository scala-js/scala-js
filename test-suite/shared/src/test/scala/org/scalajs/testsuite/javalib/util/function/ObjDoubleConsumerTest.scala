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

class ObjDoubleConsumerTest {
  @Test def accept(): Unit = {
    // side-effects
    var current: String = ""

    val op = new ObjDoubleConsumer[String] {
      override def accept(left: String, right: Double): Unit = current += s"$left $right "
    }

    op.accept("First", 1.1)
    op.accept("Second", 2.2)
    assertEquals(current, "First 1.1 Second 2.2 ")
  }
}
