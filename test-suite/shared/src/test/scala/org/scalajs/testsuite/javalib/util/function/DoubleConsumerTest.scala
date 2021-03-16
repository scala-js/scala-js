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

class DoubleConsumerTest {
  @Test def accept(): Unit = {
    // Side-effects
    var current: Double = 0

    val add = new DoubleConsumer {
      override def accept(value: Double): Unit = current += value
    }

    add.accept(5)
    assertEquals(5, current, 0)
    add.accept(15)
    assertEquals(20, current, 0)
  }

  @Test def andThen(): Unit = {
    // Side-effects
    var buffer = scala.collection.mutable.ListBuffer.empty[Double]

    val add = new DoubleConsumer {
      override def accept(value: Double): Unit = buffer += value
    }
    val add2x = new DoubleConsumer {
      override def accept(value: Double): Unit = buffer += value * 2
    }
    val merged: DoubleConsumer = add.andThen(add2x)

    merged.accept(1d)
    assertEquals(List(1d, 2d), buffer.toList)
    merged.accept(4d)
    assertEquals(List(1d, 2d, 4d, 8d), buffer.toList)
  }
}
