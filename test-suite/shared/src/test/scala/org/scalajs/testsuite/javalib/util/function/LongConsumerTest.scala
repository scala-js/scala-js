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

class LongConsumerTest {
  @Test def accept(): Unit = {
    // side-effects
    var current: Long = 0

    val add = new LongConsumer {
      override def accept(value: Long): Unit = current += value
    }

    add.accept(3)
    assertEquals(current, 3)
    add.accept(-10)
    assertEquals(current, -7)
  }

  @Test def andThen(): Unit = {
    // side-effects
    var buffer = scala.collection.mutable.ListBuffer.empty[Long]

    val add = new LongConsumer {
      override def accept(value: Long): Unit = buffer += value
    }
    val add10x = new LongConsumer {
      override def accept(value: Long): Unit = buffer += value * 10
    }
    val f: LongConsumer = add.andThen(add10x)

    f.accept(1)
    assertEquals(List(1L, 10L), buffer.toList)
    f.accept(2)
    assertEquals(List(1L, 10L, 2L, 20L), buffer.toList)
  }
}
