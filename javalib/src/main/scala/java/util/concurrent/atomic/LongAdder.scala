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

package java.util.concurrent.atomic

import java.io.Serializable

class LongAdder extends Number with Serializable {
  private[this] var value: Long = 0L

  final def add(x: Long): Unit =
    value = value + x

  final def increment(): Unit =
    value = value + 1

  final def decrement(): Unit =
    value = value - 1

  final def sum(): Long =
    value

  final def reset(): Unit =
    value = 0

  final def sumThenReset(): Long = {
    val result = value
    reset()
    result
  }

  override def toString(): String =
    String.valueOf(value)

  final def longValue(): Long =
    value

  final def intValue(): Int =
    value.toInt

  final def floatValue(): Float =
    value.toFloat

  final def doubleValue(): Double =
    value.toDouble
}
