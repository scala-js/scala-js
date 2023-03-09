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

import java.util.function.BinaryOperator
import java.util.function.UnaryOperator

class AtomicReference[T <: AnyRef](
    private[this] var value: T) extends Serializable {

  def this() = this(null.asInstanceOf[T])

  final def get(): T = value

  final def set(newValue: T): Unit =
    value = newValue

  final def lazySet(newValue: T): Unit =
    set(newValue)

  final def compareAndSet(expect: T, update: T): Boolean = {
    if (expect ne value) false else {
      value = update
      true
    }
  }

  final def weakCompareAndSet(expect: T, update: T): Boolean =
    compareAndSet(expect, update)

  final def getAndSet(newValue: T): T = {
    val old = value
    value = newValue
    old
  }

  final def getAndUpdate(updateFunction: UnaryOperator[T]): T = {
    val old = value
    value = updateFunction.apply(old)
    old
  }

  final def updateAndGet(updateFunction: UnaryOperator[T]): T = {
    val old = value
    value = updateFunction.apply(old)
    value
  }

  final def getAndAccumulate(x: T, accumulatorFunction: BinaryOperator[T]): T = {
    val old = value
    value = accumulatorFunction.apply(old, x)
    old
  }

  final def accumulateAndGet(x: T, accumulatorFunction: BinaryOperator[T]): T = {
    val old = value
    value = accumulatorFunction.apply(old, x)
    value
  }

  override def toString(): String =
    String.valueOf(value)
}
