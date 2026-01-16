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

import java.util.function.LongBinaryOperator
import java.util.function.LongUnaryOperator

class AtomicLong(private[this] var value: Long) extends Number with Serializable {
  def this() = this(0L)

  final def get(): Long = value

  final def set(newValue: Long): Unit =
    value = newValue

  final def lazySet(newValue: Long): Unit =
    set(newValue)

  final def getAndSet(newValue: Long): Long = {
    val old = value
    value = newValue
    old
  }

  final def compareAndSet(expect: Long, update: Long): Boolean = {
    if (expect != value) false
    else {
      value = update
      true
    }
  }

  final def weakCompareAndSet(expect: Long, update: Long): Boolean =
    compareAndSet(expect, update)

  final def getAndIncrement(): Long =
    getAndAdd(1L)

  final def getAndDecrement(): Long =
    getAndAdd(-1L)

  @inline final def getAndAdd(delta: Long): Long = {
    val old = value
    value = old + delta
    old
  }

  final def incrementAndGet(): Long =
    addAndGet(1L)

  final def decrementAndGet(): Long =
    addAndGet(-1L)

  @inline final def addAndGet(delta: Long): Long = {
    val newValue = value + delta
    value = newValue
    newValue
  }

  final def getAndUpdate(updateFunction: LongUnaryOperator): Long = {
    val old = value
    value = updateFunction.applyAsLong(old)
    old
  }

  final def updateAndGet(updateFunction: LongUnaryOperator): Long = {
    val old = value
    value = updateFunction.applyAsLong(old)
    value
  }

  final def getAndAccumulate(x: Long, accumulatorFunction: LongBinaryOperator): Long = {
    val old = value
    value = accumulatorFunction.applyAsLong(old, x)
    old
  }

  final def accumulateAndGet(x: Long, accumulatorFunction: LongBinaryOperator): Long = {
    val old = value
    value = accumulatorFunction.applyAsLong(old, x)
    value
  }

  override def toString(): String =
    value.toString()

  def intValue(): Int = value.toInt
  def longValue(): Long = value
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value.toDouble
}
