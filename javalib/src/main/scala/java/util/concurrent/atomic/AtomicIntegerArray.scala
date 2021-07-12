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

class AtomicIntegerArray(length: Int) extends Serializable {
  def this(array: Array[Int]) = {
    this(array.length)
    System.arraycopy(array, 0, inner, 0, length)
  }

  private val inner: Array[Int] = new Array[Int](length)

  final def length(): Int =
    inner.length

  final def get(i: Int): Int =
    inner(i)

  final def set(i: Int, newValue: Int): Unit =
    inner(i) = newValue

  final def lazySet(i: Int, newValue: Int): Unit =
    set(i, newValue)

  final def getAndSet(i: Int, newValue: Int): Int = {
    val ret = get(i)
    set(i, newValue)
    ret
  }

  final def compareAndSet(i: Int, expect: Int, update: Int): Boolean = {
    if (get(i) != expect) {
      false
    } else {
      set(i, update)
      true
    }
  }

  final def weakCompareAndSet(i: Int, expect: Int, update: Int): Boolean =
    compareAndSet(i, expect, update)

  final def getAndIncrement(i: Int): Int =
    getAndAdd(i, 1)

  final def getAndDecrement(i: Int): Int =
    getAndAdd(i, -1)

  final def getAndAdd(i: Int, delta: Int): Int = {
    val ret = get(i)
    set(i, ret + delta)
    ret
  }

  final def incrementAndGet(i: Int): Int =
    addAndGet(i, 1)

  final def decrementAndGet(i: Int): Int =
    addAndGet(i, -1)

  final def addAndGet(i: Int, delta: Int): Int = {
    set(i, get(i) + delta)
    get(i)
  }

  override def toString(): String =
    java.util.Arrays.toString(inner)
}
