/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent.atomic

import scala.language.implicitConversions

import org.junit.Test
import org.junit.Assert._

class AtomicTest {

  @Test def atomicLongTest(): Unit = {
    val atomic = new java.util.concurrent.atomic.AtomicLong(10)
    assertEquals(10L, atomic.get())
    atomic.set(20)
    assertEquals(20L, atomic.get())
    assertEquals(20L, atomic.getAndIncrement())
    assertEquals(21L, atomic.get())
    assertEquals(21L, atomic.getAndDecrement())
    assertEquals(20L, atomic.get())
    assertEquals(20L, atomic.getAndSet(0))
    assertEquals(0L, atomic.get())
    assertEquals(1L, atomic.incrementAndGet())
    assertEquals(1L, atomic.get())
    assertEquals(0L, atomic.decrementAndGet())
    assertEquals(0L, atomic.get())
    assertEquals(10L, atomic.addAndGet(10))
    assertEquals(10L, atomic.get())
    assertEquals(10, atomic.intValue)
    assertEquals(10L, atomic.longValue)
    assertEquals(10.0f, atomic.floatValue, 0.0f)
    assertEquals(10.0, atomic.doubleValue, 0.0)
    assertFalse(atomic.compareAndSet(1, 20))
    assertEquals(10L, atomic.get())
    assertTrue(atomic.compareAndSet(10, 20))
    assertEquals(20L, atomic.get())
  }

  @Test def atomicIntegerTest(): Unit = {
    val atomic = new java.util.concurrent.atomic.AtomicInteger(10)
    assertEquals(10, atomic.get())
    atomic.set(20)
    assertEquals(20, atomic.get())
    assertEquals(20, atomic.getAndIncrement())
    assertEquals(21, atomic.get())
    assertEquals(21, atomic.getAndDecrement())
    assertEquals(20, atomic.get())
    assertEquals(20, atomic.getAndSet(0))
    assertEquals(0, atomic.get())
    assertEquals(1, atomic.incrementAndGet())
    assertEquals(1, atomic.get())
    assertEquals(0, atomic.decrementAndGet())
    assertEquals(0, atomic.get())
    assertEquals(10, atomic.addAndGet(10))
    assertEquals(10, atomic.get())
    assertEquals(10, atomic.intValue)
    assertEquals(10L, atomic.longValue)
    assertEquals(10.0f, atomic.floatValue, 0.0f)
    assertEquals(10, atomic.doubleValue, 0.0)
    assertFalse(atomic.compareAndSet(1, 20))
    assertEquals(10, atomic.get())
    assertTrue(atomic.compareAndSet(10, 20))
    assertEquals(20, atomic.get())
  }

  @Test def atomicBooleanTest(): Unit = {
    val atomic = new java.util.concurrent.atomic.AtomicBoolean(true)
    assertTrue(atomic.get())
    atomic.set(false)
    assertFalse(atomic.get())
    assertFalse(atomic.compareAndSet(true, true))
    assertFalse(atomic.get())
    assertTrue(atomic.compareAndSet(false, true))
    assertTrue(atomic.get())
    assertTrue(atomic.getAndSet(false))
    assertFalse(atomic.get())
  }

  @Test def atomicReferenceTest(): Unit = {
    val thing1 = Foo(5)
    val thing1bis = Foo(5) // equals(), but not the same reference
    val thing2 = Foo(10)

    // sanity
    assertEquals(thing1bis, thing1)
    assertNotEquals(thing1, thing2)
    assertSame(thing1, thing1)
    assertNotSame(thing1bis, thing1)

    // actual test
    val atomic = new java.util.concurrent.atomic.AtomicReference(thing1)
    assertSame(thing1, atomic.get())
    atomic.set(thing2)
    assertSame(thing2, atomic.get())
    assertFalse(atomic.compareAndSet(thing1, thing1))
    assertSame(thing2, atomic.get())
    assertTrue(atomic.compareAndSet(thing2, thing1))
    assertSame(thing1, atomic.get())
    assertFalse(atomic.compareAndSet(thing1bis, thing2))
    assertSame(thing1, atomic.getAndSet(thing2))
    assertSame(thing2, atomic.get())
  }

  @Test def atomicReferenceArrayTest(): Unit = {
    val thing1 = Foo(5)
    val thing1bis = Foo(5) // equals(), but not the same reference
    val thing2 = Foo(10)

    val atomic = new java.util.concurrent.atomic.AtomicReferenceArray[Foo](2)
    assertEquals(2, atomic.length)
    assertNull(atomic.get(0))
    atomic.set(0, thing1)
    assertSame(thing1, atomic.get(0))
    atomic.set(1, thing2)
    assertSame(thing2, atomic.get(1))
    assertFalse(atomic.compareAndSet(0, thing2, thing1))
    assertSame(thing1, atomic.get(0))
    assertTrue(atomic.compareAndSet(1, thing2, thing1))
    assertSame(thing1, atomic.get(1))
    assertFalse(atomic.compareAndSet(1, thing1bis, thing2))
    assertSame(thing1, atomic.getAndSet(1, thing2))
    assertSame(thing2, atomic.get(1))

    val initArray = Array(thing1,thing2)
    val atomic2 = new java.util.concurrent.atomic.AtomicReferenceArray[Foo](initArray)
    assertSame(thing1, atomic2.get(0))
    assertSame(thing2, atomic2.get(1))
  }

  @Test def atomicLongArrayTest(): Unit = {
    val a = 7L
    val b = 17L
    val c = 177L

    val atomic = new java.util.concurrent.atomic.AtomicLongArray(2)
    assertEquals(2, atomic.length)
    assertEquals(0L, atomic.get(0))
    atomic.set(0, a)
    assertEquals(a, atomic.get(0))
    atomic.set(1, b)
    assertEquals(b, atomic.get(1))
    assertFalse(atomic.compareAndSet(0, b, a))
    assertEquals(a, atomic.get(0))
    assertTrue(atomic.compareAndSet(1, b, a))
    assertEquals(a, atomic.get(1))
    assertFalse(atomic.compareAndSet(1, c, b))
    assertEquals(a, atomic.getAndSet(1, b))
    assertEquals(b, atomic.get(1))
    assertEquals(b, atomic.getAndIncrement(1))
    assertEquals(b + 1, atomic.get(1))
    assertEquals(b + 2, atomic.incrementAndGet(1))
    assertEquals(b + 2, atomic.getAndDecrement(1))
    assertEquals(b, atomic.decrementAndGet(1))
    assertEquals(a, atomic.getAndAdd(0, 2))
    assertEquals(a + 2, atomic.get(0))
    assertEquals(a, atomic.addAndGet(0, -2))

    val initArray = Array(a, b)
    val atomic2 = new java.util.concurrent.atomic.AtomicLongArray(initArray)
    assertEquals(a, atomic2.get(0))
    assertEquals(b, atomic2.get(1))
    initArray(0) = c
    assertEquals(a, atomic2.get(0))
  }
}

case class Foo(i: Int)
