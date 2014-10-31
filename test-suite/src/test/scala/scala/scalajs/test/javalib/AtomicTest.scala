/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test.javalib

import scala.language.implicitConversions

import scala.scalajs.js

import scala.scalajs.test.JasmineTest

object AtomicTest extends JasmineTest {

  describe("java.util.concurrent.AtomicLong") {

    it("Should have all the normal operations") {
      val atomic = new java.util.concurrent.atomic.AtomicLong(10)
      expect(atomic.get()).toEqual(10)
      atomic.set(20)
      expect(atomic.get()).toEqual(20)
      expect(atomic.getAndIncrement()).toEqual(20)
      expect(atomic.get()).toEqual(21)
      expect(atomic.getAndDecrement()).toEqual(21)
      expect(atomic.get()).toEqual(20)
      expect(atomic.getAndSet(0)).toEqual(20)
      expect(atomic.get()).toEqual(0)
      expect(atomic.incrementAndGet()).toEqual(1)
      expect(atomic.get()).toEqual(1)
      expect(atomic.decrementAndGet()).toEqual(0)
      expect(atomic.get()).toEqual(0)
      expect(atomic.addAndGet(10)).toEqual(10)
      expect(atomic.get()).toEqual(10)
      expect(atomic.intValue).toEqual(10)
      expect(atomic.longValue).toEqual(10L)
      expect(atomic.floatValue).toEqual(10.0f)
      expect(atomic.doubleValue).toEqual(10)
      expect(atomic.compareAndSet(1, 20)).toEqual(false)
      expect(atomic.get()).toEqual(10)
      expect(atomic.compareAndSet(10, 20)).toEqual(true)
      expect(atomic.get()).toEqual(20)
    }
  }
  describe("java.util.concurrent.AtomicInteger") {
    it("Should have all the normal operations") {
      val atomic = new java.util.concurrent.atomic.AtomicInteger(10)
      expect(atomic.get()).toEqual(10)
      atomic.set(20)
      expect(atomic.get()).toEqual(20)
      expect(atomic.getAndIncrement()).toEqual(20)
      expect(atomic.get()).toEqual(21)
      expect(atomic.getAndDecrement()).toEqual(21)
      expect(atomic.get()).toEqual(20)
      expect(atomic.getAndSet(0)).toEqual(20)
      expect(atomic.get()).toEqual(0)
      expect(atomic.incrementAndGet()).toEqual(1)
      expect(atomic.get()).toEqual(1)
      expect(atomic.decrementAndGet()).toEqual(0)
      expect(atomic.get()).toEqual(0)
      expect(atomic.addAndGet(10)).toEqual(10)
      expect(atomic.get()).toEqual(10)
      expect(atomic.intValue).toEqual(10)
      expect(atomic.longValue).toEqual(10L)
      expect(atomic.floatValue).toEqual(10.0f)
      expect(atomic.doubleValue).toEqual(10)
      expect(atomic.compareAndSet(1, 20)).toEqual(false)
      expect(atomic.get()).toEqual(10)
      expect(atomic.compareAndSet(10, 20)).toEqual(true)
      expect(atomic.get()).toEqual(20)
    }
  }
  describe("java.util.concurrent.AtomicBoolean") {
    it("Should have all the normal operations") {
      val atomic = new java.util.concurrent.atomic.AtomicBoolean(true)
      expect(atomic.get()).toEqual(true)
      atomic.set(false)
      expect(atomic.get()).toEqual(false)
      expect(atomic.compareAndSet(true, true)).toEqual(false)
      expect(atomic.get()).toEqual(false)
      expect(atomic.compareAndSet(false, true)).toEqual(true)
      expect(atomic.get()).toEqual(true)
      expect(atomic.getAndSet(false)).toEqual(true)
      expect(atomic.get()).toEqual(false)
    }
  }
  describe("java.util.concurrent.AtomicReference") {
    it("Should have all the normal operations") {
      val thing1 = Foo(5)
      val thing1bis = Foo(5) // equals(), but not the same reference
      val thing2 = Foo(10)

      implicit def foo2js(foo: Foo): js.Any = foo.asInstanceOf[js.Any]

      // sanity
      expect(thing1 == thing1bis).toBeTruthy
      expect(thing1 == thing2).toBeFalsy
      expect(thing1).toBe(thing1)
      expect(thing1).not.toBe(thing1bis)

      // actual test
      val atomic = new java.util.concurrent.atomic.AtomicReference(thing1)
      expect(atomic.get()).toBe(thing1)
      atomic.set(thing2)
      expect(atomic.get()).toBe(thing2)
      expect(atomic.compareAndSet(thing1, thing1)).toEqual(false)
      expect(atomic.get()).toBe(thing2)
      expect(atomic.compareAndSet(thing2, thing1)).toEqual(true)
      expect(atomic.get()).toBe(thing1)
      expect(atomic.compareAndSet(thing1bis, thing2)).toEqual(false)
      expect(atomic.getAndSet(thing2)).toBe(thing1)
      expect(atomic.get()).toBe(thing2)
    }
  }

  case class Foo(i: Int)
}
