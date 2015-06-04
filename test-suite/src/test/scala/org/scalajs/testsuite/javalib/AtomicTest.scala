/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.language.implicitConversions

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

object AtomicTest extends JasmineTest {

  describe("java.util.concurrent.atomic.AtomicLong") {

    it("should have all the normal operations") {
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
      expect(atomic.compareAndSet(1, 20)).toBeFalsy
      expect(atomic.get()).toEqual(10)
      expect(atomic.compareAndSet(10, 20)).toBeTruthy
      expect(atomic.get()).toEqual(20)
    }
  }
  describe("java.util.concurrent.atomic.AtomicInteger") {
    it("should have all the normal operations") {
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
      expect(atomic.compareAndSet(1, 20)).toBeFalsy
      expect(atomic.get()).toEqual(10)
      expect(atomic.compareAndSet(10, 20)).toBeTruthy
      expect(atomic.get()).toEqual(20)
    }
  }
  describe("java.util.concurrent.atomic.AtomicBoolean") {
    it("should have all the normal operations") {
      val atomic = new java.util.concurrent.atomic.AtomicBoolean(true)
      expect(atomic.get()).toEqual(true)
      atomic.set(false)
      expect(atomic.get()).toEqual(false)
      expect(atomic.compareAndSet(true, true)).toBeFalsy
      expect(atomic.get()).toEqual(false)
      expect(atomic.compareAndSet(false, true)).toBeTruthy
      expect(atomic.get()).toEqual(true)
      expect(atomic.getAndSet(false)).toBeTruthy
      expect(atomic.get()).toEqual(false)
    }
  }
  describe("java.util.concurrent.atomic.AtomicReference") {
    it("should have all the normal operations") {
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
      expect(atomic.compareAndSet(thing1, thing1)).toBeFalsy
      expect(atomic.get()).toBe(thing2)
      expect(atomic.compareAndSet(thing2, thing1)).toBeTruthy
      expect(atomic.get()).toBe(thing1)
      expect(atomic.compareAndSet(thing1bis, thing2)).toBeFalsy
      expect(atomic.getAndSet(thing2)).toBe(thing1)
      expect(atomic.get()).toBe(thing2)
    }
  }

  describe("java.util.concurrent.atomic.AtomicReferenceArray") {
    it("should have all the normal operations") {
      val thing1 = Foo(5)
      val thing1bis = Foo(5) // equals(), but not the same reference
      val thing2 = Foo(10)

      implicit def foo2js(foo: Foo): js.Any = foo.asInstanceOf[js.Any]

      val atomic = new java.util.concurrent.atomic.AtomicReferenceArray[Foo](2)
      expect(atomic.length).toBe(2)
      expect(atomic.get(0)).toBeNull
      atomic.set(0, thing1)
      expect(atomic.get(0)).toBe(thing1)
      atomic.set(1, thing2)
      expect(atomic.get(1)).toBe(thing2)
      expect(atomic.compareAndSet(0, thing2, thing1)).toBeFalsy
      expect(atomic.get(0)).toBe(thing1)
      expect(atomic.compareAndSet(1, thing2, thing1)).toBeTruthy
      expect(atomic.get(1)).toBe(thing1)
      expect(atomic.compareAndSet(1, thing1bis, thing2)).toBeFalsy
      expect(atomic.getAndSet(1, thing2)).toBe(thing1)
      expect(atomic.get(1)).toBe(thing2)

      val initArray = Array(thing1,thing2)
      val atomic2 = new java.util.concurrent.atomic.AtomicReferenceArray[Foo](initArray)
      expect(atomic2.get(0)).toBe(thing1)
      expect(atomic2.get(1)).toBe(thing2)
    }
  }

  case class Foo(i: Int)
}
