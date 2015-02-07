/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js

object OptimizerTest extends JasmineTest {

  describe("Inlineable classes") {

    it("must update fields of `this` in the computation of other fields - #1153") {
      val foo = new InlineClassDependentFields(5)
      expect(foo.x).toEqual(5)
      expect(foo.b).toBeTruthy
      expect(foo.y).toEqual(11)
    }

    it("must not break code that assigns `this` to a field") {
      val foo = new InlineClassThisAlias(5)
      expect(foo.z).toEqual(5)
    }

  }

  describe("Optimizer regression tests") {

    it("must not break * (-1) for Int - #1453") {
      @noinline
      def start0: Int = (() => 10)()

      val start = start0
      val step = -1
      val numRangeElements = start - 1
      val lastElement = start + (numRangeElements - 1) * step
      expect(lastElement).toEqual(2)
    }

    it("must not break * (-1) for Float and Double - #1478") {
      @noinline
      def a: Float = (() => 5.0f)()
      expect(a * -1.0f).toEqual(-5.0f)

      @noinline
      def b: Double = (() => 7.0)()
      expect(b * -1.0).toEqual(-7.0)
    }

    it("must not break foreach on downward Range - #1453") {
      @noinline
      def start0: Int = (() => 10)()

      val elements = js.Array[Int]()
      for (i <- start0 to 2 by -1) {
        if (i < 0)
          sys.error("Going into infinite loop")
        elements.push(i)
      }
      expect(elements).toEqual(js.Array(10, 9, 8, 7, 6, 5, 4, 3, 2))
    }

  }

  @inline
  class InlineClassDependentFields(val x: Int) {
    val b = x > 3
    val y = if (b) x + 6 else x-2
  }

  @inline
  class InlineClassThisAlias(val x: Int) {
    val t = this
    val y = x
    val z = t.y
  }

}
