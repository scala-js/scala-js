/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.language.implicitConversions

import scala.collection.mutable

import scala.scalajs.js
import js.|

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.Typechecking._

import js.JSConverters._

object UnionTypeTest extends JasmineTest {

  private implicit def unionAsJSAny(x: _ | _): js.Any = x.asInstanceOf[js.Any]

  describe("js.| (postive)") {

    it("Left and right") {
      val x1: Int | String = 4
      expect(x1).toBe(4)

      val x2: Int | String = "hello"
      expect(x2).toBe("hello")
    }

    it("Left and right with subtyping") {
      val list = List(1, 2, 3)

      val x1: Seq[Int] | CharSequence = list
      expect(x1.isInstanceOf[List[_]]).toBeTruthy
      expect(x1.asInstanceOf[List[_]].toJSArray).toEqual(js.Array(1, 2, 3))

      val x2: Seq[Int] | CharSequence = "hello"
      expect(x2).toBe("hello")
    }

    it("Three types") {
      val x1: Int | String | Boolean = 3
      expect(x1).toBe(3)

      val x2: Int | String | Boolean = "hello"
      expect(x2).toBe("hello")

      val x3: Int | String | Boolean = false
      expect(x3).toBe(false)
    }

    it("Upcast") {
      val x1: List[Int] | String = "hello"
      val x2: Seq[Int] | CharSequence = x1
      expect(x2).toBe("hello")
    }

    it("Int as Double") {
      val x1: Double | String = 3
      expect(x1).toBe(3)
    }

    it("Swap base types") {
      val x1: Int | String = 3
      val x2: String | Int = x1
      expect(x2).toBe(3)
    }

    it("Permutations for 3 base types") {
      val x: Int | String | Boolean = 3

      val x1: Int | Boolean | String = x
      val x2: String | Int | Boolean = x
      val x3: String | Boolean | Int = x
      val x4: Boolean | Int | String = x
      val x5: Boolean | String | Int = x

      expect(x1).toBe(3)
      expect(x2).toBe(3)
      expect(x3).toBe(3)
      expect(x4).toBe(3)
      expect(x5).toBe(3)
    }

    it("Permutations of 2 base types to 3 base types") {
      val x1: Int | String = 3
      val x2: Int | Boolean = false
      val x3: Boolean | String = "hello"

      val y1: Int | String | Boolean = x1
      val y2: Int | String | Boolean = x2
      val y3: Int | String | Boolean = x3

      expect(y1).toBe(3)
      expect(y2).toBe(false)
      expect(y3).toBe("hello")
    }

    it("Partial upper bound") {
      val x: Int | String | Boolean = "hello"

      val x1: AnyVal | String = x
      val x2: String | AnyVal = x

      expect(x1).toBe("hello")
      expect(x2).toBe("hello")

      /* Note: the *total* upper bound does not work without an explicit
       * `merge`, because the expected type is not an | type.
       */
    }

    it("merge") {
      val x1: Int | Boolean = 4
      val y1: AnyVal = x1.merge
      expect(y1.asInstanceOf[js.Any]).toBe(4)

      val x2: String | java.nio.CharBuffer = "hello"
      val y2: CharSequence = x2.merge
      expect(y2.asInstanceOf[js.Any]).toBe("hello")

      val x3: Int | String | Boolean | java.nio.CharBuffer = "hello"
      val y3: CharSequence | AnyVal = x3.merge
      expect(y3.asInstanceOf[js.Any]).toBe("hello")

      val x4: List[Int] | Vector[Int] | mutable.Buffer[Int] = List(3, 5)
      val y4: Seq[Int] = x4.merge
      expect(y4.toJSArray).toEqual(js.Array(3, 5))
    }

    it("js.UndefOr[A | B] inference") {
      val a: String = "hello"

      expect(a: Int | String).toBe(a)
      expect(a: js.UndefOr[Int] | String).toBe(a)
      expect(a: Int | js.UndefOr[String]).toBe(a)
      expect(a: js.UndefOr[Int] | js.UndefOr[String]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[Int]] | js.UndefOr[String]).toBe(a)
      expect(a: js.UndefOr[Int] | js.UndefOr[js.UndefOr[String]]).toBe(a)

      expect(a: js.UndefOr[Int | String]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[Int] | String]).toBe(a)
      expect(a: js.UndefOr[Int | js.UndefOr[String]]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[Int] | js.UndefOr[String]]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[js.UndefOr[Int]] | js.UndefOr[String]]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[Int] | js.UndefOr[js.UndefOr[String]]]).toBe(a)

      expect(a: js.UndefOr[String | Int]).toBe(a)
      expect(a: js.UndefOr[String | Int]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[String] | Int]).toBe(a)
      expect(a: js.UndefOr[String | js.UndefOr[Int]]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[String] | js.UndefOr[Int]]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[String] | js.UndefOr[js.UndefOr[Int]]]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[js.UndefOr[String]] | js.UndefOr[Int]]).toBe(a)

      // Confirm that we're working with triple unions too

      expect(a: js.UndefOr[String | Object | Int]).toBe(a)
      expect(a: js.UndefOr[String | Int | Object]).toBe(a)
      expect(a: js.UndefOr[Int | String | Object]).toBe(a)
      expect(a: js.UndefOr[Int | Object | String]).toBe(a)
      expect(a: js.UndefOr[Object | String | Int]).toBe(a)
      expect(a: js.UndefOr[Object | Object | String]).toBe(a)

      expect(a: js.UndefOr[js.UndefOr[String] | Object | Int]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[String] | Int | Object]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[Int] | String | Object]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[Int] | Object | String]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[Object] | String | Int]).toBe(a)
      expect(a: js.UndefOr[js.UndefOr[Object] | Object | String]).toBe(a)
    }
  }

  describe("js.| (negative)") {

    /* Error messages vary a lot depending on the version of Scala, so we do
     * not test them.
     */

    it("Neither left nor right") {
      typeError(
          "3: Boolean | String")
    }

    it("None of three types") {
      typeError(
          "3: Boolean | String | List[Int]")
    }

    it("Wrong type parameter on left or right") {
      typeError(
          "List(1, 2): List[String] | String")
      typeError(
          "List(1, 2): String | List[String]")
    }

    it("Left of |-type is not a subtype of rhs") {
      typeError(
          "(1: Int | List[String]): String | List[String]")
    }

    it("Right of |-type is not a subtype of rhs") {
      typeError(
          "(1: Int | List[String]): String | Int")
    }

    it("merge with an incorrect subtype") {
      typeError(
          "(List(1, 2): List[Int] | Set[Int]).merge: Seq[Int]")
    }

  }
}
