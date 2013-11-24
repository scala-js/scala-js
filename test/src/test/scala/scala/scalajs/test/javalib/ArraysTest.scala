/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.js
import scala.scalajs.test.ScalaJSTest

import java.util.{ Arrays, Comparator }

object ArraysTest extends ScalaJSTest {

  val stringComparator = new Comparator[String]() {
    def compare(s1: String, s2: String) = s1.compareTo(s2)
  }

  val intComparator = new Comparator[Int]() {
    def compare(i1: Int, i2: Int) = i1 - i2
  }

  describe("java.util.Arrays") {

    it("should respond to `sort` for Int") {
      val scalaInts = Array(5, 3, 6, 1, 2, 4)
      val ints = new Array[Object](scalaInts.length)
      for (i <- 0 until scalaInts.length)
        ints(i) = scalaInts(i).asInstanceOf[Object]
      val sorted = Array(1, 2, 3, 4, 5, 6)

      Arrays.sort(ints, intComparator.asInstanceOf[Comparator[Object]])
      expect(ints).toEqual(Array(1, 2, 3, 4, 5, 6))
    }

    it("should respond to `sort` for String") {
      val scalajs: Array[Object] = Array("S", "c", "a", "l", "a", ".", "j", "s")
      val sorted = Array(".", "S", "a", "a", "c", "j", "l", "s")

      Arrays.sort(scalajs, stringComparator.asInstanceOf[Comparator[Object]])
      expect(scalajs).toEqual(sorted)
    }

    it("should respond to `fill` for Int") {
      val ints = new Array[Int](6)
      Arrays.fill(ints, 42)
      expect(ints).toEqual(Array(42, 42, 42, 42, 42, 42))

      Arrays.fill(ints, -1)
      expect(ints).toEqual(Array(-1, -1, -1, -1, -1, -1))
    }

  }
}
