/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package scalalib

import scala.scalajs.test.JasmineTest

object EnumerationTest extends JasmineTest {

  describe("scala.Enumeration") {

    it("should use explicit naming for enumerated values - #38") {
      object HelpLevel extends Enumeration {
        type HelpLevel = Value
        val None = Value("None")
        val Basic = Value("Basic")
        val Medium = Value("Medium")
        val Full = Value("Full")
      }

      val h = HelpLevel.None

      expect(h.toString).toEqual("None")
    }

    it("should allow implicit naming for values") {
      object HelpLevel extends Enumeration {
        type HelpLevel = Value
        val None, Basic, Medium, Full = Value
        val Special = Value(100)
        val / = Value
      }

      val h = HelpLevel.Medium
      expect(h.toString).toEqual("Medium")
      expect(HelpLevel.Special.toString).toEqual("Special")
      expect(HelpLevel./.toString).toEqual("$div")
    }

    it("should respond to `toString`") {
      expect(FooBarEnum.toString).toEqual("FooBarEnum")
    }

    it("should respond to `values`") {
      expect(FooBarEnum.values.toString).toEqual(
          "FooBarEnum.ValueSet(A, B, C, D, E, F)")
    }

  }

  /** Object is here due to issues with Enumeration.toString inside closures */
  object FooBarEnum extends Enumeration {
    val A,B,C,D,E,F = Value
  }

}
