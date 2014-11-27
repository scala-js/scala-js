/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import org.scalajs.jasminetest.JasmineTest

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

    it("should give a pseudo toString to unnamed values") {
      object Test extends Enumeration {
        private val nullStr: String = null
        val A = Value(nullStr) // Circumvent compiler replacement and warning
      }

      expect(Test.A.toString.startsWith(
          "<Unknown name for enum field #0 of class ")).toBeTruthy
    }

    it("should give a graceful error message upon name based query when unnamed fields are present") {
      object Test extends Enumeration {
        private val nullStr: String = null
        val A = Value(nullStr) // Circumvent compiler replacement and warning
      }

      expect(() => Test.withName("A")).toThrow
      expect {
        try { Test.withName("A"); ??? }
        catch { case e: NoSuchElementException => e.getMessage }
      } toContain {
        """Couldn't find enum field with name A.
          |However, there were the following unnamed fields:""".stripMargin
      }
    }

    it("should respond to `toString`") {
      expect(FooBarEnum.toString).toEqual("FooBarEnum")
    }

    it("should respond to `values`") {
      expect(FooBarEnum.values.toString).toEqual(
          "FooBarEnum.ValueSet(A, B, C, D, E, F)")
    }

    it("should allow setting nextName") {
      object Test extends Enumeration {
        nextName = Iterator("x","y","z")
        val a,b,c = Value
      }

      expect(Test.values.mkString("|")).toEqual("x|y|z")
    }

  }

  /** Object is here due to issues with Enumeration.toString inside closures */
  object FooBarEnum extends Enumeration {
    val A,B,C,D,E,F = Value
  }

}
