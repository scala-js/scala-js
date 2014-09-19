/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.scalalib

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object SymbolTest extends JasmineTest {

  describe("scala.Symbol") {

    it("should ensure unique identity") {
      def expectEqual(sym1: Symbol, sym2: Symbol): Unit = {
        expect(sym1 eq sym2).toBeTruthy
        expect(sym1 == sym2).toBeTruthy
        expect(sym1.equals(sym2)).toBeTruthy
        expect(sym1.## == sym2.##).toBeTruthy
      }

      expectEqual('ScalaJS, Symbol("ScalaJS"))
      expectEqual('$, Symbol("$"))
      expectEqual('-, Symbol("-"))

      val `42` = Symbol("42")
      val map = Map[Symbol, js.Any](Symbol("ScalaJS") -> "Scala.js", '$ -> 1.2, `42` -> 42)
      expect(map('ScalaJS)).toEqual("Scala.js")
      expect(map(Symbol("$"))).toEqual(1.2)
      expect(map(Symbol("42"))).toEqual(42)
      expect(map(`42`)).toEqual(42)
    }

    it("should support `name`") {
      val scalajs = 'ScalaJS

      expect(scalajs.name).toEqual("ScalaJS")
      expect(Symbol("$").name).toEqual("$")
      expect('$$.name).toEqual("$$")
      expect('-.name).toEqual("-")
      expect('*.name).toEqual("*")
      expect(Symbol("'").name).toEqual("'")
      expect(Symbol("\"").name).toEqual("\"")
    }

    it("should support `toString`") {
      val scalajs = 'ScalaJS

      expect(scalajs.toString).toEqual("'ScalaJS")
      expect(Symbol("$").toString).toEqual("'$")
      expect('$$.toString).toEqual("'$$")
      expect('-.toString).toEqual("'-")
      expect('*.toString).toEqual("'*")
      expect(Symbol("'").toString).toEqual("''")
      expect(Symbol("\"").toString).toEqual("'\"")
    }

  }

}
