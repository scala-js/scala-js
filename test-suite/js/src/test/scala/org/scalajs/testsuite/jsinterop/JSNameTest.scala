/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

import org.scalajs.jasminetest.JasmineTest

object JSNameTest extends JasmineTest {

  describe("@JSName") {

    it("should work with defs that are properties") {
      val obj = js.Dynamic.literal(jsDef = 1).asInstanceOf[PropDefFacade]
      expect(obj.internalDef).toEqual(1)
    }

    it("should work with vals") {
      val obj = js.Dynamic.literal(jsVal = "hi").asInstanceOf[PropValFacade]
      expect(obj.internalVal).toEqual("hi")
    }

    it("should work with vars") {
      val obj = js.Dynamic.literal(jsVar = 0.1).asInstanceOf[PropVarFacade]
      expect(obj.internalVar).toEqual(0.1)
      obj.internalVar = 0.2
      expect(obj.internalVar).toEqual(0.2)
    }

    it("should allow names ending in _=") {
      val d = js.Dynamic.literal("a_=" -> 1)
      val f = d.asInstanceOf[UndEqNamed]

      expect(f.a).toEqual(1)
      f.a = 2
      expect(d.selectDynamic("a_=")).toEqual(2)
      expect(f.a).toEqual(2)
    }

  }

  @js.native
  trait PropDefFacade extends js.Any {
    @JSName("jsDef")
    def internalDef: Int = js.native
  }

  @js.native
  trait PropValFacade extends js.Any {
    @JSName("jsVal")
    val internalVal: String = js.native
  }

  @js.native
  trait PropVarFacade extends js.Any {
    @JSName("jsVar")
    var internalVar: Double = js.native
  }

  @js.native
  trait UndEqNamed extends js.Any {
    @JSName("a_=")
    def a: Int = js.native

    @JSName("a_=")
    def a_=(x: Int): Unit = js.native
  }

}
