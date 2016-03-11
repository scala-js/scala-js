/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Test

class JSNameTest {
  import JSNameTest._

  @Test def should_work_with_defs_that_are_properties(): Unit = {
    val obj = js.Dynamic.literal(jsDef = 1).asInstanceOf[PropDefFacade]
    assertEquals(1, obj.internalDef)
  }

  @Test def should_work_with_vals(): Unit = {
    val obj = js.Dynamic.literal(jsVal = "hi").asInstanceOf[PropValFacade]
    assertEquals("hi", obj.internalVal)
  }

  @Test def should_work_with_vars(): Unit = {
    val obj = js.Dynamic.literal(jsVar = 0.1).asInstanceOf[PropVarFacade]
    assertEquals(0.1, obj.internalVar)
    obj.internalVar = 0.2
    assertEquals(0.2, obj.internalVar)
  }

  @Test def should_work_with_defs_that_are_properties_in_Scala_js_defined_trait_issue_2197(): Unit = {
    val obj = js.Dynamic.literal(jsDef = 1).asInstanceOf[PropDefSJSDefined]
    assertEquals(1, obj.internalDef)
  }

  @Test def should_work_with_vals_in_Scala_js_defined_trait_issue_2197(): Unit = {
    val obj = js.Dynamic.literal(jsVal = "hi").asInstanceOf[PropValSJSDefined]
    assertEquals("hi", obj.internalVal)
  }

  @Test def should_work_with_vars_in_Scala_js_defined_trait_issue_2197(): Unit = {
    val obj = js.Dynamic.literal(jsVar = 0.1).asInstanceOf[PropVarSJSDefined]
    assertEquals(0.1, obj.internalVar)
    obj.internalVar = 0.2
    assertEquals(0.2, obj.internalVar)
  }

  @Test def should_allow_names_ending_in__=(): Unit = {
    val d = js.Dynamic.literal("a_=" -> 1)
    val f = d.asInstanceOf[UndEqNamed]

    assertEquals(1, f.a)
    f.a = 2
    assertEquals(2, d.selectDynamic("a_="))
    assertEquals(2, f.a)
  }
}

object JSNameTest {

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

  @ScalaJSDefined
  trait PropDefSJSDefined extends js.Any {
    @JSName("jsDef")
    def internalDef: Int
  }

  @ScalaJSDefined
  trait PropValSJSDefined extends js.Any {
    @JSName("jsVal")
    val internalVal: String
  }

  @ScalaJSDefined
  trait PropVarSJSDefined extends js.Any {
    @JSName("jsVar")
    var internalVar: Double
  }

  @js.native
  trait UndEqNamed extends js.Any {
    @JSName("a_=")
    def a: Int = js.native

    @JSName("a_=")
    def a_=(x: Int): Unit = js.native
  }

}
