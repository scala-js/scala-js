/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Test
import org.junit.Assert._

class RegressionJSTest {

  @Test def should_not_swallow_Unit_expressions_when_converting_to_js_Any_issue_83(): Unit = {
    var effectHappened = false
    def doEffect(): Unit = effectHappened = true
    def f(): js.Any = doEffect()
    f()
    assertTrue(effectHappened)
  }

  @Test def should_resolve_overloads_on_scala_Function_apply_when_converting_to_js_Function_issue_125(): Unit = {
    class Fct extends Function1[Int, Any] {
      def apply(n: Int): Int = n
    }

    val scalaFunction = new Fct
    val jsFunction: js.Any = scalaFunction
    val thisFunction: js.ThisFunction = scalaFunction
  }

  @Test def should_not_put_bad_flags_on_caseaccessor_export_forwarders_issue_1191(): Unit = {
    // This test used to choke patmat

    @JSExportAll
    case class T(one: Int, two: Int)

    val T(a, b) = T(1, 2)

    assertEquals(1, a)
    assertEquals(2, b)
  }

  @Test def should_transform_js_dynamic_x_receiver_issue_2804(): Unit = {
    @ScalaJSDefined
    class Foo extends js.Object

    assertTrue(js.isUndefined(js.constructorOf[Foo].x))
    assertTrue(js.isUndefined(js.constructorOf[Foo].y))
  }

}
