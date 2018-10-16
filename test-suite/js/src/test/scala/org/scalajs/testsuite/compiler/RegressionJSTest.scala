/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.compiler

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Test
import org.junit.Assert._

class RegressionJSTest {
  import RegressionJSTest._

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
    class Foo extends js.Object

    assertTrue(js.isUndefined(js.constructorOf[Foo].x))
    assertTrue(js.isUndefined(js.constructorOf[Foo].y))
  }

  @Test def super_mixin_call_in_2_12_issue_3013_ScalaOuter_JSInner(): Unit = {
    import Bug3013_ScalaOuter_JSInner._

    val b = new B
    val c = new b.C
    assertEquals("A1", c.t1())
    assertEquals("A2", c.t2())
    assertEquals("B", c.t3())
  }

  @Test def emit_anon_JS_function_class_data_with_2_11_Xexperimental_issue_3222(): Unit = {
    val initSourceMapper: Option[js.Function1[Int, Int]] = None
    val sourceMapper: js.Function1[Int, Int] = {
      initSourceMapper.getOrElse {
        (s: Int) => s
      }
    }
    assertEquals(4, sourceMapper(4))
  }

}

object RegressionJSTest {

  /* The combination Scala/Scala is done in the cross-platform RegressionTest.
   *
   * The combinations where the outer class is a JS type cannot happen by
   * construction, because they would require a non-native JS trait with a
   * concrete method, which is prohibited.
   */
  object Bug3013_ScalaOuter_JSInner {
    trait A1 {
      private val s = "A1"
      def f(): String = s
    }

    trait A2 {
      private val s = "A2"
      def f(): String = s
    }

    class B extends A1 with A2 {
      override def f(): String = "B"

      class C extends js.Object {
        def t1(): String = B.super[A1].f()
        def t2(): String = B.super[A2].f()
        def t3(): String = B.this.f()
      }
    }
  }

}
