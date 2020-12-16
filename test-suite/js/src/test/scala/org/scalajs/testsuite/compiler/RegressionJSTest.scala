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

  @Test def preserveUnitExpressionsWhenConvertingToJSAny_Issue83(): Unit = {
    var effectHappened = false
    def doEffect(): Unit = effectHappened = true
    def f(): js.Any = doEffect()
    f()
    assertTrue(effectHappened)
  }

  @Test def resolveOverloadsOnScalaFunctionApplyWhenConvertingToJSFunction_Issue125(): Unit = {
    class Fct extends Function1[Int, Any] {
      def apply(n: Int): Int = n
    }

    val scalaFunction = new Fct
    val jsFunction: js.Any = scalaFunction
    val thisFunction: js.ThisFunction = scalaFunction
  }

  @Test def badFlagsOnCaseaccessorExportForwarders_Issue1191(): Unit = {
    // This test used to choke patmat

    @JSExportAll
    case class T(one: Int, two: Int)

    val T(a, b) = T(1, 2)

    assertEquals(1, a)
    assertEquals(2, b)
  }

  @Test def transformJSDynamicXReceiver_Issue2804(): Unit = {
    class Foo extends js.Object

    assertTrue(js.isUndefined(js.constructorOf[Foo].x))
    assertTrue(js.isUndefined(js.constructorOf[Foo].y))
  }

  @Test def superMixinCallIn212ScalaOuterJSInner_Issue3013(): Unit = {
    import Bug3013_ScalaOuter_JSInner._

    val b = new B
    val c = new b.C
    assertEquals("A1", c.t1())
    assertEquals("A2", c.t2())
    assertEquals("B", c.t3())
  }

  @Test def emitAnonJSFunctionClassDataWith211Xexperimental_Issue3222(): Unit = {
    val initSourceMapper: Option[js.Function1[Int, Int]] = None
    val sourceMapper: js.Function1[Int, Int] = {
      initSourceMapper.getOrElse {
        (s: Int) => s
      }
    }
    assertEquals(4, sourceMapper(4))
  }

  @Test def lambdaReturningObjectLiteral_Issue3926(): Unit = {
    @noinline
    def f(): () => js.Dynamic =
      () => js.Dynamic.literal(foo = 5)

    val obj1 = f()()
    assertEquals("object", js.typeOf(obj1))
    assertEquals(5, obj1.foo)

    @noinline
    def g(): js.Function0[js.Dynamic] =
      () => js.Dynamic.literal(bar = 6)

    val obj2 = g()()
    assertEquals("object", js.typeOf(obj2))
    assertEquals(6, obj2.bar)
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
