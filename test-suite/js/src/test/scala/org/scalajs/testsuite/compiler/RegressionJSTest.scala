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
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

import org.scalajs.testsuite.utils.JSAssert._

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

  @Test def preserveSideEffectsOfJSOpsWithBigInts_Issue4621(): Unit = {
    assumeTrue("requires BigInts support", jsBigInts)

    @noinline def bi(x: Int): js.BigInt = js.BigInt(x)

    // These must be stored as `val`s first in order to trigger the original problem
    val bi5: Any = bi(5)
    val bi0: Any = bi(0)
    val bi1: Any = bi(1)

    assertThrows(classOf[js.JavaScriptException], {
      bi5.asInstanceOf[js.Dynamic] / bi0.asInstanceOf[js.Dynamic]
      fail("unreachable") // required for the above line to be in statement position
    })
    assertThrows(classOf[js.JavaScriptException], {
      +bi5.asInstanceOf[js.Dynamic]
      fail("unreachable")
    })
  }

  @Test def preserveSideEffectsOfJSOpsWithCustomValueOf_Issue4621(): Unit = {
    // This must be a `val` in order to trigger the original problem
    val obj: Any = new js.Object {
      override def valueOf(): Double =
        throw new UnsupportedOperationException()
    }

    assertThrows(classOf[UnsupportedOperationException], {
      obj.asInstanceOf[js.Dynamic] + 5.asInstanceOf[js.Dynamic]
      fail("unreachable")
    })
    assertThrows(classOf[UnsupportedOperationException], {
      -obj.asInstanceOf[js.Dynamic]
      fail("unreachable")
    })
  }

  @Test def captureLoopValInLambda(): Unit = {
    // Test of a regression that appeared during fixing of #2675.
    val functions = js.Array[js.Function0[Int]]()

    var i = 0
    while (i != 5) {
      val j = i
      functions.push(() => j)
      i += 1
    }

    val result = functions.map(_())

    assertJSArrayEquals(js.Array(0, 1, 2, 3, 4), result)
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
