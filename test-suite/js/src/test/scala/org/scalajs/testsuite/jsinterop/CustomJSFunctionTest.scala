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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.JSAssert._

class CustomJSFunctionTest {

  import CustomJSFunctionTest._

  @Test def customJSFunctions(): Unit = {
    val array = js.Array(4, 6, 2, 3).asInstanceOf[js.Dynamic]

    val f1: MyJSFunction1[Int, Int] = { _ * 2 }
    assertEquals(42, f1(21))
    assertJSArrayEquals(js.Array(8, 12, 4, 6), array.map(f1).asInstanceOf[js.Array[Int]])

    val f2: MyJSFunctionWithRestParam[String, String] = { args =>
      args.mkString(", ")
    }
    assertEquals("", f2())
    assertEquals("foo", f2("foo"))
    assertEquals("foo, bar, baz", f2("foo", "bar", "baz"))

    val f3: MyJSFunction1WithRestParam[Int, String, String] = { (x, args) =>
      "" + x + ": " + args.mkString(", ")
    }
    assertEquals("1: ", f3(1))
    assertEquals("2: foo", f3(2, "foo"))
    assertEquals("3: foo, bar, baz", f3(3, "foo", "bar", "baz"))

    val f4: MyJSFunctionWithDefaultParameter[Int] = { (x, y) => x + y.getOrElse(5) }
    assertEquals(26, f4(21))
    assertEquals(31, f4(21, 10))
  }

  @Test def customJSFunctionsCallingConvention(): Unit = {
    /* This test makes sure that that the custom JS function types, whose
     * implementations are tested in the test above, indeed behave at call
     * site with the right semantics. Otherwise, we could have two-way mistakes
     * that hide each other.
     */

    val getAllArgs = new js.Function("...args", "return args;")

    assertJSArrayEquals(js.Array(21),
        getAllArgs.asInstanceOf[MyJSFunction1[Int, js.Array[Int]]](21))

    assertJSArrayEquals(js.Array(5, 4, 8),
        getAllArgs.asInstanceOf[MyJSFunctionWithRestParam[Int, js.Array[Int]]](5, 4, 8))

    assertJSArrayEquals(js.Array(5, 4, 8),
        getAllArgs.asInstanceOf[MyJSFunction1WithRestParam[Int, Int, js.Array[Int]]](5, 4, 8))

    assertJSArrayEquals(js.Array(21),
        getAllArgs.asInstanceOf[MyJSFunctionWithDefaultParameter[js.Array[Int]]](21))
    assertJSArrayEquals(js.Array(21, 4),
        getAllArgs.asInstanceOf[MyJSFunctionWithDefaultParameter[js.Array[Int]]](21, 4))
  }

  @Test def customJSThisFunctions(): Unit = {
    case class Foo(x: Int)

    val f1: MyJSThisFunction2[Foo, Int, Int, Int] = { (foo, x, y) => foo.x + x + y }
    assertEquals(30, f1(Foo(5), 21, 4))

    val f2: MyJSThisFunctionWithRestParam[Foo, String, String] = { (foo, args) =>
      "" + foo + ": " + args.mkString(", ")
    }
    assertEquals("Foo(1): ", f2(Foo(1)))
    assertEquals("Foo(2): foo", f2(Foo(2), "foo"))
    assertEquals("Foo(3): foo, bar, baz", f2(Foo(3), "foo", "bar", "baz"))
  }

  @Test def customJSThisFunctionsCallingConvention(): Unit = {
    /* Same as customJSFunctionsCallingConvention() but for the JS this
     * functions tested in customJSThisFunctions().
     */

    case class Foo(x: Int)

    val getAllArgs = new js.Function("...args", "return [this].concat(args);")

    assertJSArrayEquals(js.Array(Foo(5), 21, "hello"),
        getAllArgs.asInstanceOf[MyJSThisFunction2[Foo, Int, String, js.Array[Any]]](Foo(5), 21, "hello"))

    assertJSArrayEquals(js.Array(Foo(5), 4, 8),
        getAllArgs.asInstanceOf[MyJSThisFunctionWithRestParam[Foo, Int, js.Array[Any]]](Foo(5), 4, 8))
  }

}

object CustomJSFunctionTest {
  trait MyJSFunction1[-T1, +R] extends js.Function {
    def apply(x1: T1): R
  }

  trait MyJSFunctionWithRestParam[-T, +R] extends js.Function {
    def apply(args: T*): R
  }

  trait MyJSFunction1WithRestParam[-T1, -Ts, +R] extends js.Function {
    def apply(x1: T1, args: Ts*): R
  }

  trait MyJSFunctionWithDefaultParameter[+R] extends js.Function {
    def apply(x1: Int, x2: js.UndefOr[Int] = js.undefined): R
  }

  trait MyJSThisFunction2[-This, -T1, -T2, +R] extends js.ThisFunction {
    def apply(thiz: This, x1: T1, x2: T2): R
  }

  trait MyJSThisFunctionWithRestParam[-This, -T, +R] extends js.ThisFunction {
    def apply(thiz: This, args: T*): R
  }
}
