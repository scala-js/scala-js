/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

class ThisFunctionTest {

  @Test def should_provide_an_implicit_conversion_from_Scala_function_to_js_ThisFunction(): Unit = {
    val g = js.eval("""
        var g = function(f, x) { return f.call(x, 42, x.foo); }; g;
    """).asInstanceOf[js.Function2[js.ThisFunction2[ // scalastyle:ignore
        js.Dynamic, Int, String, String], js.Dynamic, String]]

    val f = { (thiz: js.Dynamic, v: Int, u: String) =>
      import js.DynamicImplicits.truthValue
      assertTrue(thiz)
      val thiz_foobar = thiz.foobar
      assertEquals("foobar", thiz_foobar)
      u + v
    }
    val obj = js.Object().asInstanceOf[js.Dynamic]
    obj.foo = "foo"
    obj.foobar = "foobar"
    assertEquals("foo42", g(f, obj))
  }

  @Test def should_accept_a_lambda_where_a_js_ThisFunction_is_expected(): Unit = {
    val g = js.eval("""
        var g = function(f, x) { return f.call(x, 42, x.foo); }; g;
    """).asInstanceOf[js.Function2[js.ThisFunction2[ // scalastyle:ignore
        js.Dynamic, Int, String, String], js.Dynamic, String]]

    val obj = js.Object().asInstanceOf[js.Dynamic]
    obj.foo = "foo"
    obj.foobar = "foobar"
    val res = g({ (thiz: js.Dynamic, v: Int, u: String) =>
      import js.DynamicImplicits.truthValue
      assertTrue(thiz)
      val thiz_foobar = thiz.foobar
      assertEquals("foobar", thiz_foobar)
      u + v
    }, obj)
    assertEquals("foo42", res)
  }

  @Test def should_bind_the_first_argument_to_this_when_applying_js_ThisFunctionN(): Unit = {
    val g = js.eval("""
        var g = function(x) { return this.foo + ":" + x; }; g;
    """).asInstanceOf[js.ThisFunction1[js.Dynamic, Int, String]]
    val obj = js.Object().asInstanceOf[js.Dynamic]
    obj.foo = "foo"
    assertEquals("foo:42", g(obj, 42))
  }

  @Test def should_provide_an_implicit_conversion_from_js_ThisFunction_to_Scala_function(): Unit = {
    val g = js.eval("""
        var g = function(x) { return this.foo + ":" + x; }; g;
    """).asInstanceOf[js.ThisFunction1[js.Dynamic, Int, String]]
    val f: scala.Function2[js.Dynamic, Int, String] = g
    val obj = js.Object().asInstanceOf[js.Dynamic]
    obj.foo = "foo"
    assertEquals("foo:42", f(obj, 42))
  }

  @Test def thisFunction_in_trait_issue2643(): Unit = {
    trait TraitWithThisFunction {
      def create = {
        val f = { (passedThis: js.Dynamic) =>
          passedThis
        }
        js.Dynamic.literal(
          "foo" -> ({ (passedThis: js.Dynamic) => {
            passedThis
          } }: js.ThisFunction0[js.Dynamic, js.Dynamic]),
          "bar" -> js.ThisFunction.fromFunction1(f),
          "foobar" -> (f: js.ThisFunction)
        )
      }
    }

    class TraitWithThisFunctionImpl extends TraitWithThisFunction

    val objFactory = new TraitWithThisFunctionImpl()
    val obj = new TraitWithThisFunctionImpl().create
    val thisValue = new js.Object

    assertSame(thisValue, obj.foo.call(thisValue))
    assertSame(thisValue, obj.bar.call(thisValue))
    assertSame(thisValue, obj.foobar.call(thisValue))
  }

}
