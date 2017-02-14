/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.language.implicitConversions

import scala.scalajs.js
import js.JSConverters._

import js.annotation.JSExport

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.JSAssert._

class DynamicTest {

  implicit def dyn2Bool(dyn: js.Dynamic): Boolean =
    dyn.asInstanceOf[Boolean]

  implicit def dyn2Int(dyn: js.Dynamic): Int =
    dyn.asInstanceOf[Int]

  implicit def dyn2AnyRef(dyn: js.Dynamic): AnyRef =
    dyn.asInstanceOf[AnyRef]

  // scala.scalajs.js.Dynamic

  @Test def should_workaround_Scala_2_10_issue_with_implicit_conversion_for_dynamic_fields_named_x_issue_8(): Unit = {
    class Point(val x: Int, val y: Int)

    def jsonToPoint(json: js.Dynamic): Point = {
      new Point(json.x.toString.toInt, json.y.toString.toInt)
    }

    val json = js.eval("var dynamicTestPoint = { x: 1, y: 2 }; dynamicTestPoint;")
    val point = jsonToPoint(json.asInstanceOf[js.Dynamic])

    assertEquals(1, point.x)
    assertEquals(2, point.y)
  }

  @Test def should_allow_to_call_functions_with_arguments_named_x(): Unit = {
    class A {
      def a: Int = 1
    }

    class B extends A {
      @JSExport
      def x(par: Int): Int = a + par // make sure `this` is bound correctly in JS
    }

    val b = (new B).asInstanceOf[js.Dynamic]

    assertEquals(11, b.x(10))
  }

  @Test def should_allow_instanciating_JS_classes_dynamically_issue_10(): Unit = {
    val DynamicTestClass = js.eval("""
        var DynamicTestClass = function(x) {
          this.x = x;
        };
        DynamicTestClass;
        """).asInstanceOf[js.Dynamic]
    val obj = js.Dynamic.newInstance(DynamicTestClass)("Scala.js")
    assertEquals("Scala.js", obj.x)
  }

  @Test def should_allow_instantiating_JS_classes_dynamically_with_varargs_issue_708(): Unit = {
    val DynamicTestClassVarArgs = js.eval("""
        var DynamicTestClassVarArgs = function() {
          this.count = arguments.length;
          for (var i = 0; i < arguments.length; i++)
            this['elem'+i] = arguments[i];
        };
        DynamicTestClassVarArgs;
        """).asInstanceOf[js.Dynamic]

    val obj1 = js.Dynamic.newInstance(DynamicTestClassVarArgs)("Scala.js")
    val obj1_count = obj1.count
    assertEquals(1, obj1_count)
    val obj1_elem0 = obj1.elem0
    assertEquals("Scala.js", obj1_elem0)

    val obj2 = js.Dynamic.newInstance(DynamicTestClassVarArgs)(
        "Scala.js", 42, true)
    val obj2_count = obj2.count
    assertEquals(3, obj2_count)
    val obj2_elem0 = obj2.elem0
    assertEquals("Scala.js", obj2_elem0)
    val obj2_elem1 = obj2.elem1
    assertEquals(42, obj2_elem1)
    val obj2_elem2 = obj2.elem2
    assertTrue(obj2_elem2)

    def obj3Args: Seq[js.Any] = Seq("Scala.js", 42, true)
    val obj3 = js.Dynamic.newInstance(DynamicTestClassVarArgs)(obj3Args: _*)
    val obj3_count = obj3.count
    assertEquals(3, obj3_count)
    val obj3_elem0 = obj3.elem0
    assertEquals("Scala.js", obj3_elem0)
    val obj3_elem1 = obj3.elem1
    assertEquals(42, obj3_elem1)
    val obj3_elem2 = obj3.elem2
    assertTrue(obj3_elem2)

    // Check backward binary compatibility with the 0.6.{0,1,2} codegen output
    val obj4 = scala.scalajs.runtime.newJSObjectWithVarargs(
        DynamicTestClassVarArgs, obj3Args.toJSArray).asInstanceOf[js.Dynamic]
    val obj4_count = obj4.count
    assertEquals(3, obj4_count)
    val obj4_elem0 = obj4.elem0
    assertEquals("Scala.js", obj4_elem0)
    val obj4_elem1 = obj4.elem1
    assertEquals(42, obj4_elem1)
    val obj4_elem2 = obj4.elem2
    assertTrue(obj4_elem2)
  }

  @Test def should_provide_an_object_literal_construction(): Unit = {
    import js.Dynamic.{ literal => obj }
    val x = obj(foo = 3, bar = "foobar")
    val x_foo = x.foo
    assertEquals(3, x_foo.asInstanceOf[Int])
    val x_bar = x.bar
    assertEquals("foobar", x_bar)
    val x_unknown = x.unknown
    assertJSUndefined(x_unknown)

    val y = obj(
        inner = obj(name = "inner obj"),
        fun = { () => 42 }
    )
    val y_inner_name = y.inner.name
    assertEquals("inner obj", y_inner_name)
    assertEquals(42, y.fun())
    val obj_anything = obj().anything
    assertJSUndefined(obj_anything)
  }

  @Test def object_literal_in_statement_position_issue_1627(): Unit = {
    // Just make sure it does not cause a SyntaxError
    js.Dynamic.literal(foo = "bar")
    // and also test the case without param (different code path in Printers)
    js.Dynamic.literal()
  }

  @Test def should_provide_object_literal_construction_with_dynamic_naming(): Unit = {
    import js.Dynamic.{ literal => obj }
    val x = obj("foo" -> 3, "bar" -> "foobar")
    val x_foo = x.foo
    assertEquals(3, x_foo)
    val x_bar = x.bar
    assertEquals("foobar", x_bar)
    val x_unknown = x.unknown
    assertJSUndefined(x_unknown)

    val tup1 = ("hello1", 3: js.Any)
    val tup2 = ("hello2", 10: js.Any)

    val y = obj(tup1, tup2)
    val y_hello1 = y.hello1
    assertEquals(3, y_hello1)
    val y_hello2 = y.hello2
    assertEquals(10, y_hello2)

    var count = 0
    val z = obj({ count += 1; ("foo", "bar")})
    val z_foo = z.foo
    assertEquals("bar", z_foo)
    assertEquals(1, count)
  }

  @Test def should_preserve_evaluation_order_of_keys_and_values(): Unit = {
    import js.Dynamic.{ literal => obj }

    val orderCheck = Array.newBuilder[Int]
    val x = obj(
        { orderCheck += 1; "foo" } -> { orderCheck += 2; 3 },
        { orderCheck += 3; "bar" } -> { orderCheck += 4; "foobar" })
    val x_foo = x.foo
    assertEquals(3, x_foo)
    val x_bar = x.bar
    assertEquals("foobar", x_bar)
    val x_unknown = x.unknown
    assertJSUndefined(x_unknown)
    assertArrayEquals(Array(1, 2, 3, 4), orderCheck.result())

    val orderCheck2 = Array.newBuilder[Int]

    def tup1 = ({ orderCheck2 += 1; "hello1" }, { orderCheck2 += 2; 3: js.Any })
    def tup2 = ({ orderCheck2 += 3; "hello2" }, { orderCheck2 += 4; 10: js.Any })

    val y = obj(tup1, tup2)
    val y_hello1 = y.hello1
    assertEquals(3, y_hello1)
    val y_hello2 = y.hello2
    assertEquals(10, y_hello2)
    assertArrayEquals(Array(1, 2, 3, 4), orderCheck2.result())

    @noinline def block[A](a: A): A = a

    val orderCheck3 = Array.newBuilder[Int]
    val z = obj(
        { val a = block("foo"); orderCheck3 += 1; a } ->
          { val a = block(3); orderCheck3 += 2; a },
        { val a = block("bar"); orderCheck3 += 3; a } ->
          { val a = block("foobar"); orderCheck3 += 4; a })
    val z_foo = z.foo
    assertEquals(3, z_foo)
    val z_bar = z.bar
    assertEquals("foobar", z_bar)
    val z_unknown = z.unknown
    assertJSUndefined(z_unknown)
    assertArrayEquals(Array(1, 2, 3, 4), orderCheck3.result())
  }

  @Test def should_allow_to_create_an_empty_object_with_the_literal_syntax(): Unit = {
    import js.Dynamic.{ literal => obj }
    val x = obj()
    assertTrue(x.isInstanceOf[js.Object])
  }

  @Test def should_properly_encode_object_literal_property_names(): Unit = {
    import js.Dynamic.{ literal => obj }

    val obj0 = obj("3-" -> 42)
    val `obj0_3-` = obj0.`3-`
    assertEquals(42, `obj0_3-`)

    val obj0Dict = obj0.asInstanceOf[js.Dictionary[js.Any]]
    assertEquals(42, obj0Dict("3-"))

    val checkEvilProperties = js.eval("""
      function dynamicLiteralNameEncoding_checkEvilProperties(x) {
        return x['.o[3√!|-pr()per7:3$];'] === ' such eval ';
      }
      dynamicLiteralNameEncoding_checkEvilProperties
    """).asInstanceOf[js.Function1[js.Any, Boolean]]
    val obj1 = obj(
        ".o[3√!|-pr()per7:3$];" -> " such eval ").asInstanceOf[js.Dictionary[js.Any]]
    assertEquals(" such eval ", obj1(".o[3√!|-pr()per7:3$];"))
    assertTrue(checkEvilProperties(obj1))

    val checkQuotesProperty = js.eval("""
      function dynamicLiteralNameEncoding_quote(x) {
        return x["'" + '"'] === 7357;
      }
      dynamicLiteralNameEncoding_quote
    """).asInstanceOf[js.Function1[js.Any, Boolean]]

    val quote = '"'

    Seq(
      obj("'" + quote -> 7357),
      obj(s"'$quote" -> 7357),
      obj("'\"" -> 7357),
      obj("'" + quote -> 7357)
    ).foreach { o =>
      val dict = o.asInstanceOf[js.Dictionary[js.Any]]
      assertEquals(7357, dict("'\""))
      assertEquals(7357, dict("'" + quote))
      assertEquals(7357, dict(s"'$quote"))
      assertTrue(checkQuotesProperty(o))
    }
  }

  @Test def `should_accept_:__*_arguments_for_literal_construction_issue_1743`(): Unit = {
    import js.Dynamic.literal

    val fields = Seq[(String, js.Any)]("foo" -> 42, "bar" -> "foobar")

    /* Note: we cannot write
     * literal(fields: _*)
     * because scalac does not like it. But we still have to support the
     * expanded notation.
     */

    val x = literal.applyDynamic("apply")(fields: _*)
    val x_foo = x.foo
    assertEquals(42, x_foo)
    val x_bar = x.bar
    assertEquals("foobar", x_bar)

    val y = literal.applyDynamicNamed("apply")(fields: _*)
    val y_foo = y.foo
    assertEquals(42, y_foo)
    val y_bar = y.bar
    assertEquals("foobar", y_bar)
  }

  @Test def should_allow_object_literals_to_have_duplicate_keys_issue_1595(): Unit = {
    import js.Dynamic.{literal => obj}

    // Basic functionality
    val a = obj(foo = 4, bar = 5, foo = 6)
    val a_foo = a.foo
    assertEquals(6, a_foo) // last wins
    val a_bar = a.bar
    assertEquals(5, a_bar)

    // Side-effects of overwritten properties are kept
    var counter = 0
    val b = obj(foo = { counter += 1; "foo" }, bar = "bar", foo = "foobar")
    assertEquals(1, counter)
    val b_foo = b.foo
    assertEquals("foobar", b_foo)
    val b_bar = b.bar
    assertEquals("bar", b_bar)

    // In a position where unnesting is required - #1628
    @noinline
    def test(x: js.Dynamic): Unit = {
      assertEquals(6, x.foo) // last wins
      assertEquals(5, x.bar)
    }
    test(obj(foo = 4, bar = 5, foo = 6))
  }

  @Test def should_return_subclasses_of_js_Object_in_literal_construction_issue_783(): Unit = {
    import js.Dynamic.{ literal => obj }

    val a: js.Object = obj(theValue = 1)
    assertTrue(a.hasOwnProperty("theValue"))
    assertFalse(a.hasOwnProperty("noValue"))

    val b: js.Object = obj("theValue" -> 2)
    assertTrue(b.hasOwnProperty("theValue"))
    assertFalse(b.hasOwnProperty("noValue"))

  }
}
