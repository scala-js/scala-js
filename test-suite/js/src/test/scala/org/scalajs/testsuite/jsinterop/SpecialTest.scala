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

import org.scalajs.testsuite.utils.AssertThrows._

class SpecialTest {
  import SpecialTest._

  // scala.scalajs.js.special.strictEquals

  @Test def strictEqualsTest(): Unit = {
    import js.special.strictEquals

    val o1 = new js.Object
    val o2 = new js.Object
    assertTrue(strictEquals(o1, o1))
    assertFalse(strictEquals(o1, o2))
    assertTrue(strictEquals(+0.0, -0.0))
    assertTrue(strictEquals(-0.0, +0.0))
    assertFalse(strictEquals(Double.NaN, Double.NaN))
  }

  // scala.scalajs.js.special.in

  @Test def inTest(): Unit = {
    val o = js.Dynamic.literal(foo = 5, bar = "foobar")
    assertTrue(js.special.in("foo", o))
    assertFalse(js.special.in("foobar", o))
    assertTrue(js.special.in("toString", o)) // in prototype
  }

  // scala.scalajs.js.special.instanceof

  @Test def instanceofTest(): Unit = {
    import js.special.instanceof

    val ObjectCtor = js.constructorOf[js.Object]
    val DateCtor = js.constructorOf[js.Date]

    val obj = new js.Object
    assertTrue(instanceof(obj, ObjectCtor))
    assertFalse(instanceof(obj, DateCtor))

    val date = new js.Date
    assertTrue(instanceof(date, ObjectCtor))
    assertTrue(instanceof(date, DateCtor))

    val functionCtor: js.ThisFunction0[js.Dynamic, Unit] = { (thiz: js.Dynamic) =>
      thiz.foo = 5
    }
    assertFalse(instanceof(obj, functionCtor))
    val bar = js.Dynamic.newInstance(functionCtor.asInstanceOf[js.Dynamic])()
    assertTrue(instanceof(bar, functionCtor))
  }

  // scala.scalajs.js.special.delete

  @Test def should_provide_an_equivalent_of_the_JS_delete_keyword_issue_255(): Unit = {
    val obj = js.Dynamic.literal(foo = 42, bar = "foobar")

    assertEquals(42, obj.foo)
    assertEquals("foobar", obj.bar)
    js.special.delete(obj, "foo")
    assertFalse(obj.hasOwnProperty("foo"))
    assertEquals("foobar", obj.bar)
  }

  @Test def should_behave_as_specified_when_deleting_a_non_configurable_property_issue_461_issue_679(): Unit = {
    val obj = js.Dynamic.literal()
    js.Object.defineProperty(obj, "nonconfig",
        js.Dynamic.literal(value = 4, writable = false).asInstanceOf[js.PropertyDescriptor])
    assertEquals(4, obj.nonconfig)
    assertThrows(classOf[Exception], js.special.delete(obj, "nonconfig"))
    assertEquals(4, obj.nonconfig)
  }

  @Test def should_treat_delete_as_a_statement_issue_907(): Unit = {
    val obj = js.Dynamic.literal(a = "A")
    js.special.delete(obj, "a")
  }

  @Test def should_desugar_arguments_to_delete_statements_issue_908(): Unit = {
    val kh = js.Dynamic.literal(key = "a").asInstanceOf[KeyHolder]
    val obj = js.Dynamic.literal(a = "A")
    def a[T](foo: String): T = obj.asInstanceOf[T]
    js.special.delete(a[js.Object]("foo"), kh.key)
  }

  // js.special.fileLevelThis

  @Test def fileLevelThis_can_be_used_to_detect_the_global_object(): Unit = {
    val globalObject = {
      import js.Dynamic.{global => g}
      if (js.typeOf(g.global) != "undefined" && (g.global.Object eq g.Object)) {
        // Node.js environment detected
        g.global
      } else {
        // In all other well-known environment, we can use the global `this`
        js.special.fileLevelThis.asInstanceOf[js.Dynamic]
      }
    }

    assertSame(js.Math, globalObject.Math)
  }

  // js.special.debugger

  @Test def should_support_debugger_statements_through_the_whole_pipeline_issue_1402(): Unit = {
    /* A function that hopefully persuades the optimizer not to optimize
     * we need a debugger statement that is unreachable, but not eliminated.
     */
    @noinline
    class A(var z: Int = 4) {
      var x: Int = _
      var y: Int = _

      @noinline
      def plus(x0: Int, y0: Int): Int = {
        x = x0
        y = y0
        var res = 0
        while (x > 0 || y > 0 || z > 0) {
          if (x > 0) x -= 1
          else if (y > 0) y -= 1
          else z -= 1
          res += 1
        }
        res
      }
    }

    if (new A().plus(5, 10) < 3)
      js.special.debugger()
  }

}

object SpecialTest {
  @js.native
  trait KeyHolder extends js.Object {
    def key: String = js.native
  }
}
