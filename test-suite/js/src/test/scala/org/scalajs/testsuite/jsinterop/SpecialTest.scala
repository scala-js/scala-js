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
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class SpecialTest {
  import SpecialTest._

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
    // This doesn't work on Rhino due to lack of full strict mode support - #679
    assumeFalse("Assumed not executing in Rhino", executingInRhino)

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

  // js.special.globalThis

  @Test def globalThis_can_be_used_to_detect_the_global_object(): Unit = {
    val globalObject = {
      import js.Dynamic.{global => g}
      // We've got to use selectDynamic explicitly not to crash Scala 2.10
      if (js.typeOf(g.selectDynamic("global")) != "undefined" &&
          (g.selectDynamic("global").selectDynamic("Object") eq g.selectDynamic("Object"))) {
        // Node.js environment detected
        g.selectDynamic("global")
      } else {
        // In all other well-known environment, we can use the global `this`
        js.special.globalThis
      }
    }

    assertSame(js.Dynamic.global, globalObject)
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
