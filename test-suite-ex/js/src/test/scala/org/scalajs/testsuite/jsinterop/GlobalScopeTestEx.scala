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

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.testsuite.utils.Platform._

/** Additional tests for access to the global scope.
 *
 *  If moved to testSuite, those tests will cause the test suite to be emitted
 *  significantly more slowly on the first pass, because a "dangerous global
 *  ref" is accessed.
 */
class GlobalScopeTestEx {
  import GlobalScopeTestEx._

  @Test def accessDangerousGlobalRef(): Unit = {
    assumeTrue("Assuming execution in Node.js", executingInNodeJS)

    nodejs_runInThisContext("""
      let $h_O = "evil global";
    """)

    assertEquals("evil global", js.Dynamic.global.$h_O)
    assertEquals("evil global", GlobalScope.`$h_O`)

    js.Dynamic.global.$h_O = "more evil"
    assertEquals("more evil", js.Dynamic.global.$h_O)
    assertEquals("more evil", GlobalScope.`$h_O`)

    GlobalScope.`$h_O` = "yet a bit more evil"
    assertEquals("yet a bit more evil", js.Dynamic.global.$h_O)
    assertEquals("yet a bit more evil", GlobalScope.`$h_O`)
  }

  @Test def canStillCreateAnArray(): Unit = {
    val a = new Array[Int](3)
    a(1) = 42
    assertEquals(42, a(1))
    assertSame(classOf[Array[Int]], a.getClass)
  }

  @Test def accessGlobalRefThatServesAsThisInDefaultMethods_Issue2972(): Unit = {
    js.eval("""var $thiz = "evil thiz";""");

    trait Foo {
      @noinline def foo(): String = GlobalScope.`$thiz` + bar()

      def bar(): String
    }

    class Bar(s: String) extends Foo {
      @noinline def bar(): String = s
    }

    val bar = new Bar(" babar")
    assertEquals("evil thiz babar", bar.foo())
  }

}

object GlobalScopeTestEx {
  @js.native
  @JSGlobalScope
  object GlobalScope extends js.Any {
    var `$h_O`: String = js.native
    var `$thiz`: String = js.native
  }
}
