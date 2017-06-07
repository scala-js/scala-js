/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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

  @Test def access_dangerous_global_ref(): Unit = {
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

  @Test def can_still_create_an_array(): Unit = {
    val a = new Array[Int](3)
    a(1) = 42
    assertEquals(42, a(1))
    assertSame(classOf[Array[Int]], a.getClass)
  }

}

object GlobalScopeTestEx {
  @js.native
  @JSGlobalScope
  object GlobalScope extends js.Any {
    var `$h_O`: String = js.native
  }
}
