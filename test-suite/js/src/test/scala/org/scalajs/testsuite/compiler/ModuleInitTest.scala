/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

class ModuleInitTest {
  import ModuleInitTest._

  @Test def should_only_execute_module_initializers_once(): Unit = {
    assumeTrue("Assumed compliant Module", hasCompliantModule)
    val x = A.Y
    val y = A.cs.head
    assertTrue(x ne null)
    assertTrue(y eq null)
    assertTrue(x eq A.Y)
    assertEquals(1, Counter.c)
  }
}

object ModuleInitTest {

  object Counter {
    var c: Int = 0
  }

  object A {
    private def blankSym = ""

    sealed abstract class C(symbol: String)
    object Y extends C(blankSym) {
      Counter.c += 1
    }

    val cs = Vector[C](Y)
  }
}
