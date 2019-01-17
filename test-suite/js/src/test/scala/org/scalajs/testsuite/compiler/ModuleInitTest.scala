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

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

class ModuleInitTest {
  import ModuleInitTest._

  @Test def should_only_execute_module_initializers_once(): Unit = {
    assumeTrue("Assumed compliant Module", hasCompliantModuleInit)
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
