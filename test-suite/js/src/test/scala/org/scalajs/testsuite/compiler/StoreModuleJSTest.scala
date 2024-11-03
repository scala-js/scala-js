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

import org.junit.Test
import org.junit.Assert._

class StoreModuleJSTest {
  import StoreModuleJSTest._

  @Test def jsModuleClass(): Unit = {
    val a = JSObjA
    val b = JSObjB

    assertNotNull(a)
    assertNotNull(b)
    assertSame(a, b.a)
    assertSame(b, a.b)
  }
}

object StoreModuleJSTest {
  object JSObjA extends js.Object {
    val b = JSObjB
  }

  object JSObjB extends js.Object {
    val a = JSObjA
  }
}
