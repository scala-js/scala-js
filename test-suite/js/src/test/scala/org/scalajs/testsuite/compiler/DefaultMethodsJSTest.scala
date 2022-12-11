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

class DefaultMethodsJSTest {

  import DefaultMethodsJSTest._

  @Test def inheritSimpleDefaultMethod(): Unit = {
    class InheritSimpleDefaultMethod extends SimpleInterfaceWithDefault {
      def value: Int = 5
    }

    val o = new InheritSimpleDefaultMethod
    assertEquals(9, o.foo(4))
  }
}

object DefaultMethodsJSTest {
  trait SimpleInterfaceWithDefault {
    def value: Int

    def foo(x: Int): Int = value + x
  }
}
