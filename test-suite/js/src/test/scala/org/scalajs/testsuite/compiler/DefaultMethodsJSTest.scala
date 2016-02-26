/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js.annotation.JavaDefaultMethod

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

    @JavaDefaultMethod
    def foo(x: Int): Int = value + x
  }
}
