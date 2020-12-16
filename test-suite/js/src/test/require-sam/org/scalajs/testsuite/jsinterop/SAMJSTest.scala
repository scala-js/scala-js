/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.JSAssert._

class SAMJSTest {

  import SAMJSTest._

  @Test def samNestedInAnonJSClass_Issue3264(): Unit = {
    val outer = new SAMInAnonJSClass_ParentJSClass {
      def foo(x: Int): Int = {
        val innerSAM: SAMInAnonJSClass_MySAM = _ * 2
        innerSAM.bar(x + 3)
      }
    }

    assertEquals(16, outer.foo(5))
  }

}

object SAMJSTest {
  abstract class SAMInAnonJSClass_ParentJSClass extends js.Object {
    def foo(x: Int): Int
  }

  trait SAMInAnonJSClass_MySAM {
    def bar(x: Int): Int
  }
}
