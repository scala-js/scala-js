/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Test
import org.junit.Assert._

class EventObjectTest {

  @Test def shouldHaveProtectedSourceField(): Unit = {
    import java.util._
    val mySource = new AnyRef
    new EventObject(mySource) {
      assertEquals(mySource, super.source)
    }
  }

  @Test def shouldHaveGetSourceMethod): Unit = {
    import java.util._
    val mySource = new AnyRef
    assertEquals(mySource, new EventObject(mySource).getSource)
  }


}
