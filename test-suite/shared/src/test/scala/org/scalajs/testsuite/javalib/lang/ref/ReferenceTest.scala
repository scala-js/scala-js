/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang.ref

import org.junit.Test
import org.junit.Assert._

class ReferenceTest {

  @Test def should_have_all_the_normal_operations(): Unit = {
    val s = "string"
    val ref = new java.lang.ref.WeakReference(s)
    assertEquals(s, ref.get)
    assertEquals(false, ref.enqueue)
    assertEquals(false, ref.isEnqueued)
    ref.clear()
    assert(ref.get == null)
  }
}
