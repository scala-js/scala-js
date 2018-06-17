/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.noircheck

import org.junit.Test
import org.junit.Assert._

class DummyParentsTest {

  @Test def linking_stages_should_provide_dummy_parents_if_required(): Unit = {

    // java.util.WeakHashMap is not defined
    class DummyWeakHashMap extends java.util.WeakHashMap

    val x = "1".toInt

    if (x + x < 0) {
      // Ensure DummyWeakHashMap is not DCEd, but never instantiated
      assertEquals(0, new DummyWeakHashMap().size())
    }

  }
}
