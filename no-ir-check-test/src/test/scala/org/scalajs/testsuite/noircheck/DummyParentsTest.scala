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
