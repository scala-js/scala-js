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

package org.scalajs.testsuite.javalib.lang.ref

import org.junit.Test
import org.junit.Assert._

class ReferenceTest {

  @Test def normalOperations(): Unit = {
    val s = "string"
    val ref = new java.lang.ref.WeakReference(s)
    assertEquals(s, ref.get)
    assertEquals(false, ref.enqueue)
    assertEquals(false, ref.isEnqueued)
    ref.clear()
    assert(ref.get == null)
  }
}
