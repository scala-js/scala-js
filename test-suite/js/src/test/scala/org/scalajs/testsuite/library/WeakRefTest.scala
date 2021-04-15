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

package org.scalajs.testsuite.library

import org.junit.Assert._
import org.junit.Test

import scala.scalajs.js

class WeakRefTest {
  @Test def testDeref(): Unit = {
    val obj = new js.Date()
    val ref = new js.WeakRef(obj)
    val derefedInferredType = ref.deref()
    val derefedAssertType: js.UndefOr[js.Date] = derefedInferredType
    assertTrue(derefedAssertType.contains(obj))
  }
}
