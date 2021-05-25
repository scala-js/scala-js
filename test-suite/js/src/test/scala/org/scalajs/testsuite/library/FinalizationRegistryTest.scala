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

class FinalizationRegistryTest {
  @Test def testMethods(): Unit = {
    val registry = new js.FinalizationRegistry[js.Date, String, Any]((heldValue: String) => ())

    val obj1 = new js.Date()
    registry.register(obj1, "foo")

    val obj2 = new js.Date()
    registry.register(obj2, "bar", obj2)

    val obj3 = new js.Date()
    val unregisterToken = new js.Object()
    registry.register(obj3, "bar", unregisterToken)

    val nonExistingUnregisterToken = new js.Object()
    assertFalse(registry.unregister(nonExistingUnregisterToken))

    assertFalse(registry.unregister(obj1))

    assertTrue(registry.unregister(obj2))
    assertFalse(registry.unregister(obj2))

    assertTrue(registry.unregister(unregisterToken))
    assertFalse(registry.unregister(unregisterToken))
  }
}
