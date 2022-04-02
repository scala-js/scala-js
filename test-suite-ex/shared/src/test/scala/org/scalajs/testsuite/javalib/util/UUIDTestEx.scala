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

package org.scalajs.testsuite.javalib.util

import java.util.UUID

import org.junit.Test
import org.junit.Assert._

/** Additional tests for `java.util.UUID` that require
 *  `java.security.SecureRandom`.
 */
class UUIDTestEx {

  @Test def randomUUID(): Unit = {
    val uuid1 = UUID.randomUUID()
    assertEquals(2, uuid1.variant())
    assertEquals(4, uuid1.version())

    val uuid2 = UUID.randomUUID()
    assertEquals(2, uuid2.variant())
    assertEquals(4, uuid2.version())

    assertNotEquals(uuid1, uuid2)
  }

}
