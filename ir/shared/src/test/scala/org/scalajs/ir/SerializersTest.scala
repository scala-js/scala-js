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

package org.scalajs.ir

import org.junit.Test
import org.junit.Assert._

class SerializersTest {
  @Test def testHacksUseBelow(): Unit = {
    import Serializers.Hacks

    val hacks1_0 = new Hacks("1.0")
    assertFalse(hacks1_0.useBelow(0))
    assertTrue(hacks1_0.useBelow(1))
    assertTrue(hacks1_0.useBelow(5))
    assertTrue(hacks1_0.useBelow(15))
    assertTrue(hacks1_0.useBelow(1000))

    val hacks1_7 = new Hacks("1.7")
    assertFalse(hacks1_7.useBelow(0))
    assertFalse(hacks1_7.useBelow(1))
    assertFalse(hacks1_7.useBelow(5))
    assertFalse(hacks1_7.useBelow(7))
    assertTrue(hacks1_7.useBelow(8))
    assertTrue(hacks1_7.useBelow(15))
    assertTrue(hacks1_7.useBelow(1000))

    val hacks1_50 = new Hacks("1.50")
    assertFalse(hacks1_50.useBelow(0))
    assertFalse(hacks1_50.useBelow(1))
    assertFalse(hacks1_50.useBelow(5))
    assertFalse(hacks1_50.useBelow(15))
    assertTrue(hacks1_50.useBelow(1000))

    // Non-stable versions never get any hacks
    val hacks1_9_snapshot = new Hacks("1.9-SNAPSHOT")
    assertFalse(hacks1_9_snapshot.useBelow(0))
    assertFalse(hacks1_9_snapshot.useBelow(1))
    assertFalse(hacks1_9_snapshot.useBelow(5))
    assertFalse(hacks1_9_snapshot.useBelow(15))
    assertFalse(hacks1_9_snapshot.useBelow(1000))

    // Incompatible versions never get any hacks
    val hacks2_5 = new Hacks("2.5")
    assertFalse(hacks2_5.useBelow(0))
    assertFalse(hacks2_5.useBelow(1))
    assertFalse(hacks2_5.useBelow(5))
    assertFalse(hacks2_5.useBelow(15))
    assertFalse(hacks2_5.useBelow(1000))
  }
}
