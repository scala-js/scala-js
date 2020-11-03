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

package org.scalajs.linker.interface

import org.junit.Test
import org.junit.Assert._

class ModuleInitializerFingerprintTest {
  def assertFingerprintsNotEquals(mi1: ModuleInitializer, mi2: ModuleInitializer): Unit = {
    assertNotEquals(ModuleInitializer.fingerprint(mi1), ModuleInitializer.fingerprint(mi2))
  }

  @Test
  def noFingerprintCollisionClassName(): Unit = {
    val mi1 = ModuleInitializer.mainMethod("Test1", "main1")
    val mi2 = ModuleInitializer.mainMethod("Test2", "main1")
    assertFingerprintsNotEquals(mi1, mi2)
  }

  @Test
  def noFingerprintCollisionMainMethodName(): Unit = {
    val mi1 = ModuleInitializer.mainMethod("Test1", "main1")
    val mi2 = ModuleInitializer.mainMethod("Test1", "main2")
    assertFingerprintsNotEquals(mi1, mi2)
  }

  @Test
  def noFingerprintCollisionType(): Unit = {
    val mi1 = ModuleInitializer.mainMethod("Test1", "main1")
    val mi2 = ModuleInitializer.mainMethodWithArgs("Test1", "main1", List())
    assertFingerprintsNotEquals(mi1, mi2)
  }

  @Test
  def noFingerprintCollisionArgs(): Unit = {
    val mi1 = ModuleInitializer.mainMethodWithArgs("Test1", "main1", List())
    val mi2 = ModuleInitializer.mainMethodWithArgs("Test1", "main1", List("x"))
    assertFingerprintsNotEquals(mi1, mi2)
  }
}
