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

import java.net.URI

import org.junit.Test
import org.junit.Assert._

import Semantics.RuntimeClassNameMapper._

class StandardConfigFingerprintTest {
  def assertFingerprintsNotEquals(sc1: StandardConfig, sc2: StandardConfig): Unit = {
    assertNotEquals(StandardConfig.fingerprint(sc1),
        StandardConfig.fingerprint(sc2))
  }

  @Test
  def noFingerprintCollisionCheckIR(): Unit = {
    val sc1 = StandardConfig().withCheckIR(false)
    val sc2 = StandardConfig().withCheckIR(true)
    assertFingerprintsNotEquals(sc1, sc2)
  }

  @Test
  def noFingerprintCollisionRelativizeSourceMapBase(): Unit = {
    val sc1 = StandardConfig().withRelativizeSourceMapBase(None)
    val sc2 = StandardConfig().withRelativizeSourceMapBase(Some(new URI("a")))
    assertFingerprintsNotEquals(sc1, sc2)
  }

  @Test
  def noFingerprintCollisionESFeatures(): Unit = {
    val sc1 = StandardConfig().withESFeatures(_.withUseECMAScript2015(true))
    val sc2 = StandardConfig().withESFeatures(_.withUseECMAScript2015(false))
    assertFingerprintsNotEquals(sc1, sc2)
  }

  @Test
  def noFingerprintCollisionRuntimeClassNameMapper(): Unit = {
    val sc1 = StandardConfig().withSemantics(_.withRuntimeClassNameMapper(
        keepAll() andThen discardAll()))
    val sc2 = StandardConfig().withSemantics(_.withRuntimeClassNameMapper(
        regexReplace("""\d+""".r, "0")))
    assertFingerprintsNotEquals(sc1, sc2)
  }
}
