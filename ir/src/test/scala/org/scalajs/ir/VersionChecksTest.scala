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

class VersionChecksTest {
  private def assertThrows(body: => Unit) = {
    val ok = try {
      body
      false
    } catch {
      case _: Exception => true
    }

    if (!ok)
      fail("expected exception")
  }

  private def ok(current: String, binary: String) =
    new VersionChecks(current, binary)

  private def bad(current: String, binary: String) =
    assertThrows(new VersionChecks(current, binary))

  @Test def failOnBadSyntax: Unit = {
    bad("2.0", "2.0")  // current needs patch
    bad("2.0.0", "2")  // binary needs major
  }

  @Test def checkConsistency: Unit = {
    ok("2.0.0-M1", "2.0-M1")
    ok("2.0.1-M1", "2.0")
    ok("2.2.0", "2.1")
    ok("2.2.0", "2.2")
    ok("2.2.0-M1", "2.1")
    ok("2.1.1-M1", "2.1")

    // binary is newer
    bad("2.1.0", "2.2")
    bad("2.2.0-M1", "2.2")

    // binary is pre-release in non-matching version.
    bad("2.3.0", "2.2-M1")
    bad("2.2.1", "2.2-M1")
    bad("2.3.0-M1", "2.2-M1")
    bad("2.2.0", "2.2-M1")

    // major is different
    bad("1.0.1", "2.0")
  }

  @Test def checkSupportedNormal: Unit = {
    val v = new VersionChecks("2.5.2", "2.4")

    v.checkSupported("2.4")
    v.checkSupported("2.3")
    v.checkSupported("2.2")
    v.checkSupported("2.1")
    v.checkSupported("2.0")

    assertThrows(v.checkSupported("1.4"))
    assertThrows(v.checkSupported("2.5"))
    assertThrows(v.checkSupported("2.6"))
    assertThrows(v.checkSupported("2.2-SNAPSHOT"))
  }

  @Test def checkSupportedPreRelease: Unit = {
    val v = new VersionChecks("3.2.0-M1", "3.2-M1")

    v.checkSupported("3.0")
    v.checkSupported("3.1")
    v.checkSupported("3.2-M1")

    assertThrows(v.checkSupported("3.2"))
    assertThrows(v.checkSupported("2.1-M1"))
  }

  @Test def binaryCrossVersion: Unit = {
    def test(full: String, emitted: String, cross: String): Unit =
      assertEquals(cross, new VersionChecks(full, emitted).binaryCross)

    test("1.0.0", "1.0", "1")
    test("1.0.2", "1.0", "1")
    test("1.0.2-M1", "1.0", "1")
    test("1.0.0-SNAPSHOT", "1.0-SNAPSHOT", "1.0-SNAPSHOT")
    test("1.0.0-M1", "1.0-M1", "1.0-M1")
    test("1.2.0-SNAPSHOT", "1.2-SNAPSHOT", "1")
    test("1.2.0-M1", "1.2-M1", "1")
    test("1.3.0-M1", "1.2", "1")
  }
}
