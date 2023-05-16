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

package org.scalajs.linker.frontend.modulesplitter

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Names.ClassName

import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Whitebox tests for `InternalModuleIDGenerator`. */
class InternalModuleIDGeneratorTest {
  @Test def testForClassName(): Unit = {
    val testPublicModuleIDs = List(ModuleID("test.-Public"), ModuleID("test.-Other-Public"))
    val generator = new InternalModuleIDGenerator.ForClassNames(testPublicModuleIDs)

    def test(expected: String, classNameString: String): Unit =
      assertEquals(expected, generator.forClassName(ClassName(classNameString)).id)

    test("java.lang.-String", "java.lang.String")
    test("java.lang.-String-Builder", "java.lang.StringBuilder")

    test("test---S.foo-----Bar", "test-S.foo--Bar")

    test("test.-u0000e9ét-u0000e9é", "test.été")
    test("test.-u0000c9Ét-u0000e9é", "test.Été")

    test("test.-u0001f3ǳ", "test.ǳ") // U+01F3 Latin Small Letter Dz
    test("test.-u0001f1Ǳ", "test.Ǳ") // U+01F1 Latin Capital Letter Dz
    test("test.-u0001f2ǲ", "test.ǲ") // U+01F2 Latin Capital Letter D with Small Letter Z

    test("test.-Public.", "test.Public")
    test("test.-Other-Public.", "test.OtherPublic")
  }

  @Test def testForDigest(): Unit = {
    val goodModuleID = ModuleID("good")
    val otherGoodModuleID = ModuleID("othergood")
    val collidingModuleID = ModuleID("internal-mod")
    val collidingCaseInsensitiveModuleID = ModuleID("InTernal--mod")
    val collidingCaseInsensitiveModuleID2 = ModuleID("İnTernal-mod") // U+0130 Latin Capital Letter I with Dot Above

    val digest = Array(0x12.toByte, 0x34.toByte, 0xef.toByte)

    val generator1 = new InternalModuleIDGenerator.ForDigests(Nil)
    assertEquals("internal-1234ef", generator1.forDigest(digest).id)

    val generator2 = new InternalModuleIDGenerator.ForDigests(List(goodModuleID, otherGoodModuleID))
    assertEquals("internal-1234ef", generator2.forDigest(digest).id)

    val generator3 = new InternalModuleIDGenerator.ForDigests(List(goodModuleID, collidingModuleID))
    assertEquals("internal--1234ef", generator3.forDigest(digest).id)

    val generator4 = new InternalModuleIDGenerator.ForDigests(List(collidingCaseInsensitiveModuleID, goodModuleID))
    assertEquals("internal---1234ef", generator4.forDigest(digest).id)

    val generator5 = new InternalModuleIDGenerator.ForDigests(List(collidingCaseInsensitiveModuleID2, goodModuleID))
    assertEquals("internal--1234ef", generator5.forDigest(digest).id)
  }
}
