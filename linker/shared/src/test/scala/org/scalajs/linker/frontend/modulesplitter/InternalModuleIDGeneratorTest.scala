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
    val testPublicModuleIDs = List(ModuleID("test.Public"), ModuleID("test.OtherPublic"))
    val generator = new InternalModuleIDGenerator.ForClassNames(testPublicModuleIDs)

    def test(expected: String, classNameString: String): Unit =
      assertEquals(expected, generator.forClassName(ClassName(classNameString)).id)

    test("java.lang.String", "java.lang.String")
    test("java.lang.StringBuilder", "java.lang.StringBuilder")

    test("test-S.foo--Bar", "test-S.foo--Bar")

    test("test.été", "test.été")
    test("test.Été", "test.Été")

    test("test.ǳ", "test.ǳ") // U+01F3 Latin Small Letter Dz
    test("test.Ǳ", "test.Ǳ") // U+01F1 Latin Capital Letter Dz
    test("test.ǲ", "test.ǲ") // U+01F2 Latin Capital Letter D with Small Letter Z

    test("test.Public.", "test.Public")
    test("test.OtherPublic.", "test.OtherPublic")
  }

  @Test def testForDigest(): Unit = {
    val goodModuleID = ModuleID("good")
    val otherGoodModuleID = ModuleID("othergood")
    val collidingModuleID = ModuleID("internal-mod")

    val digest = Array(0x12.toByte, 0x34.toByte, 0xef.toByte)

    val generator1 = new InternalModuleIDGenerator.ForDigests(Nil)
    assertEquals("internal-1234ef", generator1.forDigest(digest).id)

    val generator2 = new InternalModuleIDGenerator.ForDigests(List(goodModuleID, otherGoodModuleID))
    assertEquals("internal-1234ef", generator2.forDigest(digest).id)

    val generator3 = new InternalModuleIDGenerator.ForDigests(List(goodModuleID, collidingModuleID))
    assertEquals("internal--1234ef", generator3.forDigest(digest).id)
  }
}
