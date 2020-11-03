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

package org.scalajs.testsuite.scalalib

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.Platform.scalaVersion

class SymbolTestScala2 {

  /** This is a Scala 2.x only test because:
   *  Dotty no longer supports symbol literal.
   */
  @Test def should_support_symbol_literal(): Unit = {
    val scalajs = 'ScalaJS

    assertEquals(Symbol("ScalaJS"), scalajs)
    assertEquals(Symbol("$"), '$)
    assertEquals(Symbol("$$"), '$$)
    assertEquals(Symbol("-"), '-)
    assertEquals(Symbol("*"), '*)
  }

  /** This test is similar to the one found in SymbolTest with the same name.
   *  But it uses symbol literals that are not supported on Dotty.
   */
  @Test def should_ensure_unique_identity(): Unit = {
    def expectEqual(sym1: Symbol, sym2: Symbol): Unit = {
      assertTrue(sym1 eq sym2)
      assertEquals(sym2, sym1)
      assertEquals(sym2.##, sym1.##)
    }

    expectEqual('ScalaJS, Symbol("ScalaJS"))
    expectEqual('$, Symbol("$"))
    expectEqual('-, Symbol("-"))

    val `42` = Symbol("42")
    val map = Map[Symbol, Any](Symbol("ScalaJS") -> "Scala.js", '$ -> 1.2, `42` -> 42)
    assertEquals("Scala.js", map('ScalaJS))
    assertEquals(1.2, map(Symbol("$")))
    assertEquals(42, map(Symbol("42")))
    assertEquals(42, map(`42`))
  }
}
