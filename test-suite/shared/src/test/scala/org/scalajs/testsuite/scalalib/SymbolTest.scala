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

class SymbolTest {

  @Test def should_ensure_unique_identity(): Unit = {
    def expectEqual(sym1: Symbol, sym2: Symbol): Unit = {
      assertTrue(sym1 eq sym2)
      assertEquals(sym2, sym1)
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

  @Test def should_support_name(): Unit = {
    val scalajs = 'ScalaJS

    assertEquals("ScalaJS", scalajs.name)
    assertEquals("$", Symbol("$").name)
    assertEquals("$$", '$$.name)
    assertEquals("-", '-.name)
    assertEquals("*", '*.name)
    assertEquals("'", Symbol("'").name)
    assertEquals("\"", Symbol("\"").name)
  }

  @Test def should_support_toString(): Unit = {
    val scalajs = 'ScalaJS

    val toStringUsesQuoteSyntax = {
      scalaVersion.startsWith("2.10.") ||
      scalaVersion.startsWith("2.11.") ||
      scalaVersion.startsWith("2.12.") ||
      scalaVersion == "2.13.0" ||
      scalaVersion == "2.13.1" ||
      scalaVersion == "2.13.2"
    }

    if (toStringUsesQuoteSyntax) {
      assertEquals("'ScalaJS", scalajs.toString)
      assertEquals("'$", Symbol("$").toString)
      assertEquals("'$$", '$$.toString)
      assertEquals("'-", '-.toString)
      assertEquals("'*", '*.toString)
      assertEquals("''", Symbol("'").toString)
      assertEquals("'\"", Symbol("\"").toString)
    } else {
      assertEquals("Symbol(ScalaJS)", scalajs.toString)
      assertEquals("Symbol($)", Symbol("$").toString)
      assertEquals("Symbol($$)", '$$.toString)
      assertEquals("Symbol(-)", '-.toString)
      assertEquals("Symbol(*)", '*.toString)
      assertEquals("Symbol(')", Symbol("'").toString)
      assertEquals("Symbol(\")", Symbol("\"").toString)
    }
  }
}
