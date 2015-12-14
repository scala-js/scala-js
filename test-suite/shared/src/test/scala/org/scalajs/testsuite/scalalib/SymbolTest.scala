/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import org.junit.Test
import org.junit.Assert._

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

    assertEquals("'ScalaJS", scalajs.toString)
    assertEquals("'$", Symbol("$").toString)
    assertEquals("'$$", '$$.toString)
    assertEquals("'-", '-.toString)
    assertEquals("'*", '*.toString)
    assertEquals("''", Symbol("'").toString)
    assertEquals("'\"", Symbol("\"").toString)
  }
}
