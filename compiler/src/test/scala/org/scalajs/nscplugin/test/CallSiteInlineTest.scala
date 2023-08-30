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

package org.scalajs.nscplugin.test

import util._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.{Trees => js, Types => jstpe}
import org.scalajs.ir.Names
import org.scalajs.ir.Names._

class CallSiteInlineTest extends JSASTTest {
  object SMN {
    def unapply(ident: js.MethodIdent): Some[String] =
      Some(ident.name.simpleName.nameString)
  }

  @Test
  def testInline: Unit = {
    val flags = {
      """
      object A {
        println("F"): @inline
      }
      """.extractOne("println call") {
        case js.Apply(flags, _, SMN("println"), _) => flags
      }
    }

    assertTrue(flags.inline)
  }

  @Test
  def testNoinline: Unit = {
    val flags = {
      """
      object A {
        println("F"): @noinline
      }
      """.extractOne("println call") {
        case js.Apply(flags, _, SMN("println"), _) => flags
      }
    }

    assertTrue(flags.noinline)
  }

  @Test
  def testInlineNullary: Unit = {
    val flags = {
      """
      object A {
        Map.empty: @inline
      }
      """.extractOne("Map.empty") {
        case js.Apply(flags, _, SMN("empty"), _) => flags
      }
    }

    assertTrue(flags.inline)
  }

  @Test
  def testNoinlineNullary: Unit = {
    val flags = {
      """
      object A {
        Map.empty: @noinline
      }
      """.extractOne("Map.empty") {
        case js.Apply(flags, _, SMN("empty"), _) => flags
      }
    }

    assertTrue(flags.noinline)
  }

  @Test
  def testInlineStatic: Unit = {
    val flags = {
      """
      object A {
        Integer.compare(1, 2): @inline
      }
      """.extractOne("compare call") {
        case js.ApplyStatic(flags, _, SMN("compare"), _) => flags
      }
    }

    assertTrue(flags.inline)
  }

  @Test
  def testNoinlineStatic: Unit = {
    val flags = {
      """
      object A {
        Integer.compare(1, 2): @noinline
      }
      """.extractOne("compare call") {
        case js.ApplyStatic(flags, _, SMN("compare"), _) => flags
      }
    }

    assertTrue(flags.noinline)
  }
}
