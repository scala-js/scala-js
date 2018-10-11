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

package org.scalajs.core.compiler.test

import util.JSASTTest

import org.junit.Test
import org.junit.Assert._

import scala.reflect.internal.util.BatchSourceFile

import org.scalajs.core.ir.{Trees => js}

class PositionTest extends JSASTTest {

  @Test
  def virtualFilePosition: Unit = {

    val name = "<foo with illegal URI chars: %%>"
    val source = new BatchSourceFile(name,
        """class A { def x = 1 }""")

    var found = false
    sourceAST(source) traverse {
      case lit: js.IntLiteral =>
        found = true
        assertEquals(
            "Scheme of virtual file URI should be `virtualfile'",
            "virtualfile", lit.pos.source.getScheme)
        assertEquals(
            "Scheme specific part of virtual file URI should be its path",
            name, lit.pos.source.getSchemeSpecificPart)
    }

    assertTrue("Should have IntLiteral tree", found)

  }

}
