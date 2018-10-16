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

import org.scalajs.ir.{Trees => js}

class JSExportASTTest extends JSASTTest {

  @Test
  def inheritExportMethods: Unit = {
    """
    import scala.scalajs.js.annotation.JSExport

    class A {
      @JSExport
      def foo = 1
    }

    class B extends A {
      @JSExport
      override def foo = 2
    }
    """.hasExactly(1, "definitions of property `foo`") {
      case js.PropertyDef(_, js.StringLiteral("foo"), _, _) =>
    }
  }

}
