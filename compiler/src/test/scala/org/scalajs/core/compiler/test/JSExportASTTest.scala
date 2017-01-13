package org.scalajs.core.compiler.test

import util._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.ir.{Trees => js}

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
