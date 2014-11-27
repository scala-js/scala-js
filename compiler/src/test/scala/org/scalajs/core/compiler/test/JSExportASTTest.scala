package org.scalajs.core.compiler.test

import util._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.ir.{Trees => js}

class JSExportASTTest extends JSASTTest {

  @Test
  def inheritExportMethods: Unit = {

    var props = 0

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
    """.traverse {
      case js.PropertyDef(js.StringLiteral("foo"), _, _, _) =>
        props += 1
    }

    assertEquals("Only define the property `foo` once", props, 1)

  }

}
