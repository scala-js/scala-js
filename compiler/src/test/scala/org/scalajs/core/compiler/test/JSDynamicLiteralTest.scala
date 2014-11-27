package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
import org.junit.Test

class JSDynamicLiteralTest extends DirectTest with TestHelpers {

  override def preamble =
    """import scala.scalajs.js.Dynamic.{ literal => lit }
    """

  @Test
  def callApplyOnly = {

    // selectDynamic (with any name)
    expr"""
    lit.helloWorld
    """.fails() // Scala error, no string checking due to versions

    // applyDynamicNamed with wrong method name
    expr"""
    lit.helloWorld(a = "a")
    """ hasErrors
    """
      |newSource1.scala:3: error: js.Dynamic.literal does not have a method named helloWorld
      |    lit.helloWorld(a = "a")
      |                  ^
    """

    // applyDynamic with wrong method name
    expr"""
    lit.helloWorld("a" -> "a")
    """ hasErrors
    """
      |newSource1.scala:3: error: js.Dynamic.literal does not have a method named helloWorld
      |    lit.helloWorld("a" -> "a")
      |                  ^
    """

  }

  @Test
  def goodTypesOnly = {

    // Bad value type (applyDynamic)
    """
    class A {
      val x = new Object()
      def foo = lit("a" -> x)
    }
    """.fails()

    // Bad key type (applyDynamic)
    """
    class A {
      val x = Seq()
      def foo = lit(x -> "a")
    }
    """.fails()

    // Bad value type (applyDynamicNamed)
    """
    class A {
      val x = new Object()
      def foo = lit(a = x)
    }
    """.fails()

  }

  @Test
  def noNonLiteralMethodName = {

    // applyDynamicNamed
    """
    class A {
      val x = "string"
      def foo = lit.applyDynamicNamed(x)()
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: js.Dynamic.literal.applyDynamicNamed may not be called directly
      |      def foo = lit.applyDynamicNamed(x)()
      |                                        ^
    """

    // applyDynamic
    """
    class A {
      val x = "string"
      def foo = lit.applyDynamic(x)()
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: js.Dynamic.literal.applyDynamic may not be called directly
      |      def foo = lit.applyDynamic(x)()
      |                                   ^
    """

  }

}
