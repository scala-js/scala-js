package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
import org.junit.Test

class DiverseErrorsTest extends DirectTest with TestHelpers  {

  override def preamble =
    """import scala.scalajs.js
    """

  @Test
  def noIsInstanceOnJSRaw = {

    """
    trait JSRaw extends js.Object

    class A {
      val a: AnyRef = "asdf"
      def x = a.isInstanceOf[JSRaw]
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: isInstanceOf[JSRaw] not supported because it is a raw JS trait
      |      def x = a.isInstanceOf[JSRaw]
      |                            ^
    """

  }

}
