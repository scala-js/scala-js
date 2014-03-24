package scala.scalajs.compiler.test

import scala.scalajs.compiler.test.util._

import org.junit.Test

class EnumerationInteropTest extends DirectTest with TestHelpers {

  @Test
  def warnIfUnableToTransform = {

    """
    class A extends Enumeration {
      val a = {
        println("oh, oh!")
        Value
      }
    }
    """ hasWarns
    """
      |newSource1.scala:5: warning: Couldn't transform call to Enumeration.Value.
      |The resulting program is unlikely to function properly as this
      |operation requires reflection.
      |        Value
      |        ^
    """

  }

}
