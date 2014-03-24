package scala.scalajs.compiler.test

import scala.scalajs.compiler.test.util._
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

  @Test
  def noReflCallOnUndefinedObjectFun = {

    """
    class A {
      type Waitable = Any { def wait(): Unit }
      def foo(obj: Waitable) = obj.wait()
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You tried to call wait on AnyRef reflectively, but this
      |method does not make sense in Scala.js. You may not call it
      |      def foo(obj: Waitable) = obj.wait()
      |                                       ^
    """

  }

}
