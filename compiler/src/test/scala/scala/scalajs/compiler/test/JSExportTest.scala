package scala.scalajs.compiler.test

import scala.scalajs.compiler.test.util._
import org.junit.Test

class JSExportTest extends DirectTest with TestHelpers {

  override def preamble =
    """import scala.scalajs.js.annotation.JSExport
    """

  @Test
  def noDoubleUnderscoreExport = {
    // Normal exports
    """
    class A {
      @JSExport(name = "__")
      def foo = 1

      @JSExport
      def bar__(x: Int) = x
    }

    @JSExport
    class B__
    """ hasErrors
    """
      |newSource1.scala:4: error: An exported name may not contain a double underscore (`__`)
      |      @JSExport(name = "__")
      |                       ^
      |newSource1.scala:8: error: An exported name may not contain a double underscore (`__`)
      |      def bar__(x: Int) = x
      |          ^
      |newSource1.scala:12: error: An exported name may not contain a double underscore (`__`)
      |    class B__
      |          ^
    """

  }

  @Test
  def noConflictingExport = {
    """
    class Confl {
      @JSExport("value")
      def hello = "foo"

      @JSExport("value")
      def world = "bar"
    }
    """ fails() // No error test, Scala version dependent error messages

    """
    class Confl {
      class Box[T](val x: T)

      @JSExport
      def ub(x: Box[String]): String = x.x
      @JSExport
      def ub(x: Box[Int]): Int = x.x
    }
    """ fails() // No error test, Scala version dependent error messages

    """
    class Confl {
      @JSExport
      def rtType(x: Short) = x

      @JSExport
      def rtType(x: Int) = x
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Cannot disambiguate overloads for exported method $js$exported$meth$rtType with types
      |  (x: Int)Object
      |  (x: Short)Object
      |      @JSExport
      |       ^
    """
  }

  @Test
  def noAnyValReturn = {
    """
    class AnyValRet {
      @JSExport
      def anyVal: AnyVal = 1

      @JSExport
      def badGen[T](x: T) = x
    }
    """ hasErrors
    """
       |newSource1.scala:4: error: You may not export a method whose return type is neither a subtype of
       |AnyRef nor a concrete subtype of AnyVal (i.e. a value class or a
       |primitive value type).
       |      @JSExport
       |       ^
       |newSource1.scala:7: error: You may not export a method whose return type is neither a subtype of
       |AnyRef nor a concrete subtype of AnyVal (i.e. a value class or a
       |primitive value type).
       |      @JSExport
       |       ^
    """
  }

  @Test
  def noExportLocal = {
    // Local class
    """
    class A {
      def method = {
        @JSExport
        class A
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local class or object
      |        @JSExport
      |         ^
    """

    // Local object
    """
    class A {
      def method = {
        @JSExport
        object A
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local class or object
      |        @JSExport
      |         ^
    """

  }

}
