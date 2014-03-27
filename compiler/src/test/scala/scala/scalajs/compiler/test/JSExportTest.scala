package scala.scalajs.compiler.test

import scala.scalajs.compiler.test.util._
import org.junit.Test
import org.junit.Ignore

class JSExportTest extends DirectTest with TestHelpers {

  override def preamble =
    """import scala.scalajs.js.annotation.{JSExport, JSExportDescendentObjects}
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

    // Inherited exports
    """
    @JSExportDescendentObjects
    trait A

    package fo__o {
      object B extends A
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: B may not have a double underscore (`__`) in its fully qualified
      |name, since it is forced to be exported by a @JSExportDescendentObjects on trait A
      |      object B extends A
      |             ^
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

    """
    class Confl {
      @JSExport
      def foo(x: Int)(ys: Int*) = x

      @JSExport
      def foo(x: Int*) = x
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Cannot disambiguate overloads for exported method $js$exported$meth$foo with types
      |  (x: Seq)Object
      |  (x: Int, ys: Seq)Object
      |      @JSExport
      |       ^
    """

    """
    class Confl {
      @JSExport
      def foo(x: Int = 1) = x
      @JSExport
      def foo(x: String*) = x
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Cannot disambiguate overloads for exported method $js$exported$meth$foo with types
      |  (x: Int)Object
      |  (x: Seq)Object
      |      @JSExport
      |       ^
    """

    """
    class Confl {
      @JSExport
      def foo(x: Int, y: String)(z: Int = 1) = x
      @JSExport
      def foo(x: Short, y: String)(z: String*) = x
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Cannot disambiguate overloads for exported method $js$exported$meth$foo with types
      |  (x: Int, y: String, z: Int)Object
      |  (x: Short, y: String, z: Seq)Object
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
      |newSource1.scala:5: error: You may not export a local class
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
      |newSource1.scala:5: error: You may not export a local object
      |        @JSExport
      |         ^
    """

    // Local method
    """
    class A {
      def method = {
        @JSExport
        def foo = 1
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local definition
      |        @JSExport
      |         ^
    """

    // Local val
    """
    class A {
      def method = {
        @JSExport
        val x = 1
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local definition
      |        @JSExport
      |         ^
    """

    // Local var
    """
    class A {
      def method = {
        @JSExport
        var x = 1
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local definition
      |        @JSExport
      |         ^
    """

  }

  @Test
  def noMiddleVarArg = {

    """
    class A {
      @JSExport
      def method(xs: Int*)(ys: String) = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: In an exported method, a *-parameter must come last (through all parameter lists)
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noMiddleDefaultParam = {

    """
    class A {
      @JSExport
      def method(x: Int = 1)(y: String) = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: In an exported method, all parameters with defaults must be at the end
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportTrait = {

    """
    @JSExport
    trait Test
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not export a trait
      |    @JSExport
      |     ^
    """

  }

  @Test
  def noExportNonPublicClassOrObject = {

    """
    @JSExport
    private class A

    @JSExport
    protected class B
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not export a non-public class
      |    @JSExport
      |     ^
      |newSource1.scala:6: error: You may not export a non-public class
      |    @JSExport
      |     ^
    """

    """
    @JSExport
    private object A

    @JSExport
    protected object B
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not export an non-public object
      |    @JSExport
      |     ^
      |newSource1.scala:6: error: You may not export an non-public object
      |    @JSExport
      |     ^
    """

  }

  @Test
  def noExportNonPublicMember = {

    """
    class A {
      @JSExport
      private def foo = 1

      @JSExport
      protected def bar = 2
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a non-public method
      |      @JSExport
      |       ^
      |newSource1.scala:7: error: You may not export a non-public method
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportNestedClass = {

    """
    class A {
      @JSExport
      class Nested
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a nested class. Create an exported factory method in the outer class to work around this limitation.
      |      @JSExport
      |       ^
    """

    """
    object A {
      @JSExport
      class Nested
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a nested class. Create an exported factory method in the outer class to work around this limitation.
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportNestedObject = {

    """
    class A {
      @JSExport
      object Nested
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a nested object
      |      @JSExport
      |       ^
    """

    """
    object A {
      @JSExport
      object Nested
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a nested object
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportJSRaw = {

    """
    import scala.scalajs.js

    @JSExport
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a class extending js.Any
      |    @JSExport
      |     ^
    """

    """
    import scala.scalajs.js

    @JSExport
    class A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a constructor of a subclass of js.Any
      |    @JSExport
      |     ^
    """

  }

  @Test
  def noExportJSRawMember = {

    """
    import scala.scalajs.js

    class A extends js.Object {
      @JSExport
      def foo = 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: You may not export a method of a subclass of js.Any
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noBadSetterType = {

    // Bad param list
    """
    class A {
      @JSExport
      def foo_=(x: Int, y: Int) = ()
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: A method ending in _= will be exported as setter. But foo_= does not have the right signature to do so (single argument, unit return type).
      |      @JSExport
      |       ^
    """

    // Bad return type
    """
    class A {
      @JSExport
      def foo_=(x: Int) = "string"
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: A method ending in _= will be exported as setter. But foo_= does not have the right signature to do so (single argument, unit return type).
      |      @JSExport
      |       ^
    """

  }

  @Test
  def gracefulDoubleDefaultFail = {
    // This used to blow up (i.e. not just fail), because PrepJSExports asked
    // for the symbol of the default parameter getter of [[y]], and asserted its
    // not overloaded. Since the Scala compiler only fails later on this, the
    // assert got triggered and made the compiler crash
    """
    class A {
      @JSExport
      def foo(x: String, y: String = "hello") = x
      def foo(x: Int, y: String = "bar") = x
    }
    """ fails()
  }

}
