package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._

import org.junit.Test
import org.junit.Ignore

// scalastyle:off line.size.limit

class ScalaJSDefinedTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._
    """

  @Test
  def noSJSDefinedOnScalaEntity: Unit = {
    val objs = List("class", "trait", "object")

    for {
      obj <- objs
    } {
      s"""
      @ScalaJSDefined
      $obj A
      """ hasErrors
      s"""
        |newSource1.scala:6: error: @ScalaJSDefined is only allowed on classes extending js.Any
        |      $obj A
        |       ${" " * obj.length}^
      """
    }
  }

  @Test
  def noExtendAnyRef: Unit = {
    """
    @ScalaJSDefined
    class A extends js.Any
    """ hasErrors
    """
      |newSource1.scala:6: error: A Scala.js-defined JS class cannot directly extend AnyRef. It must extend a JS class (native or not).
      |    class A extends js.Any
      |          ^
    """
  }

  @Test
  def noExtendNativeTrait: Unit = {
    """
    trait NativeTrait extends js.Object

    @ScalaJSDefined
    class A extends NativeTrait

    object Container {
      val x = new NativeTrait {}
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |    class A extends NativeTrait
      |          ^
      |newSource1.scala:11: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |      val x = new NativeTrait {}
      |                  ^
    """
  }

  @Test
  def noApplyMethod: Unit = {
    """
    @ScalaJSDefined
    class A extends js.Object {
      def apply(arg: Int): Int = arg
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: A Scala.js-defined JavaScript class cannot declare a method named `apply` without `@JSName`
      |      def apply(arg: Int): Int = arg
      |          ^
    """
  }

  @Test
  def noBracketAccess: Unit = {
    """
    @ScalaJSDefined
    class A extends js.Object {
      @JSBracketAccess
      def foo(index: Int, arg: Int): Int = arg
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @JSBracketAccess is not allowed in Scala.js-defined JS classes
      |      def foo(index: Int, arg: Int): Int = arg
      |          ^
    """
  }

  @Test
  def noBracketCall: Unit = {
    """
    @ScalaJSDefined
    class A extends js.Object {
      @JSBracketCall
      def foo(m: String, arg: Int): Int = arg
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @JSBracketCall is not allowed in Scala.js-defined JS classes
      |      def foo(m: String, arg: Int): Int = arg
      |          ^
    """
  }

  @Test
  def noOverloadedPrivate: Unit = {
    """
    @ScalaJSDefined
    class A extends js.Object {
      private def foo(i: Int): Int = i
      private def foo(s: String): String = s
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Private methods in Scala.js-defined JS classes cannot be overloaded. Use different names instead.
      |      private def foo(i: Int): Int = i
      |                  ^
      |newSource1.scala:8: error: Private methods in Scala.js-defined JS classes cannot be overloaded. Use different names instead.
      |      private def foo(s: String): String = s
      |                  ^
    """

    """
    object Enclosing {
      @ScalaJSDefined
      class A extends js.Object {
        private[Enclosing] def foo(i: Int): Int = i
        private def foo(s: String): String = s
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Private methods in Scala.js-defined JS classes cannot be overloaded. Use different names instead.
      |        private[Enclosing] def foo(i: Int): Int = i
      |                               ^
      |newSource1.scala:9: error: Private methods in Scala.js-defined JS classes cannot be overloaded. Use different names instead.
      |        private def foo(s: String): String = s
      |                    ^
    """

    """
    @ScalaJSDefined
    class A extends js.Object {
      private def foo(i: Int): Int = i
      def foo(s: String): String = s
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Private methods in Scala.js-defined JS classes cannot be overloaded. Use different names instead.
      |      private def foo(i: Int): Int = i
      |                  ^
    """

    """
    object Enclosing {
      @ScalaJSDefined
      class A extends js.Object {
        private[Enclosing] def foo(i: Int): Int = i
        def foo(s: String): String = s
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Private methods in Scala.js-defined JS classes cannot be overloaded. Use different names instead.
      |        private[Enclosing] def foo(i: Int): Int = i
      |                               ^
    """
  }

  @Test
  def noVirtualQualifiedPrivate: Unit = {
    """
    object Enclosing {
      @ScalaJSDefined
      class A extends js.Object {
        private[Enclosing] def foo(i: Int): Int = i
        private[Enclosing] val x: Int = 3
        private[Enclosing] var y: Int = 5
      }

      @ScalaJSDefined
      class B extends A {
        override private[Enclosing] final def foo(i: Int): Int = i + 1
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Qualified private members in Scala.js-defined JS classes must be final
      |        private[Enclosing] def foo(i: Int): Int = i
      |                               ^
      |newSource1.scala:9: error: Qualified private members in Scala.js-defined JS classes must be final
      |        private[Enclosing] val x: Int = 3
      |                               ^
      |newSource1.scala:10: error: Qualified private members in Scala.js-defined JS classes must be final
      |        private[Enclosing] var y: Int = 5
      |                               ^
    """

    """
    object Enclosing {
      @ScalaJSDefined
      abstract class A extends js.Object {
        private[Enclosing] def foo(i: Int): Int
        private[Enclosing] val x: Int
        private[Enclosing] var y: Int
      }

      @ScalaJSDefined
      class B extends A {
        override private[Enclosing] final def foo(i: Int): Int = i + 1
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Qualified private members in Scala.js-defined JS classes must be final
      |        private[Enclosing] def foo(i: Int): Int
      |                               ^
      |newSource1.scala:9: error: Qualified private members in Scala.js-defined JS classes must be final
      |        private[Enclosing] val x: Int
      |                               ^
      |newSource1.scala:10: error: Qualified private members in Scala.js-defined JS classes must be final
      |        private[Enclosing] var y: Int
      |                               ^
    """

    """
    object Enclosing {
      @ScalaJSDefined
      class A extends js.Object {
        final private[Enclosing] def foo(i: Int): Int = i
      }
    }
    """.succeeds

    """
    object Enclosing {
      @ScalaJSDefined
      class A extends js.Object {
        private def foo(i: Int): Int = i
        private[this] def bar(i: Int): Int = i + 1
      }
    }
    """.succeeds

    """
    object Enclosing {
      @ScalaJSDefined
      abstract class A extends js.Object {
        final private[Enclosing] def foo(i: Int): Int
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: abstract member may not have final modifier
      |        final private[Enclosing] def foo(i: Int): Int
      |                                     ^
    """
  }

  @Test
  def noSJSDefinedObject: Unit = {
    """
    @ScalaJSDefined
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Only classes can be Scala.js-defined
      |    object A extends js.Object
      |           ^
    """
  }

  @Test
  def noSJSDefinedTrait: Unit = {
    """
    @ScalaJSDefined
    trait A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Only classes can be Scala.js-defined
      |    trait A extends js.Object
      |          ^
    """
  }

  @Test
  def noSecondaryCtor: Unit = {
    """
    @ScalaJSDefined
    class A(x: Int, y: Int) extends js.Object {
      def this(x: Int) = this(x, 5)
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Implementation restriction: Scala.js-defined JS classes cannot have secondary constructors
      |      def this(x: Int) = this(x, 5)
      |          ^
    """
  }

  @Test
  def noSuperConstructorCallWithSpread: Unit = {
    """
    class A(args: Int*) extends js.Object

    @ScalaJSDefined
    class B(args: Int*) extends A(args: _*)
    """.fails
  }

  @Test
  def noUseJsNative: Unit = {
    """
    @ScalaJSDefined
    class A extends js.Object {
      def foo = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: js.native may only be used as stub implementation in facade types
      |      def foo = js.native
      |                   ^
    """

    """
    class A {
      val x = new js.Object {
        def a: Int = js.native
      }
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: js.native may only be used as stub implementation in facade types
      |        def a: Int = js.native
      |                        ^
    """
  }

  @Test
  def noNonLiteralJSName: Unit = {
    """
    object A {
      val a = "Hello"
      final val b = "World"
    }

    @ScalaJSDefined
    class B extends js.Object {
      @JSName(A.a)
      def foo: Int = js.native
      @JSName(A.b)
      def bar: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:12: error: The argument to JSName must be a literal string
      |      @JSName(A.a)
      |       ^
    """
  }

  @Test
  def noApplyProperty: Unit = {
    // def apply

    """
    @ScalaJSDefined
    class A extends js.Object {
      def apply: Int = 42
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: A member named apply represents function application in JavaScript. A parameterless member should be exported as a property. You must add @JSName("apply")
      |      def apply: Int = 42
      |          ^
    """

    """
    @ScalaJSDefined
    class A extends js.Object {
      @JSName("apply")
      def apply: Int = 42
    }
    """.succeeds

    // val apply

    """
    @ScalaJSDefined
    class A extends js.Object {
      val apply: Int = 42
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: A member named apply represents function application in JavaScript. A parameterless member should be exported as a property. You must add @JSName("apply")
      |      val apply: Int = 42
      |          ^
    """

    """
    @ScalaJSDefined
    class A extends js.Object {
      @JSName("apply")
      val apply: Int = 42
    }
    """.succeeds

    // var apply

    """
    @ScalaJSDefined
    class A extends js.Object {
      var apply: Int = 42
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: A member named apply represents function application in JavaScript. A parameterless member should be exported as a property. You must add @JSName("apply")
      |      var apply: Int = 42
      |          ^
    """

    """
    @ScalaJSDefined
    class A extends js.Object {
      @JSName("apply")
      var apply: Int = 42
    }
    """.succeeds
  }

  @Test
  def noExportClass: Unit = {
    """
    @ScalaJSDefined
    @JSExport
    class A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Implementation restriction: exporting a Scala.js-defined JS class is not supported
      |    @JSExport
      |     ^
    """

  }

}
