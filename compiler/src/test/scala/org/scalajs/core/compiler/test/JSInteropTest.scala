package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._

import org.junit.Test
import org.junit.Ignore

// scalastyle:off line.size.limit

class JSInteropTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """import scala.scalajs.js
    """

  @Test
  def noInnerClassTraitObject: Unit = {

    val objs = List("class", "trait", "object")

    for {
      outer <- objs
      inner <- objs
    } yield {
      s"""
      $outer A extends js.Object {
        $inner A
      }
      """ hasErrors
      s"""
        |newSource1.scala:4: error: Traits, classes and objects extending js.Any may not have inner traits, classes or objects
        |        $inner A
        |         ${" " * inner.length}^
      """
    }

  }

  @Test
  def noBadSetters: Unit = {

    """
    class A extends js.Object {
      def foo_=(x: Int): Int = js.native
      def bar_=(x: Int, y: Int): Unit = js.native
      def goo_=(x: Int*): Unit = js.native
      def hoo_=(x: Int = 1): Unit = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Raw JS setters must return Unit
      |      def foo_=(x: Int): Int = js.native
      |          ^
      |newSource1.scala:5: error: Raw JS setters must have exactly one argument
      |      def bar_=(x: Int, y: Int): Unit = js.native
      |          ^
      |newSource1.scala:6: error: Raw JS setters may not have repeated params
      |      def goo_=(x: Int*): Unit = js.native
      |          ^
      |newSource1.scala:7: error: Raw JS setters may not have default params
      |      def hoo_=(x: Int = 1): Unit = js.native
      |          ^
    """

  }

  @Test
  def noBadBracketCall: Unit = {

    """
    class A extends js.Object {
      @js.annotation.JSBracketCall
      def foo(): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: @JSBracketCall methods must have at least one non-repeated parameter
      |      def foo(): Int = js.native
      |          ^
    """

  }

  @Test
  def onlyJSRawTraits: Unit = {

    """
    trait A
    class B extends js.Object with A
    """ hasErrors
    """
      |newSource1.scala:4: error: B extends A which does not extend js.Any.
      |    class B extends js.Object with A
      |          ^
    """

    """
    trait A
    class B extends js.Object with Serializable
    """ hasErrors
    """
      |newSource1.scala:4: error: B extends scala.Serializable which does not extend js.Any.
      |    class B extends js.Object with Serializable
      |          ^
    """

  }

  @Test
  def noAnonymousClass: Unit = {

    """
    class A {
      val x = new js.Object {
        def a: Int = js.native
      }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Anonymous classes may not extend js.Any
      |      val x = new js.Object {
      |                  ^
    """

  }

  @Test
  def noCaseClassObject: Unit = {

    """
    case class A(x: Int) extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: Classes and objects extending js.Any may not have a case modifier
      |    case class A(x: Int) extends js.Object
      |               ^
    """

    """
    case object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: Classes and objects extending js.Any may not have a case modifier
      |    case object B extends js.Object
      |                ^
    """

  }

  @Test
  def notNested: Unit = {

    val outers = List("class", "trait")
    val inners = List("trait", "class", "object")

    for {
      outer <- outers
      inner <- inners
    } yield {

      val errTrg = if (inner == "object") "Objects" else "Classes"

      s"""
      $outer A {
        $inner Inner extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:4: error: $errTrg extending js.Any may not be defined inside a class or trait
        |        $inner Inner extends js.Object
        |         ${" " * inner.length}^
      """
    }

  }

  @Test
  def noGlobalScopeClass: Unit = {

    """
    class A extends js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:3: error: Only objects may extend js.GlobalScope
      |    class A extends js.GlobalScope
      |          ^
    """

    """
    trait A extends js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:3: error: Only objects may extend js.GlobalScope
      |    trait A extends js.GlobalScope
      |          ^
    """

  }

  @Test
  def noLocalClass: Unit = {

    """
    object A {
      def a = {
        class B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Local classes and objects may not extend js.Any
      |        class B extends js.Object
      |              ^
    """

  }

  @Test
  def noLocalObject: Unit = {

    """
    object A {
      def a = {
        object B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Local classes and objects may not extend js.Any
      |        object B extends js.Object
      |               ^
    """

  }

  @Test
  def noNativeInJSAny: Unit = {

    """
    class A extends js.Object {
      @native
      def value: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Methods in a js.Any may not be @native
      |      def value: Int = js.native
      |          ^
    """

  }

  @Test
  def warnJSAnyBody: Unit = {

    """
    class A extends js.Object {
      def value: Int = ???
      val x: Int = ???
    }
    """ hasWarns
    """
      |newSource1.scala:4: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      def value: Int = ???
      |                       ^
      |newSource1.scala:5: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      val x: Int = ???
      |                   ^
    """

    """
    trait A extends js.Object {
      def value: Int
      val x: Int
    }
    """ hasWarns
    """
      |newSource1.scala:4: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      def value: Int
      |          ^
      |newSource1.scala:5: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      val x: Int
      |          ^
    """

  }

  @Test
  def noCallSecondaryCtor: Unit = {

    """
    class A(x: Int, y: Int) extends js.Object {
      def this(x: Int) = this(x, 5)
      def this() = this(7)
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: A secondary constructor of a class extending js.Any may only call the primary constructor
      |      def this() = this(7)
      |          ^
    """

  }

  @Test
  def noUseJsNative: Unit = {

    """
    class A {
      def foo = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: js.native may only be used as stub implementation in facade types
      |      def foo = js.native
      |                   ^
    """

  }

  @Test
  def warnNothingRaw: Unit = {

    """
    class A extends js.Object {
      def foo = js.native
      val bar = js.native
    }
    """ hasWarns
    """
      |newSource1.scala:4: warning: The type of foo got inferred as Nothing. To suppress this warning, explicitly ascribe the type.
      |      def foo = js.native
      |          ^
      |newSource1.scala:5: warning: The type of bar got inferred as Nothing. To suppress this warning, explicitly ascribe the type.
      |      val bar = js.native
      |          ^
    """

  }

  @Test
  def noNonLiteralJSName: Unit = {

    """
    import js.annotation.JSName

    object A {
      val a = "Hello"
      final val b = "World"
    }

    class B extends js.Object {
      @JSName(A.a)
      def foo: Int = js.native
      @JSName(A.b)
      def bar: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: The argument to JSName must be a literal string
      |      @JSName(A.a)
      |       ^
    """

    // #1664
    """
    import js.annotation.JSName

    object A {
      val a = "Hello"
    }

    @JSName(A.a)
    object B extends js.Object

    @JSName(A.a)
    class C extends js.Object
    """ hasErrors
    """
      |newSource1.scala:9: error: The argument to JSName must be a literal string
      |    @JSName(A.a)
      |     ^
      |newSource1.scala:12: error: The argument to JSName must be a literal string
      |    @JSName(A.a)
      |     ^
    """

  }

}
