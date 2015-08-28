package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._

import org.junit.Test
import org.junit.Ignore

// scalastyle:off line.size.limit

class JSInteropTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._
    """

  @Test
  def warnNoJSNativeAnnotation: Unit = {

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      $obj A extends js.Object
      """ hasWarns
      s"""
        |newSource1.scala:5: warning: Classes, traits and objects inheriting from js.Any should be annotated with @js.native, unless they have @ScalaJSDefined. The default will switch to Scala.js-defined in the next major version of Scala.js.
        |      $obj A extends js.Object
        |      ${" " * obj.length} ^
      """
    }

  }

  @Test
  def noJSNativeAnnotWithSJSDefinedAnnot: Unit = {

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @ScalaJSDefined
      @js.native
      $obj A extends js.Object
      """ hasErrors
      s"""
        |newSource1.scala:7: error: @ScalaJSDefined and @js.native cannot be used together
        |      $obj A extends js.Object
        |      ${" " * obj.length} ^
      """
    }

  }

  @Test
  def noInnerClassTraitObject: Unit = {

    for {
      outer <- Seq("class", "trait")
      inner <- Seq("class", "trait", "object")
      innerSJSDefined <- Seq(false, true)
    } yield {
      val innerLine =
        if (innerSJSDefined) s"@ScalaJSDefined $inner A extends js.Object"
        else s"$inner A"
      s"""
      @js.native
      $outer A extends js.Object {
        $innerLine
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: Native JS traits and classes may not have inner traits, classes or objects
        |        $innerLine
        |        ${" " * innerLine.indexOf('A')}^
      """
    }

  }

  @Test
  def noScalaStuffInsideNativeJSObject: Unit = {

    for {
      inner <- Seq("class", "trait", "object")
    } yield {
      s"""
      @js.native
      object A extends js.Object {
        $inner A
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: Native JS objects cannot contain inner Scala traits, classes or objects (i.e., not extending js.Any)
        |        $inner A
        |        ${" " * inner.length} ^
      """
    }

  }

  @Test
  def noScalaJSDefinedClassObjectInsideNativeJSObject: Unit = {

    for {
      inner <- Seq("class", "object")
    } yield {
      s"""
      @js.native
      object A extends js.Object {
        @ScalaJSDefined $inner A extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: Native JS objects cannot contain inner Scala.js-defined JS classes or objects
        |        @ScalaJSDefined $inner A extends js.Object
        |                        ${" " * inner.length} ^
      """
    }

  }

  @Test
  def noBadSetters: Unit = {

    """
    @js.native
    class A extends js.Object {
      def foo_=(x: Int): Int = js.native
      def bar_=(x: Int, y: Int): Unit = js.native
      def goo_=(x: Int*): Unit = js.native
      def hoo_=(x: Int = 1): Unit = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Raw JS setters must return Unit
      |      def foo_=(x: Int): Int = js.native
      |          ^
      |newSource1.scala:8: error: Raw JS setters must have exactly one argument
      |      def bar_=(x: Int, y: Int): Unit = js.native
      |          ^
      |newSource1.scala:9: error: Raw JS setters may not have repeated params
      |      def goo_=(x: Int*): Unit = js.native
      |          ^
      |newSource1.scala:10: error: Raw JS setters may not have default params
      |      def hoo_=(x: Int = 1): Unit = js.native
      |          ^
    """

  }

  @Test
  def noBadBracketCall: Unit = {

    """
    @js.native
    class A extends js.Object {
      @js.annotation.JSBracketCall
      def foo(): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @JSBracketCall methods must have at least one non-repeated parameter
      |      def foo(): Int = js.native
      |          ^
    """

  }

  @Test
  def onlyJSRawTraits: Unit = {

    """
    @js.native
    trait A
    @js.native
    class B extends js.Object with A
    """ hasErrors
    """
      |newSource1.scala:8: error: B extends A which does not extend js.Any.
      |    class B extends js.Object with A
      |          ^
    """

    """
    @js.native
    trait A
    @js.native
    class B extends js.Object with Serializable
    """ hasErrors
    """
      |newSource1.scala:8: error: B extends scala.Serializable which does not extend js.Any.
      |    class B extends js.Object with Serializable
      |          ^
    """

  }

  @Test
  def noCaseClassObject: Unit = {

    """
    @js.native
    case class A(x: Int) extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes and objects extending js.Any may not have a case modifier
      |    case class A(x: Int) extends js.Object
      |               ^
    """

    """
    @js.native
    case object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes and objects extending js.Any may not have a case modifier
      |    case object B extends js.Object
      |                ^
    """

    """
    @ScalaJSDefined
    case class A(x: Int) extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes and objects extending js.Any may not have a case modifier
      |    case class A(x: Int) extends js.Object
      |               ^
    """

    """
    @ScalaJSDefined
    case object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes and objects extending js.Any may not have a case modifier
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
      outerSJSDefined <- Seq(false, true)
    } yield {
      val outerLine =
        if (outerSJSDefined) s"@ScalaJSDefined $outer A extends js.Object"
        else s"$outer A"

      val errTrg = if (inner == "object") "objects" else "classes"

      s"""
      $outerLine {
        @js.native
        $inner Inner extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: Traits and classes may not have inner native JS traits, classes or objects
        |        $inner Inner extends js.Object
        |         ${" " * inner.length}^
      """
    }

  }

  @Test
  def noGlobalScopeClass: Unit = {

    """
    @js.native
    class A extends js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:6: error: Only native objects may extend js.GlobalScope
      |    class A extends js.GlobalScope
      |          ^
    """

    """
    @js.native
    trait A extends js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:6: error: Only native objects may extend js.GlobalScope
      |    trait A extends js.GlobalScope
      |          ^
    """

    """
    @ScalaJSDefined
    class A extends js.Object with js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:6: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |    class A extends js.Object with js.GlobalScope
      |          ^
    """

    """
    @ScalaJSDefined
    trait A extends js.Object with js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:6: error: A Scala.js-defined JS trait cannot directly extend a native JS trait.
      |    trait A extends js.Object with js.GlobalScope
      |          ^
    """

    """
    @ScalaJSDefined
    object A extends js.Object with js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:6: error: A Scala.js-defined JS object cannot directly extend a native JS trait.
      |    object A extends js.Object with js.GlobalScope
      |           ^
    """

  }

  @Test
  def noLocalClass: Unit = {

    """
    object A {
      def a = {
        @js.native
        class B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Local native JS classes and objects are not allowed
      |        class B extends js.Object
      |              ^
    """

  }

  @Test
  def noLocalObject: Unit = {

    """
    object A {
      def a = {
        @js.native
        object B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Local native JS classes and objects are not allowed
      |        object B extends js.Object
      |               ^
    """

  }

  @Test
  def noNativeInJSAny: Unit = {

    """
    @js.native
    class A extends js.Object {
      @native
      def value: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Methods in a js.Any may not be @native
      |      def value: Int = js.native
      |          ^
    """

  }

  @Test
  def warnJSAnyBody: Unit = {

    """
    @js.native
    class A extends js.Object {
      def value: Int = ???
      val x: Int = ???
    }
    """ hasWarns
    """
      |newSource1.scala:7: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      def value: Int = ???
      |                       ^
      |newSource1.scala:8: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      val x: Int = ???
      |                   ^
    """

  }

  @Test
  def noWarnJSAnyDeferred: Unit = {

    """
    @js.native
    abstract class A extends js.Object {
      def value: Int
      val x: Int
    }
    """.hasNoWarns

    """
    @js.native
    trait A extends js.Object {
      def value: Int
      val x: Int
    }
    """.hasNoWarns

  }

  @Test
  def noCallSecondaryCtor: Unit = {

    """
    @js.native
    class A(x: Int, y: Int) extends js.Object {
      def this(x: Int) = this(x, 5)
      def this() = this(7)
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: A secondary constructor of a class extending js.Any may only call the primary constructor
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
      |newSource1.scala:6: error: js.native may only be used as stub implementation in facade types
      |      def foo = js.native
      |                   ^
    """

  }

  @Test
  def warnNothingRaw: Unit = {

    """
    @js.native
    class A extends js.Object {
      def foo = js.native
      val bar = js.native
    }
    """ hasWarns
    """
      |newSource1.scala:7: warning: The type of foo got inferred as Nothing. To suppress this warning, explicitly ascribe the type.
      |      def foo = js.native
      |          ^
      |newSource1.scala:8: warning: The type of bar got inferred as Nothing. To suppress this warning, explicitly ascribe the type.
      |      val bar = js.native
      |          ^
    """

  }

  @Test
  def noNativeClassObjectWithoutJSNameInsideScalaObject: Unit = {

    """
    object A {
      @js.native
      class B extends js.Object
    }
    """ hasWarns
    """
      |newSource1.scala:7: warning: Native JS classes inside non-native objects should have an @JSName annotation. This will be enforced in 1.0.
      |      class B extends js.Object
      |            ^
    """

    """
    object A {
      @js.native
      object B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS objects inside non-native objects must have an @JSName annotation
      |      object B extends js.Object
      |             ^
    """

    """
    object A {
      @JSName("InnerB")
      @js.native
      class B extends js.Object
      @JSName("InnerC")
      @js.native
      object C extends js.Object
    }
    """.hasNoWarns

    """
    object A {
      @js.native
      trait B extends js.Object
    }
    """.hasNoWarns

    """
    @js.native
    object A extends js.Object {
      @js.native
      class B extends js.Object
      @js.native
      trait C extends js.Object
      @js.native
      object D extends js.Object
    }
    """.hasNoWarns

  }

  @Test
  def noNativeClassObjectInsideScalaJSDefinedObject: Unit = {

    for {
      inner <- Seq("class", "object")
    } {
      s"""
      @ScalaJSDefined
      object A extends js.Object {
        @js.native
        $inner B extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:8: error: Scala.js-defined JS objects may not have inner native JS classes or objects
        |        $inner B extends js.Object
        |        ${" " * inner.length} ^
      """
    }

  }

  @Test
  def noNonLiteralJSName: Unit = {

    """
    import js.annotation.JSName

    object A {
      val a = "Hello"
      final val b = "World"
    }

    @js.native
    class B extends js.Object {
      @JSName(A.a)
      def foo: Int = js.native
      @JSName(A.b)
      def bar: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: The argument to JSName must be a literal string
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
    @js.native
    object B extends js.Object

    @JSName(A.a)
    @js.native
    class C extends js.Object
    """ hasErrors
    """
      |newSource1.scala:11: error: The argument to JSName must be a literal string
      |    @JSName(A.a)
      |     ^
      |newSource1.scala:15: error: The argument to JSName must be a literal string
      |    @JSName(A.a)
      |     ^
    """

  }

  @Test
  def noApplyProperty: Unit = {

    // def apply

    """
    @js.native
    trait A extends js.Object {
      def apply: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: A member named apply represents function application in JavaScript. A parameterless member should be exported as a property. You must add @JSName("apply")
      |      def apply: Int = js.native
      |          ^
    """

    """
    import js.annotation.JSName

    @js.native
    trait A extends js.Object {
      @JSName("apply")
      def apply: Int = js.native
    }
    """.succeeds

    // val apply

    """
    @js.native
    trait A extends js.Object {
      val apply: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: A member named apply represents function application in JavaScript. A parameterless member should be exported as a property. You must add @JSName("apply")
      |      val apply: Int = js.native
      |          ^
    """

    """
    import js.annotation.JSName

    @js.native
    trait A extends js.Object {
      @JSName("apply")
      val apply: Int = js.native
    }
    """.succeeds

    // var apply

    """
    @js.native
    trait A extends js.Object {
      var apply: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: A member named apply represents function application in JavaScript. A parameterless member should be exported as a property. You must add @JSName("apply")
      |      var apply: Int = js.native
      |          ^
    """

    """
    import js.annotation.JSName

    @js.native
    trait A extends js.Object {
      @JSName("apply")
      var apply: Int = js.native
    }
    """.succeeds

  }

}
