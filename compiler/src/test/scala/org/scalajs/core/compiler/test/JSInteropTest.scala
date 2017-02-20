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

  private val JSNativeLoadSpecAnnots = Seq(
      "JSName" -> "@JSName(\"foo\")",
      "JSImport" -> "@JSImport(\"foo\", \"bar\")",
      "JSGlobalScope" -> "@JSGlobalScope"
  )

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
  def warnJSPackageObjectDeprecated: Unit = {

    s"""
    package object jspackage extends js.Object
    """ hasWarns
    s"""
      |newSource1.scala:5: warning: Package objects inheriting from js.Any are deprecated. Use a normal object instead.
      |    package object jspackage extends js.Object
      |                   ^
    """

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
  def noJSNameAnnotOnNonJSNative: Unit = {

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @ScalaJSDefined
      @JSName("foo")
      $obj A extends js.Object

      object Sym {
        val sym = js.Symbol()
      }

      @ScalaJSDefined
      @JSName(Sym.sym)
      $obj B extends js.Object
      """ hasWarns
      s"""
        |newSource1.scala:6: warning: Non JS-native classes, traits and objects should not have an @JSName annotation, as it does not have any effect. This will be enforced in 1.0.
        |      @JSName("foo")
        |       ^
        |newSource1.scala:14: warning: Non JS-native classes, traits and objects should not have an @JSName annotation, as it does not have any effect. This will be enforced in 1.0.
        |      @JSName(Sym.sym)
        |       ^
      """
    }

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSName("foo")
      $obj A

      object Sym {
        val sym = js.Symbol()
      }

      @JSName(Sym.sym)
      $obj B
      """ hasWarns
      s"""
        |newSource1.scala:5: warning: Non JS-native classes, traits and objects should not have an @JSName annotation, as it does not have any effect. This will be enforced in 1.0.
        |      @JSName("foo")
        |       ^
        |newSource1.scala:12: warning: Non JS-native classes, traits and objects should not have an @JSName annotation, as it does not have any effect. This will be enforced in 1.0.
        |      @JSName(Sym.sym)
        |       ^
      """
    }

  }

  @Test
  def noJSImportAnnotOnNonJSNative: Unit = {

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @ScalaJSDefined
      @JSImport("foo", JSImport.Namespace)
      $obj A extends js.Object
      """ hasErrors
      s"""
        |newSource1.scala:6: error: Non JS-native classes, traits and objects may not have an @JSImport annotation.
        |      @JSImport("foo", JSImport.Namespace)
        |       ^
      """
    }

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSImport("foo", JSImport.Namespace)
      $obj A
      """ hasErrors
      s"""
        |newSource1.scala:5: error: Non JS-native classes, traits and objects may not have an @JSImport annotation.
        |      @JSImport("foo", JSImport.Namespace)
        |       ^
      """
    }

  }

  @Test
  def noJSNameAnnotOnTrait: Unit = {

    s"""
    @js.native
    @JSName("foo")
    trait A extends js.Object

    object Sym {
      val sym = js.Symbol()
    }

    @js.native
    @JSName(Sym.sym)
    trait B extends js.Object
    """ hasWarns
    s"""
      |newSource1.scala:6: warning: Traits should not have an @JSName annotation, as it does not have any effect. This will be enforced in 1.0.
      |    @JSName("foo")
      |     ^
      |newSource1.scala:14: warning: Traits should not have an @JSName annotation, as it does not have any effect. This will be enforced in 1.0.
      |    @JSName(Sym.sym)
      |     ^
    """

  }

  @Test
  def noJSImportAnnotOnTrait: Unit = {

    s"""
    @js.native
    @JSImport("foo", JSImport.Namespace)
    trait A extends js.Object
    """ hasErrors
    s"""
      |newSource1.scala:6: error: Traits may not have an @JSImport annotation.
      |    @JSImport("foo", JSImport.Namespace)
      |     ^
    """

  }

  @Test def noTwoJSNativeLoadSpecAnnots: Unit = {
    for {
      (firstAnnotName, firstAnnot) <- JSNativeLoadSpecAnnots
      (secondAnnotName, secondAnnot) <- JSNativeLoadSpecAnnots
    } {
      val expectedMessageShort = {
        if (firstAnnotName == "JSName" && secondAnnotName == firstAnnotName)
          "warning: A duplicate @JSName annotation is ignored, and should be removed. This will be enforced in 1.0."
        else if (firstAnnotName == "JSGlobalScope" && secondAnnotName == "JSName")
          "warning: An @JSName annotation is ignored in the presence of @JSGlobalScope (or extends js.GlobalScope), and should be removed. This will be enforced in 1.0."
        else
          "error: Native JS classes and objects can only have one annotation among JSName, JSImport and JSGlobalScope (extending js.GlobalScope is treated as having @JSGlobalScope)."
      }

      val onlyWarn = expectedMessageShort.startsWith("warning: ")

      val expectedMessage = {
        s"""
          |newSource1.scala:7: $expectedMessageShort
          |$secondAnnot
          | ^
        """
      }

      val kinds = {
        if (firstAnnotName == "JSGlobalScope" || secondAnnotName == "JSGlobalScope")
          Seq("object")
        else
          Seq("class", "object")
      }

      for (kind <- kinds) {
        val snippet = {
          s"""
            |@js.native
            |$firstAnnot
            |$secondAnnot
            |$kind A extends js.Object
          """.stripMargin
        }

        if (onlyWarn)
          snippet hasWarns expectedMessage
        else
          snippet hasErrors expectedMessage
      }
    }
  }

  @Test
  def noJSNativeAnnotWithoutJSAny: Unit = {

    """
    @js.native
    class A
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    class A
      |          ^
    """

    """
    @js.native
    trait A
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    trait A
      |          ^
    """

    """
    @js.native
    object A
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    object A
      |           ^
    """

    """
    @js.native
    class A extends Enumeration
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    class A extends Enumeration
      |          ^
    """

    """
    @js.native
    object A extends Enumeration
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    object A extends Enumeration
      |           ^
    """

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
  def noNonSyntheticCompanionInsideNativeJSObject: Unit = {

    // See #1891: The default parameter generates a synthetic companion object
    // The synthetic companion should be allowed, but it may not be expliclit

    """
    @js.native object A extends js.Object {
      @js.native class B(x: Int = ???) extends js.Object
      object B
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS objects cannot contain inner Scala traits, classes or objects (i.e., not extending js.Any)
      |      object B
      |             ^
    """

    """
    @js.native object A extends js.Object {
      @js.native class B(x: Int = ???) extends js.Object
    }
    """.succeeds

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
  def noBadBracketAccess: Unit = {

    """
    @js.native
    class A extends js.Object {
      @js.annotation.JSBracketAccess
      def foo(): Int = js.native

      @js.annotation.JSBracketAccess
      def bar(x: Int, y: Int, z: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @JSBracketAccess methods must have one or two parameters
      |      def foo(): Int = js.native
      |          ^
      |newSource1.scala:11: error: @JSBracketAccess methods must have one or two parameters
      |      def bar(x: Int, y: Int, z: Int): Int = js.native
      |          ^
    """

    """
    @js.native
    class A extends js.Object {
      @js.annotation.JSBracketAccess
      def foo(x: Int, y: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @JSBracketAccess methods with two parameters must return Unit
      |      def foo(x: Int, y: Int): Int = js.native
      |          ^
    """

    """
    @js.native
    class A extends js.Object {
      @js.annotation.JSBracketAccess
      def bar(x: Int*): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @JSBracketAccess methods may not have repeated parameters
      |      def bar(x: Int*): Int = js.native
      |              ^
    """

    """
    @js.native
    class A extends js.Object {
      @js.annotation.JSBracketAccess
      def bar(x: Int = 1): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @JSBracketAccess methods may not have default parameters
      |      def bar(x: Int = 1): Int = js.native
      |              ^
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
    trait A
    @js.native
    class B extends js.Object with A
    """ hasErrors
    """
      |newSource1.scala:7: error: B extends A which does not extend js.Any.
      |    class B extends js.Object with A
      |          ^
    """

    """
    @js.native
    class B extends js.Object with Serializable
    """ hasErrors
    """
      |newSource1.scala:6: error: B extends scala.Serializable which does not extend js.Any.
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
      |newSource1.scala:6: error: Only native JS objects can have an @JSGlobalScope annotation (or extend js.GlobalScope).
      |    class A extends js.GlobalScope
      |          ^
    """

    """
    @js.native
    trait A extends js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:6: error: Traits may not have an @JSGlobalScope annotation.
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
  def noJSNameOnJSGlobalScope: Unit = {
    // #2320
    """
    @js.native
    @JSName("foo")
    object Bar extends js.GlobalScope
    """ containsWarns
    """
      |newSource1.scala:6: warning: An @JSName annotation is ignored in the presence of @JSGlobalScope (or extends js.GlobalScope), and should be removed. This will be enforced in 1.0.
      |    @JSName("foo")
      |     ^
    """

  }

  @Test
  def noJSImportOnJSGlobalScope: Unit = {

    """
    @js.native
    @JSImport("foo", JSImport.Namespace)
    object Bar extends js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS classes and objects can only have one annotation among JSName, JSImport and JSGlobalScope (extending js.GlobalScope is treated as having @JSGlobalScope).
      |    @JSImport("foo", JSImport.Namespace)
      |     ^
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
      |newSource1.scala:7: warning: Native JS classes inside non-native objects should have an @JSName or @JSImport annotation. This will be enforced in 1.0.
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
      |newSource1.scala:7: error: Native JS objects inside non-native objects must have an @JSName or @JSImport annotation
      |      object B extends js.Object
      |             ^
    """

    // From issue #2401
    """
    package object A {
      @js.native
      object B extends js.Object
    }
    """.hasNoWarns

    """
    package object A {
      @js.native
      class B extends js.Object
    }
    """.hasNoWarns

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
      @JSImport("InnerB", JSImport.Namespace)
      @js.native
      class B extends js.Object
      @JSImport("InnerC", JSImport.Namespace)
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
  def nestedJSGlobalScopeWithoutJSName: Unit = {
    // #2319
    """
    object Outer {
      @js.native
      object Foo extends js.GlobalScope
    }
    """.succeeds

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
      |newSource1.scala:14: error: A string argument to JSName must be a literal string
      |      @JSName(A.a)
      |                ^
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
      |newSource1.scala:11: error: A string argument to JSName must be a literal string
      |    @JSName(A.a)
      |              ^
      |newSource1.scala:15: error: A string argument to JSName must be a literal string
      |    @JSName(A.a)
      |              ^
    """

  }

  @Test
  def noNonStaticStableJSNameSymbol: Unit = {

    """
    import js.annotation.JSName

    class A {
      val a = js.Symbol("foo")
    }

    @js.native
    class B extends js.Object {
      @JSName(js.Symbol())
      def foo: Int = js.native
      @JSName(new A().a)
      def bar: Int = js.native
    }

    @ScalaJSDefined
    class C extends js.Object {
      @JSName(js.Symbol())
      def foo: Int = js.native
      @JSName(new A().a)
      def bar: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:13: error: A js.Symbol argument to JSName must be a static, stable identifier
      |      @JSName(js.Symbol())
      |                       ^
      |newSource1.scala:15: error: A js.Symbol argument to JSName must be a static, stable identifier
      |      @JSName(new A().a)
      |                      ^
      |newSource1.scala:21: error: A js.Symbol argument to JSName must be a static, stable identifier
      |      @JSName(js.Symbol())
      |                       ^
      |newSource1.scala:23: error: A js.Symbol argument to JSName must be a static, stable identifier
      |      @JSName(new A().a)
      |                      ^
    """

  }

  @Test
  def noJSImportOnMembers: Unit = {

    """
    @js.native
    class Foo extends js.Object {
      @JSImport("bar1", JSImport.Namespace)
      val bar1: Int = js.native
      @JSImport("bar2", JSImport.Namespace)
      var bar2: Int = js.native
      @JSImport("bar3", JSImport.Namespace)
      def bar3: Int = js.native

      @js.native
      @JSImport("Inner", JSImport.Namespace)
      class Inner extends js.Object

      @js.native
      @JSImport("Inner", JSImport.Namespace)
      object Inner extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Methods and fields cannot be annotated with @JSImport.
      |      val bar1: Int = js.native
      |          ^
      |newSource1.scala:10: error: Methods and fields cannot be annotated with @JSImport.
      |      var bar2: Int = js.native
      |          ^
      |newSource1.scala:12: error: Methods and fields cannot be annotated with @JSImport.
      |      def bar3: Int = js.native
      |          ^
      |newSource1.scala:16: error: Native JS traits and classes may not have inner traits, classes or objects
      |      class Inner extends js.Object
      |            ^
      |newSource1.scala:20: error: Native JS traits and classes may not have inner traits, classes or objects
      |      object Inner extends js.Object
      |             ^
    """

  }

  @Test
  def noNonLiteralJSImport: Unit = {

    """
    object A {
      val a = "Hello"
    }

    @JSImport(A.a, JSImport.Namespace)
    @js.native
    object B1 extends js.Object

    @JSImport(A.a, "B2")
    @js.native
    object B2 extends js.Object

    @JSImport("B3", A.a)
    @js.native
    object B3 extends js.Object

    @JSImport(A.a, JSImport.Namespace)
    @js.native
    object C1 extends js.Object

    @JSImport(A.a, "C2")
    @js.native
    object C2 extends js.Object

    @JSImport("C3", A.a)
    @js.native
    object C3 extends js.Object

    @JSImport(A.a, A.a)
    @js.native
    object D extends js.Object
    """ hasErrors
    """
      |newSource1.scala:9: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, JSImport.Namespace)
      |                ^
      |newSource1.scala:13: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, "B2")
      |                ^
      |newSource1.scala:17: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport("B3", A.a)
      |                      ^
      |newSource1.scala:21: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, JSImport.Namespace)
      |                ^
      |newSource1.scala:25: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, "C2")
      |                ^
      |newSource1.scala:29: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport("C3", A.a)
      |                      ^
      |newSource1.scala:33: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, A.a)
      |                ^
      |newSource1.scala:33: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport(A.a, A.a)
      |                     ^
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

  @Test
  def noJSSymbolNameOnTopLevelClassesAndObjects: Unit = {
    for {
      kind <- Seq("class", "object")
    } {
      s"""
      object Sym {
        val sym = js.Symbol()
      }

      @JSName(Sym.sym)
      @js.native
      $kind A extends js.Object
      """ hasErrors
      """
        |newSource1.scala:9: error: @JSName with a js.Symbol can only be used on members of JavaScript types
        |      @JSName(Sym.sym)
        |       ^
      """
    }
  }

  @Test
  def noJSSymbolNameOnNestedNativeClassesAndObjects: Unit = {
    for {
      kind <- Seq("class", "object")
    } {
      s"""
      object Sym {
        val sym = js.Symbol()
      }

      @js.native
      object Enclosing extends js.Object {
        @JSName(Sym.sym)
        @js.native
        $kind A extends js.Object
      }
      """ hasErrors
      """
        |newSource1.scala:11: error: Implementation restriction: @JSName with a js.Symbol is not supported on nested native classes and objects
        |        @JSName(Sym.sym)
        |         ^
      """
    }
  }

  @Test
  def warnOnDuplicateJSNameAnnotOnMember: Unit = {
    for {
      kind <- Seq("class", "object", "trait")
    } {
      """
      object A {
        val a = js.Symbol()
      }

      @js.native
      class A extends js.Object {
        @JSName(A.a)
        @JSName("foo")
        def a: Int = js.native
      }
      """ hasWarns
      """
        |newSource1.scala:12: warning: A duplicate @JSName annotation is ignored. This will become an error in 1.0.0.
        |        @JSName("foo")
        |         ^
      """
    }
  }

  @Test
  def scalaJSDefinedJSNameOverrideWarnings: Unit = {
    """
    @ScalaJSDefined
    abstract class A extends js.Object {
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      override def bar() = 1
    }
    """.hasNoWarns

    """
    @ScalaJSDefined
    trait A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      @JSName("foo")
      override def bar() = 1
    }
    """.hasNoWarns

    """
    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      @JSName("foo")
      override def bar() = 1
    }
    """.hasNoWarns

    """
    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      @JSName("baz")
      override def bar() = 1
    }
    """ hasWarns
    """
      |newSource1.scala:13: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): Int in class B with JSName 'baz'
      |    is conflicting with
      |def bar(): Int in class A with JSName 'foo'
      |
      |      override def bar() = 1
      |                   ^
    """

    """
    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      override def bar() = 1
    }
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): Int in class B with JSName 'bar'
      |    is conflicting with
      |def bar(): Int in class A with JSName 'foo'
      |
      |      override def bar() = 1
      |                   ^
    """

    """
    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Object
    }
    @ScalaJSDefined
    abstract class B extends A {
      override def bar(): String
    }
    @ScalaJSDefined
    class C extends B {
      override def bar() = "1"
    }
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class B with JSName 'bar'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'foo'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class C with JSName 'bar'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'foo'
      |
      |      override def bar() = "1"
      |                   ^
    """

    """
    @ScalaJSDefined
    abstract class A extends js.Object {
      def bar(): Object
    }
    @ScalaJSDefined
    abstract class B extends A {
      @JSName("foo")
      override def bar(): String
    }
    @ScalaJSDefined
    class C extends B {
      override def bar() = "1"
    }
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class B with JSName 'foo'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'bar'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class C with JSName 'bar'
      |    is conflicting with
      |override def bar(): String in class B with JSName 'foo'
      |
      |      override def bar() = "1"
      |                   ^
    """

    """
    @ScalaJSDefined
    class A extends js.Object {
      def foo: Int = 5
    }
    @ScalaJSDefined
    trait B extends A {
      @JSName("bar")
      def foo: Int
    }
    @ScalaJSDefined
    class C extends B
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in class A with JSName 'foo'
      |    is conflicting with
      |def foo: Int in trait B with JSName 'bar'
      |
      |      def foo: Int
      |          ^
    """

    """
    @ScalaJSDefined
    class A extends js.Object {
      @JSName("bar")
      def foo: Int = 5
    }
    @ScalaJSDefined
    trait B extends A {
      def foo: Int
    }
    @ScalaJSDefined
    class C extends B
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in class A with JSName 'bar'
      |    is conflicting with
      |def foo: Int in trait B with JSName 'foo'
      |
      |      def foo: Int
      |          ^
    """

    """
    @ScalaJSDefined
    class A[T] extends js.Object {
      @JSName("bar")
      def foo(x: T): T = x
    }
    @ScalaJSDefined
    class B extends A[Int] {
      override def foo(x: Int): Int = x
    }
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class B with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A with JSName 'bar'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    @ScalaJSDefined
    trait A[T] extends js.Object {
      @JSName("bar")
      def foo(x: T): T
    }
    @ScalaJSDefined
    class B extends A[Int] {
      override def foo(x: Int): Int = x
    }
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class B with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait A with JSName 'bar'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    @ScalaJSDefined
    class A[T] extends js.Object {
      @JSName("bar")
      def foo(x: T): T = x
    }
    @ScalaJSDefined
    trait B extends A[Int] {
      def foo(x: Int): Int
    }
    @ScalaJSDefined
    class C extends B {
      override def foo(x: Int): Int = x
    }
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo(x: Int): Int in class A with JSName 'bar'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'foo'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class C with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A with JSName 'bar'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    @ScalaJSDefined
    class A[T] extends js.Object {
      def foo(x: T): T = x
    }
    @ScalaJSDefined
    trait B extends A[Int] {
      @JSName("bar")
      def foo(x: Int): Int
    }
    @ScalaJSDefined
    class C extends B {
      override def foo(x: Int): Int = x
    }
    """ hasWarns
    """
      |newSource1.scala:12: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo(x: Int): Int in class A with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'bar'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class C with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'bar'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    @ScalaJSDefined
    trait A extends js.Object {
      def foo: Int
    }
    @ScalaJSDefined
    trait B extends js.Object {
      @JSName("bar")
      def foo: Int
    }
    @ScalaJSDefined
    trait C extends A with B
    """ hasWarns
    """
      |newSource1.scala:15: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in trait B with JSName 'bar'
      |    is conflicting with
      |def foo: Int in trait A with JSName 'foo'
      |
      |    trait C extends A with B
      |          ^
    """

    """
    @ScalaJSDefined
    trait A extends js.Object {
      def foo: Int
    }
    @ScalaJSDefined
    trait B extends js.Object {
      @JSName("bar")
      def foo: Int
    }
    @ScalaJSDefined
    abstract class C extends A with B
    """ hasWarns
    """
      |newSource1.scala:15: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in trait B with JSName 'bar'
      |    is conflicting with
      |def foo: Int in trait A with JSName 'foo'
      |
      |    abstract class C extends A with B
      |                   ^
    """
  }

  @Test
  def scalaJSDefinedJSNameWithSymbolOverrideWarnings: Unit = {
    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    trait A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      @JSName(Syms.sym1)
      override def bar() = 1
    }
    """.hasNoWarns

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      @JSName(Syms.sym1)
      override def bar() = 1
    }
    """.hasNoWarns

    """
    object Syms {
      val sym1 = js.Symbol()
      val sym2 = js.Symbol()
    }

    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      @JSName(Syms.sym2)
      override def bar() = 1
    }
    """ hasWarns
    """
      |newSource1.scala:18: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): Int in class B with JSName 'Syms.sym2'
      |    is conflicting with
      |def bar(): Int in class A with JSName 'Syms.sym1'
      |
      |      override def bar() = 1
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      @JSName("baz")
      override def bar() = 1
    }
    """ hasWarns
    """
      |newSource1.scala:17: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): Int in class B with JSName 'baz'
      |    is conflicting with
      |def bar(): Int in class A with JSName 'Syms.sym1'
      |
      |      override def bar() = 1
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      @JSName(Syms.sym1)
      override def bar() = 1
    }
    """ hasWarns
    """
      |newSource1.scala:17: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): Int in class B with JSName 'Syms.sym1'
      |    is conflicting with
      |def bar(): Int in class A with JSName 'foo'
      |
      |      override def bar() = 1
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    @ScalaJSDefined
    class B extends A {
      override def bar() = 1
    }
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): Int in class B with JSName 'bar'
      |    is conflicting with
      |def bar(): Int in class A with JSName 'Syms.sym1'
      |
      |      override def bar() = 1
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Object
    }
    @ScalaJSDefined
    abstract class B extends A {
      override def bar(): String
    }
    @ScalaJSDefined
    class C extends B {
      override def bar() = "1"
    }
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class B with JSName 'bar'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'Syms.sym1'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:20: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class C with JSName 'bar'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'Syms.sym1'
      |
      |      override def bar() = "1"
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    abstract class A extends js.Object {
      def bar(): Object
    }
    @ScalaJSDefined
    abstract class B extends A {
      @JSName(Syms.sym1)
      override def bar(): String
    }
    @ScalaJSDefined
    class C extends B {
      override def bar() = "1"
    }
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class B with JSName 'Syms.sym1'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'bar'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:20: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class C with JSName 'bar'
      |    is conflicting with
      |override def bar(): String in class B with JSName 'Syms.sym1'
      |
      |      override def bar() = "1"
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    class A extends js.Object {
      def foo: Int = 5
    }
    @ScalaJSDefined
    trait B extends A {
      @JSName(Syms.sym1)
      def foo: Int
    }
    @ScalaJSDefined
    class C extends B
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in class A with JSName 'foo'
      |    is conflicting with
      |def foo: Int in trait B with JSName 'Syms.sym1'
      |
      |      def foo: Int
      |          ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    class A extends js.Object {
      @JSName(Syms.sym1)
      def foo: Int = 5
    }
    @ScalaJSDefined
    trait B extends A {
      def foo: Int
    }
    @ScalaJSDefined
    class C extends B
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in class A with JSName 'Syms.sym1'
      |    is conflicting with
      |def foo: Int in trait B with JSName 'foo'
      |
      |      def foo: Int
      |          ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    class A[T] extends js.Object {
      @JSName(Syms.sym1)
      def foo(x: T): T = x
    }
    @ScalaJSDefined
    class B extends A[Int] {
      override def foo(x: Int): Int = x
    }
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class B with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A with JSName 'Syms.sym1'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    trait A[T] extends js.Object {
      @JSName(Syms.sym1)
      def foo(x: T): T
    }
    @ScalaJSDefined
    class B extends A[Int] {
      override def foo(x: Int): Int = x
    }
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class B with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait A with JSName 'Syms.sym1'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    class A[T] extends js.Object {
      @JSName(Syms.sym1)
      def foo(x: T): T = x
    }
    @ScalaJSDefined
    trait B extends A[Int] {
      def foo(x: Int): Int
    }
    @ScalaJSDefined
    class C extends B {
      override def foo(x: Int): Int = x
    }
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo(x: Int): Int in class A with JSName 'Syms.sym1'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'foo'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:20: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class C with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A with JSName 'Syms.sym1'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    class A[T] extends js.Object {
      def foo(x: T): T = x
    }
    @ScalaJSDefined
    trait B extends A[Int] {
      @JSName(Syms.sym1)
      def foo(x: Int): Int
    }
    @ScalaJSDefined
    class C extends B {
      override def foo(x: Int): Int = x
    }
    """ hasWarns
    """
      |newSource1.scala:16: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo(x: Int): Int in class A with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'Syms.sym1'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:20: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class C with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'Syms.sym1'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    trait A extends js.Object {
      def foo: Int
    }
    @ScalaJSDefined
    trait B extends js.Object {
      @JSName(Syms.sym1)
      def foo: Int
    }
    @ScalaJSDefined
    trait C extends A with B
    """ hasWarns
    """
      |newSource1.scala:19: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in trait B with JSName 'Syms.sym1'
      |    is conflicting with
      |def foo: Int in trait A with JSName 'foo'
      |
      |    trait C extends A with B
      |          ^
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    @ScalaJSDefined
    trait A extends js.Object {
      def foo: Int
    }
    @ScalaJSDefined
    trait B extends js.Object {
      @JSName(Syms.sym1)
      def foo: Int
    }
    @ScalaJSDefined
    abstract class C extends A with B
    """ hasWarns
    """
      |newSource1.scala:19: warning: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in trait B with JSName 'Syms.sym1'
      |    is conflicting with
      |def foo: Int in trait A with JSName 'foo'
      |
      |    abstract class C extends A with B
      |                   ^
    """
  }

  @Test
  def noDefaultConstructorArgsIfModuleIsJSNative: Unit = {
    """
    @ScalaJSDefined
    class A(x: Int = 1) extends js.Object
    @js.native
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Implementation restriction: constructors of Scala.js-defined JS classes cannot have default parameters if their companion module is JS native.
      |    class A(x: Int = 1) extends js.Object
      |          ^
    """

    """
    class A(x: Int = 1)
    @js.native
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: Implementation restriction: constructors of Scala classes cannot have default parameters if their companion module is JS native.
      |    class A(x: Int = 1)
      |          ^
    """
  }

  @Test // #2547
  def noDefaultOverrideCrash: Unit = {
    """
    @js.native
    class NativeBase extends js.Object {
        def add(option: js.Any = js.native): js.Any = js.native
    }
    @ScalaJSDefined
    class Derived extends NativeBase {
        override def add(option: js.Any): js.Any = super.add(option)
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: When overriding a native method with default arguments, the overriding method must explicitly repeat the default arguments.
      |        override def add(option: js.Any): js.Any = super.add(option)
      |                         ^
    """

    """
    @js.native
    trait NativeTrait extends js.Object {
        def add(option: js.Any = js.native): js.Any = js.native
    }

    @js.native
    class NativeBase extends NativeTrait

    @ScalaJSDefined
    class Derived extends NativeBase {
        override def add(option: js.Any): js.Any = super.add(option)
    }
    """ hasErrors
    """
      |newSource1.scala:15: error: When overriding a native method with default arguments, the overriding method must explicitly repeat the default arguments.
      |        override def add(option: js.Any): js.Any = super.add(option)
      |                         ^
    """
  }
}
