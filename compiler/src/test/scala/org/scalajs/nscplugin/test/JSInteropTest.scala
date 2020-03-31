/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._

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
      "JSGlobal" -> "@JSGlobal",
      "JSGlobal" -> "@JSGlobal(\"foo\")",
      "JSImport" -> "@JSImport(\"foo\", \"bar\")",
      "JSImport" -> "@JSImport(\"foo\", \"bar\", globalFallback = \"baz\")",
      "JSGlobalScope" -> "@JSGlobalScope"
  )

  private def ifHasNewRefChecks(msg: String): String = {
    val version = scala.util.Properties.versionNumberString
    if (version.startsWith("2.11.") ||
        version.startsWith("2.12.")) {
      ""
    } else {
      msg.stripMargin.trim()
    }
  }

  @Test
  def warnJSPackageObjectDeprecated: Unit = {

    s"""
    package object jspackage extends js.Object
    """ hasErrors
    s"""
      |newSource1.scala:5: error: Package objects may not extend js.Any.
      |    package object jspackage extends js.Object
      |                   ^
    """

  }

  @Test
  def noJSNameAnnotOnNonJSNative: Unit = {

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSName("foo")
      $obj A extends js.Object

      object Sym {
        val sym = js.Symbol()
      }

      @JSName(Sym.sym)
      $obj B extends js.Object
      """ hasErrors
      s"""
        |newSource1.scala:5: error: Non JS-native classes, traits and objects may not have an @JSName annotation.
        |      @JSName("foo")
        |       ^
        |newSource1.scala:12: error: Non JS-native classes, traits and objects may not have an @JSName annotation.
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
      """ hasErrors
      s"""
        |newSource1.scala:5: error: Non JS-native classes, traits and objects may not have an @JSName annotation.
        |      @JSName("foo")
        |       ^
        |newSource1.scala:12: error: Non JS-native classes, traits and objects may not have an @JSName annotation.
        |      @JSName(Sym.sym)
        |       ^
      """
    }

  }

  @Test
  def okJSNameOnNestedObjects: Unit = {

    """
    class A extends js.Object {
      @JSName("foo")
      object toto

      @JSName("bar")
      object tata extends js.Object
    }
    """.hasNoWarns

    """
    class A extends js.Object {
      @JSName("foo")
      private object toto

      @JSName("bar")
      private object tata extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Non JS-native classes, traits and objects may not have an @JSName annotation.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:9: error: Non JS-native classes, traits and objects may not have an @JSName annotation.
      |      @JSName("bar")
      |       ^
    """

  }

  @Test
  def noJSGlobalAnnotOnNonJSNative: Unit = {

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSGlobal
      $obj A extends js.Object

      @JSGlobal("Foo")
      $obj B extends js.Object
      """ hasErrors
      s"""
        |newSource1.scala:5: error: Non JS-native classes, traits and objects may not have an @JSGlobal annotation.
        |      @JSGlobal
        |       ^
        |newSource1.scala:8: error: Non JS-native classes, traits and objects may not have an @JSGlobal annotation.
        |      @JSGlobal("Foo")
        |       ^
      """
    }

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSGlobal
      $obj A

      @JSGlobal("Foo")
      $obj B
      """ hasErrors
      s"""
        |newSource1.scala:5: error: Non JS-native classes, traits and objects may not have an @JSGlobal annotation.
        |      @JSGlobal
        |       ^
        |newSource1.scala:8: error: Non JS-native classes, traits and objects may not have an @JSGlobal annotation.
        |      @JSGlobal("Foo")
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
      @JSImport("foo", JSImport.Namespace)
      $obj A extends js.Object
      """ hasErrors
      s"""
        |newSource1.scala:5: error: Non JS-native classes, traits and objects may not have an @JSImport annotation.
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

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSImport("foo", JSImport.Namespace, globalFallback = "Foo")
      $obj A extends js.Object
      """ hasErrors
      s"""
        |newSource1.scala:5: error: Non JS-native classes, traits and objects may not have an @JSImport annotation.
        |      @JSImport("foo", JSImport.Namespace, globalFallback = "Foo")
        |       ^
      """
    }

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSImport("foo", JSImport.Namespace, globalFallback = "Foo")
      $obj A
      """ hasErrors
      s"""
        |newSource1.scala:5: error: Non JS-native classes, traits and objects may not have an @JSImport annotation.
        |      @JSImport("foo", JSImport.Namespace, globalFallback = "Foo")
        |       ^
      """
    }

  }

  @Test
  def noJSGlobalScopeAnnotOnNonJSNative: Unit = {

    """
    @JSGlobalScope
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: Only native JS objects can have an @JSGlobalScope annotation.
      |    @JSGlobalScope
      |     ^
    """

    """
    @JSGlobalScope
    object A
    """ hasErrors
    """
      |newSource1.scala:5: error: Only native JS objects can have an @JSGlobalScope annotation.
      |    @JSGlobalScope
      |     ^
    """

  }
  @Test
  def noJSNameAnnotOnClass: Unit = {
    """
    @js.native
    @JSName("Foo")
    class A extends js.Object

    @js.native
    @JSName("Foo")
    abstract class B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSName annotations are not allowed on top level classes or objects (or classes and objects inside Scala objects).
      |    @JSName("Foo")
      |     ^
      |newSource1.scala:7: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |    class A extends js.Object
      |          ^
      |newSource1.scala:10: error: @JSName annotations are not allowed on top level classes or objects (or classes and objects inside Scala objects).
      |    @JSName("Foo")
      |     ^
      |newSource1.scala:11: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |    abstract class B extends js.Object
      |                   ^
    """
  }

  @Test
  def noJSNameAnnotOnObject: Unit = {
    """
    @js.native
    @JSName("Foo")
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSName annotations are not allowed on top level classes or objects (or classes and objects inside Scala objects).
      |    @JSName("Foo")
      |     ^
      |newSource1.scala:7: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |    object A extends js.Object
      |           ^
    """
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
    """ hasErrors
    s"""
      |newSource1.scala:6: error: Traits may not have an @JSName annotation.
      |    @JSName("foo")
      |     ^
      |newSource1.scala:14: error: Traits may not have an @JSName annotation.
      |    @JSName(Sym.sym)
      |     ^
    """

  }

  @Test
  def noJSGlobalAnnotOnTrait: Unit = {

    s"""
    @js.native
    @JSGlobal
    trait A extends js.Object
    """ hasErrors
    s"""
      |newSource1.scala:6: error: Traits may not have an @JSGlobal annotation.
      |    @JSGlobal
      |     ^
    """

    s"""
    @js.native
    @JSGlobal("Foo")
    trait A extends js.Object
    """ hasErrors
    s"""
      |newSource1.scala:6: error: Traits may not have an @JSGlobal annotation.
      |    @JSGlobal("Foo")
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

    s"""
    @js.native
    @JSImport("foo", JSImport.Namespace, globalFallback = "Foo")
    trait A extends js.Object
    """ hasErrors
    s"""
      |newSource1.scala:6: error: Traits may not have an @JSImport annotation.
      |    @JSImport("foo", JSImport.Namespace, globalFallback = "Foo")
      |     ^
    """

  }

  @Test def noTwoJSNativeLoadSpecAnnots: Unit = {
    for {
      (firstAnnotName, firstAnnot) <- JSNativeLoadSpecAnnots
      (secondAnnotName, secondAnnot) <- JSNativeLoadSpecAnnots
    } {
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

        snippet hasErrors s"""
          |newSource1.scala:7: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
          |$secondAnnot
          | ^
        """
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
  def noInnerScalaClassTraitObjectInJSNative: Unit = {

    for {
      outer <- Seq("class", "trait")
      inner <- Seq("class", "trait", "object")
    } yield {
      val jsGlobalAnnot =
        if (outer == "trait") ""
        else "@JSGlobal"
      s"""
      @js.native $jsGlobalAnnot
      $outer A extends js.Object {
        $inner A
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: Native JS traits, classes and objects cannot contain inner Scala traits, classes or objects (i.e., not extending js.Any)
        |        $inner A
        |        ${" " * inner.length} ^
      """
    }

  }

  @Test
  def noInnerNonNativeJSClassTraitObjectInJSNative: Unit = {

    for {
      outer <- Seq("class", "trait")
      inner <- Seq("class", "trait", "object")
    } yield {
      val jsGlobalAnnot =
        if (outer == "trait") ""
        else "@JSGlobal"
      s"""
      @js.native $jsGlobalAnnot
      $outer A extends js.Object {
        $inner A extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: Native JS classes and traits cannot contain non-native JS classes, traits or objects
        |        $inner A extends js.Object
        |        ${" " * inner.length} ^
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
      @JSGlobal
      object A extends js.Object {
        $inner A
      }
      """ hasErrors
      s"""
        |newSource1.scala:8: error: Native JS traits, classes and objects cannot contain inner Scala traits, classes or objects (i.e., not extending js.Any)
        |        $inner A
        |        ${" " * inner.length} ^
      """
    }

  }

  @Test
  def noNonSyntheticCompanionInsideNativeJSObject: Unit = {

    // See #1891: The default parameter generates a synthetic companion object
    // The synthetic companion should be allowed, but it may not be explicit

    """
    @js.native @JSGlobal object A extends js.Object {
      @js.native class B(x: Int = ???) extends js.Object
      object B
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS traits, classes and objects cannot contain inner Scala traits, classes or objects (i.e., not extending js.Any)
      |      object B
      |             ^
    """

    """
    @js.native @JSGlobal object A extends js.Object {
      @js.native class B(x: Int = ???) extends js.Object
    }
    """.succeeds

  }

  @Test
  def noNonNativeJSTypesInsideNativeJSObject: Unit = {

    for {
      inner <- Seq("class", "object")
    } yield {
      s"""
      @js.native
      @JSGlobal
      object A extends js.Object {
        $inner A extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:8: error: Native JS objects cannot contain inner non-native JS classes or objects
        |        $inner A extends js.Object
        |        ${" " * inner.length} ^
      """
    }

  }

  @Test
  def noBadSetters: Unit = {

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      def foo_=(x: Int): Int = js.native
      def bar_=(x: Int, y: Int): Unit = js.native
      def goo_=(x: Int*): Unit = js.native
      def hoo_=(x: Int = 1): Unit = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: JS setters must return Unit
      |      def foo_=(x: Int): Int = js.native
      |          ^
      |newSource1.scala:9: error: JS setters must have exactly one argument
      |      def bar_=(x: Int, y: Int): Unit = js.native
      |          ^
      |newSource1.scala:10: error: JS setters may not have repeated params
      |      def goo_=(x: Int*): Unit = js.native
      |          ^
      |newSource1.scala:11: error: JS setters may not have default params
      |      def hoo_=(x: Int = 1): Unit = js.native
      |          ^
    """

  }

  @Test
  def noBadBracketAccess: Unit = {

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @js.annotation.JSBracketAccess
      def foo(): Int = js.native

      @js.annotation.JSBracketAccess
      def bar(x: Int, y: Int, z: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSBracketAccess methods must have one or two parameters
      |      def foo(): Int = js.native
      |          ^
      |newSource1.scala:12: error: @JSBracketAccess methods must have one or two parameters
      |      def bar(x: Int, y: Int, z: Int): Int = js.native
      |          ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @js.annotation.JSBracketAccess
      def foo(x: Int, y: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSBracketAccess methods with two parameters must return Unit
      |      def foo(x: Int, y: Int): Int = js.native
      |          ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @js.annotation.JSBracketAccess
      def bar(x: Int*): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSBracketAccess methods may not have repeated parameters
      |      def bar(x: Int*): Int = js.native
      |              ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @js.annotation.JSBracketAccess
      def bar(x: Int = 1): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSBracketAccess methods may not have default parameters
      |      def bar(x: Int = 1): Int = js.native
      |              ^
    """

  }

  @Test
  def noBadBracketCall: Unit = {

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @js.annotation.JSBracketCall
      def foo(): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSBracketCall methods must have at least one non-repeated parameter
      |      def foo(): Int = js.native
      |          ^
    """

  }

  @Test
  def onlyJSTraits: Unit = {

    """
    trait A

    @js.native
    @JSGlobal
    class B extends js.Object with A
    """ hasErrors
    """
      |newSource1.scala:9: error: B extends A which does not extend js.Any.
      |    class B extends js.Object with A
      |          ^
    """

    """
    @js.native
    @JSGlobal
    class B extends js.Object with java.io.Serializable
    """ hasErrors
    """
      |newSource1.scala:7: error: B extends java.io.Serializable which does not extend js.Any.
      |    class B extends js.Object with java.io.Serializable
      |          ^
    """

  }

  @Test
  def noCaseClassObject: Unit = {

    """
    @js.native
    @JSGlobal
    case class A(x: Int) extends js.Object
    """ hasErrors
    """
      |newSource1.scala:7: error: Classes and objects extending js.Any may not have a case modifier
      |    case class A(x: Int) extends js.Object
      |               ^
    """

    """
    @js.native
    @JSGlobal
    case object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:7: error: Classes and objects extending js.Any may not have a case modifier
      |    case object B extends js.Object
      |                ^
    """

    """
    case class A(x: Int) extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: Classes and objects extending js.Any may not have a case modifier
      |    case class A(x: Int) extends js.Object
      |               ^
    """

    """
    case object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: Classes and objects extending js.Any may not have a case modifier
      |    case object B extends js.Object
      |                ^
    """

  }

  @Test
  def noNativeJSNestedInScalaClassTrait: Unit = {

    val outers = List("class", "trait")
    val inners = List("trait", "class", "object")

    for {
      outer <- outers
      inner <- inners
    } yield {
      val jsGlobalAnnot =
        if (inner == "trait") ""
        else "@JSGlobal"

      val errTrg = if (inner == "object") "objects" else "classes"

      s"""
      $outer A {
        @js.native $jsGlobalAnnot
        $inner Inner extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: Scala traits and classes may not have inner native JS traits, classes or objects
        |        $inner Inner extends js.Object
        |         ${" " * inner.length}^
      """
    }

  }

  @Test
  def noNativeJSNestedInNonNativeJS: Unit = {

    val outers = List("class", "trait", "object")
    val inners = List("class", "trait", "object")

    for {
      outer <- outers
      inner <- inners
    } yield {
      val jsGlobalAnnot =
        if (inner == "trait") ""
        else "@JSGlobal"

      val errTrg = if (inner == "object") "objects" else "classes"

      s"""
      $outer A extends js.Object {
        @js.native $jsGlobalAnnot
        $inner Inner extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: non-native JS classes, traits and objects may not have inner native JS classes, traits or objects
        |        $inner Inner extends js.Object
        |         ${" " * inner.length}^
      """
    }

  }

  @Test
  def noLocalClass: Unit = {

    """
    object A {
      def a = {
        @js.native
        @JSGlobal
        class B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: Local native JS classes and objects are not allowed
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
        @JSGlobal
        object B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: Local native JS classes and objects are not allowed
      |        object B extends js.Object
      |               ^
    """

  }

  @Test
  def noNativeInJSAny: Unit = {

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @native
      def value: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: Methods in a js.Any may not be @native
      |      def value: Int = js.native
      |          ^
    """

  }

  @Test
  def checkJSAnyBody: Unit = {

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      def value: Int = ???
      val x: Int = ???
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Concrete members of JS native types may only call js.native.
      |      def value: Int = ???
      |                       ^
      |newSource1.scala:9: error: Concrete members of JS native types may only call js.native.
      |      val x: Int = ???
      |                   ^
    """

  }

  @Test
  def noWarnJSAnyDeferred: Unit = {

    """
    @js.native
    @JSGlobal
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
    @JSGlobal
    class A(x: Int, y: Int) extends js.Object {
      def this(x: Int) = this(x, 5)
      def this() = this(7)
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: A secondary constructor of a class extending js.Any may only call the primary constructor
      |      def this() = this(7)
      |          ^
    """

  }

  @Test
  def noPrivateMemberInNative: Unit = {

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      private[this] val a: Int = js.native
      private val b: Int = js.native
      private[A] val c: Int = js.native

      private[this] var d: Int = js.native
      private var e: Int = js.native
      private[A] var f: Int = js.native

      private[this] def g(): Int = js.native
      private def h(): Int = js.native
      private[A] def i(): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private[this] val a: Int = js.native
      |                        ^
      |newSource1.scala:9: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private val b: Int = js.native
      |                  ^
      |newSource1.scala:10: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private[A] val c: Int = js.native
      |                     ^
      |newSource1.scala:12: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private[this] var d: Int = js.native
      |                        ^
      |newSource1.scala:13: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private var e: Int = js.native
      |                  ^
      |newSource1.scala:14: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private[A] var f: Int = js.native
      |                     ^
      |newSource1.scala:16: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private[this] def g(): Int = js.native
      |                        ^
      |newSource1.scala:17: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private def h(): Int = js.native
      |                  ^
      |newSource1.scala:18: error: Native JS classes may not have private members. Use a public member in a private facade instead.
      |      private[A] def i(): Int = js.native
      |                     ^
    """

  }

  @Test
  def noPrivateConstructorInNative: Unit = {

    """
    @js.native
    @JSGlobal
    class A private () extends js.Object
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS classes may not have private constructors. Use `private[this]` to declare an internal constructor.
      |    class A private () extends js.Object
      |            ^
    """

    """
    @js.native
    @JSGlobal
    class A private[A] () extends js.Object
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS classes may not have private constructors. Use `private[this]` to declare an internal constructor.
      |    class A private[A] () extends js.Object
      |            ^
    """

    """
    @js.native
    @JSGlobal
    class A private[this] () extends js.Object
    """.hasNoWarns

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
  def warnNothingInNativeJS: Unit = {

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      def foo = js.native
      val bar = js.native
    }
    """ hasWarns
    """
      |newSource1.scala:8: warning: The type of foo got inferred as Nothing. To suppress this warning, explicitly ascribe the type.
      |      def foo = js.native
      |          ^
      |newSource1.scala:9: warning: The type of bar got inferred as Nothing. To suppress this warning, explicitly ascribe the type.
      |      val bar = js.native
      |          ^
    """

  }

  @Test
  def nativeClassMustHaveLoadingSpec: Unit = {
    """
    @js.native
    class A extends js.Object

    @js.native
    abstract class B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |    class A extends js.Object
      |          ^
      |newSource1.scala:9: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |    abstract class B extends js.Object
      |                   ^
    """
  }

  @Test
  def nativeObjectMustHaveLoadingSpec: Unit = {
    """
    @js.native
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |    object A extends js.Object
      |           ^
    """
  }

  @Test
  def noNativeClassObjectWithoutExplicitNameInsideScalaObject: Unit = {

    """
    object A {
      @js.native
      class B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
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
      |newSource1.scala:7: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |      object B extends js.Object
      |             ^
    """

    """
    object A {
      @js.native
      @JSGlobal
      class B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS classes and objects inside non-native objects must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    """
    object A {
      @js.native
      @JSGlobal
      object B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS classes and objects inside non-native objects must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    // From issue #2401
    """
    package object A {
      @js.native
      object B extends js.Object

      @js.native
      @JSGlobal
      object C extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |      object B extends js.Object
      |             ^
    """

    """
    package object A {
      @js.native
      class B extends js.Object

      @js.native
      @JSGlobal
      class C extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |      class B extends js.Object
      |            ^
    """

    """
    object A {
      @JSName("InnerB")
      @js.native
      class B extends js.Object

      @JSName("InnerC")
      @js.native
      abstract class C extends js.Object

      @JSName("InnerD")
      @js.native
      object D extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSName annotations are not allowed on top level classes or objects (or classes and objects inside Scala objects).
      |      @JSName("InnerB")
      |       ^
      |newSource1.scala:8: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |      class B extends js.Object
      |            ^
      |newSource1.scala:10: error: @JSName annotations are not allowed on top level classes or objects (or classes and objects inside Scala objects).
      |      @JSName("InnerC")
      |       ^
      |newSource1.scala:12: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |      abstract class C extends js.Object
      |                     ^
      |newSource1.scala:14: error: @JSName annotations are not allowed on top level classes or objects (or classes and objects inside Scala objects).
      |      @JSName("InnerD")
      |       ^
      |newSource1.scala:16: error: Native JS classes and objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |      object D extends js.Object
      |             ^
    """

    """
    object A {
      @JSGlobal("InnerB")
      @js.native
      class B extends js.Object

      @JSGlobal("InnerC")
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
      @JSImport("InnerB", JSImport.Namespace, globalFallback = "Foo")
      @js.native
      class B extends js.Object

      @JSImport("InnerC", JSImport.Namespace, globalFallback = "Foo")
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
    @JSGlobal
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
  def noNonLiteralJSName: Unit = {

    """
    import js.annotation.JSName

    object A {
      val a = "Hello"
      final val b = "World"
    }

    @js.native
    @JSGlobal
    class B extends js.Object {
      @JSName(A.a)
      def foo: Int = js.native
      @JSName(A.b)
      def bar: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:15: error: A string argument to JSName must be a literal string
      |      @JSName(A.a)
      |                ^
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
    @JSGlobal
    class B extends js.Object {
      @JSName(js.Symbol())
      def foo: Int = js.native
      @JSName(new A().a)
      def bar: Int = js.native
    }

    class C extends js.Object {
      @JSName(js.Symbol())
      def foo: Int = js.native
      @JSName(new A().a)
      def bar: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A js.Symbol argument to JSName must be a static, stable identifier
      |      @JSName(js.Symbol())
      |                       ^
      |newSource1.scala:16: error: A js.Symbol argument to JSName must be a static, stable identifier
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
  def noSelfReferenceJSNameSymbol: Unit = {

    """
    object A extends js.Object {
      val a = js.Symbol("foo")

      @JSName(a)
      def foo: Int = 1
    }
    """ hasWarns
    """
      |newSource1.scala:8: warning: This symbol is defined in the same object as the annotation's target. This will cause a stackoverflow at runtime
      |      @JSName(a)
      |              ^
    """

    // Native objects are OK, since we do not control definition order.
    """
    @JSGlobal
    @js.native
    object A extends js.Object {
      val a: js.Symbol = js.native

      @JSName(a)
      def foo: Int = js.native
    }
    """.succeeds

  }

  @Test
  def noJSGlobalOnMembersOfClassesAndTraits: Unit = {

    for (outer <- Seq("class", "trait")) {
      s"""
      @js.native ${if (outer == "trait") "" else "@JSGlobal"}
      $outer Foo extends js.Object {
        @JSGlobal("bar1")
        val bar1: Int = js.native
        @JSGlobal("bar2")
        var bar2: Int = js.native
        @JSGlobal("bar3")
        def bar3: Int = js.native

        @js.native
        @JSGlobal("Inner")
        class Inner extends js.Object

        @js.native
        @JSGlobal("Inner")
        object Inner extends js.Object

        @js.native
        @JSGlobal
        class InnerImplied extends js.Object

        @js.native
        @JSGlobal
        object InnerImplied extends js.Object
      }
      """ hasErrors
      """
        |newSource1.scala:8: error: Methods and fields cannot be annotated with @JSGlobal.
        |        val bar1: Int = js.native
        |            ^
        |newSource1.scala:10: error: Methods and fields cannot be annotated with @JSGlobal.
        |        var bar2: Int = js.native
        |            ^
        |newSource1.scala:12: error: Methods and fields cannot be annotated with @JSGlobal.
        |        def bar3: Int = js.native
        |            ^
        |newSource1.scala:15: error: Nested JS classes and objects cannot have an @JSGlobal annotation.
        |        @JSGlobal("Inner")
        |         ^
        |newSource1.scala:19: error: Nested JS classes and objects cannot have an @JSGlobal annotation.
        |        @JSGlobal("Inner")
        |         ^
        |newSource1.scala:23: error: Nested JS classes and objects cannot have an @JSGlobal annotation.
        |        @JSGlobal
        |         ^
        |newSource1.scala:27: error: Nested JS classes and objects cannot have an @JSGlobal annotation.
        |        @JSGlobal
        |         ^
      """
    }

  }

  @Test
  def noJSGlobalOnMembersOfObjects: Unit = {

    s"""
    @js.native @JSGlobal
    object Foo extends js.Object {
      @JSGlobal("bar1")
      val bar1: Int = js.native
      @JSGlobal("bar2")
      var bar2: Int = js.native
      @JSGlobal("bar3")
      def bar3: Int = js.native

      @js.native
      @JSGlobal("Inner")
      class Inner extends js.Object

      @js.native
      @JSGlobal("Inner")
      object Inner extends js.Object

      @js.native
      @JSGlobal
      class InnerImplied extends js.Object

      @js.native
      @JSGlobal
      object InnerImplied extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Methods and fields cannot be annotated with @JSGlobal.
      |      val bar1: Int = js.native
      |          ^
      |newSource1.scala:10: error: Methods and fields cannot be annotated with @JSGlobal.
      |      var bar2: Int = js.native
      |          ^
      |newSource1.scala:12: error: Methods and fields cannot be annotated with @JSGlobal.
      |      def bar3: Int = js.native
      |          ^
      |newSource1.scala:15: error: Nested JS classes and objects cannot have an @JSGlobal annotation.
      |      @JSGlobal("Inner")
      |       ^
      |newSource1.scala:19: error: Nested JS classes and objects cannot have an @JSGlobal annotation.
      |      @JSGlobal("Inner")
      |       ^
      |newSource1.scala:23: error: Nested JS classes and objects cannot have an @JSGlobal annotation.
      |      @JSGlobal
      |       ^
      |newSource1.scala:27: error: Nested JS classes and objects cannot have an @JSGlobal annotation.
      |      @JSGlobal
      |       ^
    """

  }

  @Test
  def noJSImportOnMembersOfClassesAndTraits: Unit = {

    for {
      outer <- Seq("class", "trait")
      fallbackStr <- Seq("", ", globalFallback = \"Foo\"")
    } {
      s"""
      @js.native ${if (outer == "trait") "" else "@JSGlobal"}
      $outer Foo extends js.Object {
        @JSImport("bar1", JSImport.Namespace$fallbackStr)
        val bar1: Int = js.native
        @JSImport("bar2", JSImport.Namespace$fallbackStr)
        var bar2: Int = js.native
        @JSImport("bar3", JSImport.Namespace$fallbackStr)
        def bar3: Int = js.native

        @js.native
        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        class Inner extends js.Object

        @js.native
        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        object Inner extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:8: error: Methods and fields cannot be annotated with @JSImport.
        |        val bar1: Int = js.native
        |            ^
        |newSource1.scala:10: error: Methods and fields cannot be annotated with @JSImport.
        |        var bar2: Int = js.native
        |            ^
        |newSource1.scala:12: error: Methods and fields cannot be annotated with @JSImport.
        |        def bar3: Int = js.native
        |            ^
        |newSource1.scala:15: error: Nested JS classes and objects cannot have an @JSImport annotation.
        |        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:19: error: Nested JS classes and objects cannot have an @JSImport annotation.
        |        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        |         ^
      """
    }

  }

  @Test
  def noJSImportOnMembersOfObjects: Unit = {

    for {
      fallbackStr <- Seq("", ", globalFallback = \"Foo\"")
    } {
      s"""
      @js.native @JSGlobal
      object Foo extends js.Object {
        @JSImport("bar1", JSImport.Namespace$fallbackStr)
        val bar1: Int = js.native
        @JSImport("bar2", JSImport.Namespace$fallbackStr)
        var bar2: Int = js.native
        @JSImport("bar3", JSImport.Namespace$fallbackStr)
        def bar3: Int = js.native

        @js.native
        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        class Inner extends js.Object

        @js.native
        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        object Inner extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:8: error: Methods and fields cannot be annotated with @JSImport.
        |        val bar1: Int = js.native
        |            ^
        |newSource1.scala:10: error: Methods and fields cannot be annotated with @JSImport.
        |        var bar2: Int = js.native
        |            ^
        |newSource1.scala:12: error: Methods and fields cannot be annotated with @JSImport.
        |        def bar3: Int = js.native
        |            ^
        |newSource1.scala:15: error: Nested JS classes and objects cannot have an @JSImport annotation.
        |        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:19: error: Nested JS classes and objects cannot have an @JSImport annotation.
        |        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        |         ^
      """
    }

  }

  @Test
  def noNonLiteralJSGlobal: Unit = {

    """
    object A {
      val a = "Hello"
    }

    @JSGlobal(A.a)
    @js.native
    object B extends js.Object

    @JSGlobal(A.a)
    @js.native
    class C extends js.Object
    """ hasErrors
    """
      |newSource1.scala:9: error: The argument to @JSGlobal must be a literal string.
      |    @JSGlobal(A.a)
      |                ^
      |newSource1.scala:13: error: The argument to @JSGlobal must be a literal string.
      |    @JSGlobal(A.a)
      |                ^
    """

  }

  @Test
  def noNonJSIdentifierJSGlobal: Unit = {

    """
    @js.native
    @JSGlobal
    class `not-a-valid-JS-identifier` extends js.Object

    @js.native
    @JSGlobal("not-a-valid-JS-identifier")
    object A extends js.Object

    @js.native
    @JSGlobal("not-a-valid-JS-identifier.further")
    object B extends js.Object

    @js.native
    @JSGlobal("TopLevel.not-a-valid-JS-identifier") // valid
    object C extends js.Object

    @js.native
    @JSGlobal("")
    object D extends js.Object

    @js.native
    @JSGlobal(".tricky")
    object E extends js.Object
    """ hasErrors
    """
      |newSource1.scala:7: error: The name of a JS global variable must be a valid JS identifier (got 'not-a-valid-JS-identifier')
      |    class `not-a-valid-JS-identifier` extends js.Object
      |          ^
      |newSource1.scala:11: error: The name of a JS global variable must be a valid JS identifier (got 'not-a-valid-JS-identifier')
      |    object A extends js.Object
      |           ^
      |newSource1.scala:15: error: The name of a JS global variable must be a valid JS identifier (got 'not-a-valid-JS-identifier')
      |    object B extends js.Object
      |           ^
      |newSource1.scala:23: error: The name of a JS global variable must be a valid JS identifier (got '')
      |    object D extends js.Object
      |           ^
      |newSource1.scala:27: error: The name of a JS global variable must be a valid JS identifier (got '')
      |    object E extends js.Object
      |           ^
    """

    """
    @js.native
    @JSImport("foo.js", "foo", globalFallback = "not-a-valid-JS-identifier")
    object A extends js.Object

    @js.native
    @JSImport("foo.js", "foo", globalFallback = "not-a-valid-JS-identifier.further")
    object B extends js.Object

    @js.native
    @JSImport("foo.js", "foo", globalFallback = "TopLevel.not-a-valid-JS-identifier") // valid
    object C extends js.Object

    @js.native
    @JSImport("foo.js", "foo", globalFallback = "")
    object D extends js.Object

    @js.native
    @JSImport("foo.js", "foo", globalFallback = ".tricky")
    object E extends js.Object
    """ hasErrors
    """
      |newSource1.scala:7: error: The name of a JS global variable must be a valid JS identifier (got 'not-a-valid-JS-identifier')
      |    object A extends js.Object
      |           ^
      |newSource1.scala:11: error: The name of a JS global variable must be a valid JS identifier (got 'not-a-valid-JS-identifier')
      |    object B extends js.Object
      |           ^
      |newSource1.scala:19: error: The name of a JS global variable must be a valid JS identifier (got '')
      |    object D extends js.Object
      |           ^
      |newSource1.scala:23: error: The name of a JS global variable must be a valid JS identifier (got '')
      |    object E extends js.Object
      |           ^
    """

  }

  @Test
  def noNonLiteralJSImport: Unit = {

    // Without global fallback

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

    // With constant (valid) global fallback

    """
    object A {
      val a = "Hello"
    }

    @JSImport(A.a, JSImport.Namespace, globalFallback = "GlobB1")
    @js.native
    object B1 extends js.Object

    @JSImport(A.a, "B2", globalFallback = "GlobB2")
    @js.native
    object B2 extends js.Object

    @JSImport("B3", A.a, globalFallback = "GlobB3")
    @js.native
    object B3 extends js.Object

    @JSImport(A.a, JSImport.Namespace, globalFallback = "GlobC1")
    @js.native
    object C1 extends js.Object

    @JSImport(A.a, "C2", globalFallback = "GlobC2")
    @js.native
    object C2 extends js.Object

    @JSImport("C3", A.a, globalFallback = "GlobC3")
    @js.native
    object C3 extends js.Object

    @JSImport(A.a, A.a, globalFallback = "GlobD")
    @js.native
    object D extends js.Object
    """ hasErrors
    """
      |newSource1.scala:9: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, JSImport.Namespace, globalFallback = "GlobB1")
      |                ^
      |newSource1.scala:13: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, "B2", globalFallback = "GlobB2")
      |                ^
      |newSource1.scala:17: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport("B3", A.a, globalFallback = "GlobB3")
      |                      ^
      |newSource1.scala:21: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, JSImport.Namespace, globalFallback = "GlobC1")
      |                ^
      |newSource1.scala:25: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, "C2", globalFallback = "GlobC2")
      |                ^
      |newSource1.scala:29: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport("C3", A.a, globalFallback = "GlobC3")
      |                      ^
      |newSource1.scala:33: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, A.a, globalFallback = "GlobD")
      |                ^
      |newSource1.scala:33: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport(A.a, A.a, globalFallback = "GlobD")
      |                     ^
    """

    // With variable (invalid) global fallback

    """
    object A {
      val a = "Hello"
    }

    @JSImport(A.a, JSImport.Namespace, globalFallback = A.a)
    @js.native
    object B1 extends js.Object

    @JSImport(A.a, "B2", globalFallback = A.a)
    @js.native
    object B2 extends js.Object

    @JSImport("B3", A.a, globalFallback = A.a)
    @js.native
    object B3 extends js.Object

    @JSImport(A.a, JSImport.Namespace, globalFallback = A.a)
    @js.native
    object C1 extends js.Object

    @JSImport(A.a, "C2", globalFallback = A.a)
    @js.native
    object C2 extends js.Object

    @JSImport("C3", A.a, globalFallback = A.a)
    @js.native
    object C3 extends js.Object

    @JSImport(A.a, A.a, globalFallback = A.a)
    @js.native
    object D extends js.Object
    """ hasErrors
    """
      |newSource1.scala:9: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, JSImport.Namespace, globalFallback = A.a)
      |                ^
      |newSource1.scala:9: error: The third argument to @JSImport, when present, must be a literal string.
      |    @JSImport(A.a, JSImport.Namespace, globalFallback = A.a)
      |                                                          ^
      |newSource1.scala:13: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, "B2", globalFallback = A.a)
      |                ^
      |newSource1.scala:13: error: The third argument to @JSImport, when present, must be a literal string.
      |    @JSImport(A.a, "B2", globalFallback = A.a)
      |                                            ^
      |newSource1.scala:17: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport("B3", A.a, globalFallback = A.a)
      |                      ^
      |newSource1.scala:17: error: The third argument to @JSImport, when present, must be a literal string.
      |    @JSImport("B3", A.a, globalFallback = A.a)
      |                                            ^
      |newSource1.scala:21: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, JSImport.Namespace, globalFallback = A.a)
      |                ^
      |newSource1.scala:21: error: The third argument to @JSImport, when present, must be a literal string.
      |    @JSImport(A.a, JSImport.Namespace, globalFallback = A.a)
      |                                                          ^
      |newSource1.scala:25: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, "C2", globalFallback = A.a)
      |                ^
      |newSource1.scala:25: error: The third argument to @JSImport, when present, must be a literal string.
      |    @JSImport(A.a, "C2", globalFallback = A.a)
      |                                            ^
      |newSource1.scala:29: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport("C3", A.a, globalFallback = A.a)
      |                      ^
      |newSource1.scala:29: error: The third argument to @JSImport, when present, must be a literal string.
      |    @JSImport("C3", A.a, globalFallback = A.a)
      |                                            ^
      |newSource1.scala:33: error: The first argument to @JSImport must be a literal string.
      |    @JSImport(A.a, A.a, globalFallback = A.a)
      |                ^
      |newSource1.scala:33: error: The second argument to @JSImport must be literal string or the JSImport.Namespace object.
      |    @JSImport(A.a, A.a, globalFallback = A.a)
      |                     ^
      |newSource1.scala:33: error: The third argument to @JSImport, when present, must be a literal string.
      |    @JSImport(A.a, A.a, globalFallback = A.a)
      |                                           ^
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
  def noAbstractLocalJSClass: Unit = {
    """
    object Enclosing {
      def method(): Unit = {
        abstract class AbstractLocalJSClass extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Implementation restriction: local JS classes cannot be abstract
      |        abstract class AbstractLocalJSClass extends js.Object
      |                       ^
    """
  }

  @Test
  def noLoadJSConstructorOfUnstableRef: Unit = {
    """
    class Enclosing {
      class InnerJSClass extends js.Object
    }

    object A {
      def method(): Any =
        js.constructorOf[Enclosing#InnerJSClass]
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: stable reference to a JS class required but Enclosing#InnerJSClass found
      |        js.constructorOf[Enclosing#InnerJSClass]
      |                        ^
    """

    // version-dependent error message due to https://github.com/scala/bug/issues/10619
    """
    class Enclosing {
      class InnerJSClass extends js.Object
    }

    object A {
      def newEnclosing: Enclosing = new Enclosing

      def method(): Any =
        js.constructorOf[newEnclosing.InnerJSClass]
    }
    """.fails()

    """
    class Enclosing {
      class InnerJSClass extends js.Object
    }

    object A {
      def method(a: Any): Boolean =
        a.isInstanceOf[Enclosing#InnerJSClass]
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: stable reference to a JS class required but Enclosing#InnerJSClass found
      |        a.isInstanceOf[Enclosing#InnerJSClass]
      |                      ^
    """

    // version-dependent error message due to https://github.com/scala/bug/issues/10619
    """
    class Enclosing {
      class InnerJSClass extends js.Object
    }

    object A {
      def newEnclosing: Enclosing = new Enclosing

      def method(a: Any): Boolean =
        a.isInstanceOf[newEnclosing.InnerJSClass]
    }
    """.fails()
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
      @JSGlobal
      object Enclosing extends js.Object {
        @JSName(Sym.sym)
        @js.native
        $kind A extends js.Object
      }
      """ hasErrors
      """
        |newSource1.scala:12: error: Implementation restriction: @JSName with a js.Symbol is not supported on nested native classes and objects
        |        @JSName(Sym.sym)
        |         ^
      """
    }
  }

  @Test
  def noDuplicateJSNameAnnotOnMember: Unit = {
    for {
      kind <- Seq("class", "object", "trait")
    } {
      """
      object A {
        val a = js.Symbol()
      }

      @js.native
      @JSGlobal
      class A extends js.Object {
        @JSName(A.a)
        @JSName("foo")
        def a: Int = js.native
      }
      """ hasErrors
      """
        |newSource1.scala:13: error: A member can only have a single @JSName annotation.
        |        @JSName("foo")
        |         ^
      """
    }
  }

  @Test
  def nonNativeJSTypesNameOverrideErrors: Unit = {
    """
    abstract class A extends js.Object {
      def bar(): Int
    }
    class B extends A {
      override def bar() = 1
    }
    """.hasNoWarns

    """
    trait A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    class B extends A {
      @JSName("foo")
      override def bar() = 1
    }
    """.hasNoWarns

    """
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    class B extends A {
      @JSName("foo")
      override def bar() = 1
    }
    """.hasNoWarns

    """
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    class B extends A {
      @JSName("baz")
      override def bar() = 1
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): Int in class B with JSName 'baz'
      |    is conflicting with
      |def bar(): Int in class A with JSName 'foo'
      |
      |      override def bar() = 1
      |                   ^
    """

    """
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    class B extends A {
      override def bar() = 1
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): Int in class B with JSName 'bar'
      |    is conflicting with
      |def bar(): Int in class A with JSName 'foo'
      |
      |      override def bar() = 1
      |                   ^
    """

    """
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Object
    }
    abstract class B extends A {
      override def bar(): String
    }
    class C extends B {
      override def bar() = "1"
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class B with JSName 'bar'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'foo'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class C with JSName 'bar'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'foo'
      |
      |      override def bar() = "1"
      |                   ^
    """

    """
    abstract class A extends js.Object {
      def bar(): Object
    }
    abstract class B extends A {
      @JSName("foo")
      override def bar(): String
    }
    class C extends B {
      override def bar() = "1"
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class B with JSName 'foo'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'bar'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class C with JSName 'bar'
      |    is conflicting with
      |override def bar(): String in class B with JSName 'foo'
      |
      |      override def bar() = "1"
      |                   ^
    """

    """
    class A extends js.Object {
      def foo: Int = 5
    }
    trait B extends A {
      @JSName("bar")
      def foo: Int
    }
    class C extends B
    """ hasErrors
    s"""
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in class A with JSName 'foo'
      |    is conflicting with
      |def foo: Int in trait B with JSName 'bar'
      |
      |      def foo: Int
      |          ^
      |${ifHasNewRefChecks("""
        |newSource1.scala:12: error: A member of a JS class is overriding another member with a different JS name.
        |
        |def foo: Int in class A with JSName 'foo'
        |    is conflicting with
        |def foo: Int in trait B with JSName 'bar'
        |
        |    class C extends B
        |          ^
      """)}
    """

    """
    class A extends js.Object {
      @JSName("bar")
      def foo: Int = 5
    }
    trait B extends A {
      def foo: Int
    }
    class C extends B
    """ hasErrors
    s"""
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in class A with JSName 'bar'
      |    is conflicting with
      |def foo: Int in trait B with JSName 'foo'
      |
      |      def foo: Int
      |          ^
      |${ifHasNewRefChecks("""
        |newSource1.scala:12: error: A member of a JS class is overriding another member with a different JS name.
        |
        |def foo: Int in class A with JSName 'bar'
        |    is conflicting with
        |def foo: Int in trait B with JSName 'foo'
        |
        |    class C extends B
        |          ^
      """)}
    """

    """
    class A[T] extends js.Object {
      @JSName("bar")
      def foo(x: T): T = x
    }
    class B extends A[Int] {
      override def foo(x: Int): Int = x
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class B with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A with JSName 'bar'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    trait A[T] extends js.Object {
      @JSName("bar")
      def foo(x: T): T
    }
    class B extends A[Int] {
      override def foo(x: Int): Int = x
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class B with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait A with JSName 'bar'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    class A[T] extends js.Object {
      @JSName("bar")
      def foo(x: T): T = x
    }
    trait B extends A[Int] {
      def foo(x: Int): Int
    }
    class C extends B {
      override def foo(x: Int): Int = x
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo(x: Int): Int in class A with JSName 'bar'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'foo'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class C with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A with JSName 'bar'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    class A[T] extends js.Object {
      def foo(x: T): T = x
    }
    trait B extends A[Int] {
      @JSName("bar")
      def foo(x: Int): Int
    }
    class C extends B {
      override def foo(x: Int): Int = x
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo(x: Int): Int in class A with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'bar'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def foo(x: Int): Int in class C with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'bar'
      |
      |      override def foo(x: Int): Int = x
      |                   ^
    """

    """
    trait A extends js.Object {
      def foo: Int
    }
    trait B extends js.Object {
      @JSName("bar")
      def foo: Int
    }
    trait C extends A with B
    """ hasErrors
    """
      |newSource1.scala:12: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in trait B with JSName 'bar'
      |    is conflicting with
      |def foo: Int in trait A with JSName 'foo'
      |
      |    trait C extends A with B
      |          ^
    """

    """
    trait A extends js.Object {
      def foo: Int
    }
    trait B extends js.Object {
      @JSName("bar")
      def foo: Int
    }
    abstract class C extends A with B
    """ hasErrors
    """
      |newSource1.scala:12: error: A member of a JS class is overriding another member with a different JS name.
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
  def nonNativeJSTypesJSNameWithSymbolOverrideErrors: Unit = {
    """
    object Syms {
      val sym1 = js.Symbol()
    }

    trait A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    class B extends A {
      @JSName(Syms.sym1)
      override def bar() = 1
    }
    """.hasNoWarns

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
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

    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    class B extends A {
      @JSName(Syms.sym2)
      override def bar() = 1
    }
    """ hasErrors
    """
      |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS name.
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

    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    class B extends A {
      @JSName("baz")
      override def bar() = 1
    }
    """ hasErrors
    """
      |newSource1.scala:15: error: A member of a JS class is overriding another member with a different JS name.
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

    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    class B extends A {
      @JSName(Syms.sym1)
      override def bar() = 1
    }
    """ hasErrors
    """
      |newSource1.scala:15: error: A member of a JS class is overriding another member with a different JS name.
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

    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Int
    }
    class B extends A {
      override def bar() = 1
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
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

    abstract class A extends js.Object {
      @JSName(Syms.sym1)
      def bar(): Object
    }
    abstract class B extends A {
      override def bar(): String
    }
    class C extends B {
      override def bar() = "1"
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class B with JSName 'bar'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'Syms.sym1'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:17: error: A member of a JS class is overriding another member with a different JS name.
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

    abstract class A extends js.Object {
      def bar(): Object
    }
    abstract class B extends A {
      @JSName(Syms.sym1)
      override def bar(): String
    }
    class C extends B {
      override def bar() = "1"
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
      |
      |override def bar(): String in class B with JSName 'Syms.sym1'
      |    is conflicting with
      |def bar(): Object in class A with JSName 'bar'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:17: error: A member of a JS class is overriding another member with a different JS name.
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

    class A extends js.Object {
      def foo: Int = 5
    }
    trait B extends A {
      @JSName(Syms.sym1)
      def foo: Int
    }
    class C extends B
    """ hasErrors
    s"""
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in class A with JSName 'foo'
      |    is conflicting with
      |def foo: Int in trait B with JSName 'Syms.sym1'
      |
      |      def foo: Int
      |          ^
      |${ifHasNewRefChecks("""
        |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS name.
        |
        |def foo: Int in class A with JSName 'foo'
        |    is conflicting with
        |def foo: Int in trait B with JSName 'Syms.sym1'
        |
        |    class C extends B
        |          ^
      """)}
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    class A extends js.Object {
      @JSName(Syms.sym1)
      def foo: Int = 5
    }
    trait B extends A {
      def foo: Int
    }
    class C extends B
    """ hasErrors
    s"""
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo: Int in class A with JSName 'Syms.sym1'
      |    is conflicting with
      |def foo: Int in trait B with JSName 'foo'
      |
      |      def foo: Int
      |          ^
      |${ifHasNewRefChecks("""
        |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS name.
        |
        |def foo: Int in class A with JSName 'Syms.sym1'
        |    is conflicting with
        |def foo: Int in trait B with JSName 'foo'
        |
        |    class C extends B
        |          ^
      """)}
    """

    """
    object Syms {
      val sym1 = js.Symbol()
    }

    class A[T] extends js.Object {
      @JSName(Syms.sym1)
      def foo(x: T): T = x
    }
    class B extends A[Int] {
      override def foo(x: Int): Int = x
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
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

    trait A[T] extends js.Object {
      @JSName(Syms.sym1)
      def foo(x: T): T
    }
    class B extends A[Int] {
      override def foo(x: Int): Int = x
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
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

    class A[T] extends js.Object {
      @JSName(Syms.sym1)
      def foo(x: T): T = x
    }
    trait B extends A[Int] {
      def foo(x: Int): Int
    }
    class C extends B {
      override def foo(x: Int): Int = x
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo(x: Int): Int in class A with JSName 'Syms.sym1'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'foo'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:17: error: A member of a JS class is overriding another member with a different JS name.
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

    class A[T] extends js.Object {
      def foo(x: T): T = x
    }
    trait B extends A[Int] {
      @JSName(Syms.sym1)
      def foo(x: Int): Int
    }
    class C extends B {
      override def foo(x: Int): Int = x
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS name.
      |
      |def foo(x: Int): Int in class A with JSName 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B with JSName 'Syms.sym1'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:17: error: A member of a JS class is overriding another member with a different JS name.
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

    trait A extends js.Object {
      def foo: Int
    }
    trait B extends js.Object {
      @JSName(Syms.sym1)
      def foo: Int
    }
    trait C extends A with B
    """ hasErrors
    """
      |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS name.
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

    trait A extends js.Object {
      def foo: Int
    }
    trait B extends js.Object {
      @JSName(Syms.sym1)
      def foo: Int
    }
    abstract class C extends A with B
    """ hasErrors
    """
      |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS name.
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
    class A(x: Int = 1) extends js.Object

    @js.native
    @JSGlobal
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: Implementation restriction: constructors of non-native JS classes cannot have default parameters if their companion module is JS native.
      |    class A(x: Int = 1) extends js.Object
      |          ^
    """

    """
    class A(x: Int = 1)

    @js.native
    @JSGlobal
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
    @JSGlobal
    class NativeBase extends js.Object {
      def add(option: js.Any = js.native): js.Any = js.native
    }
    class Derived extends NativeBase {
      override def add(option: js.Any): js.Any = super.add(option)
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: When overriding a native method with default arguments, the overriding method must explicitly repeat the default arguments.
      |      override def add(option: js.Any): js.Any = super.add(option)
      |                       ^
    """

    """
    @js.native
    trait NativeTrait extends js.Object {
      def add(option: js.Any = js.native): js.Any = js.native
    }

    @js.native
    @JSGlobal
    class NativeBase extends NativeTrait

    class Derived extends NativeBase {
      override def add(option: js.Any): js.Any = super.add(option)
    }
    """ hasErrors
    """
      |newSource1.scala:15: error: When overriding a native method with default arguments, the overriding method must explicitly repeat the default arguments.
      |      override def add(option: js.Any): js.Any = super.add(option)
      |                       ^
    """
  }

  @Test // # 3969
  def overrideEqualsHashCode: Unit = {
    for {
      obj <- List("class", "object")
    } {
      s"""
      $obj A extends js.Object {
        override def hashCode(): Int = 1
        override def equals(obj: Any): Boolean = false

        // this one works as expected (so allowed)
        override def toString(): String = "frobber"

        /* these are allowed, since they are protected in jl.Object.
         * as a result, only the overrides can be called. So the fact that they
         * do not truly override the methods in jl.Object is not observable.
         */
        override def clone(): Object = null
        override def finalize(): Unit = ()

        // other methods in jl.Object are final.
      }
      """ hasWarns
      """
         |newSource1.scala:6: warning: Overriding hashCode in a JS class does not change its hash code. To silence this warning, change the name of the method and optionally add @JSName("hashCode").
         |        override def hashCode(): Int = 1
         |                     ^
         |newSource1.scala:7: warning: Overriding equals in a JS class does not change how it is compared. To silence this warning, change the name of the method and optionally add @JSName("equals").
         |        override def equals(obj: Any): Boolean = false
         |                     ^
      """
    }

    for {
      obj <- List("class", "object")
    } {
      s"""
      @js.native
      @JSGlobal
      $obj A extends js.Object {
        override def hashCode(): Int = js.native
        override def equals(obj: Any): Boolean = js.native
      }
      """ hasWarns
      """
         |newSource1.scala:8: warning: Overriding hashCode in a JS class does not change its hash code. To silence this warning, change the name of the method and optionally add @JSName("hashCode").
         |        override def hashCode(): Int = js.native
         |                     ^
         |newSource1.scala:9: warning: Overriding equals in a JS class does not change how it is compared. To silence this warning, change the name of the method and optionally add @JSName("equals").
         |        override def equals(obj: Any): Boolean = js.native
         |                     ^
      """
    }

    """
    @js.native
    trait A extends js.Any {
      override def hashCode(): Int = js.native
      override def equals(obj: Any): Boolean = js.native
    }
    """ hasWarns
    """
       |newSource1.scala:7: warning: Overriding hashCode in a JS class does not change its hash code. To silence this warning, change the name of the method and optionally add @JSName("hashCode").
       |      override def hashCode(): Int = js.native
       |                   ^
       |newSource1.scala:8: warning: Overriding equals in a JS class does not change how it is compared. To silence this warning, change the name of the method and optionally add @JSName("equals").
       |      override def equals(obj: Any): Boolean = js.native
       |                   ^
    """

    """
    trait A extends js.Any {
      override def hashCode(): Int
      override def equals(obj: Any): Boolean
    }
    """ hasWarns
    """
       |newSource1.scala:6: warning: Overriding hashCode in a JS class does not change its hash code. To silence this warning, change the name of the method and optionally add @JSName("hashCode").
       |      override def hashCode(): Int
       |                   ^
       |newSource1.scala:7: warning: Overriding equals in a JS class does not change how it is compared. To silence this warning, change the name of the method and optionally add @JSName("equals").
       |      override def equals(obj: Any): Boolean
       |                   ^
    """
  }

}
