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

  private def version = scala.util.Properties.versionNumberString

  private def ifHasNewRefChecks(msg: String): String = {
    if (version.startsWith("2.12.")) {
      ""
    } else {
      msg.stripMargin.trim()
    }
  }

  @Test def warnJSPackageObjectDeprecated: Unit = {

    s"""
    package object jspackage extends js.Object
    """ hasErrors
    s"""
      |newSource1.scala:5: error: Package objects may not extend js.Any.
      |    package object jspackage extends js.Object
      |                   ^
    """

  }

  @Test def noJSNameAnnotOnNonJSNative: Unit = {

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
      """
        |newSource1.scala:5: error: @JSName can only be used on members of JS types.
        |      @JSName("foo")
        |       ^
        |newSource1.scala:12: error: @JSName can only be used on members of JS types.
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
      """
        |newSource1.scala:5: error: @JSName can only be used on members of JS types.
        |      @JSName("foo")
        |       ^
        |newSource1.scala:12: error: @JSName can only be used on members of JS types.
        |      @JSName(Sym.sym)
        |       ^
      """
    }

    """
    object Container {
      @JSName("foo")
      val a: Int = 1

      @JSName("foo")
      var b: Int = 2

      @JSName("foo")
      def c: Int = 3

      @JSName("foo")
      def d_=(v: Int): Unit = ()

      @JSName("foo")
      def e(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSName can only be used on members of JS types.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:9: error: @JSName can only be used on members of JS types.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:12: error: @JSName can only be used on members of JS types.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:15: error: @JSName can only be used on members of JS types.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:18: error: @JSName can only be used on members of JS types.
      |      @JSName("foo")
      |       ^
    """

  }

  @Test def okJSNameOnNestedObjects: Unit = {

    """
    class A extends js.Object {
      @JSName("foo")
      object toto

      @JSName("bar")
      object tata extends js.Object
    }
    """.hasNoWarns()

    """
    class A extends js.Object {
      @JSName("foo")
      private object toto

      @JSName("bar")
      private object tata extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSName cannot be used on private members.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:9: error: @JSName cannot be used on private members.
      |      @JSName("bar")
      |       ^
    """

  }

  @Test def noJSGlobalAnnotOnNonJSNative: Unit = {

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSGlobal
      $obj A extends js.Object

      @JSGlobal("Foo")
      $obj B extends js.Object
      """ hasErrors
      """
        |newSource1.scala:5: error: @JSGlobal can only be used on native JS definitions (with @js.native).
        |      @JSGlobal
        |       ^
        |newSource1.scala:8: error: @JSGlobal can only be used on native JS definitions (with @js.native).
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
      """
        |newSource1.scala:5: error: @JSGlobal can only be used on native JS definitions (with @js.native).
        |      @JSGlobal
        |       ^
        |newSource1.scala:8: error: @JSGlobal can only be used on native JS definitions (with @js.native).
        |      @JSGlobal("Foo")
        |       ^
      """
    }

    """
    object Container {
      @JSGlobal
      val a: Int = 1

      @JSGlobal
      var b: Int = 2

      @JSGlobal
      def c: Int = 3

      @JSGlobal
      def d_=(v: Int): Unit = ()

      @JSGlobal
      def e(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSGlobal can only be used on native JS definitions (with @js.native).
      |      @JSGlobal
      |       ^
      |newSource1.scala:9: error: @JSGlobal can only be used on native JS definitions (with @js.native).
      |      @JSGlobal
      |       ^
      |newSource1.scala:12: error: @JSGlobal can only be used on native JS definitions (with @js.native).
      |      @JSGlobal
      |       ^
      |newSource1.scala:15: error: @JSGlobal can only be used on native JS definitions (with @js.native).
      |      @JSGlobal
      |       ^
      |newSource1.scala:18: error: @JSGlobal can only be used on native JS definitions (with @js.native).
      |      @JSGlobal
      |       ^
    """

  }

  @Test def noJSImportAnnotOnNonJSNative: Unit = {

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSImport("foo", JSImport.Namespace)
      $obj A extends js.Object
      """ hasErrors
      """
        |newSource1.scala:5: error: @JSImport can only be used on native JS definitions (with @js.native).
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
      """
        |newSource1.scala:5: error: @JSImport can only be used on native JS definitions (with @js.native).
        |      @JSImport("foo", JSImport.Namespace)
        |       ^
      """
    }

    """
    object Container {
      @JSImport("foo", "bar")
      val a: Int = 1

      @JSImport("foo", "bar")
      var b: Int = 2

      @JSImport("foo", "bar")
      def c: Int = 3

      @JSImport("foo", "bar")
      def d_=(v: Int): Unit = ()

      @JSImport("foo", "bar")
      def e(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar")
      |       ^
      |newSource1.scala:9: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar")
      |       ^
      |newSource1.scala:12: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar")
      |       ^
      |newSource1.scala:15: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar")
      |       ^
      |newSource1.scala:18: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar")
      |       ^
    """

    for {
      obj <- Seq("class", "trait", "object")
    } yield {
      s"""
      @JSImport("foo", JSImport.Namespace, globalFallback = "Foo")
      $obj A extends js.Object
      """ hasErrors
      """
        |newSource1.scala:5: error: @JSImport can only be used on native JS definitions (with @js.native).
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
      """
        |newSource1.scala:5: error: @JSImport can only be used on native JS definitions (with @js.native).
        |      @JSImport("foo", JSImport.Namespace, globalFallback = "Foo")
        |       ^
      """
    }

    """
    object Container {
      @JSImport("foo", "bar", globalFallback = "Foo")
      val a: Int = 1

      @JSImport("foo", "bar", globalFallback = "Foo")
      var b: Int = 2

      @JSImport("foo", "bar", globalFallback = "Foo")
      def c: Int = 3

      @JSImport("foo", "bar", globalFallback = "Foo")
      def d_=(v: Int): Unit = ()

      @JSImport("foo", "bar", globalFallback = "Foo")
      def e(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar", globalFallback = "Foo")
      |       ^
      |newSource1.scala:9: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar", globalFallback = "Foo")
      |       ^
      |newSource1.scala:12: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar", globalFallback = "Foo")
      |       ^
      |newSource1.scala:15: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar", globalFallback = "Foo")
      |       ^
      |newSource1.scala:18: error: @JSImport can only be used on native JS definitions (with @js.native).
      |      @JSImport("foo", "bar", globalFallback = "Foo")
      |       ^
    """

  }

  @Test def noJSGlobalScopeAnnotOnNonJSNative: Unit = {

    """
    @JSGlobalScope
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: @JSGlobalScope can only be used on native JS objects (with @js.native).
      |    @JSGlobalScope
      |     ^
    """

    """
    @JSGlobalScope
    object A
    """ hasErrors
    """
      |newSource1.scala:5: error: @JSGlobalScope can only be used on native JS objects (with @js.native).
      |    @JSGlobalScope
      |     ^
    """

  }
  @Test def noJSNameAnnotOnClass: Unit = {
    """
    @js.native
    @JSName("Foo")
    class A extends js.Object

    @js.native
    @JSName("Foo")
    abstract class B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSName can only be used on members of JS types.
      |    @JSName("Foo")
      |     ^
      |newSource1.scala:7: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |    class A extends js.Object
      |          ^
      |newSource1.scala:10: error: @JSName can only be used on members of JS types.
      |    @JSName("Foo")
      |     ^
      |newSource1.scala:11: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |    abstract class B extends js.Object
      |                   ^
    """
  }

  @Test def noJSNameAnnotOnObject: Unit = {
    """
    @js.native
    @JSName("Foo")
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSName can only be used on members of JS types.
      |    @JSName("Foo")
      |     ^
      |newSource1.scala:7: error: Native JS objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |    object A extends js.Object
      |           ^
    """
  }

  @Test def noJSNameAnnotOnTrait: Unit = {

    s"""
    object Sym {
      val sym = js.Symbol()
    }

    @js.native @JSGlobal
    object Container extends js.Object {
      @js.native
      @JSName("foo")
      trait A extends js.Object

      @js.native
      @JSName(Sym.sym)
      trait B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:12: error: @JSName cannot be used on traits.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:16: error: @JSName cannot be used on traits.
      |      @JSName(Sym.sym)
      |       ^
    """

  }

  @Test def noJSNameAnnotOnNativeValDef: Unit = {

    s"""
    object Sym {
      val sym = js.Symbol()
    }

    object Container {
      @js.native
      @JSName("foo")
      val a: Int = js.native

      @js.native
      @JSName("foo")
      def b: Int = js.native

      @js.native
      @JSName("foo")
      def c(x: Int): Int = js.native

      @js.native
      @JSName(Sym.sym)
      val d: Int = js.native

      @js.native
      @JSName(Sym.sym)
      def e: Int = js.native

      @js.native
      @JSName(Sym.sym)
      def f(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: @JSName can only be used on members of JS types.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:12: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |      val a: Int = js.native
      |          ^
      |newSource1.scala:15: error: @JSName can only be used on members of JS types.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:16: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |      def b: Int = js.native
      |          ^
      |newSource1.scala:19: error: @JSName can only be used on members of JS types.
      |      @JSName("foo")
      |       ^
      |newSource1.scala:20: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |      def c(x: Int): Int = js.native
      |          ^
      |newSource1.scala:23: error: @JSName can only be used on members of JS types.
      |      @JSName(Sym.sym)
      |       ^
      |newSource1.scala:24: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |      val d: Int = js.native
      |          ^
      |newSource1.scala:27: error: @JSName can only be used on members of JS types.
      |      @JSName(Sym.sym)
      |       ^
      |newSource1.scala:28: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |      def e: Int = js.native
      |          ^
      |newSource1.scala:31: error: @JSName can only be used on members of JS types.
      |      @JSName(Sym.sym)
      |       ^
      |newSource1.scala:32: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |      def f(x: Int): Int = js.native
      |          ^
    """

  }

  @Test def noJSGlobalAnnotOnTrait: Unit = {

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

  @Test def noJSImportAnnotOnTrait: Unit = {

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

  @Test def noJSGlobalScopeExceptOnObjects: Unit = {
    """
    @js.native @JSGlobalScope
    class A extends js.Any

    @js.native @JSGlobalScope
    trait B extends js.Any

    object Container {
      @js.native @JSGlobalScope
      class C extends js.Any

      @js.native @JSGlobalScope
      trait D extends js.Any

      @js.native @JSGlobalScope
      val a: Int = js.native

      @js.native @JSGlobalScope
      def b: Int = js.native

      @js.native @JSGlobalScope
      def c(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: @JSGlobalScope can only be used on native JS objects (with @js.native).
      |    @js.native @JSGlobalScope
      |                ^
      |newSource1.scala:8: error: Traits may not have an @JSGlobalScope annotation.
      |    @js.native @JSGlobalScope
      |                ^
      |newSource1.scala:12: error: @JSGlobalScope can only be used on native JS objects (with @js.native).
      |      @js.native @JSGlobalScope
      |                  ^
      |newSource1.scala:15: error: Traits may not have an @JSGlobalScope annotation.
      |      @js.native @JSGlobalScope
      |                  ^
      |newSource1.scala:18: error: @JSGlobalScope can only be used on native JS objects (with @js.native).
      |      @js.native @JSGlobalScope
      |                  ^
      |newSource1.scala:19: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |      val a: Int = js.native
      |          ^
      |newSource1.scala:21: error: @JSGlobalScope can only be used on native JS objects (with @js.native).
      |      @js.native @JSGlobalScope
      |                  ^
      |newSource1.scala:24: error: @JSGlobalScope can only be used on native JS objects (with @js.native).
      |      @js.native @JSGlobalScope
      |                  ^
    """
  }

  @Test def noTwoJSNativeLoadSpecAnnots: Unit = {
    for {
      (firstAnnotName, firstAnnot) <- JSNativeLoadSpecAnnots
      (secondAnnotName, secondAnnot) <- JSNativeLoadSpecAnnots
    } {
      if (firstAnnotName == "JSGlobalScope" || secondAnnotName == "JSGlobalScope") {
        s"""
          |@js.native
          |$firstAnnot
          |$secondAnnot
          |object A extends js.Object
        """.stripMargin hasErrors s"""
          |newSource1.scala:7: error: Native JS objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
          |$secondAnnot
          | ^
        """
      } else {
        s"""
          |@js.native
          |$firstAnnot
          |$secondAnnot
          |object A extends js.Object
          |
          |@js.native
          |$firstAnnot
          |$secondAnnot
          |class A extends js.Object
        """.stripMargin hasErrors s"""
          |newSource1.scala:7: error: Native JS objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
          |$secondAnnot
          | ^
          |newSource1.scala:12: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
          |$secondAnnot
          | ^
        """

        if (firstAnnot != "@JSGlobal" && secondAnnot != "@JSGlobal") {
          s"""
            |object Container {
            |  @js.native
            |  $firstAnnot
            |  $secondAnnot
            |  val a: Int = js.native
            |
            |  @js.native
            |  $firstAnnot
            |  $secondAnnot
            |  def b: Int = js.native
            |
            |  @js.native
            |  $firstAnnot
            |  $secondAnnot
            |  def c(x: Int): Int = js.native
            |}
          """.stripMargin hasErrors s"""
          |newSource1.scala:8: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
          |  $secondAnnot
          |   ^
          |newSource1.scala:13: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
          |  $secondAnnot
          |   ^
          |newSource1.scala:18: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
          |  $secondAnnot
          |   ^
          """
        }
      }
    }
  }

  @Test def noJSNativeAnnotWithoutJSAny: Unit = {

    // With the correct amount of native load spec annotations
    """
    @js.native @JSGlobal
    class A

    @js.native
    trait B

    @js.native @JSGlobal
    object C

    @js.native @JSGlobal
    class D extends Enumeration

    @js.native @JSGlobal
    object E extends Enumeration
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    class A
      |          ^
      |newSource1.scala:9: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    trait B
      |          ^
      |newSource1.scala:12: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    object C
      |           ^
      |newSource1.scala:15: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    class D extends Enumeration
      |          ^
      |newSource1.scala:18: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    object E extends Enumeration
      |           ^
    """

    // With an incorrect amount of native load spec annotations
    """
    @js.native
    class A

    @js.native @JSGlobal
    trait B

    @js.native
    object C

    @js.native
    class D extends Enumeration

    @js.native
    object E extends Enumeration
    """ hasErrors
    """
      |newSource1.scala:6: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    class A
      |          ^
      |newSource1.scala:9: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    trait B
      |          ^
      |newSource1.scala:12: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    object C
      |           ^
      |newSource1.scala:15: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    class D extends Enumeration
      |          ^
      |newSource1.scala:18: error: Classes, traits and objects not extending js.Any may not have an @js.native annotation
      |    object E extends Enumeration
      |           ^
    """

  }

  @Test def noInnerScalaClassTraitObjectInJSNative: Unit = {

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

  @Test def noInnerNonNativeJSClassTraitObjectInJSNative: Unit = {

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

  @Test def noScalaStuffInsideNativeJSObject: Unit = {

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

  @Test def noNonSyntheticCompanionInsideNativeJSObject: Unit = {

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
    """.succeeds()

  }

  @Test def noNonNativeJSTypesInsideNativeJSObject: Unit = {

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

  @Test def jsNativeValDefsHaveJSNativeRHS: Unit = {
    """
    object Container {
      @js.native @JSGlobal("a")
      val a: Int = 1

      @js.native @JSGlobal("b")
      def b: Int = 3

      @js.native @JSGlobal("c")
      def c(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: @js.native members may only call js.native.
      |      val a: Int = 1
      |                   ^
      |newSource1.scala:10: error: @js.native members may only call js.native.
      |      def b: Int = 3
      |                   ^
      |newSource1.scala:13: error: @js.native members may only call js.native.
      |      def c(x: Int): Int = x + 1
      |                             ^
    """
  }

  @Test def noJSBracketAccessOnJSNativeValDefs: Unit = {
    """
    object Container {
      @js.native @JSGlobal("a")
      @JSBracketAccess
      val a: Int = js.native

      @js.native @JSGlobal("b")
      @JSBracketAccess
      def b: Int = js.native

      @js.native @JSGlobal("c")
      @JSBracketAccess
      def c(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: @JSBracketAccess can only be used on members of JS types.
      |      @JSBracketAccess
      |       ^
      |newSource1.scala:11: error: @JSBracketAccess can only be used on members of JS types.
      |      @JSBracketAccess
      |       ^
      |newSource1.scala:15: error: @JSBracketAccess can only be used on members of JS types.
      |      @JSBracketAccess
      |       ^
    """
  }

  @Test def noJSBracketCallOnJSNativeValDefs: Unit = {
    """
    object Container {
      @js.native @JSGlobal("a")
      @JSBracketCall
      val a: Int = js.native

      @js.native @JSGlobal("b")
      @JSBracketCall
      def b: Int = js.native

      @js.native @JSGlobal("c")
      @JSBracketCall
      def c(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: @JSBracketCall can only be used on members of JS types.
      |      @JSBracketCall
      |       ^
      |newSource1.scala:11: error: @JSBracketCall can only be used on members of JS types.
      |      @JSBracketCall
      |       ^
      |newSource1.scala:15: error: @JSBracketCall can only be used on members of JS types.
      |      @JSBracketCall
      |       ^
    """
  }

  @Test def noJSNativeValDefsInJSObjects: Unit = {
    """
    object A {
      val sym = js.Symbol("foo")
    }

    object NonNativeContainer extends js.Object {
      @js.native @JSGlobal("a")
      val a: Int = js.native

      @js.native @JSGlobal("b")
      def b: Int = js.native

      @js.native @JSGlobal("c")
      def c(x: Int): Int = js.native

      @js.native @JSName("foo")
      val d: Int = js.native

      @js.native @JSName("bar")
      def e(x: Int): Int = js.native

      @js.native @JSName(A.sym)
      val f: Int = js.native

      @js.native @JSName(A.sym)
      def g(x: Int): Int = js.native
    }

    @js.native @JSGlobal
    object NativeContainer extends js.Object {
      @js.native @JSGlobal("a")
      val a: Int = js.native

      @js.native @JSGlobal("b")
      def b: Int = js.native

      @js.native @JSGlobal("c")
      def c(x: Int): Int = js.native

      @js.native @JSName("foo")
      val d: Int = js.native

      @js.native @JSName("bar")
      def e(x: Int): Int = js.native

      @js.native @JSName(A.sym)
      val f: Int = js.native

      @js.native @JSName(A.sym)
      def g(x: Int): Int = js.native
    }

    @js.native @JSGlobal
    object NativeContainer2 extends js.Object {
      @js.native
      val a: Int = js.native

      @js.native
      def b: Int = js.native

      @js.native
      def c(x: Int): Int = js.native

      @js.native
      val d: Int = js.native

      @js.native
      def e(x: Int): Int = js.native

      @js.native @JSName(A.sym)
      val f: Int = js.native

      @js.native @JSName(A.sym)
      def g(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: @js.native vals and defs can only appear in static Scala objects
      |      val a: Int = js.native
      |          ^
      |newSource1.scala:14: error: @js.native vals and defs can only appear in static Scala objects
      |      def b: Int = js.native
      |          ^
      |newSource1.scala:17: error: @js.native vals and defs can only appear in static Scala objects
      |      def c(x: Int): Int = js.native
      |          ^
      |newSource1.scala:20: error: @js.native vals and defs can only appear in static Scala objects
      |      val d: Int = js.native
      |          ^
      |newSource1.scala:23: error: @js.native vals and defs can only appear in static Scala objects
      |      def e(x: Int): Int = js.native
      |          ^
      |newSource1.scala:26: error: @js.native vals and defs can only appear in static Scala objects
      |      val f: Int = js.native
      |          ^
      |newSource1.scala:29: error: @js.native vals and defs can only appear in static Scala objects
      |      def g(x: Int): Int = js.native
      |          ^
      |newSource1.scala:35: error: @js.native vals and defs can only appear in static Scala objects
      |      val a: Int = js.native
      |          ^
      |newSource1.scala:38: error: @js.native vals and defs can only appear in static Scala objects
      |      def b: Int = js.native
      |          ^
      |newSource1.scala:41: error: @js.native vals and defs can only appear in static Scala objects
      |      def c(x: Int): Int = js.native
      |          ^
      |newSource1.scala:44: error: @js.native vals and defs can only appear in static Scala objects
      |      val d: Int = js.native
      |          ^
      |newSource1.scala:47: error: @js.native vals and defs can only appear in static Scala objects
      |      def e(x: Int): Int = js.native
      |          ^
      |newSource1.scala:50: error: @js.native vals and defs can only appear in static Scala objects
      |      val f: Int = js.native
      |          ^
      |newSource1.scala:53: error: @js.native vals and defs can only appear in static Scala objects
      |      def g(x: Int): Int = js.native
      |          ^
      |newSource1.scala:59: error: @js.native vals and defs can only appear in static Scala objects
      |      val a: Int = js.native
      |          ^
      |newSource1.scala:62: error: @js.native vals and defs can only appear in static Scala objects
      |      def b: Int = js.native
      |          ^
      |newSource1.scala:65: error: @js.native vals and defs can only appear in static Scala objects
      |      def c(x: Int): Int = js.native
      |          ^
      |newSource1.scala:68: error: @js.native vals and defs can only appear in static Scala objects
      |      val d: Int = js.native
      |          ^
      |newSource1.scala:71: error: @js.native vals and defs can only appear in static Scala objects
      |      def e(x: Int): Int = js.native
      |          ^
      |newSource1.scala:74: error: @js.native vals and defs can only appear in static Scala objects
      |      val f: Int = js.native
      |          ^
      |newSource1.scala:77: error: @js.native vals and defs can only appear in static Scala objects
      |      def g(x: Int): Int = js.native
      |          ^
    """
  }

  @Test def noJSNativeSetters: Unit = {
    """
    object Container {
      @js.native @JSGlobal("foo")
      def foo_=(x: Int): Int = js.native
      @js.native @JSGlobal("bar")
      def bar_=(x: Int, y: Int): Unit = js.native
      @js.native @JSGlobal("goo")
      def goo_=(x: Int*): Unit = js.native
      @js.native @JSGlobal("hoo")
      def hoo_=(x: Int = 1): Unit = js.native

      @js.native @JSImport("module.js", "foo")
      def foo2_=(x: Int): Int = js.native
      @js.native @JSImport("module.js", "bar")
      def bar2_=(x: Int, y: Int): Unit = js.native
      @js.native @JSImport("module.js", "goo")
      def goo2_=(x: Int*): Unit = js.native
      @js.native @JSImport("module.js", "hoo")
      def hoo2_=(x: Int = 1): Unit = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_=(x: Int): Int = js.native
      |          ^
      |newSource1.scala:9: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def bar_=(x: Int, y: Int): Unit = js.native
      |          ^
      |newSource1.scala:11: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def goo_=(x: Int*): Unit = js.native
      |          ^
      |newSource1.scala:13: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def hoo_=(x: Int = 1): Unit = js.native
      |          ^
      |newSource1.scala:16: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo2_=(x: Int): Int = js.native
      |          ^
      |newSource1.scala:18: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def bar2_=(x: Int, y: Int): Unit = js.native
      |          ^
      |newSource1.scala:20: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def goo2_=(x: Int*): Unit = js.native
      |          ^
      |newSource1.scala:22: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def hoo2_=(x: Int = 1): Unit = js.native
      |          ^
    """

    // containsErrors because some versions of the compiler use `_=` and some use `_=' (notice the quotes)
    """
    object Container {
      @js.native @JSGlobal("foo")
      val foo_= : Int = js.native
    }
    """ containsErrors
    """
      |newSource1.scala:7: error: Names of vals or vars may not end in `_=
    """

    // containsErrors because some versions of the compiler use `_=` and some use `_=' (notice the quotes)
    """
    object Container {
      @js.native @JSImport("module.js")
      val foo_= : Int = js.native
    }
    """ containsErrors
    """
      |newSource1.scala:7: error: Names of vals or vars may not end in `_=
    """
  }

  @Test def noJSNativeVars: Unit = {
    """
    object Container {
      @js.native @JSGlobal("foo")
      var foo: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      var foo: Int = js.native
      |          ^
    """
  }

  @Test def noJSNativeLazyVals: Unit = {
    """
    object Container {
      @js.native @JSGlobal("foo")
      lazy val foo: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      lazy val foo: Int = js.native
      |               ^
    """
  }

  @Test def jsNativeValDefsCannotImplementAbstractMethod: Unit = {
    """
    abstract class Parent {
      val a: Int
      def b: Int
      def c(x: Int): Int
    }

    object Container extends Parent {
      @js.native @JSGlobal("a")
      val a: Int = js.native

      @js.native @JSGlobal("b")
      def b: Int = js.native

      @js.native @JSGlobal("c")
      def c(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:13: error: An @js.native member cannot implement the inherited member Parent.a
      |      val a: Int = js.native
      |          ^
      |newSource1.scala:16: error: An @js.native member cannot implement the inherited member Parent.b
      |      def b: Int = js.native
      |          ^
      |newSource1.scala:19: error: An @js.native member cannot implement the inherited member Parent.c
      |      def c(x: Int): Int = js.native
      |          ^
    """
  }

  @Test def jsNativeValDefsCannotOverrideConcreteMethod: Unit = {
    """
    class Parent {
      val a: Int = 1
      def b: Int = 2
      def c(x: Int): Int = x + 1
    }

    object Container extends Parent {
      @js.native @JSGlobal("a")
      override val a: Int = js.native

      @js.native @JSGlobal("b")
      override def b: Int = js.native

      @js.native @JSGlobal("c")
      override def c(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:13: error: An @js.native member cannot override the inherited member Parent.a
      |      override val a: Int = js.native
      |                   ^
      |newSource1.scala:16: error: An @js.native member cannot override the inherited member Parent.b
      |      override def b: Int = js.native
      |                   ^
      |newSource1.scala:19: error: An @js.native member cannot override the inherited member Parent.c
      |      override def c(x: Int): Int = js.native
      |                   ^
    """
  }

  @Test def noBadSetters: Unit = {

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

  @Test def noBadBracketAccess: Unit = {

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

  @Test def noBadBracketCall: Unit = {

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
  def noJSOperatorAndJSName: Unit = {
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      @JSName("bar")
      def +(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: A member can have at most one annotation among @JSName, @JSOperator, @JSBracketAccess and @JSBracketCall.
      |      @JSName("bar")
      |       ^
    """
  }

  @Test // #4284
  def noBracketAccessAndJSName: Unit = {
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSBracketAccess
      @JSName("bar")
      def bar(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: A member can have at most one annotation among @JSName, @JSOperator, @JSBracketAccess and @JSBracketCall.
      |      @JSName("bar")
      |       ^
    """
  }

  // #4284
  @Test def noBracketCallAndJSName: Unit = {
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSBracketCall
      @JSName("bar")
      def bar(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: A member can have at most one annotation among @JSName, @JSOperator, @JSBracketAccess and @JSBracketCall.
      |      @JSName("bar")
      |       ^
    """
  }

  // #4284
  @Test def noBracketAccessAndBracketCall: Unit = {
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSBracketAccess
      @JSBracketCall
      def bar(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: A member can have at most one annotation among @JSName, @JSOperator, @JSBracketAccess and @JSBracketCall.
      |      @JSBracketCall
      |       ^
    """
  }

  @Test def noBadUnaryOp: Unit = {
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      def unary_!(x: Int*): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSOperator methods with the name 'unary_!' may not have any parameters
      |      def unary_!(x: Int*): Int = js.native
      |          ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      def unary_-(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSOperator methods with the name 'unary_-' may not have any parameters
      |      def unary_-(x: Int): Int = js.native
      |          ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      def unary_%(): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSOperator cannot be used on a method with the name 'unary_%' because it is not one of the JavaScript operators
      |      def unary_%(): Int = js.native
      |          ^
    """
  }

  @Test def noBadBinaryOp: Unit = {
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      def +(x: Int*): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: warning: Method '+' should have an explicit @JSName or @JSOperator annotation because its name is one of the JavaScript operators
      |      def +(x: Int*): Int = js.native
      |          ^
      |newSource1.scala:8: error: methods representing binary operations may not have repeated parameters
      |      def +(x: Int*): Int = js.native
      |            ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      def +(x: Int*): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: methods representing binary operations may not have repeated parameters
      |      def +(x: Int*): Int = js.native
      |            ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      def +(x: Int, y: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSOperator methods with the name '+' must have exactly one parameter
      |      def +(x: Int, y: Int): Int = js.native
      |          ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      def %%(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSOperator cannot be used on a method with the name '%%' because it is not one of the JavaScript operators
      |      def %%(x: Int): Int = js.native
      |          ^
    """
  }

  @Test def onlyJSTraits: Unit = {

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

  @Test def noCaseClassObject: Unit = {

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

  @Test def noNativeJSNestedInScalaClassTrait: Unit = {

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
        |newSource1.scala:7: error: Scala traits and classes may not have native JS members
        |        $inner Inner extends js.Object
        |         ${" " * inner.length}^
      """
    }

  }

  @Test def noNativeJSNestedInNonNativeJS: Unit = {

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
        |newSource1.scala:7: error: non-native JS classes, traits and objects may not have native JS members
        |        $inner Inner extends js.Object
        |         ${" " * inner.length}^
      """
    }

  }

  @Test def noLocalJSNative: Unit = {
    """
    object A {
      def a = {
        @js.native @JSGlobal
        class B extends js.Object

        @js.native @JSGlobal
        object C extends js.Object

        @js.native @JSGlobal
        val d: Int = js.native

        @js.native @JSGlobal
        var e: Int = js.native

        @js.native @JSGlobal
        def f: Int = js.native

        @js.native @JSGlobal
        def f_=(v: Int): Unit = js.native

        @js.native @JSGlobal
        def g(x: Int): Int = js.native

        @js.native @JSGlobal
        lazy val h: Int = js.native
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @js.native is not allowed on local definitions
      |        class B extends js.Object
      |              ^
      |newSource1.scala:11: error: @js.native is not allowed on local definitions
      |        object C extends js.Object
      |               ^
      |newSource1.scala:14: error: @js.native is not allowed on local definitions
      |        val d: Int = js.native
      |            ^
      |newSource1.scala:17: error: @js.native is not allowed on local definitions
      |        var e: Int = js.native
      |            ^
      |newSource1.scala:20: error: @js.native is not allowed on local definitions
      |        def f: Int = js.native
      |            ^
      |newSource1.scala:23: error: @js.native is not allowed on local definitions
      |        def f_=(v: Int): Unit = js.native
      |            ^
      |newSource1.scala:26: error: @js.native is not allowed on local definitions
      |        def g(x: Int): Int = js.native
      |            ^
      |newSource1.scala:29: error: @js.native is not allowed on local definitions
      |        lazy val h: Int = js.native
      |                 ^
    """
  }

  @Test def noNativeInJSAny: Unit = {

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

  @Test def checkJSAnyBody: Unit = {

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

  @Test def noWarnJSAnyDeferred: Unit = {

    """
    @js.native
    @JSGlobal
    abstract class A extends js.Object {
      def value: Int
      val x: Int
    }
    """.hasNoWarns()

    """
    @js.native
    trait A extends js.Object {
      def value: Int
      val x: Int
    }
    """.hasNoWarns()

  }

  @Test def noCallSecondaryCtor: Unit = {

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

  @Test def noPrivateMemberInNative: Unit = {

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

  @Test def noPrivateConstructorInNative: Unit = {

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
    """.hasNoWarns()

  }

  @Test def noUseJsNative: Unit = {

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

  @Test def warnNothingInNativeJS: Unit = {

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

  @Test def nativeClassHasLoadingSpec: Unit = {
    """
    @js.native
    class A extends js.Object

    @js.native
    abstract class B extends js.Object

    object Container {
      @js.native
      class C extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |    class A extends js.Object
      |          ^
      |newSource1.scala:9: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |    abstract class B extends js.Object
      |                   ^
      |newSource1.scala:13: error: Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport.
      |      class C extends js.Object
      |            ^
    """
  }

  @Test def nativeObjectHasLoadingSpec: Unit = {
    """
    @js.native
    object A extends js.Object

    object Container {
      @js.native
      object B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |    object A extends js.Object
      |           ^
      |newSource1.scala:10: error: Native JS objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope.
      |      object B extends js.Object
      |             ^
    """
  }

  @Test def noNativeDefinitionNamedApplyWithoutExplicitName: Unit = {

    """
    @js.native
    @JSGlobal
    class apply extends js.Object

    @js.native
    @JSGlobal
    object apply extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |    @JSGlobal
      |     ^
      |newSource1.scala:10: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |    @JSGlobal
      |     ^
    """

    """
    @js.native
    @JSImport("foo.js")
    class apply extends js.Object

    @js.native
    @JSImport("foo.js")
    object apply extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |    @JSImport("foo.js")
      |     ^
      |newSource1.scala:10: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |    @JSImport("foo.js")
      |     ^
    """

    """
    object A {
      @js.native
      @JSGlobal
      class apply extends js.Object

      @js.native
      @JSGlobal
      object apply extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
      |newSource1.scala:11: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js")
      class apply extends js.Object

      @js.native
      @JSImport("foo.js")
      object apply extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
      |newSource1.scala:11: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
    """

    """
    package object A {
      @js.native
      @JSGlobal
      class apply extends js.Object

      @js.native
      @JSGlobal
      object apply extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
      |newSource1.scala:11: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    """
    package object A {
      @js.native
      @JSImport("foo.js")
      class apply extends js.Object

      @js.native
      @JSImport("foo.js")
      object apply extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
      |newSource1.scala:11: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
    """

    """
    object A {
      @js.native
      @JSGlobal
      val apply: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js")
      val apply: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
    """

    """
    object A {
      @js.native
      @JSGlobal
      def apply: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js")
      def apply: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
    """

    """
    object A {
      @js.native
      @JSGlobal
      def apply(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js")
      def apply(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions named 'apply' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
    """

    """
    @JSGlobal("apply")
    @js.native
    class apply extends js.Object

    @JSGlobal("apply")
    @js.native
    object apply extends js.Object

    object A {
      @JSGlobal("apply")
      @js.native
      class apply extends js.Object

      @JSGlobal("apply")
      @js.native
      object apply extends js.Object
    }

    object B {
      @JSGlobal("apply")
      @js.native
      val apply: Int = js.native
    }

    object C {
      @JSGlobal("apply")
      @js.native
      def apply: Int = js.native
    }

    object D {
      @JSGlobal("apply")
      @js.native
      def apply(x: Int): Int = js.native
    }
    """.hasNoWarns()

    """
    @JSImport("foo.js", "apply")
    @js.native
    class apply extends js.Object

    @JSImport("foo.js", "apply")
    @js.native
    object apply extends js.Object

    object A {
      @JSImport("foo.js", "apply")
      @js.native
      class apply extends js.Object

      @JSImport("foo.js", "apply")
      @js.native
      object apply extends js.Object
    }

    object B {
      @JSImport("foo.js", "apply")
      @js.native
      val apply: Int = js.native
    }

    object C {
      @JSImport("foo.js", "apply")
      @js.native
      def apply: Int = js.native
    }

    object D {
      @JSImport("foo.js", "apply")
      @js.native
      def apply(x: Int): Int = js.native
    }
    """.hasNoWarns()

    """
    @JSImport("foo.js", "apply", globalFallback = "apply")
    @js.native
    class apply extends js.Object

    @JSImport("foo.js", "apply", globalFallback = "apply")
    @js.native
    object apply extends js.Object

    object A {
      @JSImport("foo.js", "apply", globalFallback = "apply")
      @js.native
      class apply extends js.Object

      @JSImport("foo.js", "apply", globalFallback = "apply")
      @js.native
      object apply extends js.Object
    }
    """.hasNoWarns()

  }

  @Test def noNativeDefinitionWithSetterNameWithoutExplicitName: Unit = {

    """
    @js.native
    @JSGlobal
    class foo_= extends js.Object

    @js.native
    @JSGlobal
    object foo_= extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSGlobal
      |    @JSGlobal
      |     ^
      |newSource1.scala:10: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSGlobal
      |    @JSGlobal
      |     ^
    """

    """
    @js.native
    @JSImport("foo.js")
    class foo_= extends js.Object

    @js.native
    @JSImport("foo.js")
    object foo_= extends js.Object
    """ hasErrors
    """
      |newSource1.scala:6: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSImport
      |    @JSImport("foo.js")
      |     ^
      |newSource1.scala:10: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSImport
      |    @JSImport("foo.js")
      |     ^
    """

    """
    object A {
      @js.native
      @JSGlobal
      class foo_= extends js.Object

      @js.native
      @JSGlobal
      object foo_= extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
      |newSource1.scala:11: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js")
      class foo_= extends js.Object

      @js.native
      @JSImport("foo.js")
      object foo_= extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
      |newSource1.scala:11: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
    """

    """
    package object A {
      @js.native
      @JSGlobal
      class foo_= extends js.Object

      @js.native
      @JSGlobal
      object foo_= extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
      |newSource1.scala:11: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
    """

    """
    package object A {
      @js.native
      @JSImport("foo.js")
      class foo_= extends js.Object

      @js.native
      @JSImport("foo.js")
      object foo_= extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
      |newSource1.scala:11: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
    """

    // containsErrors because some versions of the compiler use `_=` and some use `_=' (notice the quotes)
    """
    object A {
      @js.native
      @JSGlobal
      val foo_= : Int = js.native
    }
    """ containsErrors
    """
      |newSource1.scala:8: error: Names of vals or vars may not end in `_=
    """

    // containsErrors because some versions of the compiler use `_=` and some use `_=' (notice the quotes)
    """
    object A {
      @js.native
      @JSImport("foo.js")
      val foo_= : Int = js.native
    }
    """ containsErrors
    """
      |newSource1.scala:8: error: Names of vals or vars may not end in `_=
    """

    // containsErrors because some versions of the compiler use `_=` and some use `_=' (notice the quotes)
    """
    object A {
      @js.native
      @JSGlobal
      var foo_= : Int = js.native
    }
    """ containsErrors
    """
      |newSource1.scala:8: error: Names of vals or vars may not end in `_=
    """

    """
    object A {
      @js.native
      @JSGlobal
      def foo_= : Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
      |newSource1.scala:8: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_= : Int = js.native
      |          ^
    """

    """
    object A {
      @js.native
      @JSGlobal("foo")
      def foo_= : Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_= : Int = js.native
      |          ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js")
      def foo_= : Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
      |newSource1.scala:8: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_= : Int = js.native
      |          ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js", "foo")
      def foo_= : Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_= : Int = js.native
      |          ^
    """

    """
    object A {
      @js.native
      @JSGlobal
      def foo_=(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSGlobal
      |      @JSGlobal
      |       ^
      |newSource1.scala:8: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_=(x: Int): Int = js.native
      |          ^
    """

    """
    object A {
      @js.native
      @JSGlobal("foo")
      def foo_=(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_=(x: Int): Int = js.native
      |          ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js")
      def foo_=(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Native JS definitions with a name ending in '_=' must have an explicit name in @JSImport
      |      @JSImport("foo.js")
      |       ^
      |newSource1.scala:8: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_=(x: Int): Int = js.native
      |          ^
    """

    """
    object A {
      @js.native
      @JSImport("foo.js", "foo")
      def foo_=(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @js.native is not allowed on vars, lazy vals and setter defs
      |      def foo_=(x: Int): Int = js.native
      |          ^
    """

    """
    @JSGlobal("foo")
    @js.native
    class foo_= extends js.Object

    @JSGlobal("foo")
    @js.native
    object foo_= extends js.Object

    object A {
      @JSGlobal("foo")
      @js.native
      class foo_= extends js.Object

      @JSGlobal("foo")
      @js.native
      object foo_= extends js.Object
    }
    """.hasNoWarns()

    """
    @JSImport("foo.js", "foo_=")
    @js.native
    class foo_= extends js.Object

    @JSImport("foo.js", "foo_=")
    @js.native
    object foo_= extends js.Object

    object A {
      @JSImport("foo.js", "foo_=")
      @js.native
      class foo_= extends js.Object

      @JSImport("foo.js", "foo_=")
      @js.native
      object foo_= extends js.Object
    }
    """.hasNoWarns()

    """
    @JSImport("foo.js", "foo_=", globalFallback = "foo")
    @js.native
    class foo_= extends js.Object

    @JSImport("foo.js", "foo_=", globalFallback = "foo")
    @js.native
    object foo_= extends js.Object

    object A {
      @JSImport("foo.js", "foo_=", globalFallback = "foo")
      @js.native
      class foo_= extends js.Object

      @JSImport("foo.js", "foo_=", globalFallback = "foo")
      @js.native
      object foo_= extends js.Object
    }
    """.hasNoWarns()

  }

  @Test def noNonLiteralJSName: Unit = {

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

  @Test def noNonStaticStableJSNameSymbol: Unit = {

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

  @Test def noSelfReferenceJSNameSymbol: Unit = {

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
    """.succeeds()

  }

  @Test def noJSGlobalOnMembersOfClassesAndTraits: Unit = {

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
        |newSource1.scala:7: error: @JSGlobal can only be used on native JS definitions (with @js.native).
        |        @JSGlobal("bar1")
        |         ^
        |newSource1.scala:9: error: @JSGlobal can only be used on native JS definitions (with @js.native).
        |        @JSGlobal("bar2")
        |         ^
        |newSource1.scala:11: error: @JSGlobal can only be used on native JS definitions (with @js.native).
        |        @JSGlobal("bar3")
        |         ^
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

  @Test def noJSGlobalOnMembersOfObjects: Unit = {

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
      |newSource1.scala:7: error: @JSGlobal can only be used on native JS definitions (with @js.native).
      |      @JSGlobal("bar1")
      |       ^
      |newSource1.scala:9: error: @JSGlobal can only be used on native JS definitions (with @js.native).
      |      @JSGlobal("bar2")
      |       ^
      |newSource1.scala:11: error: @JSGlobal can only be used on native JS definitions (with @js.native).
      |      @JSGlobal("bar3")
      |       ^
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

  @Test def noJSImportOnMembersOfClassesAndTraits: Unit = {

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
        |newSource1.scala:7: error: @JSImport can only be used on native JS definitions (with @js.native).
        |        @JSImport("bar1", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:9: error: @JSImport can only be used on native JS definitions (with @js.native).
        |        @JSImport("bar2", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:11: error: @JSImport can only be used on native JS definitions (with @js.native).
        |        @JSImport("bar3", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:15: error: Nested JS classes and objects cannot have an @JSImport annotation.
        |        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:19: error: Nested JS classes and objects cannot have an @JSImport annotation.
        |        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        |         ^
      """
    }

  }

  @Test def noJSImportOnMembersOfObjects: Unit = {

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
        |newSource1.scala:7: error: @JSImport can only be used on native JS definitions (with @js.native).
        |        @JSImport("bar1", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:9: error: @JSImport can only be used on native JS definitions (with @js.native).
        |        @JSImport("bar2", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:11: error: @JSImport can only be used on native JS definitions (with @js.native).
        |        @JSImport("bar3", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:15: error: Nested JS classes and objects cannot have an @JSImport annotation.
        |        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        |         ^
        |newSource1.scala:19: error: Nested JS classes and objects cannot have an @JSImport annotation.
        |        @JSImport("Inner", JSImport.Namespace$fallbackStr)
        |         ^
      """
    }

  }

  @Test def noNonLiteralJSGlobal: Unit = {

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

  @Test def noNonJSIdentifierJSGlobal: Unit = {

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

  @Test def noNonLiteralJSImport: Unit = {

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

  @Test def noApplyProperty: Unit = {

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
    """.succeeds()

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
    """.succeeds()

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
    """.succeeds()

  }

  @Test def noAbstractLocalJSClass: Unit = {
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

  @Test def noLoadJSConstructorOfUnstableRef: Unit = {
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

  @Test def noJSSymbolNameOnNestedNativeClassesAndObjects: Unit = {
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
      s"""
        |newSource1.scala:12: error: Implementation restriction: @JSName with a js.Symbol is not supported on nested native classes and objects
        |        @JSName(Sym.sym)
        |         ^
      """
    }
  }

  @Test def noBracketCallOrBracketAccessOnJSClasses: Unit = {
    // native
    """
    @js.native
    @JSGlobal
    @JSBracketCall
    class A extends js.Object

    @js.native
    @JSGlobal
    @JSBracketAccess
    object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:7: error: @JSBracketCall can only be used on members of JS types.
      |    @JSBracketCall
      |     ^
      |newSource1.scala:12: error: @JSBracketAccess can only be used on members of JS types.
      |    @JSBracketAccess
      |     ^
    """

    // Non-native
    """
    @JSBracketCall
    class A extends js.Object

    @JSBracketAccess
    object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: @JSBracketCall can only be used on members of JS types.
      |    @JSBracketCall
      |     ^
      |newSource1.scala:8: error: @JSBracketAccess can only be used on members of JS types.
      |    @JSBracketAccess
      |     ^
    """

    // Nested native
    """
    @js.native
    @JSGlobal
    object Enclosing extends js.Object {
      @JSBracketCall
      @js.native
      class A extends js.Object

      @JSBracketAccess
      @js.native
      object B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: @JSBracketCall can only be used on methods.
      |      @JSBracketCall
      |       ^
      |newSource1.scala:12: error: @JSBracketAccess can only be used on methods.
      |      @JSBracketAccess
      |       ^
    """

    // Nested non-native
    """
    object Enclosing extends js.Object {
      @JSBracketCall
      object A extends js.Object

      @JSBracketAccess
      class B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: @JSBracketCall can only be used on methods.
      |      @JSBracketCall
      |       ^
      |newSource1.scala:9: error: @JSBracketAccess can only be used on methods.
      |      @JSBracketAccess
      |       ^
    """
  }

  @Test def noDuplicateJSNameAnnotOnMember: Unit = {
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
        |newSource1.scala:13: error: A member can have at most one annotation among @JSName, @JSOperator, @JSBracketAccess and @JSBracketCall.
        |        @JSName("foo")
        |         ^
      """
    }
  }

  @Test def nonNativeJSTypesNameOverrideErrors: Unit = {
    """
    abstract class A extends js.Object {
      def bar(): Int
    }
    class B extends A {
      override def bar() = 1
    }
    """.hasNoWarns()

    """
    trait A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    class B extends A {
      @JSName("foo")
      override def bar() = 1
    }
    """.hasNoWarns()

    """
    abstract class A extends js.Object {
      @JSName("foo")
      def bar(): Int
    }
    class B extends A {
      @JSName("foo")
      override def bar() = 1
    }
    """.hasNoWarns()

    // #4375
    """
    abstract class Parent extends js.Object {
      type TypeMember <: CharSequence
      type JSTypeMember <: js.Object

      type Foo = Int
      @JSName("Babar") def Bar: Int = 5
    }

    class Child extends Parent {
      type TypeMember = String
      override type JSTypeMember = js.Date // the override keyword makes no difference

      @JSName("Foobar") def Foo: Int = 5
      type Bar = Int
    }
    """.hasNoWarns()

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
      |newSource1.scala:11: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): Int in class B called from JS as method 'baz'
      |    is conflicting with
      |def bar(): Int in class A called from JS as method 'foo'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): Int in class B called from JS as method 'bar'
      |    is conflicting with
      |def bar(): Int in class A called from JS as method 'foo'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): String in class B called from JS as method 'bar'
      |    is conflicting with
      |def bar(): Object in class A called from JS as method 'foo'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): String in class C called from JS as method 'bar'
      |    is conflicting with
      |def bar(): Object in class A called from JS as method 'foo'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): String in class B called from JS as method 'foo'
      |    is conflicting with
      |def bar(): Object in class A called from JS as method 'bar'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): String in class C called from JS as method 'bar'
      |    is conflicting with
      |override def bar(): String in class B called from JS as method 'foo'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo: Int in class A called from JS as property 'foo'
      |    is conflicting with
      |def foo: Int in trait B called from JS as property 'bar'
      |
      |      def foo: Int
      |          ^
      |${ifHasNewRefChecks("""
        |newSource1.scala:12: error: A member of a JS class is overriding another member with a different JS calling convention.
        |
        |def foo: Int in class A called from JS as property 'foo'
        |    is conflicting with
        |def foo: Int in trait B called from JS as property 'bar'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo: Int in class A called from JS as property 'bar'
      |    is conflicting with
      |def foo: Int in trait B called from JS as property 'foo'
      |
      |      def foo: Int
      |          ^
      |${ifHasNewRefChecks("""
        |newSource1.scala:12: error: A member of a JS class is overriding another member with a different JS calling convention.
        |
        |def foo: Int in class A called from JS as property 'bar'
        |    is conflicting with
        |def foo: Int in trait B called from JS as property 'foo'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def foo(x: Int): Int in class B called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A called from JS as method 'bar'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def foo(x: Int): Int in class B called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait A called from JS as method 'bar'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo(x: Int): Int in class A called from JS as method 'bar'
      |    is conflicting with
      |def foo(x: Int): Int in trait B called from JS as method 'foo'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def foo(x: Int): Int in class C called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A called from JS as method 'bar'
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
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo(x: Int): Int in class A called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B called from JS as method 'bar'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def foo(x: Int): Int in class C called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B called from JS as method 'bar'
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
      |newSource1.scala:12: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo: Int in trait B called from JS as property 'bar'
      |    is conflicting with
      |def foo: Int in trait A called from JS as property 'foo'
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
      |newSource1.scala:12: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo: Int in trait B called from JS as property 'bar'
      |    is conflicting with
      |def foo: Int in trait A called from JS as property 'foo'
      |
      |    abstract class C extends A with B
      |                   ^
    """
  }

  @Test def nonNativeJSTypesJSNameWithSymbolOverrideErrors: Unit = {
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
    """.hasNoWarns()

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
    """.hasNoWarns()

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
      |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): Int in class B called from JS as method 'Syms.sym2'
      |    is conflicting with
      |def bar(): Int in class A called from JS as method 'Syms.sym1'
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
      |newSource1.scala:15: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): Int in class B called from JS as method 'baz'
      |    is conflicting with
      |def bar(): Int in class A called from JS as method 'Syms.sym1'
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
      |newSource1.scala:15: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): Int in class B called from JS as method 'Syms.sym1'
      |    is conflicting with
      |def bar(): Int in class A called from JS as method 'foo'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): Int in class B called from JS as method 'bar'
      |    is conflicting with
      |def bar(): Int in class A called from JS as method 'Syms.sym1'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): String in class B called from JS as method 'bar'
      |    is conflicting with
      |def bar(): Object in class A called from JS as method 'Syms.sym1'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:17: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): String in class C called from JS as method 'bar'
      |    is conflicting with
      |def bar(): Object in class A called from JS as method 'Syms.sym1'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): String in class B called from JS as method 'Syms.sym1'
      |    is conflicting with
      |def bar(): Object in class A called from JS as method 'bar'
      |
      |      override def bar(): String
      |                   ^
      |newSource1.scala:17: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def bar(): String in class C called from JS as method 'bar'
      |    is conflicting with
      |override def bar(): String in class B called from JS as method 'Syms.sym1'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo: Int in class A called from JS as property 'foo'
      |    is conflicting with
      |def foo: Int in trait B called from JS as property 'Syms.sym1'
      |
      |      def foo: Int
      |          ^
      |${ifHasNewRefChecks("""
        |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS calling convention.
        |
        |def foo: Int in class A called from JS as property 'foo'
        |    is conflicting with
        |def foo: Int in trait B called from JS as property 'Syms.sym1'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo: Int in class A called from JS as property 'Syms.sym1'
      |    is conflicting with
      |def foo: Int in trait B called from JS as property 'foo'
      |
      |      def foo: Int
      |          ^
      |${ifHasNewRefChecks("""
        |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS calling convention.
        |
        |def foo: Int in class A called from JS as property 'Syms.sym1'
        |    is conflicting with
        |def foo: Int in trait B called from JS as property 'foo'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def foo(x: Int): Int in class B called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A called from JS as method 'Syms.sym1'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def foo(x: Int): Int in class B called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait A called from JS as method 'Syms.sym1'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo(x: Int): Int in class A called from JS as method 'Syms.sym1'
      |    is conflicting with
      |def foo(x: Int): Int in trait B called from JS as method 'foo'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:17: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def foo(x: Int): Int in class C called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in class A called from JS as method 'Syms.sym1'
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
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo(x: Int): Int in class A called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B called from JS as method 'Syms.sym1'
      |
      |      def foo(x: Int): Int
      |          ^
      |newSource1.scala:17: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |override def foo(x: Int): Int in class C called from JS as method 'foo'
      |    is conflicting with
      |def foo(x: Int): Int in trait B called from JS as method 'Syms.sym1'
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
      |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo: Int in trait B called from JS as property 'Syms.sym1'
      |    is conflicting with
      |def foo: Int in trait A called from JS as property 'foo'
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
      |newSource1.scala:16: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def foo: Int in trait B called from JS as property 'Syms.sym1'
      |    is conflicting with
      |def foo: Int in trait A called from JS as property 'foo'
      |
      |    abstract class C extends A with B
      |                   ^
    """
  }

  // #4282
  @Test def jsTypesSpecialCallingConventionOverrideErrors: Unit = {
    // name "apply" vs function application
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      def apply(): Int
    }

    class B extends A {
      @JSName("apply")
      def apply(): Int
    }
    """ hasErrors
    """
      |newSource1.scala:13: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def apply(): Int in class B called from JS as method 'apply'
      |    is conflicting with
      |def apply(): Int in class A called from JS as function application
      |
      |      def apply(): Int
      |          ^
    """

    // property vs method
    """
    class A extends js.Object {
      def a: Int
    }

    class B extends A {
      def a(): Int
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def a(): Int in class B called from JS as method 'a'
      |    is conflicting with
      |def a: Int in class A called from JS as property 'a'
      |
      |      def a(): Int
      |          ^
    """

    val postUnarySpace = {
      val hasNoSpace = {
        version == "2.12.6" ||
        version == "2.12.7" ||
        version == "2.12.8" ||
        version == "2.12.9" ||
        version == "2.12.10"
      }
      if (hasNoSpace) ""
      else " "
    }

    // unary op vs thing named like it
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      def unary_+ : Int
    }

    class B extends A {
      @JSName("unary_+")
      def unary_+ : Int
    }
    """ hasErrors
    s"""
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def unary_+$postUnarySpace: Int in class B called from JS as property 'unary_+'
      |    is conflicting with
      |def unary_+$postUnarySpace: Int in class A called from JS as unary operator
      |
      |      def unary_+ : Int
      |          ^
    """

    // non-zero arg is OK
    """
    class A extends js.Object {
      def unary_+(x: String): Int = 1
    }

    class B extends A {
      @JSName("unary_+")
      override def unary_+(x: String): Int = 2
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Method 'unary_+' should have an explicit @JSName or @JSOperator annotation because its name is one of the JavaScript operators
      |      def unary_+(x: String): Int = 1
      |          ^
    """

    // binary op vs thing named like it
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      @JSOperator
      def ||(x: Int): Int
    }

    class B extends A {
      @JSName("||")
      def ||(x: Int): Int
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: A member of a JS class is overriding another member with a different JS calling convention.
      |
      |def ||(x: Int): Int in class B called from JS as method '||'
      |    is conflicting with
      |def ||(x: Int): Int in class A called from JS as binary operator
      |
      |      def ||(x: Int): Int
      |          ^
    """

    // non-single arg is OK
    """
    class A extends js.Object {
      def ||(): Int = 1
    }

    class B extends A {
      @JSName("||")
      override def ||(): Int = 2
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Method '||' should have an explicit @JSName or @JSOperator annotation because its name is one of the JavaScript operators
      |      def ||(): Int = 1
      |          ^
    """
  }

  @Test def noDefaultConstructorArgsIfModuleIsJSNative: Unit = {
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

  // #2547
  @Test def noDefaultOverrideCrash: Unit = {
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

  // # 3969
  @Test def overrideEqualsHashCode: Unit = {
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
