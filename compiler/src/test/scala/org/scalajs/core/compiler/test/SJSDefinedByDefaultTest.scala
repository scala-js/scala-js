package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._

import org.junit.Test
import org.junit.Ignore

// scalastyle:off line.size.limit

class SJSDefinedByDefaultTest extends DirectTest with TestHelpers {

  /* We add the compiler's output path itself to the classpath, for tests
   * involving separate compilation.
   */
  override def classpath: List[String] =
    super.classpath ++ List(testOutputPath)

  override def extraArgs: List[String] =
    super.extraArgs :+ "-P:scalajs:sjsDefinedByDefault"

  override def preamble: String =
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._
    """

  @Test
  def noWarnNoJSNativeAnnotation: Unit = {

    """
    class A extends js.Object
    """.hasNoWarns()

    """
    object A extends js.Object
    """.hasNoWarns()

    """
    trait A extends js.Object
    """.hasNoWarns()

  }

  @Test
  def treatedAsSJSDefined: Unit = {

    """
    class A extends js.Object {
      def foo(): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: js.native may only be used as stub implementation in facade types
      |      def foo(): Int = js.native
      |                          ^
    """

    """
    object A extends js.Object {
      def foo(): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: js.native may only be used as stub implementation in facade types
      |      def foo(): Int = js.native
      |                          ^
    """

    """
    trait A extends js.Object {
      def foo(): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: In Scala.js-defined JS traits, defs with parentheses must be abstract.
      |      def foo(): Int = js.native
      |                          ^
    """

  }

  @Test
  def noExtendNativeTrait: Unit = {
    """
    @js.native
    trait NativeTrait extends js.Object

    class A extends NativeTrait

    trait B extends NativeTrait

    object C extends NativeTrait

    object Container {
      val x = new NativeTrait {}
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |    class A extends NativeTrait
      |          ^
      |newSource1.scala:10: error: A Scala.js-defined JS trait cannot directly extend a native JS trait.
      |    trait B extends NativeTrait
      |          ^
      |newSource1.scala:12: error: A Scala.js-defined JS object cannot directly extend a native JS trait.
      |    object C extends NativeTrait
      |           ^
      |newSource1.scala:15: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |      val x = new NativeTrait {}
      |                  ^
    """
  }

  @Test
  def noExtendNativeTraitSeparateCompilation: Unit = {
    """
    @js.native
    trait NativeTrait extends js.Object
    """.succeeds()

    """
    class A extends NativeTrait

    trait B extends NativeTrait

    object C extends NativeTrait

    object Container {
      val x = new NativeTrait {}
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |    class A extends NativeTrait
      |          ^
      |newSource1.scala:7: error: A Scala.js-defined JS trait cannot directly extend a native JS trait.
      |    trait B extends NativeTrait
      |          ^
      |newSource1.scala:9: error: A Scala.js-defined JS object cannot directly extend a native JS trait.
      |    object C extends NativeTrait
      |           ^
      |newSource1.scala:12: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |      val x = new NativeTrait {}
      |                  ^
    """
  }

  @Test
  def noOverrideConcreteNonOptionalWithOptional: Unit = {
    """
    abstract class A extends js.Object {
      val a1: js.UndefOr[Int] = 5
      val a2: js.UndefOr[Int]

      def b1: js.UndefOr[Int] = 5
      def b2: js.UndefOr[Int]
    }

    trait B extends A {
      override val a1: js.UndefOr[Int] = js.undefined
      override val a2: js.UndefOr[Int] = js.undefined

      override def b1: js.UndefOr[Int] = js.undefined
      override def b2: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: Cannot override concrete val a1: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a1: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:17: error: Cannot override concrete def b1: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b1: js.UndefOr[Int] = js.undefined
      |                   ^
    """

    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      val a: js.UndefOr[Int] = js.native
      def b: js.UndefOr[Int] = js.native
    }

    trait B extends A {
      override val a: js.UndefOr[Int] = js.undefined
      override def b: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    """
      |newSource1.scala:13: error: Cannot override concrete val a: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:14: error: Cannot override concrete def b: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b: js.UndefOr[Int] = js.undefined
      |                   ^
    """

    """
    @js.native
    trait A extends js.Object {
      val a: js.UndefOr[Int] = js.native
      def b: js.UndefOr[Int] = js.native
    }

    @js.native
    @JSGlobal
    class B extends A

    trait C extends B {
      override val a: js.UndefOr[Int] = js.undefined
      override def b: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    """
      |newSource1.scala:16: error: Cannot override concrete val a: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:17: error: Cannot override concrete def b: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b: js.UndefOr[Int] = js.undefined
      |                   ^
    """
  }

  @Test
  def noOverrideConcreteNonOptionalWithOptionalSeparateCompilation1: Unit = {
    """
    abstract class A extends js.Object {
      val a1: js.UndefOr[Int] = 5
      val a2: js.UndefOr[Int]

      def b1: js.UndefOr[Int] = 5
      def b2: js.UndefOr[Int]
    }
    """.succeeds()

    """
    trait B extends A {
      override val a1: js.UndefOr[Int] = js.undefined
      override val a2: js.UndefOr[Int] = js.undefined

      override def b1: js.UndefOr[Int] = js.undefined
      override def b2: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Cannot override concrete val a1: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a1: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:9: error: Cannot override concrete def b1: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b1: js.UndefOr[Int] = js.undefined
      |                   ^
    """
  }

  @Test
  def noOverrideConcreteNonOptionalWithOptionalSeparateCompilation2: Unit = {
    """
    @js.native
    @JSGlobal
    class A extends js.Object {
      val a: js.UndefOr[Int] = js.native
      def b: js.UndefOr[Int] = js.native
    }
    """.succeeds()

    """
    trait B extends A {
      override val a: js.UndefOr[Int] = js.undefined
      override def b: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Cannot override concrete val a: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:7: error: Cannot override concrete def b: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b: js.UndefOr[Int] = js.undefined
      |                   ^
    """
  }

  @Test
  def noOverrideConcreteNonOptionalWithOptionalSeparateCompilation3: Unit = {
    """
    @js.native
    trait A extends js.Object {
      val a: js.UndefOr[Int] = js.native
      def b: js.UndefOr[Int] = js.native
    }

    @js.native
    @JSGlobal
    class B extends A
    """.succeeds()

    """
    trait C extends B {
      override val a: js.UndefOr[Int] = js.undefined
      override def b: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Cannot override concrete val a: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:7: error: Cannot override concrete def b: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b: js.UndefOr[Int] = js.undefined
      |                   ^
    """
  }

}
