package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._

import org.junit.Test
import org.junit.Ignore

// scalastyle:off line.size.limit

class JSOptionalTest extends DirectTest with TestHelpers {

  override def preamble: String = {
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._
    """
  }

  @Test
  def optionalRequiresUndefinedRHS: Unit = {
    s"""
    @ScalaJSDefined
    trait A extends js.Object {
      val a1: js.UndefOr[Int] = 5
      val a2: Int = 5

      def b1: js.UndefOr[Int] = 5
      def b2: Int = 5

      var c1: js.UndefOr[Int] = 5
      var c2: Int = 5
    }
    """ hasErrors
    s"""
      |newSource1.scala:7: error: Members of Scala.js-defined JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      val a1: js.UndefOr[Int] = 5
      |                                ^
      |newSource1.scala:8: error: Members of Scala.js-defined JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      val a2: Int = 5
      |                    ^
      |newSource1.scala:10: error: Members of Scala.js-defined JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      def b1: js.UndefOr[Int] = 5
      |                                ^
      |newSource1.scala:11: error: Members of Scala.js-defined JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      def b2: Int = 5
      |                    ^
      |newSource1.scala:13: error: Members of Scala.js-defined JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      var c1: js.UndefOr[Int] = 5
      |                                ^
      |newSource1.scala:14: error: Members of Scala.js-defined JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      var c2: Int = 5
      |                    ^
    """
  }

  @Test
  def noOverrideConcreteNonOptionalWithOptional: Unit = {
    s"""
    @ScalaJSDefined
    abstract class A extends js.Object {
      val a1: js.UndefOr[Int] = 5
      val a2: js.UndefOr[Int]

      def b1: js.UndefOr[Int] = 5
      def b2: js.UndefOr[Int]
    }

    @ScalaJSDefined
    trait B extends A {
      override val a1: js.UndefOr[Int] = js.undefined
      override val a2: js.UndefOr[Int] = js.undefined

      override def b1: js.UndefOr[Int] = js.undefined
      override def b2: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    s"""
      |newSource1.scala:16: error: Cannot override concrete val a1: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a1: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:19: error: Cannot override concrete def b1: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b1: js.UndefOr[Int] = js.undefined
      |                   ^
    """

    s"""
    @js.native
    class A extends js.Object {
      val a: js.UndefOr[Int] = js.native
      def b: js.UndefOr[Int] = js.native
    }

    @ScalaJSDefined
    trait B extends A {
      override val a: js.UndefOr[Int] = js.undefined
      override def b: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    s"""
      |newSource1.scala:13: error: Cannot override concrete val a: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:14: error: Cannot override concrete def b: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b: js.UndefOr[Int] = js.undefined
      |                   ^
    """

    s"""
    @js.native
    trait A extends js.Object {
      val a: js.UndefOr[Int] = js.native
      def b: js.UndefOr[Int] = js.native
    }

    @js.native
    class B extends A

    @ScalaJSDefined
    trait C extends B {
      override val a: js.UndefOr[Int] = js.undefined
      override def b: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    s"""
      |newSource1.scala:16: error: Cannot override concrete val a: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override val a: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:17: error: Cannot override concrete def b: scala.scalajs.js.UndefOr[Int] from A in a Scala.js-defined JS trait.
      |      override def b: js.UndefOr[Int] = js.undefined
      |                   ^
    """
  }

  @Test
  def noOptionalDefWithParens: Unit = {
    s"""
    @ScalaJSDefined
    trait A extends js.Object {
      def a(): js.UndefOr[Int] = js.undefined
      def b(x: Int): js.UndefOr[Int] = js.undefined
      def c_=(v: Int): js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    s"""
      |newSource1.scala:7: error: In Scala.js-defined JS traits, defs with parentheses must be abstract.
      |      def a(): js.UndefOr[Int] = js.undefined
      |                                    ^
      |newSource1.scala:8: error: In Scala.js-defined JS traits, defs with parentheses must be abstract.
      |      def b(x: Int): js.UndefOr[Int] = js.undefined
      |                                          ^
      |newSource1.scala:9: error: Raw JS setters must return Unit
      |      def c_=(v: Int): js.UndefOr[Int] = js.undefined
      |          ^
      |newSource1.scala:9: error: In Scala.js-defined JS traits, defs with parentheses must be abstract.
      |      def c_=(v: Int): js.UndefOr[Int] = js.undefined
      |                                            ^
    """
  }

}
