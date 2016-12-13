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
    for (kind <- Seq("class", "trait", "object")) {
      s"""
      @ScalaJSDefined
      $kind A extends js.Object {
        @JSOptional val a1: js.UndefOr[Int] = 5
        @JSOptional val a2: Int = 5

        @JSOptional def b1: js.UndefOr[Int] = 5
        @JSOptional def b2: Int = 5

        @JSOptional var c1: js.UndefOr[Int] = 5
        @JSOptional var c2: Int = 5
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional val a1: js.UndefOr[Int] = 5
        |                                              ^
        |newSource1.scala:8: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional val a2: Int = 5
        |                                  ^
        |newSource1.scala:10: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional def b1: js.UndefOr[Int] = 5
        |                                              ^
        |newSource1.scala:11: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional def b2: Int = 5
        |                                  ^
        |newSource1.scala:13: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional var c1: js.UndefOr[Int] = 5
        |                                              ^
        |newSource1.scala:14: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional var c2: Int = 5
        |                                  ^
      """
    }
  }

  @Test
  def optionalCannotBeAbstract: Unit = {
    for (kind <- Seq("abstract class", "trait")) {
      s"""
      @ScalaJSDefined
      $kind A extends js.Object {
        @JSOptional val a1: js.UndefOr[Int]
        @JSOptional val a2: Int

        @JSOptional def b1: js.UndefOr[Int]
        @JSOptional def b2: Int

        @JSOptional var c1: js.UndefOr[Int]
        @JSOptional var c2: Int
      }
      """ hasErrors
      s"""
        |newSource1.scala:7: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional val a1: js.UndefOr[Int]
        |                        ^
        |newSource1.scala:8: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional val a2: Int
        |                        ^
        |newSource1.scala:10: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional def b1: js.UndefOr[Int]
        |                        ^
        |newSource1.scala:11: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional def b2: Int
        |                        ^
        |newSource1.scala:13: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional var c1: js.UndefOr[Int]
        |                        ^
        |newSource1.scala:14: error: The right-hand-side of an @JSOptional member must be `js.undefined`.
        |        @JSOptional var c2: Int
        |                        ^
      """
    }
  }

  @Test
  def noOverrideNonOptionalWithOptional: Unit = {
    for (childKind <- Seq("class", "trait", "object")) {
      s"""
      @ScalaJSDefined
      abstract class A extends js.Object {
        val a1: js.UndefOr[Int] = 5
        val a2: js.UndefOr[Int]

        def b1: js.UndefOr[Int] = 5
        def b2: js.UndefOr[Int]
      }

      @ScalaJSDefined
      $childKind B extends A {
        @JSOptional override val a1: js.UndefOr[Int] = js.undefined
        @JSOptional override val a2: js.UndefOr[Int] = js.undefined

        @JSOptional override def b1: js.UndefOr[Int] = js.undefined
        @JSOptional override def b2: js.UndefOr[Int] = js.undefined
      }
      """ hasErrors
      s"""
        |newSource1.scala:16: error: Overriding non-optional val a1: scala.scalajs.js.UndefOr[Int] with @JSOptional override val a1: scala.scalajs.js.UndefOr[Int] is not allowed
        |        @JSOptional override val a1: js.UndefOr[Int] = js.undefined
        |                                 ^
        |newSource1.scala:17: error: Overriding non-optional val a2: scala.scalajs.js.UndefOr[Int] with @JSOptional override val a2: scala.scalajs.js.UndefOr[Int] is not allowed
        |        @JSOptional override val a2: js.UndefOr[Int] = js.undefined
        |                                 ^
        |newSource1.scala:19: error: Overriding non-optional def b1: scala.scalajs.js.UndefOr[Int] with @JSOptional override def b1: scala.scalajs.js.UndefOr[Int] is not allowed
        |        @JSOptional override def b1: js.UndefOr[Int] = js.undefined
        |                                 ^
        |newSource1.scala:20: error: Overriding non-optional def b2: scala.scalajs.js.UndefOr[Int] with @JSOptional override def b2: scala.scalajs.js.UndefOr[Int] is not allowed
        |        @JSOptional override def b2: js.UndefOr[Int] = js.undefined
        |                                 ^
      """
    }

    for (childKind <- Seq("class", "trait", "object")) {
      s"""
      @ScalaJSDefined
      trait A extends js.Object {
        val a: js.UndefOr[Int]
        def b: js.UndefOr[Int]
      }

      @ScalaJSDefined
      $childKind B extends A {
        @JSOptional override val a: js.UndefOr[Int] = js.undefined
        @JSOptional override def b: js.UndefOr[Int] = js.undefined
      }
      """ hasErrors
      s"""
        |newSource1.scala:13: error: Overriding non-optional val a: scala.scalajs.js.UndefOr[Int] with @JSOptional override val a: scala.scalajs.js.UndefOr[Int] is not allowed
        |        @JSOptional override val a: js.UndefOr[Int] = js.undefined
        |                                 ^
        |newSource1.scala:14: error: Overriding non-optional def b: scala.scalajs.js.UndefOr[Int] with @JSOptional override def b: scala.scalajs.js.UndefOr[Int] is not allowed
        |        @JSOptional override def b: js.UndefOr[Int] = js.undefined
        |                                 ^
      """
    }
  }

  @Test
  def noOptionalDefWithParens: Unit = {
    s"""
    @ScalaJSDefined
    trait A extends js.Object {
      @JSOptional def a(): js.UndefOr[Int] = js.undefined
      @JSOptional def b(x: Int): js.UndefOr[Int] = js.undefined
      @JSOptional def c_=(v: Int): js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    s"""
      |newSource1.scala:7: error: @JSOptional cannot be used on methods with parentheses
      |      @JSOptional def a(): js.UndefOr[Int] = js.undefined
      |       ^
      |newSource1.scala:8: error: @JSOptional cannot be used on methods with parentheses
      |      @JSOptional def b(x: Int): js.UndefOr[Int] = js.undefined
      |       ^
      |newSource1.scala:9: error: Raw JS setters must return Unit
      |      @JSOptional def c_=(v: Int): js.UndefOr[Int] = js.undefined
      |                      ^
      |newSource1.scala:9: error: @JSOptional cannot be used on methods with parentheses
      |      @JSOptional def c_=(v: Int): js.UndefOr[Int] = js.undefined
      |       ^
    """
  }

}
