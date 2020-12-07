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
      |newSource1.scala:6: error: Members of non-native JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      val a1: js.UndefOr[Int] = 5
      |                                ^
      |newSource1.scala:7: error: Members of non-native JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      val a2: Int = 5
      |                    ^
      |newSource1.scala:9: error: Members of non-native JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      def b1: js.UndefOr[Int] = 5
      |                                ^
      |newSource1.scala:10: error: Members of non-native JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      def b2: Int = 5
      |                    ^
      |newSource1.scala:12: error: Members of non-native JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      var c1: js.UndefOr[Int] = 5
      |                                ^
      |newSource1.scala:13: error: Members of non-native JS traits must either be abstract, or their right-hand-side must be `js.undefined`.
      |      var c2: Int = 5
      |                    ^
    """
  }

  @Test // #4319
  def optionalDefaultParamRequiresUndefinedRHS: Unit = {
    s"""
    trait A extends js.Object {
      def a(x: js.UndefOr[Int] = 1): Int
      def b(x: String = "foo"): Unit
      def c(x: js.UndefOr[Int] = js.undefined): Int // ok
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Members of non-native JS traits may not have default parameters unless their default is `js.undefined`.
      |      def a(x: js.UndefOr[Int] = 1): Int
      |                                 ^
      |newSource1.scala:7: error: Members of non-native JS traits may not have default parameters unless their default is `js.undefined`.
      |      def b(x: String = "foo"): Unit
      |                        ^
    """
  }

  @Test
  def noOptionalLazyVal: Unit = {
    s"""
    trait A extends js.Object {
      lazy val a1: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    s"""
      |newSource1.scala:6: error: A non-native JS trait cannot contain lazy vals
      |      lazy val a1: js.UndefOr[Int] = js.undefined
      |               ^
    """
  }

  @Test
  def noOverrideConcreteNonOptionalWithOptional: Unit = {
    s"""
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
    s"""
      |newSource1.scala:14: error: Cannot override concrete val a1: scala.scalajs.js.UndefOr[Int] from A in a non-native JS trait.
      |      override val a1: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:17: error: Cannot override concrete def b1: scala.scalajs.js.UndefOr[Int] from A in a non-native JS trait.
      |      override def b1: js.UndefOr[Int] = js.undefined
      |                   ^
    """

    s"""
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
    s"""
      |newSource1.scala:13: error: Cannot override concrete val a: scala.scalajs.js.UndefOr[Int] from A in a non-native JS trait.
      |      override val a: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:14: error: Cannot override concrete def b: scala.scalajs.js.UndefOr[Int] from A in a non-native JS trait.
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
    @JSGlobal
    class B extends A

    trait C extends B {
      override val a: js.UndefOr[Int] = js.undefined
      override def b: js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    s"""
      |newSource1.scala:16: error: Cannot override concrete val a: scala.scalajs.js.UndefOr[Int] from A in a non-native JS trait.
      |      override val a: js.UndefOr[Int] = js.undefined
      |                   ^
      |newSource1.scala:17: error: Cannot override concrete def b: scala.scalajs.js.UndefOr[Int] from A in a non-native JS trait.
      |      override def b: js.UndefOr[Int] = js.undefined
      |                   ^
    """
  }

  @Test
  def noOptionalDefWithParens: Unit = {
    s"""
    trait A extends js.Object {
      def a(): js.UndefOr[Int] = js.undefined
      def b(x: Int): js.UndefOr[Int] = js.undefined
      def c_=(v: Int): js.UndefOr[Int] = js.undefined
    }
    """ hasErrors
    s"""
      |newSource1.scala:6: error: In non-native JS traits, defs with parentheses must be abstract.
      |      def a(): js.UndefOr[Int] = js.undefined
      |                                    ^
      |newSource1.scala:7: error: In non-native JS traits, defs with parentheses must be abstract.
      |      def b(x: Int): js.UndefOr[Int] = js.undefined
      |                                          ^
      |newSource1.scala:8: error: JS setters must return Unit
      |      def c_=(v: Int): js.UndefOr[Int] = js.undefined
      |          ^
      |newSource1.scala:8: error: In non-native JS traits, defs with parentheses must be abstract.
      |      def c_=(v: Int): js.UndefOr[Int] = js.undefined
      |                                            ^
    """
  }

}
