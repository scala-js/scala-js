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
import org.scalajs.nscplugin.test.util.VersionDependentUtils.scalaVersion

import org.junit.Assume._
import org.junit.Test

// scalastyle:off line.size.limit

class JSSAMTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._
    """

  @Test
  def noSAMAsJSType: Unit = {
    """
    @js.native
    trait Foo extends js.Object {
      def foo(x: Int): Int
    }

    trait Bar extends js.Object {
      def bar(x: Int): Int
    }

    class Foobar extends js.Function {
      def foobar(x: Int): Int
    }

    class A {
      val foo: Foo = x => x + 1
      val bar: Bar = x => x + 1
      val foobar: Foobar = x => x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:19: error: Using an anonymous function as a SAM for the JavaScript type Foo is not allowed because it is not a trait extending js.Function. Use an anonymous class instead.
      |      val foo: Foo = x => x + 1
      |                       ^
      |newSource1.scala:20: error: Using an anonymous function as a SAM for the JavaScript type Bar is not allowed because it is not a trait extending js.Function. Use an anonymous class instead.
      |      val bar: Bar = x => x + 1
      |                       ^
      |newSource1.scala:21: error: Using an anonymous function as a SAM for the JavaScript type Foobar is not allowed because it is not a trait extending js.Function. Use an anonymous class instead.
      |      val foobar: Foobar = x => x + 1
      |                             ^
    """
  }

  @Test
  def noSAMOfNativeJSFunctionType: Unit = {
    """
    @js.native
    trait Foo extends js.Function {
      def apply(x: Int): Int
    }

    @js.native
    trait Bar extends js.Function {
      def bar(x: Int = 5): Int
    }

    class A {
      val foo: Foo = x => x + 1
      val bar: Bar = x => x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:16: error: Using an anonymous function as a SAM for the JavaScript type Foo is not allowed because it is a native JS type. It is not possible to directly implement it.
      |      val foo: Foo = x => x + 1
      |                       ^
      |newSource1.scala:17: error: Using an anonymous function as a SAM for the JavaScript type Bar is not allowed because it is a native JS type. It is not possible to directly implement it.
      |      val bar: Bar = x => x + 1
      |                       ^
    """
  }

  @Test
  def noSAMOfNonApplyJSType: Unit = {
    """
    trait Foo extends js.Function {
      def foo(x: Int): Int
    }

    class A {
      val foo: Foo = x => x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: Using an anonymous function as a SAM for the JavaScript type Foo is not allowed because its single abstract method is not named `apply`. Use an anonymous class instead.
      |      val foo: Foo = x => x + 1
      |                       ^
    """
  }

  @Test
  def missingThisArgForJSThisFunction: Unit = {
    """
    trait BadThisFunction1 extends js.ThisFunction {
      def apply(): Int
    }

    trait BadThisFunction2 extends js.ThisFunction {
      def apply(args: Int*): Int
    }

    class A {
      val badThisFunction1: BadThisFunction1 = () => 42
      val badThisFunction2: BadThisFunction2 = args => args.size
    }
    """ hasErrors
    """
      |newSource1.scala:14: error: The SAM or apply method for a js.ThisFunction must have a leading non-varargs parameter
      |      val badThisFunction1: BadThisFunction1 = () => 42
      |                                                  ^
      |newSource1.scala:15: error: The SAM or apply method for a js.ThisFunction must have a leading non-varargs parameter
      |      val badThisFunction2: BadThisFunction2 = args => args.size
      |                                                    ^
    """
  }

  @Test
  def noNonsensicalJSFunctionTypes: Unit = {
    """
    class BadFunctionIsClass extends js.Function {
      def apply(x: Int): Int
    }

    trait BadFunctionExtendsNonFunction extends js.Object {
      def apply(x: Int): Int
    }

    class SubclassOfFunction extends js.Function

    trait BadFunctionExtendsSubclassOfFunction extends SubclassOfFunction {
      def apply(x: Int): Int
    }

    trait BadFunctionParametricMethod extends js.Function {
      def apply[A](x: A): A
    }

    trait BadFunctionOverloaded extends js.Function {
      def apply(x: Int): Int
      def apply(x: String): String
    }

    trait BadFunctionMultipleAbstract extends js.Function {
      def apply(x: Int): Int
      def foo(x: Int): Int
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: A non-native JS type can only declare an abstract method named `apply` without `@JSName` if it is the SAM of a trait that extends js.Function
      |      def apply(x: Int): Int
      |          ^
      |newSource1.scala:10: error: A non-native JS type can only declare an abstract method named `apply` without `@JSName` if it is the SAM of a trait that extends js.Function
      |      def apply(x: Int): Int
      |          ^
      |newSource1.scala:16: error: A non-native JS type can only declare an abstract method named `apply` without `@JSName` if it is the SAM of a trait that extends js.Function
      |      def apply(x: Int): Int
      |          ^
      |newSource1.scala:20: error: A non-native JS type can only declare an abstract method named `apply` without `@JSName` if it is the SAM of a trait that extends js.Function
      |      def apply[A](x: A): A
      |          ^
      |newSource1.scala:24: error: A non-native JS type can only declare an abstract method named `apply` without `@JSName` if it is the SAM of a trait that extends js.Function
      |      def apply(x: Int): Int
      |          ^
      |newSource1.scala:25: error: A non-native JS type can only declare an abstract method named `apply` without `@JSName` if it is the SAM of a trait that extends js.Function
      |      def apply(x: String): String
      |          ^
      |newSource1.scala:29: error: A non-native JS type can only declare an abstract method named `apply` without `@JSName` if it is the SAM of a trait that extends js.Function
      |      def apply(x: Int): Int
      |          ^
    """
  }

}
