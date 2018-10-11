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

package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._

import org.junit.Test
import org.junit.Ignore

// scalastyle:off line.size.limit

class JSNativeByDefaultTest extends DirectTest with TestHelpers {

  /* We add the compiler's output path itself to the classpath, for tests
   * involving separate compilation.
   */
  override def classpath: List[String] =
    super.classpath ++ List(testOutputPath)

  override def extraArgs: List[String] =
    super.extraArgs.filter(_ != "-P:scalajs:sjsDefinedByDefault")

  override def preamble: String =
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._
    """

  @Test
  def warnNoJSNativeAnnotation: Unit = {

    """
    class A extends js.Object
    """ containsWarns
    """
      |newSource1.scala:5: warning: Classes, traits and objects inheriting from js.Any should be annotated with @js.native, unless they have @ScalaJSDefined. The default will switch to Scala.js-defined in the next major version of Scala.js.
      |    class A extends js.Object
      |          ^
    """

    """
    object A extends js.Object
    """ containsWarns
    """
      |newSource1.scala:5: warning: Classes, traits and objects inheriting from js.Any should be annotated with @js.native, unless they have @ScalaJSDefined. The default will switch to Scala.js-defined in the next major version of Scala.js.
      |    object A extends js.Object
      |           ^
    """

    """
    trait A extends js.Object
    """ hasWarns
    """
      |newSource1.scala:5: warning: Classes, traits and objects inheriting from js.Any should be annotated with @js.native, unless they have @ScalaJSDefined. The default will switch to Scala.js-defined in the next major version of Scala.js.
      |    trait A extends js.Object
      |          ^
    """

  }

  @Test
  def treatedAsJSNative: Unit = {

    """
    @JSGlobal
    class A extends js.Object {
      def foo(): Int = 42
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Classes, traits and objects inheriting from js.Any should be annotated with @js.native, unless they have @ScalaJSDefined. The default will switch to Scala.js-defined in the next major version of Scala.js.
      |    class A extends js.Object {
      |          ^
      |newSource1.scala:7: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      def foo(): Int = 42
      |                       ^
    """

    """
    @JSGlobal
    object A extends js.Object {
      def foo(): Int = 42
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Classes, traits and objects inheriting from js.Any should be annotated with @js.native, unless they have @ScalaJSDefined. The default will switch to Scala.js-defined in the next major version of Scala.js.
      |    object A extends js.Object {
      |           ^
      |newSource1.scala:7: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      def foo(): Int = 42
      |                       ^
    """

    """
    trait A extends js.Object {
      def foo(): Int = 42
    }
    """ hasWarns
    """
      |newSource1.scala:5: warning: Classes, traits and objects inheriting from js.Any should be annotated with @js.native, unless they have @ScalaJSDefined. The default will switch to Scala.js-defined in the next major version of Scala.js.
      |    trait A extends js.Object {
      |          ^
      |newSource1.scala:6: warning: Members of traits, classes and objects extending js.Any may only contain members that call js.native. This will be enforced in 1.0.
      |      def foo(): Int = 42
      |                       ^
    """

  }

  @Test
  def noExtendNativeTrait: Unit = {
    """
    trait NativeTrait extends js.Object

    @ScalaJSDefined
    class A extends NativeTrait

    @ScalaJSDefined
    trait B extends NativeTrait

    @ScalaJSDefined
    object C extends NativeTrait

    object Container {
      val x = new NativeTrait {}
    }
    """ hasErrors
    """
      |newSource1.scala:5: warning: Classes, traits and objects inheriting from js.Any should be annotated with @js.native, unless they have @ScalaJSDefined. The default will switch to Scala.js-defined in the next major version of Scala.js.
      |    trait NativeTrait extends js.Object
      |          ^
      |newSource1.scala:7: warning: @ScalaJSDefined is deprecated: add `-P:scalajs:sjsDefinedByDefault` to your scalac options and simply remove `@ScalaJSDefined`
      |    @ScalaJSDefined
      |     ^
      |newSource1.scala:8: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |    class A extends NativeTrait
      |          ^
      |newSource1.scala:10: warning: @ScalaJSDefined is deprecated: add `-P:scalajs:sjsDefinedByDefault` to your scalac options and simply remove `@ScalaJSDefined`
      |    @ScalaJSDefined
      |     ^
      |newSource1.scala:11: error: A Scala.js-defined JS trait cannot directly extend a native JS trait.
      |    trait B extends NativeTrait
      |          ^
      |newSource1.scala:13: warning: @ScalaJSDefined is deprecated: add `-P:scalajs:sjsDefinedByDefault` to your scalac options and simply remove `@ScalaJSDefined`
      |    @ScalaJSDefined
      |     ^
      |newSource1.scala:14: error: A Scala.js-defined JS object cannot directly extend a native JS trait.
      |    object C extends NativeTrait
      |           ^
      |newSource1.scala:17: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |      val x = new NativeTrait {}
      |                  ^
    """
  }

  @Test
  def noExtendNativeTraitSeparateCompilation: Unit = {
    """
    trait NativeTrait extends js.Object
    """.succeeds()

    """
    @ScalaJSDefined
    class A extends NativeTrait

    @ScalaJSDefined
    trait B extends NativeTrait

    @ScalaJSDefined
    object C extends NativeTrait

    object Container {
      val x = new NativeTrait {}
    }
    """ hasErrors
    """
      |newSource1.scala:5: warning: @ScalaJSDefined is deprecated: add `-P:scalajs:sjsDefinedByDefault` to your scalac options and simply remove `@ScalaJSDefined`
      |    @ScalaJSDefined
      |     ^
      |newSource1.scala:6: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |    class A extends NativeTrait
      |          ^
      |newSource1.scala:8: warning: @ScalaJSDefined is deprecated: add `-P:scalajs:sjsDefinedByDefault` to your scalac options and simply remove `@ScalaJSDefined`
      |    @ScalaJSDefined
      |     ^
      |newSource1.scala:9: error: A Scala.js-defined JS trait cannot directly extend a native JS trait.
      |    trait B extends NativeTrait
      |          ^
      |newSource1.scala:11: warning: @ScalaJSDefined is deprecated: add `-P:scalajs:sjsDefinedByDefault` to your scalac options and simply remove `@ScalaJSDefined`
      |    @ScalaJSDefined
      |     ^
      |newSource1.scala:12: error: A Scala.js-defined JS object cannot directly extend a native JS trait.
      |    object C extends NativeTrait
      |           ^
      |newSource1.scala:15: error: A Scala.js-defined JS class cannot directly extend a native JS trait.
      |      val x = new NativeTrait {}
      |                  ^
    """
  }

}
