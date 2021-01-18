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

  override def extraArgs: List[String] = {
    if (scalaVersion.startsWith("2.11."))
      super.extraArgs :+ "-Xexperimental"
    else
      super.extraArgs
  }

  override def preamble: String =
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._
    """

  @Test
  def noSAMAsJSType211: Unit = {
    assumeTrue(scalaVersion.startsWith("2.11."))

    """
    @js.native
    trait Foo extends js.Object {
      def foo(x: Int): Int
    }

    trait Bar extends js.Object {
      def bar(x: Int): Int
    }

    class A {
      val foo: Foo = x => x + 1
      val Bar: Bar = x => x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:15: error: Non-native JS types cannot directly extend native JS traits.
      |      val foo: Foo = x => x + 1
      |                       ^
      |newSource1.scala:16: error: $anonfun extends scala.Serializable which does not extend js.Any.
      |      val Bar: Bar = x => x + 1
      |                       ^
    """
  }

  @Test
  def noSAMAsJSType212Plus: Unit = {
    assumeTrue(!scalaVersion.startsWith("2.11."))

    """
    @js.native
    trait Foo extends js.Object {
      def foo(x: Int): Int
    }

    trait Bar extends js.Object {
      def bar(x: Int): Int
    }

    class A {
      val foo: Foo = x => x + 1
      val Bar: Bar = x => x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:15: error: Using an anonymous function as a SAM for the JavaScript type Foo is not allowed. Use an anonymous class instead.
      |      val foo: Foo = x => x + 1
      |                       ^
      |newSource1.scala:16: error: Using an anonymous function as a SAM for the JavaScript type Bar is not allowed. Use an anonymous class instead.
      |      val Bar: Bar = x => x + 1
      |                       ^
    """
  }

}
