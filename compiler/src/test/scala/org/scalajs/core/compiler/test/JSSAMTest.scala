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
  def noSAMAsJSTypeGeneric: Unit = {

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
    """.fails()

  }

  @Test
  def noSAMAsJSType212: Unit = {

    val version = scala.util.Properties.versionNumberString
    assumeTrue(!version.startsWith("2.10.") && !version.startsWith("2.11."))

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
