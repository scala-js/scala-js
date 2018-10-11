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

// scalastyle:off line.size.limit

/* This is a copy of MissingJSGlobalDeprecationsTest, but with all the
 * relevant deprecations suppressed. This tests the suppression mechanism.
 */
class MissingJSGlobalDeprecationsSuppressedTest
    extends DirectTest with TestHelpers {

  override def extraArgs: List[String] =
    super.extraArgs :+ "-P:scalajs:suppressMissingJSGlobalDeprecations"

  override def preamble: String =
    """import scala.scalajs.js, js.annotation._
    """

  @Test
  def noWarnNoAnnotClass: Unit = {
    """
    @js.native
    class A extends js.Object

    @js.native
    abstract class B extends js.Object
    """.hasNoWarns
  }

  @Test
  def noWarnNoAnnotObject: Unit = {
    """
    @js.native
    object A extends js.Object
    """.hasNoWarns
  }

  @Test
  def noWarnJSNameClass: Unit = {
    """
    @js.native
    @JSName("Foo")
    class A extends js.Object

    @js.native
    @JSName("Foo")
    abstract class B extends js.Object
    """.hasNoWarns
  }

  @Test
  def noWarnJSNameObject: Unit = {
    """
    @js.native
    @JSName("Foo")
    object A extends js.Object
    """.hasNoWarns
  }

  @Test
  def noWarnJSNameNestedClass: Unit = {
    """
    object Enclosing {
      @js.native
      @JSName("Foo")
      class A extends js.Object

      @js.native
      @JSName("Foo")
      abstract class B extends js.Object
    }
    """.hasNoWarns
  }

  @Test
  def noWarnJSNameNestObject: Unit = {
    """
    object Enclosing {
      @js.native
      @JSName("Foo")
      object A extends js.Object
    }
    """.hasNoWarns
  }

}
