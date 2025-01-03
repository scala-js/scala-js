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

import util._

import org.junit.Test
import org.junit.Assert._

// scalastyle:off line.size.limit

class LinkTimeIfTest extends TestHelpers {
  override def preamble: String = "import scala.scalajs.LinkingInfo._"

  private final val IllegalLinkTimeIfArgMessage = {
    "Illegal operation in the condition of a linkTimeIf. " +
    "Valid operations are: boolean and int primitives; " +
    "references to link-time properties; " +
    "primitive operations on booleans; " +
    "and comparisons on ints."
  }

  @Test
  def linkTimeErrorInvalidOp(): Unit = {
    """
    object A {
      def foo =
        linkTimeIf((esVersion + 1) < ESVersion.ES2015) { } { }
    }
    """ hasErrors
    s"""
      |newSource1.scala:4: error: $IllegalLinkTimeIfArgMessage
      |        linkTimeIf((esVersion + 1) < ESVersion.ES2015) { } { }
      |                              ^
    """
  }

  @Test
  def linkTimeErrorInvalidEntities(): Unit = {
    """
    object A {
      def foo(x: String) = {
        val bar = 1
        linkTimeIf(bar == 0) { } { }
      }
    }
    """ hasErrors
    s"""
      |newSource1.scala:5: error: $IllegalLinkTimeIfArgMessage
      |        linkTimeIf(bar == 0) { } { }
      |                   ^
    """

    """
    object A {
      def foo(x: String) =
        linkTimeIf("foo" == x) { } { }
    }
    """ hasErrors
    s"""
      |newSource1.scala:4: error: $IllegalLinkTimeIfArgMessage
      |        linkTimeIf("foo" == x) { } { }
      |                         ^
    """

    """
    object A {
      def bar = true
      def foo(x: String) =
        linkTimeIf(bar || !bar) { } { }
    }
    """ hasErrors
    s"""
      |newSource1.scala:5: error: $IllegalLinkTimeIfArgMessage
      |        linkTimeIf(bar || !bar) { } { }
      |                   ^
      |newSource1.scala:5: error: $IllegalLinkTimeIfArgMessage
      |        linkTimeIf(bar || !bar) { } { }
      |                           ^
    """
  }

  @Test
  def linkTimeCondInvalidTree(): Unit = {
    """
    object A {
      def bar = true
      def foo(x: String) =
        linkTimeIf(if (bar) true else false) { } { }
    }
    """ hasErrors
    s"""
      |newSource1.scala:5: error: $IllegalLinkTimeIfArgMessage
      |        linkTimeIf(if (bar) true else false) { } { }
      |                   ^
    """
  }
}
