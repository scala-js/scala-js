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

class LinkTimeIfTest extends TestHelpers {
  override def preamble: String = "import scala.scalajs.LinkingInfo._"

  // scalastyle:off line.size.limit
  @Test
  def linkTimeErrorInvalidOp(): Unit = {
    """
    object A {
      def foo =
        linkTimeIf((esVersion + 1) < ESVersion.ES2015) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Invalid operation '$plus' inside linkTimeIf. Only '==', '!=', '>', '>=', '<', '<=' operations are allowed for integer values in linkTimeIf.
      |        linkTimeIf((esVersion + 1) < ESVersion.ES2015) { } { }
      |                              ^
    """

    """
    object A {
      def foo =
        linkTimeIf(productionMode | true) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Invalid operation '$bar' inside linkTimeIf. Only '==', '!=', '&&', and '||' operations are allowed for boolean values in linkTimeIf.
      |        linkTimeIf(productionMode | true) { } { }
      |                                  ^
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
    """
      |newSource1.scala:5: error: Invalid identifier bar inside linkTimeIf. Only @linkTimeProperty annotated values can be used in linkTimeIf.
      |        linkTimeIf(bar == 0) { } { }
      |                   ^
    """

    """
    object A {
      def foo(x: String) =
        linkTimeIf("foo" == x) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Invalid literal "foo" inside linkTimeIf. Only boolean and int values can be used in linkTimeIf.
      |        linkTimeIf("foo" == x) { } { }
      |                   ^
      |newSource1.scala:4: error: Invalid identifier x inside linkTimeIf. Only @linkTimeProperty annotated values can be used in linkTimeIf.
      |        linkTimeIf("foo" == x) { } { }
      |                            ^
    """

    """
    object A {
      def bar = true
      def foo(x: String) =
        linkTimeIf(bar || !bar) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Invalid identifier inside linkTimeIf. Only @linkTimeProperty annotated values can be used in linkTimeIf.
      |        linkTimeIf(bar || !bar) { } { }
      |                   ^
      |newSource1.scala:5: error: Invalid identifier inside linkTimeIf. Only @linkTimeProperty annotated values can be used in linkTimeIf.
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
        linkTimeIf(if(bar) true else false) { } { }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Only @linkTimeProperty annotated values, int and boolean constants, and binary operations are allowd in linkTimeIf.
      |        linkTimeIf(if(bar) true else false) { } { }
      |                   ^
    """
  }
}
