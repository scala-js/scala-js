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

import org.scalajs.ir.{Trees => js}

class MatchASTTest extends JSASTTest {

  @Test
  def stripIdentityMatchEndNonUnitResult: Unit = {
    """
    object A {
      def aString: String = "a"
      def foo = aString match {
        case "a" => true
        case "b" => false
      }
    }
    """.hasExactly(1, "local variable") {
      case js.VarDef(_, _, _, _, _) =>
    }
  }

  @Test
  def stripIdentityMatchEndUnitResult: Unit = {
    """
    object A {
      def aString: String = "a"
      def foo = aString match {
        case "a" =>
        case "b" =>
      }
    }
    """.hasExactly(1, "local variable") {
      case js.VarDef(_, _, _, _, _) =>
    }
  }

  @Test
  def matchWithZeroAlternativeInSwitch: Unit = {
    """
    object A {
      def foo(x: Int): Int = (x: @scala.annotation.switch) match {
        case n if n > 5  => n
        case n if n >= 0 => 0
        case n           => -n
      }
    }
    """.hasNot("any match") {
      case js.Match(_, _, _) =>
    }
  }

  @Test
  def matchWithOneAlternativeInSwitch: Unit = {
    """
    object A {
      def foo(x: Int): Int = (x: @scala.annotation.switch) match {
        case -1 => 10
        case n  => n
      }
    }
    """.hasNot("any match") {
      case js.Match(_, _, _) =>
    }
  }

}
