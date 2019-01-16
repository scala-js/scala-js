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

package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

class NameEncoding {

  @Test def namesThatAreJSReservedWords_issue_153(): Unit = {
    // scalastyle:off class.name

    // Class name
    class break {
      // class variable
      var continue: Int = 1
      // method name
      def switch: Int = {
        // local name
        val default = 2
        default
      }
    }
    trait Foo {
      // static member (through mixin)
      def function: Int = 3
    }

    val x = new break with Foo
    assertEquals(1, x.continue)
    assertEquals(2, x.switch)
    assertEquals(3, x.function)

    // scalastyle:on class.name
  }

  @Test def namesStartingWithDigit_issue_153(): Unit = {
    // scalastyle:off class.name

    // Class name
    class `0` {
      // class variable
      var `1`: Int = 1
      // method name
      def `2`: Int = {
        // local name
        val `22` = 2
        `22`
      }
    }
    trait Foo {
      // static member (through mixin)
      def `3`: Int = 3
    }

    val x = new `0` with Foo
    assertEquals(1, x.`1`)
    assertEquals(2, x.`2`)
    assertEquals(3, x.`3`)

    // scalastyle:on class.name
  }

  @Test def javaIdentPartNotJSIdentPart(): Unit = {
    class JavaIdentPartNotJSIdentPart {
      def `€-start`(): Int = 1
      def `part-€`(): Int = 2
    }

    val x = new JavaIdentPartNotJSIdentPart
    assertEquals(1, x.`€-start`())
    assertEquals(2, x.`part-€`())
  }

  @Test def localEvalOrArguments_issue_743(): Unit = {
    val eval = 5
    assertEquals(5, eval)
    val arguments = "hello"
    assertEquals("hello", arguments)
  }

}
