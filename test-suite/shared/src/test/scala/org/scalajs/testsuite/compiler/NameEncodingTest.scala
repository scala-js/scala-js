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

class NameEncodingTest {

  @Test def namesThatAreJSReservedWords_Issue153(): Unit = {
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

  @Test def namesStartingWithDigit_Issue153(): Unit = {
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
    /* The `€` character is a valid character in a Java identifier, like all
     * currency symbols, so it stays as is in `Name`s in the IR. However, it
     * is not a valid JavaScript identifier part, so it needs to be encoded by
     * the emitter.
     */

    class JavaIdentPartNotJSIdentPart {
      val `€-start-field`: Int = 5
      val `part-€-field`: Int = 6

      @noinline def `€-start-method`(): Int = 1
      @noinline def `part-€-method`(): Int = 2
    }

    val x = new JavaIdentPartNotJSIdentPart
    assertEquals(5, x.`€-start-field`)
    val `€-start-local` = x.`€-start-field`
    assertEquals(5, `€-start-local`)
    assertEquals(1, x.`€-start-method`())
    assertEquals(6, x.`part-€-field`)
    val `part-€-local` = x.`part-€-field`
    assertEquals(6, `part-€-local`)
    assertEquals(2, x.`part-€-method`())

    class `ClassWith€Sign`(val value: Int) // scalastyle:ignore

    val obj = new `ClassWith€Sign`(5)
    assertEquals(5, obj.value)
  }

  @Test def localEvalOrArguments_Issue743(): Unit = {
    val eval = 5
    assertEquals(5, eval)
    val arguments = "hello"
    assertEquals("hello", arguments)
  }

}
