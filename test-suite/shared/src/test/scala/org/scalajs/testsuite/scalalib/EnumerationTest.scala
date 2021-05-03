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

package org.scalajs.testsuite.scalalib

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class EnumerationTest {

  @Test def valueStringEqualsToString_Issue38(): Unit = {
    object HelpLevel extends Enumeration {
      type HelpLevel = Value
      val None = Value("None")
      val Basic = Value("Basic")
      val Medium = Value("Medium")
      val Full = Value("Full")
    }

    val h = HelpLevel.None

    assertEquals("None", h.toString)
  }

  @Test def valueImplicitEqualsToString(): Unit = {
    object HelpLevel extends Enumeration {
      type HelpLevel = Value
      val None, Basic, Medium, Full = Value
      val Special = Value(100)
      val / = Value
    }

    val h = HelpLevel.Medium
    assertEquals("Medium", h.toString)
    assertEquals("Special", HelpLevel.Special.toString)
    assertEquals("$div", HelpLevel./.toString)
  }

  @Test def valueStringNullToStringResult(): Unit = {
    if (!executingInJVM) {
      object Test extends Enumeration {
        private val nullStr: String = null
        val A = Value(nullStr) // Circumvent compiler replacement and warning
      }

      assertTrue(Test.A.toString.startsWith(
        "<Unknown name for enum field #0 of class "))
    }
  }

  @Test def withNameThrowsForValueStringNull(): Unit = {
    object Test extends Enumeration {
      private val nullStr: String = null
      val A = Value(nullStr) // Circumvent compiler replacement and warning
    }

    if (!executingInJVM) {
      // In the JVM the exception thrown is a ClassCastException
      val ex = assertThrows(classOf[NoSuchElementException], Test.withName("A"))
      val subMsg = "Couldn't find enum field with name A.\n" +
          "However, there were the following unnamed fields:"
      assertTrue(ex.getMessage.contains(subMsg))
    }
  }

  @Test def testToString(): Unit = {
    assertEquals("FooBarEnum", FooBarEnum.toString)
  }

  @Test def valuesToString(): Unit = {
    assertEquals("FooBarEnum.ValueSet(A, B, C, D, E, F)",
        FooBarEnum.values.toString)
  }

  @Test def nextNameAssignment(): Unit = {
    object Test extends Enumeration {
      nextName = Iterator("x","y","z")
      val a, b, c = Value
    }

    assertEquals("x|y|z", Test.values.mkString("|"))
  }

  /** Object is here due to issues with Enumeration.toString inside closures */
  object FooBarEnum extends Enumeration {
    val A, B, C, D, E, F = Value
  }
}
