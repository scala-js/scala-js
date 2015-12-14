/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class EnumerationTest {

  @Test def should_use_explicit_naming_for_enumerated_values_issue_38(): Unit = {
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

  @Test def should_allow_implicit_naming_for_values(): Unit = {
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

  @Test def should_give_a_pseudo_toString_to_unnamed_values(): Unit = {
    if (!executingInJVM) {
      object Test extends Enumeration {
        private val nullStr: String = null
        val A = Value(nullStr) // Circumvent compiler replacement and warning
      }

      assertTrue(Test.A.toString.startsWith(
        "<Unknown name for enum field #0 of class "))
    }
  }

  @Test def should_give_a_graceful_error_message_upon_name_based_query_when_unnamed_fields_are_present(): Unit = {
    object Test extends Enumeration {
      private val nullStr: String = null
      val A = Value(nullStr) // Circumvent compiler replacement and warning
    }

    if (!executingInJVM) {
      // In the JVM the exception thrown is a ClassCastException
      val ex = expectThrows(classOf[NoSuchElementException], Test.withName("A"))
      val subMsg = "Couldn't find enum field with name A.\n" +
          "However, there were the following unnamed fields:"
      assertTrue(ex.getMessage.contains(subMsg))
    }
  }

  @Test def should_respond_to_toString(): Unit = {
    assertEquals("FooBarEnum", FooBarEnum.toString)
  }

  @Test def should_respond_to_values(): Unit = {
    assertEquals("FooBarEnum.ValueSet(A, B, C, D, E, F)",
        FooBarEnum.values.toString)
  }

  @Test def should_allow_setting_nextName(): Unit = {
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
