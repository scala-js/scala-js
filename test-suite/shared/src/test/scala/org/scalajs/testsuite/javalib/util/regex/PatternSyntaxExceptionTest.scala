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

package org.scalajs.testsuite.javalib.util.regex

import java.util.regex._

import org.junit.Test
import org.junit.Assert._

class PatternSyntaxExceptionTest {
  def pse(desc: String, regex: String, index: Int): PatternSyntaxException =
    new PatternSyntaxException(desc, regex, index)

  @Test def getIndex(): Unit = {
    assertEquals(2, pse("", "", 2).getIndex())
    assertEquals(-1, pse("", "", -1).getIndex())
  }

  @Test def getDescription(): Unit = {
    assertEquals("foo", pse("foo", "", 0).getDescription())
    assertNull(pse(null, "re", 0).getDescription())
  }

  @Test def getPattern(): Unit = {
    assertEquals("re", pse("desc", "re", 0).getPattern())
    assertNull(pse("desc", null, 0).getPattern())
  }

  @Test def getMessage(): Unit = {
    assertEquals(
        """
        |my description
        |regexp
        """.stripMargin.trim(),
        pse("my description", "regexp", -1).getMessage())

    assertEquals(
        """
        |my description near index 0
        |regexp
        |^
        """.stripMargin.trim(),
        pse("my description", "regexp", 0).getMessage())

    assertEquals(
        """
        |my description near index 3
        |regexp
        |   ^
        """.stripMargin.trim(),
        pse("my description", "regexp", 3).getMessage())

    assertEquals(
        """
        |my description near index 5
        |regexp
        |     ^
        """.stripMargin.trim(),
        pse("my description", "regexp", 5).getMessage())

    assertEquals(
        """
        |my description near index 6
        |regexp
        """.stripMargin.trim(),
        pse("my description", "regexp", 6).getMessage())

    assertEquals(
        """
        |null near index 2
        |regexp
        |  ^
        """.stripMargin.trim(),
        pse(null, "regexp", 2).getMessage())

    assertEquals(
        """
        |null
        |regexp
        """.stripMargin.trim(),
        pse(null, "regexp", -1).getMessage())

    assertEquals(
        """
        |my description near index 2
        |null
        """.stripMargin.trim(),
        pse("my description", null, 2).getMessage())

    assertEquals(
        """
        |my description near index 0
        |null
        """.stripMargin.trim(),
        pse("my description", null, 0).getMessage())

    assertEquals(
        """
        |my description
        |null
        """.stripMargin.trim(),
        pse("my description", null, -1).getMessage())
  }
}
