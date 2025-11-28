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

import java.lang.StringBuilder
import java.util.regex._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class RegexMatcherTestOnJDK11 {

  @Test def appendReplacementAndAppendTailStringBuilder(): Unit = {
    // From the JavaDoc
    val matcher = Pattern.compile("cat").matcher("one cat two cats in the yard")
    val sb = new StringBuilder()

    while (matcher.find()) {
      matcher.appendReplacement(sb, "dog")
    }
    matcher.appendTail(sb)

    assertEquals("one dog two dogs in the yard", sb.toString)
  }

  @Test def replaceAllWithReplacer(): Unit = {
    val matcher = Pattern
      .compile("a*b")
      .matcher("aabfooaabfooabfoob")
      .region(0, 3) // should be discarded
    assertEquals("0foo6foo12foo17", matcher.replaceAll(m => m.start().toString()))
  }

  @Test def replaceFirstWithReplacer(): Unit = {
    val matcher = Pattern
      .compile("dog")
      .matcher("zzzdogzzzdogzzz")
      .region(0, 3) // should be discarded
    assertEquals("zzz3zzzdogzzz", matcher.replaceFirst(m => m.start().toString()))
  }
}
