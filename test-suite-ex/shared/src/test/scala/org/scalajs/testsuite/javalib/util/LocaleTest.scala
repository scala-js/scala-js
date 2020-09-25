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

package org.scalajs.testsuite.javalib.util

import java.util.Locale

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

/** Sanity tests for the dummy implemenation of `java.util.Locale`.
 *
 *  These tests ensure that our dummy implementation of `java.util.Locale`
 *  behaves in an appropriate way. We only test specific behaviors that can
 *  cause tests to "fail to fail" if they are not respected.
 */
class LocaleTest {
  @Test def testLanguageIsNormalizedLowerCase(): Unit = {
    /* Our implementations of `String.toLowerCase(locale: Locale)` and
     * `String.toUpperCase(locale: Locale)` assume that the result of
     * `locale.getLanguage()` is always all-lowercase.
     * This test makes sure that this is indeed the case.
     */

    assertEquals("lt", new Locale("lt").getLanguage())
    assertEquals("lt", new Locale("LT").getLanguage())
    assertEquals("lt", new Locale("lT").getLanguage())
    assertEquals("lt", new Locale("Lt").getLanguage())

    assertEquals("tr", new Locale("tr").getLanguage())
    assertEquals("tr", new Locale("TR").getLanguage())
    assertEquals("tr", new Locale("tR").getLanguage())
    assertEquals("tr", new Locale("Tr").getLanguage())

    assertEquals("az", new Locale("az").getLanguage())
    assertEquals("az", new Locale("AZ").getLanguage())
    assertEquals("az", new Locale("aZ").getLanguage())
    assertEquals("az", new Locale("Az").getLanguage())

    // The normalization itself is locale-insensitive
    // This was locally tested with a JVM configured in Turkish
    assertEquals("it", new Locale("it").getLanguage())
    assertEquals("it", new Locale("IT").getLanguage())
    assertEquals("it", new Locale("iT").getLanguage())
    assertEquals("it", new Locale("It").getLanguage())
  }
}
