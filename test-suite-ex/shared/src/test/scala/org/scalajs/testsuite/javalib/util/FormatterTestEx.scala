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

import java.util.{Formatter, Locale}

import org.junit.Test
import org.junit.Assert._

/** Additional tests for java.lang.String that require `java.util.Locale`
 *  as well as classes in `java.text.*`.
 */
class FormatterTestEx {

  /* Note: there is no test for a grouping size != 3, because I (sjrd) could
   * not find any locale for which it would be different from 3.
   */

  val French = new Locale("fr")  // decimal sep ','  grouping sep '\u00A0'
  val Turkish = new Locale("tr") // special uppercase behavior

  // non-ASCII digits
  val HindiWithDevanagariDigits = {
    new Locale.Builder()
      .setLanguage("hi")
      .setExtension('u', "nu-deva")
      .build()
  }

  def assertF(locale: Locale, expected: String, format: String, args: Any*): Unit = {
    // Locale passed as constructor parameter
    val fmt1 = new Formatter(locale)
    val res1 = fmt1.format(format, args.asInstanceOf[Seq[AnyRef]]: _*).toString()
    fmt1.close()
    assertEquals(expected, res1)

    // Locale passed as argument to `format`
    val fmt2 = new Formatter()
    val res2 = fmt2.format(locale, format, args.asInstanceOf[Seq[AnyRef]]: _*).toString()
    fmt2.close()
    assertEquals(expected, res2)
  }

  @Test def testLocale(): Unit = {
    assertEquals(Locale.ROOT, new Formatter().locale())

    assertSame(Locale.ROOT, new Formatter(Locale.ROOT).locale())
    assertSame(French, new Formatter(French).locale())
    assertSame(null, new Formatter(null: Locale).locale())

    // Calling `format` with an explicit locale does not change the `locale()`
    val formatter = new Formatter(French)
    formatter.format(HindiWithDevanagariDigits, "")
    assertSame(French, formatter.locale())
  }

  @Test def testFormatFrench(): Unit = {
    // U+00A0 NO-BREAK SPACE
    assertF(French, "1\u00A0234\u00A0567", "%,d", 1234567)
    assertF(French, "1\u00A0234\u00A0567,89", "%,.2f", 1234567.89)
    assertF(French, "0012", "%04d", 12)
  }

  @Test def testFormatHindiWithDevanagariDigits(): Unit = {
    // U+0966 DEVANAGARI DIGIT ZERO through U+096F DEVANAGARI DIGIT NINE
    assertF(HindiWithDevanagariDigits, "१,२३४,५६७", "%,d", 1234567)
    assertF(HindiWithDevanagariDigits, "१,२३४,५६७.८९", "%,.2f", 1234567.89)
    assertF(HindiWithDevanagariDigits, "००१२", "%04d", 12)

    assertF(HindiWithDevanagariDigits, "0x0012", "%#06x", 0x12)
    assertF(HindiWithDevanagariDigits, "0X0012", "%#06X", 0x12)
    assertF(HindiWithDevanagariDigits, "000014", "%#06o", 12)
  }

  @Test def testFormatTurkish(): Unit = {
    // Uppercasing does not follow the locale
    assertF(Turkish, "TITLE", "%S", "title")
    assertF(Turkish, "INFINITY", "%E", Double.PositiveInfinity)
  }

  @Test def testFormatNullLocale(): Unit = {
    assertF(null, "1,234,567", "%,d", 1234567)
    assertF(null, "1,234,567.89", "%,.2f", 1234567.89)
    assertF(null, "0012", "%04d", 12)

    assertF(null, "0x0012", "%#06x", 0x12)
    assertF(null, "0X0012", "%#06X", 0x12)
    assertF(null, "000014", "%#06o", 12)
  }

}
