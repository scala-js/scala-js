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
import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

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

    // String.format
    assertEquals(expected, String.format(locale, format, args.asInstanceOf[Seq[AnyRef]]: _*))
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
    /* Since CLDR v34, which is included in JDK 13, the French
     * locale uses U+202F NARROW NO-BREAK SPACE as grouping
     * separator, instead of U+00A0 NO-BREAK SPACE.
     */
    if (!executingInJVMOnLowerThanJDK(13)) {
      // U+202F NARROW NO-BREAK SPACE
      assertF(French, "1\u202F234\u202F567", "%,d", 1234567)
      assertF(French, "1\u202F234\u202F567,89", "%,.2f", 1234567.89)
    }

    assertF(French, "0012", "%04d", 12)
    assertF(French, "0012", "%04d", new BigInteger("12"))

    assertF(French, "0014", "%04o", new BigInteger("12"))
    assertF(French, "-014", "%04o", new BigInteger("-12"))
    assertF(French, "14", "%o", new BigInteger("12"))
    assertF(French, "-14", "%o", new BigInteger("-12"))

    assertF(French, "0x0c", "%#04x", new BigInteger("12"))
    assertF(French, "-0xc", "%#04x", new BigInteger("-12"))
    assertF(French, "12", "%x", new BigInteger("18"))
    assertF(French, "-12", "%x", new BigInteger("-18"))
  }

  @Test def testFormatHindiWithDevanagariDigits(): Unit = {
    // U+0966 DEVANAGARI DIGIT ZERO through U+096F DEVANAGARI DIGIT NINE
    assertF(HindiWithDevanagariDigits, "१,२३४,५६७", "%,d", 1234567)
    assertF(HindiWithDevanagariDigits, "१,२३४,५६७.८९", "%,.2f", 1234567.89)
    assertF(HindiWithDevanagariDigits, "००१२", "%04d", 12)

    assertF(HindiWithDevanagariDigits, "0x0012", "%#06x", 0x12)
    assertF(HindiWithDevanagariDigits, "0X0012", "%#06X", 0x12)
    assertF(HindiWithDevanagariDigits, "000014", "%#06o", 12)

    assertF(HindiWithDevanagariDigits, "0x0012", "%#06x", new BigInteger("18"))
    assertF(HindiWithDevanagariDigits, "-0x012", "%#06x", new BigInteger("-18"))
    assertF(HindiWithDevanagariDigits, "12", "%x", new BigInteger("18"))
    assertF(HindiWithDevanagariDigits, "-12", "%x", new BigInteger("-18"))

    assertF(HindiWithDevanagariDigits, "000014", "%#06o", new BigInteger("12"))
    assertF(HindiWithDevanagariDigits, "-00014", "%#06o", new BigInteger("-12"))
    assertF(HindiWithDevanagariDigits, "14", "%o", new BigInteger("12"))
    assertF(HindiWithDevanagariDigits, "-14", "%o", new BigInteger("-12"))
  }

  @Test def testFormatTurkish(): Unit = {
    assumeFalse("Affected by https://bugs.openjdk.java.net/browse/JDK-8060094",
        executingInJVMOnLowerThanJDK(9))

    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    assertF(Turkish, "TİTLE", "%S", "title")
    assertF(Turkish, "İ", "%C", 'i')

    // But Infinity is not localized
    assertF(Turkish, "INFINITY", "%E", Double.PositiveInfinity)

    // Neither are booleans and null, although it does not matter as they have no 'i'
    assertF(Turkish, "FALSE", "%B", false)
    assertF(Turkish, "TRUE", "%B", true)
    assertF(Turkish, "NULL", "%S", null)
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
