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

package java.text

import java.util.Locale

/** Dummy implementation of `DecimalFormatSymbols`.
 *
 *  It is even worse than most other dummies, in the sense that it
 *  special-cases the locales that we use in our tests (`FormatterTestEx`).
 *  It is incorrect for most locales.
 */
class DecimalFormatSymbols(locale: Locale) extends NumberFormat {
  def getZeroDigit(): Char = {
    locale.getLanguage() match {
      case "hi" =>
        locale.getCountry() match {
          case "IN" => '\u0966' // '०' DEVANAGARI DIGIT ZERO
          case _    => unsupported()
        }
      case "" | "en" | "fr" =>
        '0'
      case _ =>
        unsupported()
    }
  }

  def getGroupingSeparator(): Char = {
    locale.getLanguage() match {
      case "fr"             => '\u00A0' // NO-BREAK SPACE
      case "" | "en" | "hi" => ','
      case _                => unsupported()
    }
  }

  def getDecimalSeparator(): Char = {
    locale.getLanguage() match {
      case "fr"             => ','
      case "" | "en" | "hi" => '.'
      case _                => unsupported()
    }
  }

  private def unsupported(): Nothing =
    throw new Error(s"Unsupported locale '$locale' in DecimalFormatSymbols")
}

object DecimalFormatSymbols {
  def getInstance(locale: Locale): DecimalFormatSymbols =
    new DecimalFormatSymbols(locale)
}
