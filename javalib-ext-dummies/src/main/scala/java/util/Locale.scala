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

package java.util

final class Locale(languageRaw: String, countryRaw: String)
    extends AnyRef with java.lang.Cloneable with java.io.Serializable {

  def this(languageRaw: String) = this(languageRaw, "")

  private[this] val language: String = languageRaw.toLowerCase()

  private[this] val country: String = countryRaw.toUpperCase()

  def getLanguage(): String = language

  def getCountry(): String = country

  override def toString(): String = {
    if (country == "") language
    else language + "_" + country
  }

  override def hashCode(): Int =
    language.## ^ country.##

  override def equals(that: Any): Boolean = that match {
    case that: Locale =>
      this.getLanguage() == that.getLanguage() &&
      this.getCountry() == that.getCountry()
    case _ =>
      false
  }
}

object Locale {
  val ROOT: Locale = new Locale("", "")

  // By specification, the default locale in Scala.js is always `ROOT`.
  def getDefault(): Locale = ROOT
}
