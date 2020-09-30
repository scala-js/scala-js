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

final class Locale private (languageRaw: String, countryRaw: String,
    variant: String, private val extensions: Map[Char, String])
    extends AnyRef with java.lang.Cloneable with java.io.Serializable {

  def this(languageRaw: String, countryRaw: String, variantRaw: String) =
    this(languageRaw, countryRaw, variantRaw, Collections.emptyMap())

  def this(languageRaw: String, countryRaw: String) =
    this(languageRaw, countryRaw, "")

  def this(languageRaw: String) = this(languageRaw, "", "")

  private[this] val language: String = languageRaw.toLowerCase()

  private[this] val country: String = countryRaw.toUpperCase()

  def getLanguage(): String = language

  def getCountry(): String = country

  def getVariant(): String = variant

  def hasExtensions(): Boolean = !extensions.isEmpty()

  def getExtension(key: Char): String = extensions.get(key) // nullable

  // Not fully compliant, for debugging purposes only
  override def toString(): String = {
    var result = language
    if (country != "" || variant != "" || hasExtensions())
      result += "_" + country
    if (variant != "" || hasExtensions())
      result += "_" + variant

    if (hasExtensions()) {
      import scala.Predef.charWrapper // for `to`

      val keyValues = for {
        key <- 'a' to 'z'
        value = getExtension(key)
        if value != null
      } yield {
        s"$key-$value"
      }

      result += keyValues.mkString("#", "-", "")
    }

    result
  }

  override def hashCode(): Int =
    language.## ^ country.## ^ variant.## ^ extensions.##

  override def equals(that: Any): Boolean = that match {
    case that: Locale =>
      this.getLanguage() == that.getLanguage() &&
      this.getCountry() == that.getCountry() &&
      this.getVariant() == that.getVariant() &&
      this.extensions == that.extensions
    case _ =>
      false
  }
}

object Locale {
  val ROOT: Locale = new Locale("", "")

  // By specification, the default locale in Scala.js is always `ROOT`.
  def getDefault(): Locale = ROOT

  final class Builder {
    private var language: String = ""
    private var country: String = ""
    private var variant: String = ""
    private val extensions = new java.util.HashMap[Char, String]

    def setLanguage(language: String): Builder = {
      this.language = language.toLowerCase()
      this
    }

    def setCountry(country: String): Builder = {
      this.country = country.toUpperCase()
      this
    }

    def setVariant(variant: String): Builder = {
      this.variant = variant
      this
    }

    def setExtension(key: Char, value: String): Builder = {
      extensions.put(key, value)
      this
    }

    def build(): Locale = {
      new Locale(language, country, variant,
          extensions.clone().asInstanceOf[Map[Char, String]])
    }
  }
}
