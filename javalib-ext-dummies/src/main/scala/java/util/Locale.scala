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

final class Locale private (languageRaw: String)
    extends AnyRef with java.lang.Cloneable with java.io.Serializable {

  private[this] val language: String = languageRaw.toLowerCase()

  def getLanguage(): String = language
}
