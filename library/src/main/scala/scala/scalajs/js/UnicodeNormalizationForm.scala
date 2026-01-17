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

/* All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */

package scala.scalajs.js

import scala.scalajs.js

/** A Unicode Normalization Form.
 *
 *  @see [[JSStringOps js.JSStringOps.normalize]]
 *  @see [[http://www.unicode.org/reports/tr15/ Unicode Normalization Forms]]
 */
sealed trait UnicodeNormalizationForm extends js.Any

object UnicodeNormalizationForm {

  /** Normalization Form Canonical Composition. */
  final val NFC = "NFC".asInstanceOf[js.UnicodeNormalizationForm]

  /** Normalization Form Canonical Decomposition. */
  final val NFD = "NFD".asInstanceOf[js.UnicodeNormalizationForm]

  /** Normalization Form Compatibility Composition. */
  final val NFKC = "NFKC".asInstanceOf[js.UnicodeNormalizationForm]

  /** Normalization Form Compatibility Decomposition. */
  final val NFKD = "NFKD".asInstanceOf[js.UnicodeNormalizationForm]
}
