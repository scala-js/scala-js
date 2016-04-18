/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/* All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */

package scala.scalajs.js

/** A Unicode Normalization Form.
 *
 *  @see [[JSStringOps.normalize]]
 *  @see [[http://www.unicode.org/reports/tr15/ Unicode Normalization Forms]]
 */
@native
sealed trait UnicodeNormalizationForm extends Any

object UnicodeNormalizationForm {
  /** Normalization Form Canonical Composition. */
  final val NFC = "NFC".asInstanceOf[UnicodeNormalizationForm]

  /** Normalization Form Canonical Decomposition. */
  final val NFD = "NFD".asInstanceOf[UnicodeNormalizationForm]

  /** Normalization Form Compatibility Composition. */
  final val NFKC = "NFKC".asInstanceOf[UnicodeNormalizationForm]

  /** Normalization Form Compatibility Decomposition. */
  final val NFKD = "NFKD".asInstanceOf[UnicodeNormalizationForm]
}
