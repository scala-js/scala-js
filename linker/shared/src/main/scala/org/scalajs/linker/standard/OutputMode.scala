/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.standard

/** JavaScript output mode.
 *
 *  For forward source compatibility with Scala.js 1.x, use the alias
 *  [[org.scalajs.linker.ESFeatures]] instead.
 */
sealed abstract class OutputMode {
  import OutputMode._

  /** Whether to use ECMAScript 2015 features, such as classes and arrow
   *  functions.
   */
  def useECMAScript2015: Boolean = this match {
    case ECMAScript51Isolated => false
    case ECMAScript6          => true
  }

  def withUseECMAScript2015(useECMAScript2015: Boolean): OutputMode =
    if (useECMAScript2015) ECMAScript6
    else ECMAScript51Isolated
}

/** Factory for `OutputMode`s.
 *
 *  For forward source compatibility with Scala.js 1.x, use the alias
 *  [[org.scalajs.linker.ESFeatures]] instead.
 */
object OutputMode {
  /** All the available output modes.
   *  There are listed in decreasing order of "importance", as judged by
   *  whoever maintains the back-ends.
   */
  @deprecated(
      "The notion that there is a list of existing output modes is going away.",
      "0.6.23")
  val All = List(
      ECMAScript51Isolated,
      ECMAScript6)

  /** Default configuration of the output mode.
   *
   *  - `useECMAScript2015`: false
   */
  val Defaults: OutputMode = ECMAScript51Isolated

  /** Default configuration of the output mode. */
  @deprecated("Use Defaults instead.", "0.6.23")
  val Default = Defaults

  /** Output mode compliant with ECMAScript 5.1 (deprecated alias).
   *
   *  This value is not annotated with `@deprecated` for technical reasons, but
   *  it should be considered as such.
   *
   *  Use `Defaults` instead.
   */
  case object ECMAScript51Isolated extends OutputMode

  /** Output mode compliant with ECMAScript 5.1.
   *
   *  This is the default output mode. It assumes that the target platform
   *  supports ECMAScript 5.1, ideally with correct handling of strict mode.
   */
  @deprecated("Use Defaults instead.", "0.6.23")
  val ECMAScript51: ECMAScript51Isolated.type = ECMAScript51Isolated

  /** Output mode compliant with ECMAScript 2015 (deprecated alias).
   *
   *  This value is not annotated with `@deprecated` for technical reasons, but
   *  it should be considered as such.
   *
   *  Use `Defaults.withUseECMAScript2015(true)` instead.
   */
  case object ECMAScript6 extends OutputMode

  /** Output mode compliant with ECMAScript 2015.
   *
   *  This output mode assumes that the target platform supports ECMAScript
   *  2015 (aka ES 6).
   */
  @deprecated("Use `Defaults.withUseECMAScript2015(true)` instead.", "0.6.23")
  val ECMAScript2015: ECMAScript6.type = ECMAScript6
}
