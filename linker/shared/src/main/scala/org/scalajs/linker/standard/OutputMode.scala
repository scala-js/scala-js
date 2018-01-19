/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.standard

/** JavaScript output mode. */
sealed abstract class OutputMode

object OutputMode {
  /** All the available output modes.
   *  There are listed in decreasing order of "importance", as judged by
   *  whoever maintains the back-ends.
   */
  val All = List(
      ECMAScript51Isolated,
      ECMAScript6)

  /** The default output mode. This is always the first element of [[All]] */
  val Default = All.head

  /** Output mode compliant with ECMAScript 5.1 (deprecated alias).
   *
   *  This value is not annotated with `@deprecated` for technical reasons, but
   *  it should be considered as such. Use [[ECMAScript51]] instead.
   */
  case object ECMAScript51Isolated extends OutputMode

  /** Output mode compliant with ECMAScript 5.1.
   *
   *  This is the default output mode. It assumes that the target platform
   *  supports ECMAScript 5.1, ideally with correct handling of strict mode.
   */
  val ECMAScript51: ECMAScript51Isolated.type = ECMAScript51Isolated

  /** Output mode compliant with ECMAScript 2015 (deprecated alias).
   *
   *  This value is not annotated with `@deprecated` for technical reasons, but
   *  it should be considered as such. Use [[ECMAScript2015]] instead.
   */
  case object ECMAScript6 extends OutputMode

  /** Output mode compliant with ECMAScript 2015.
   *
   *  This output mode assumes that the target platform supports ECMAScript
   *  2015 (aka ES 6).
   */
  val ECMAScript2015: ECMAScript6.type = ECMAScript6
}
