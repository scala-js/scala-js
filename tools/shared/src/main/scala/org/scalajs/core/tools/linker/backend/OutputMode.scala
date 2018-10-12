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

package org.scalajs.core.tools.linker.backend

import org.scalajs.core.tools.javascript.ESLevel

/** JavaScript output mode.
 *
 *  For forward source compatibility with Scala.js 1.x, use the alias
 *  [[org.scalajs.core.tools.linker.ESFeatures]] instead.
 */
sealed abstract class OutputMode {
  import OutputMode._

  def esLevel: ESLevel

  /** Whether to use ECMAScript 2015 features, such as classes and arrow
   *  functions.
   */
  def useECMAScript2015: Boolean = this match {
    case ECMAScript51Global   => false
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
 *  [[org.scalajs.core.tools.linker.ESFeatures]] instead.
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
      ECMAScript6,
      ECMAScript51Global)

  /** Default configuration of the output mode.
   *
   *  - `useECMAScript2015`: false
   */
  val Defaults: OutputMode = ECMAScript51Isolated

  /** Default configuration of the output mode. */
  @deprecated("Use Defaults instead.", "0.6.23")
  val Default = Defaults

  /** Legacy output mode where everything is stored in a global ScalaJS variable.
   *  This is suited to the special Rhino interpreter.
   */
  case object ECMAScript51Global extends OutputMode {
    val esLevel: ESLevel = ESLevel.ES5
  }

  /** Output mode compliant with ECMAScript 5.1 (deprecated alias).
   *
   *  This value is not annotated with `@deprecated` for technical reasons, but
   *  it should be considered as such.
   *
   *  Use `Defaults` instead.
   */
  case object ECMAScript51Isolated extends OutputMode {
    val esLevel: ESLevel = ESLevel.ES5
  }

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
  case object ECMAScript6 extends OutputMode {
    val esLevel: ESLevel = ESLevel.ES6
  }

  /** Output mode compliant with ECMAScript 2015.
   *
   *  This output mode assumes that the target platform supports ECMAScript
   *  2015 (aka ES 6).
   */
  @deprecated("Use `Defaults.withUseECMAScript2015(true)` instead.", "0.6.23")
  val ECMAScript2015: ECMAScript6.type = ECMAScript6

  // Not binary compatible, but source compatible with deprecation
  @deprecated("Support for ES6 Strong Mode was removed. Use ECMAScript6 instead.", "0.6.8")
  lazy val ECMAScript6StrongMode: ECMAScript6.type = ECMAScript6
}
