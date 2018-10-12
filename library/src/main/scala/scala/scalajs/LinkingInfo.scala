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

package scala.scalajs

object LinkingInfo {

  import scala.scalajs.runtime.linkingInfo

  /** Returns true if we are linking for production, false otherwise.
   *
   *  `productionMode` is always equal to `!developmentMode`.
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write code that should only be
   *  executed in production mode or development mode.
   *
   *  A typical usage of this method is:
   *  {{{
   *  val warningsLogger =
   *    if (productionMode) new NullLogger
   *    else new ConsoleLogger
   *  }}}
   *
   *  At link-time, `productionMode` will either be a constant true, in which
   *  case the above snippet folds into
   *  {{{
   *  val warningsLogger = new NullLogger
   *  }}}
   *  or a constant false, in which case it folds into
   *  {{{
   *  val warningsLogger = new ConsoleLogger.
   *  }}}
   *
   *  @see [[developmentMode]]
   */
  @inline
  def productionMode: Boolean =
    linkingInfo.semantics.productionMode

  /** Returns true if we are linking for development, false otherwise.
   *
   *  `developmentMode` is always equal to `!productionMode`.
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write code that should only be
   *  executed in production mode or development mode.
   *
   *  A typical usage of this method is:
   *  {{{
   *  if (developmentMode) {
   *    performExpensiveSanityChecks()
   *  }
   *  }}}
   *
   *  At link-time, `developmentMode` will either be a constant true, in which
   *  case the above snippet folds into
   *  {{{
   *  performExpensiveSanityChecks()
   *  }}}
   *  or a constant false, in which case it is dead-code-eliminated away,
   *  yielding maximum performance in production.
   *
   *  @see [[productionMode]]
   */
  @inline
  def developmentMode: Boolean =
    !productionMode

  /** Returns true if we are assuming that the target platform supports
   *  ECMAScript 6, false otherwise.
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write polyfills that can be
   *  dead-code-eliminated.
   *
   *  A typical usage of this method is:
   *  {{{
   *  if (assumingES6 || featureTest())
   *    useES6Feature()
   *  else
   *    usePolyfill()
   *  }}}
   *
   *  At link-time, `assumingES6` will either be a constant false, in which
   *  case the above snippet folds into
   *  {{{
   *  if (featureTest())
   *    useES6Feature()
   *  else
   *    usePolyfill()
   *  }}}
   *  or a constant true, in which case it folds into
   *  {{{
   *  useES6Feature()
   *  }}}
   */
  @inline
  def assumingES6: Boolean =
    linkingInfo.assumingES6

}
