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

package org.scalajs.sbtplugin

import OptimizerOptions._

/** Various options for the Scala.js optimizer tool chain
 *
 *  This is not a case class and does have a private constructor so that we
 *  can add fields in a binary-compatible manner.
 *
 *  Use [[OptimizerOptions.apply]] and the `with` methods to create a configured
 *  instance.
 */
final class OptimizerOptions private (
    /** Whether to only warn if the linker has errors */
    val bypassLinkingErrors: Boolean = false,
    /** Whether to parallelize the optimizer **/
    val parallel: Boolean = OptimizerOptions.DefaultParallel,
    /** Whether to run the optimizer in batch (i.e. non-incremental) mode */
    val batchMode: Boolean = false,
    /** Whether to run the Scala.js optimizer */
    val disableOptimizer: Boolean = false,
    /** Whether to pretty-print in fullOptJS */
    val prettyPrintFullOptJS: Boolean = false,
    /** Perform expensive checks of the sanity of the Scala.js IR */
    val checkScalaJSIR: Boolean = false,
    /** Use Google Closure Backend */
    val useClosureCompiler: Boolean = false
) {

  @deprecated(
      "Bypassing linking errors will not be possible in the next major version.",
      "0.6.6")
  def withBypassLinkingErrors(bypassLinkingErrors: Boolean): OptimizerOptions =
    copy(bypassLinkingErrors = bypassLinkingErrors)

  // Non-deprecated version to call from the sbt plugin
  private[sbtplugin] def withBypassLinkingErrorsInternal(
      bypassLinkingErrors: Boolean): OptimizerOptions = {
    copy(bypassLinkingErrors = bypassLinkingErrors)
  }

  def withParallel(parallel: Boolean): OptimizerOptions =
    copy(parallel = parallel)

  def withBatchMode(batchMode: Boolean): OptimizerOptions =
    copy(batchMode = batchMode)

  def withDisableOptimizer(disableOptimizer: Boolean): OptimizerOptions =
    copy(disableOptimizer = disableOptimizer)

  def withPrettyPrintFullOptJS(prettyPrintFullOptJS: Boolean): OptimizerOptions =
    copy(prettyPrintFullOptJS = prettyPrintFullOptJS)

  def withCheckScalaJSIR(checkScalaJSIR: Boolean): OptimizerOptions =
    copy(checkScalaJSIR = checkScalaJSIR)

  def withUseClosureCompiler(useClosureCompiler: Boolean): OptimizerOptions =
    copy(useClosureCompiler = useClosureCompiler)

  private def copy(bypassLinkingErrors: Boolean = bypassLinkingErrors,
      parallel: Boolean = parallel, batchMode: Boolean = batchMode,
      disableOptimizer: Boolean = disableOptimizer,
      prettyPrintFullOptJS: Boolean = prettyPrintFullOptJS,
      checkScalaJSIR: Boolean = checkScalaJSIR,
      useClosureCompiler: Boolean = useClosureCompiler) = {
    new OptimizerOptions(bypassLinkingErrors, parallel, batchMode,
        disableOptimizer, prettyPrintFullOptJS, checkScalaJSIR,
        useClosureCompiler)
  }

  override def toString: String = {
    s"""OptimizerOptions(
       |  bypassLinkingErrors  = $bypassLinkingErrors
       |  parallel             = $parallel
       |  batchMode            = $batchMode
       |  disableOptimizer     = $disableOptimizer
       |  prettyPrintFullOptJS = $prettyPrintFullOptJS
       |  checkScalaJSIR       = $checkScalaJSIR
       |  useClosureCompiler   = $useClosureCompiler
       |)""".stripMargin
  }

}

object OptimizerOptions {
  /* #2798 -- On Java 9+, the parallel collections on 2.10 die with a
   * `NumberFormatException` and prevent the linker from working.
   *
   * By default, we therefore pre-emptively disable the parallel optimizer in
   * case the parallel collections cannot deal with the current version of
   * Java.
   *
   * TODO This will automatically "fix itself" once we upgrade to sbt 1.x,
   * which uses Scala 2.12. We should get rid of that workaround at that point
   * for tidiness, though.
   */
  private[sbtplugin] val DefaultParallel: Boolean = {
    try {
      scala.util.Properties.isJavaAtLeast("1.8")
      true
    } catch {
      case _: NumberFormatException => false
    }
  }

  def apply(): OptimizerOptions = new OptimizerOptions()
}
