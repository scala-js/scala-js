/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


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
    /** Whether to parallelize the optimizer (currently fastOptJS only) **/
    val parallel: Boolean = true,
    /** Whether to run the optimizer in batch (i.e. non-incremental) mode */
    val batchMode: Boolean = false,
    /** Whether to run the Scala.js optimizer */
    val disableOptimizer: Boolean = false,
    /** Whether to pretty-print in fullOptJS */
    val prettyPrintFullOptJS: Boolean = false,
    /** Perform expensive checks of the sanity of the Scala.js IR */
    val checkScalaJSIR: Boolean = false
) {

  def withParallel(parallel: Boolean): OptimizerOptions = {
    new OptimizerOptions(parallel, batchMode,
        disableOptimizer, prettyPrintFullOptJS, checkScalaJSIR)
  }

  def withBatchMode(batchMode: Boolean): OptimizerOptions = {
    new OptimizerOptions(parallel, batchMode,
        disableOptimizer, prettyPrintFullOptJS, checkScalaJSIR)
  }

  def withDisableOptimizer(disableOptimizer: Boolean): OptimizerOptions = {
    new OptimizerOptions(parallel, batchMode,
        disableOptimizer, prettyPrintFullOptJS, checkScalaJSIR)
  }

  def withPrettyPrintFullOptJS(prettyPrintFullOptJS: Boolean): OptimizerOptions = {
    new OptimizerOptions(parallel, batchMode,
        disableOptimizer, prettyPrintFullOptJS, checkScalaJSIR)
  }

  def withCheckScalaJSIR(checkScalaJSIR: Boolean): OptimizerOptions = {
    new OptimizerOptions(parallel, batchMode,
        disableOptimizer, prettyPrintFullOptJS, checkScalaJSIR)
  }

  override def toString: String = {
    s"""OptimizerOptions(
       |  parallel             = $parallel
       |  batchMode            = $batchMode
       |  disableOptimizer     = $disableOptimizer
       |  prettyPrintFullOptJS = $prettyPrintFullOptJS
       |  checkScalaJSIR       = $checkScalaJSIR
       |)""".stripMargin
  }

}

object OptimizerOptions {
  def apply() = new OptimizerOptions()
}
