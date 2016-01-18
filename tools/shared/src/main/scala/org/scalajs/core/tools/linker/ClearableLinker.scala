/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker

import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.io._

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.ESLevel
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement

/** A box around a [[GenLinker]] to support clearing.
 *
 *  This further supports:
 *  - batch mode (clearing after every operation)
 *  - clearing if linker throws
 *
 *  This class is not thread-safe.
 */
final class ClearableLinker(newLinker: () => GenLinker, batchMode: Boolean)
    extends GenLinker {

  private[this] var _semantics: Semantics = _
  private[this] var _esLevel: ESLevel = _
  private[this] var _linker: GenLinker = _

  def semantics: Semantics = {
    ensureLinker()
    _semantics
  }

  def esLevel: ESLevel = {
    ensureLinker()
    _esLevel
  }

  def linkUnit(irFiles: Seq[VirtualScalaJSIRFile],
      symbolRequirements: SymbolRequirement, logger: Logger): LinkingUnit = {
    linkerOp(_.linkUnit(irFiles, symbolRequirements, logger))
  }

  def link(irFiles: Seq[VirtualScalaJSIRFile],
      output: WritableVirtualJSFile, logger: Logger): Unit = {
    linkerOp(_.link(irFiles, output, logger))
  }

  def clear(): Unit =
    _linker = null

  @inline
  private[this] def linkerOp[T](op: GenLinker => T): T = {
    ensureLinker()

    try {
      op(_linker)
    } catch {
      // Clear if we throw
      case t: Throwable =>
        clear()
        throw t
    } finally {
      // Clear if we are in batch mode
      if (batchMode)
        clear()
    }
  }

  private def ensureLinker(): Unit = {
    // Ensure we have a linker
    if (_linker == null) {
      val candidate = newLinker()

      if (_semantics == null)
        _semantics = candidate.semantics
      else
        require(_semantics == candidate.semantics, "Linker changed Semantics")

      if (_esLevel == null)
        _esLevel = candidate.esLevel
      else
        require(_esLevel == candidate.esLevel, "Linker changed ESLevel")

      _linker = candidate
    }
  }
}
