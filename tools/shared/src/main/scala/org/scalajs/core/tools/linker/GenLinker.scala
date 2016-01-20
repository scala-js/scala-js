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

/** Common supertrait of [[Linker]] and [[ClearableLinker]].
 *
 *  Essentially anything that has the [[link]] and [[linkUnit]] methods.
 */
trait GenLinker {
  def semantics: Semantics
  def esLevel: ESLevel

  def linkUnit(irFiles: Seq[VirtualScalaJSIRFile],
      symbolRequirements: SymbolRequirement, logger: Logger): LinkingUnit

  def link(irFiles: Seq[VirtualScalaJSIRFile],
      output: WritableVirtualJSFile, logger: Logger): Unit
}
