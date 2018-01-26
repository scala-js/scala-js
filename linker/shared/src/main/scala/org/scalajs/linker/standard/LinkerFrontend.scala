/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js linker            **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.standard

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.irio._

/** A frontend for a standard Scala.js linker.
 *
 *  Produces a [[LinkingUnit]].
 *
 *  You probably want to use an instance of [[Linker]], rather than this
 *  low-level class.
 *
 *  Attention: a [[LinkerFrontend]] typically does not cache the IR input. It
 *  is advisable to do so, unless all IR is already in memory.
 */
abstract class LinkerFrontend {
  /** Core specification that this linker frontend implements. */
  val coreSpec: CoreSpec

  /** Link and optionally optimize the given IR to a [[LinkingUnit]]. */
  def link(irFiles: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger): LinkingUnit
}
