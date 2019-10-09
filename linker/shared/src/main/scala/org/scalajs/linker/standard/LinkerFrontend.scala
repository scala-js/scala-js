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

package org.scalajs.linker.standard

import scala.concurrent._

import org.scalajs.logging._

import org.scalajs.linker.interface._

/** A frontend for a standard Scala.js linker.
 *
 *  Produces a [[LinkingUnit]].
 *
 *  You probably want to use an instance of [[interface.Linker]], rather than
 *  this low-level class.
 *
 *  Attention: a [[LinkerFrontend]] typically does not cache the IR input. It
 *  is advisable to do so, unless all IR is already in memory.
 */
abstract class LinkerFrontend {
  /** Core specification that this linker frontend implements. */
  val coreSpec: CoreSpec

  /** Link and optionally optimize the given IR to a [[LinkingUnit]]. */
  def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ec: ExecutionContext): Future[LinkingUnit]
}
