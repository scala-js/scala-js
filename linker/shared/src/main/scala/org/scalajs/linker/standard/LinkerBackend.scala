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

import org.scalajs.linker.interface.LinkerOutput

/** A backend of a standard Scala.js linker.
 *
 *  Produces a JavaScript file with an optional source map.
 *
 *  You probably want to use an instance of [[interface.Linker]], rather than
 *  this low-level class.
 */
abstract class LinkerBackend {

  /** Core specification that this linker backend implements. */
  val coreSpec: CoreSpec

  /** Symbols this backend needs to be present in the linking unit. */
  val symbolRequirements: SymbolRequirement

  /** Emit the given [[LinkingUnit]] to the target output.
   *
   *  The linking unit given to `emit` must:
   *
   *  - have the same `coreSpec` as this linker backend, and
   *  - contain the symbols listed in [[symbolRequirements]].
   *
   *  @param unit [[LinkingUnit]] to emit
   *  @param output File to write to
   *  @param logger Logger to use
   */
  def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit]

  /** Verify that a [[LinkingUnit]] can be processed by this [[LinkerBackend]].
   *
   *  Currently, this only tests that the linking unit core specification
   *  matches [[coreSpec]].
   *
   *  In the future, this test could be extended to test [[symbolRequirements]]
   *  too.
   *
   *  @throws java.lang.IllegalArgumentException if there is a mismatch
   */
  protected def verifyUnit(unit: LinkingUnit): Unit = {
    require(unit.coreSpec == coreSpec,
        "LinkingUnit and LinkerBackend must agree on their core specification")
  }

}
