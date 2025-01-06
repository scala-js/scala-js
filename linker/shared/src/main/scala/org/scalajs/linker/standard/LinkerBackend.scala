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

import org.scalajs.linker.interface.{IRFile, OutputDirectory, Report}

/** A backend of a standard Scala.js linker.
 *
 *  Produces a JavaScript file with an optional source map.
 *
 *  You probably want to use an instance of [[interface.Linker]], rather than
 *  this low-level class.
 */
abstract class LinkerBackend {
  /** Symbols this backend needs to be present in the linking unit. */
  val symbolRequirements: SymbolRequirement

  /** Additional IR files to inject for linking, mandated by this back-end.
   *
   *  Example: the standard emitter back-end injects `RuntimeLong.sjsir` and
   *  its companion object, unless it uses `BigInt`s to implement `Long`s.
   */
  def injectedIRFiles: Seq[IRFile]

  /** Emit the given `ModuleSet` to the target output.
   *
   *  The linking unit given to `emit` must:
   *
   *  - have the same `coreSpec` as this linker backend, and
   *  - contain the symbols listed in [[symbolRequirements]].
   *
   *  @param moduleSet `ModuleSet` to emit
   *  @param output Directory to write to
   *  @param logger Logger to use
   */
  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report]

}
