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

package org.scalajs.linker.interface

/** A single Module output specification for a linker run.
 *
 *  The fields [[moduleID]] and [[isInternal]] uniquely identify this output in
 *  a [[Linker]] run.
 *
 *  @param moduleID The ID of the module this linker output represents.
 *
 *  @param isInternal Whether this is an internal (or an external) module.
 *
 *  @param output The [[LinkerOutput]] for this module.
 *
 *  @param moduleName Name of this module for import. A [[Linker]] will use
 *      this name to include this module from one of its dependees. Must be set,
 *      if this module is internal.
 */
final class ModuleLinkerOutput private (
    val moduleID: ModuleOutputSpec.ModuleID,
    val isInternal: Boolean,
    val output: LinkerOutput,
    val moduleName: Option[String]
) {
  private def this(moduleID: ModuleOutputSpec.ModuleID, output: LinkerOutput) =
    this(moduleID, false, output, None)

  def withInternal: ModuleLinkerOutput =
    copy(isInternal = true)

  def withModuleName(moduleName: String): ModuleLinkerOutput =
    copy(moduleName = Some(moduleName))

  private def copy(isInternal: Boolean = isInternal,
      moduleName: Option[String] = moduleName): ModuleLinkerOutput = {
    new ModuleLinkerOutput(moduleID, isInternal, output, moduleName)
  }
}

object ModuleLinkerOutput {
  def apply(moduleID: ModuleOutputSpec.ModuleID, output: LinkerOutput) =
    new ModuleLinkerOutput(moduleID, output)
}
