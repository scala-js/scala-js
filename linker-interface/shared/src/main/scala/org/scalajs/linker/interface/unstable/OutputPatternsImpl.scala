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

package org.scalajs.linker.interface.unstable

import org.scalajs.linker.interface.OutputPatterns

object OutputPatternsImpl {
  def jsFile(patterns: OutputPatterns, moduleID: String): String =
    patterns.jsFile.format(moduleID)

  def sourceMapFile(patterns: OutputPatterns, moduleID: String): String =
    patterns.sourceMapFile.format(moduleID)

  def moduleName(patterns: OutputPatterns, moduleID: String): String =
    patterns.moduleName.format(moduleID)

  def jsFileURI(patterns: OutputPatterns, moduleID: String): String =
    patterns.jsFileURI.format(moduleID)

  def sourceMapURI(patterns: OutputPatterns, moduleID: String): String =
    patterns.sourceMapURI.format(moduleID)
}
