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

package org.scalajs.linker.frontend.modulesplitter

import org.scalajs.logging.Logger

import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.standard.{ModuleSet, SymbolRequirement}

trait ModuleSplitter {
  def split(unit: LinkingUnit, symbolRequirements: SymbolRequirement,
      logger: Logger): ModuleSet
}

object ModuleSplitter {
  def min(): ModuleSplitter = new MinModuleSplitter
  def max(): ModuleSplitter = new MaxModuleSplitter
}
