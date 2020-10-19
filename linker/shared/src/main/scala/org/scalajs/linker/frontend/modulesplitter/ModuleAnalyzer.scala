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

import org.scalajs.ir.Names.ClassName

import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Classifies a set of classes into Modules. */
private[modulesplitter] abstract class ModuleAnalyzer {
  import ModuleAnalyzer._

  def analyze(info: DependencyInfo): Analysis
}

private[modulesplitter] object ModuleAnalyzer {
  trait Analysis {
    /** Module this class is in.
     *
     *  If this returns None, the class is an abstract class.
     */
    def moduleForClass(className: ClassName): Option[ModuleID]
  }

  final class ClassInfo(val staticDependencies: Set[ClassName],
      val dynamicDependencies: Set[ClassName])

  final class DependencyInfo(
      val classDependencies: Map[ClassName, ClassInfo],
      val publicModuleDependencies: Map[ModuleID, Set[ClassName]]
  )
}
