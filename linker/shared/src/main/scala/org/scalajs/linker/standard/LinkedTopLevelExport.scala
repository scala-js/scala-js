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

import org.scalajs.ir.Trees.TopLevelExportDef
import org.scalajs.ir.Names.ClassName

final class LinkedTopLevelExport(
    val owningClass: ClassName, val tree: TopLevelExportDef, val staticDependencies: Set[ClassName],
    val externalDependencies: Set[String]
) {
  def exportName: String = tree.topLevelExportName
}
