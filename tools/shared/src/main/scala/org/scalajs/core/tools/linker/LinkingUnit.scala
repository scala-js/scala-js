package org.scalajs.core.tools.linker

import org.scalajs.core.tools.javascript.ESLevel

import org.scalajs.core.ir
import ir.{Definitions, Infos}

final class LinkingUnit private[linker] (
    val semantics: Semantics,
    val esLevel: ESLevel,
    val classDefs: List[LinkedClass],
    private[linker] val infos: Map[String, Infos.ClassInfo],
    val moduleInitializers: List[ModuleInitializer]
) {
  private[linker] def updated(classDefs: List[LinkedClass]): LinkingUnit = {
    val newInfos =
      infos ++ classDefs.map(cd => cd.encodedName -> cd.toInfo)
    new LinkingUnit(semantics, esLevel, classDefs, newInfos, moduleInitializers)
  }
}
