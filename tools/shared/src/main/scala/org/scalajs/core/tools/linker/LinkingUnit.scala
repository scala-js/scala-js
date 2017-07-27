package org.scalajs.core.tools.linker

import org.scalajs.core.ir
import ir.{Definitions, Infos}

import org.scalajs.core.tools.linker.standard._

final class LinkingUnit private[linker] (
    val coreSpec: CoreSpec,
    val classDefs: List[LinkedClass],
    private[linker] val infos: Map[String, Infos.ClassInfo],
    val moduleInitializers: List[ModuleInitializer]
) {
  private[linker] def updated(classDefs: List[LinkedClass]): LinkingUnit = {
    val newInfos =
      infos ++ classDefs.map(cd => cd.encodedName -> cd.toInfo)
    new LinkingUnit(coreSpec, classDefs, newInfos, moduleInitializers)
  }
}
