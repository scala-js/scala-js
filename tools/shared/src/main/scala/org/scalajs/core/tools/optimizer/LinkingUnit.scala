package org.scalajs.core.tools.optimizer

import org.scalajs.core.ir
import ir.Infos

final class LinkingUnit(
    val classDefs: List[LinkedClass],
    val infos: Map[String, Infos.ClassInfo],
    val isComplete: Boolean) {

  def updated(classDefs: List[LinkedClass], isComplete: Boolean): LinkingUnit = {
    val newInfos = infos ++ classDefs.map(cd => cd.encodedName -> cd.toInfo)
    new LinkingUnit(classDefs, newInfos, isComplete)
  }
}
