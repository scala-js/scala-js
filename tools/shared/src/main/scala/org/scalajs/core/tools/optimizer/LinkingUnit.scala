package org.scalajs.core.tools.optimizer

import org.scalajs.core.ir
import ir.{Definitions, Infos}

final class LinkingUnit(
    val classDefs: List[LinkedClass],
    val infos: Map[String, Infos.ClassInfo],
    val isComplete: Boolean) {

  import LinkingUnit._

  lazy val globalInfo: GlobalInfo = {
    infos.get(Definitions.ClassClass).fold {
      GlobalInfo(
          isParentDataAccessed = false)
    } { classClassInfo =>
      val methodNames = classClassInfo.methods.map(_.encodedName).toSet
      GlobalInfo(
          isParentDataAccessed = methodNames.contains("getSuperclass__jl_Class"))
    }
  }

  def updated(classDefs: List[LinkedClass], isComplete: Boolean): LinkingUnit = {
    val newInfos = infos ++ classDefs.map(cd => cd.encodedName -> cd.toInfo)
    new LinkingUnit(classDefs, newInfos, isComplete)
  }
}

object LinkingUnit {

  final class GlobalInfo private (
      /** Whether the parent data of class data is accessed.
       *  This is true iff the java.lang.Class.getSuperclass() method exists,
       *  since it is the only one that can do it.
       */
      val isParentDataAccessed: Boolean
  )

  object GlobalInfo {
    private[LinkingUnit] def apply(
        isParentDataAccessed: Boolean
    ): GlobalInfo = {
      new GlobalInfo(isParentDataAccessed)
    }

    val SafeApproximation: GlobalInfo =
      apply(true)
  }

}
