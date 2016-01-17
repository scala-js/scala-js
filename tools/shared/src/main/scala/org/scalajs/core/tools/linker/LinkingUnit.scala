package org.scalajs.core.tools.linker

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.ESLevel

import org.scalajs.core.ir
import ir.{Definitions, Infos}

final class LinkingUnit(
    val semantics: Semantics,
    val esLevel: ESLevel,
    val classDefs: List[LinkedClass],
    val infos: Map[String, Infos.ClassInfo],
    val isComplete: Boolean
) {

  import LinkingUnit._

  lazy val globalInfo: GlobalInfo = {
    classDefs.find(_.encodedName == Definitions.ClassClass).fold {
      GlobalInfo(
          isParentDataAccessed = false)
    } { classClassDef =>
      val methodNames = classClassDef.memberMethods.map(_.info.encodedName).toSet
      GlobalInfo(
          isParentDataAccessed = methodNames.contains("getSuperclass__jl_Class"))
    }
  }

  def updated(classDefs: List[LinkedClass], isComplete: Boolean): LinkingUnit = {
    val newInfos = infos ++ classDefs.map(cd => cd.encodedName -> cd.toInfo)
    new LinkingUnit(semantics, esLevel, classDefs, newInfos, isComplete)
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
  }

}
