/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.ir

import Trees._

final class EntryPointsInfo(
    val encodedName: String,
    val hasEntryPoint: Boolean
)

object EntryPointsInfo {
  def forClassDef(classDef: ClassDef): EntryPointsInfo = {
    val hasEntryPoint = {
      classDef.topLevelExportDefs.nonEmpty ||
      classDef.memberDefs.exists {
        case m: MethodDef =>
          m.static && m.encodedName == Definitions.StaticInitializerName
        case _ =>
          false
      }
    }
    new EntryPointsInfo(classDef.name.name, hasEntryPoint)
  }
}
