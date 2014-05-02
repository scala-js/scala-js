/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.corelib

import scala.scalajs.ir
import ir.ClassKind
import ir.Infos._

object CoreData {
  val CoreClassesInfo: Seq[ClassInfo] = Seq(ObjectInfo, StringInfo)

  private def ObjectInfo = ClassInfo(
    name = "java.lang.Object",
    encodedName = "O",
    ancestorCount = 0,
    kind = ClassKind.Class,
    superClass = "",
    ancestors = List("O"),
    methods = List(
      MethodInfo("__init__"),
      MethodInfo("init___"),
      MethodInfo("getClass__jl_Class"),
      MethodInfo("hashCode__I"),
      MethodInfo("equals__O__Z"),
      MethodInfo("clone__O",
        calledMethods = Map(
          "jl_CloneNotSupportedException" -> List("init___")
        ),
        instantiatedClasses = List("jl_CloneNotSupportedException"),
        accessedClassData = List("jl_Cloneable")
      ),
      MethodInfo("notify__V"),
      MethodInfo("notifyAll__V"),
      MethodInfo("toString__T",
        calledMethods = Map(
          "O" -> List("getClass__jl_Class", "hashCode__I"),
          "jl_Class" -> List("getName__T")
        )
      ),
      MethodInfo("finalize__V"),
      MethodInfo("clone__",
        calledMethods = Map(
          "O" -> List("clone__O")
        )
      ),
      MethodInfo("notify__",
        calledMethods = Map(
          "O" -> List("notify__V")
        )
      ),
      MethodInfo("notifyAll__",
        calledMethods = Map(
          "O" -> List("notifyAll__V")
        )
      ),
      MethodInfo("finalize__",
        calledMethods = Map(
          "O" -> List("finalize__V")
        )
      )
    )
  )

  private def StringInfo = ClassInfo(
    name = "java.lang.String",
    encodedName = "T",
    ancestorCount = 1,
    kind = ClassKind.HijackedClass,
    superClass = "O",
    ancestors = List(
      "T", "Ljava_io_Serializable", "jl_CharSequence", "jl_Comparable", "O")
  )
}
