/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.scalajs.ir
import ir.ClassKind
import ir.Infos._

object CoreData {
  val CoreClassesInfo: Seq[ClassInfo] = Seq(ObjectInfo, StringInfo)

  private def ObjectInfo = ClassInfo(
    name = "java.lang.Object",
    encodedName = "java_lang_Object",
    ancestorCount = 0,
    kind = ClassKind.Class,
    superClass = "",
    ancestors = List("java_lang_Object"),
    methods = List(
      MethodInfo("__init__"),
      MethodInfo("init___"),
      MethodInfo("getClass__Ljava_lang_Class"),
      MethodInfo("hashCode__I"),
      MethodInfo("equals__O__Z"),
      MethodInfo("clone__O",
        calledMethods = Map(
          "java_lang_CloneNotSupportedException" -> List("init___")
        ),
        instantiatedClasses = List("java_lang_CloneNotSupportedException"),
        accessedClassData = List("java_lang_Cloneable")
      ),
      MethodInfo("notify__V"),
      MethodInfo("notifyAll__V"),
      MethodInfo("toString__T",
        calledMethods = Map(
          "java_lang_Object" -> List("getClass__Ljava_lang_Class", "hashCode__I"),
          "java_lang_Class" -> List("getName__T")
        )
      ),
      MethodInfo("finalize__V"),
      MethodInfo("clone__",
        calledMethods = Map(
          "java_lang_Object" -> List("clone__O")
        )
      ),
      MethodInfo("notify__",
        calledMethods = Map(
          "java_lang_Object" -> List("notify__V")
        )
      ),
      MethodInfo("notifyAll__",
        calledMethods = Map(
          "java_lang_Object" -> List("notifyAll__V")
        )
      ),
      MethodInfo("finalize__",
        calledMethods = Map(
          "java_lang_Object" -> List("finalize__V")
        )
      )
    )
  )

  private def StringInfo = ClassInfo(
    name = "java.lang.String",
    encodedName = "java_lang_String",
    ancestorCount = 1,
    kind = ClassKind.HijackedClass,
    superClass = "java_lang_Object",
    ancestors = List(
      "java_lang_String", "java_io_Serializable",
      "java_lang_CharSequence", "java_lang_Comparable", "java_lang_Object")
  )
}
