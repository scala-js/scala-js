/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import OptData._

object CoreData {
  val CoreClassesData: Seq[ClassInfoData] = Seq(ObjectData, StringData)

  private def ObjectData = ClassInfoData(
    name = "java.lang.Object",
    ancestorCount = 0,
    isStaticModule = false,
    isInterface = false,
    isImplClass = false,
    isRawJSType = false,
    encodedName = "java_lang_Object",
    superClass = "",
    ancestors = List("java_lang_Object"),
    isExported = None,
    methods = Map(
      "__init__" -> MethodInfoData(),
      "init___" -> MethodInfoData(),
      "getClass__Ljava_lang_Class" -> MethodInfoData(),
      "hashCode__I" -> MethodInfoData(),
      "equals__O__Z" -> MethodInfoData(),
      "clone__O" -> MethodInfoData(
        calledMethods = Some(Map(
          "java_lang_CloneNotSupportedException" -> List("init___")
        )),
        instantiatedClasses = Some(List("java_lang_CloneNotSupportedException")),
        accessedClassData = Some(List("java_lang_Cloneable"))
      ),
      "notify__V" -> MethodInfoData(),
      "notifyAll__V" -> MethodInfoData(),
      "toString__T" -> MethodInfoData(
        calledMethods = Some(Map(
          "java_lang_Object" -> List("getClass__Ljava_lang_Class", "hashCode__I"),
          "java_lang_Class" -> List("getName__T")
        ))
      ),
      "finalize__V" -> MethodInfoData(),
      "clone__" -> MethodInfoData(
        calledMethods = Some(Map(
          "java_lang_Object" -> List("clone__O")
        ))
      ),
      "notify__" -> MethodInfoData(
        calledMethods = Some(Map(
          "java_lang_Object" -> List("notify__V")
        ))
      ),
      "notifyAll__" -> MethodInfoData(
        calledMethods = Some(Map(
          "java_lang_Object" -> List("notifyAll__V")
        ))
      ),
      "finalize__" -> MethodInfoData(
        calledMethods = Some(Map(
          "java_lang_Object" -> List("finalize__V")
        ))
      )
    )
  )

  private def StringData = ClassInfoData(
    name = "java.lang.String",
    ancestorCount = 1,
    isStaticModule = false,
    isInterface = false,
    isImplClass = false,
    isRawJSType = false,
    encodedName = "java_lang_String",
    superClass = "java_lang_Object",
    ancestors = List(
      "java_lang_String", "java_io_Serializable",
      "java_lang_CharSequence", "java_lang_Comparable", "java_lang_Object"),
    isExported = None,
    methods = Map.empty
  )
}
