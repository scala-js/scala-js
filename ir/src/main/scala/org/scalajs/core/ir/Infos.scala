/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

object Infos {

  final class ClassInfo private (
      val encodedName: String,
      val isExported: Boolean,
      val kind: ClassKind,
      val superClass: String,
      val parents: List[String], // does not include this class
      val methods: List[MethodInfo]
  )

  object ClassInfo {
    def apply(
        encodedName: String,
        isExported: Boolean = false,
        kind: ClassKind = ClassKind.Class,
        superClass: String = "",
        parents: List[String] = Nil,
        methods: List[MethodInfo] = Nil): ClassInfo = {
      new ClassInfo(encodedName, isExported, kind, superClass,
          parents, methods)
    }
  }

  final class MethodInfo private (
      val encodedName: String,
      val isStatic: Boolean,
      val isAbstract: Boolean,
      val isExported: Boolean,
      val methodsCalled: Map[String, List[String]],
      val methodsCalledStatically: Map[String, List[String]],
      val staticMethodsCalled: Map[String, List[String]],
      val instantiatedClasses: List[String],
      val accessedModules: List[String],
      val accessedClassData: List[String]
  )

  object MethodInfo {
    def apply(
        encodedName: String,
        isStatic: Boolean = false,
        isAbstract: Boolean = false,
        isExported: Boolean = false,
        methodsCalled: Map[String, List[String]] = Map.empty,
        methodsCalledStatically: Map[String, List[String]] = Map.empty,
        staticMethodsCalled: Map[String, List[String]] = Map.empty,
        instantiatedClasses: List[String] = Nil,
        accessedModules: List[String] = Nil,
        accessedClassData: List[String] = Nil): MethodInfo = {
      new MethodInfo(encodedName, isStatic, isAbstract, isExported,
          methodsCalled, methodsCalledStatically, staticMethodsCalled,
          instantiatedClasses, accessedModules, accessedClassData)
    }
  }

}
