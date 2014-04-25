/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

object Infos {

  sealed class RoughClassInfo protected (
      val name: String,
      val encodedName: String,
      val isExported: Boolean,
      val ancestorCount: Int
  )

  object RoughClassInfo {
    def apply(name: String, encodedName: String, isExported: Boolean,
        ancestorCount: Int): RoughClassInfo = {
      new RoughClassInfo(name, encodedName, isExported, ancestorCount)
    }
  }

  final class ClassInfo protected (
      name: String,
      encodedName: String,
      isExported: Boolean,
      ancestorCount: Int,
      val kind: ClassKind,
      val superClass: String,
      val ancestors: List[String],
      val methods: List[MethodInfo]
  ) extends RoughClassInfo(name, encodedName, isExported, ancestorCount)

  object ClassInfo {
    def apply(
        name: String,
        encodedName: String,
        isExported: Boolean = false,
        ancestorCount: Int = 0,
        kind: ClassKind = ClassKind.Class,
        superClass: String = "",
        ancestors: List[String] = Nil,
        methods: List[MethodInfo] = Nil): ClassInfo = {
      new ClassInfo(name, encodedName, isExported, ancestorCount,
          kind, superClass, ancestors, methods)
    }
  }

  final class MethodInfo private (
      val encodedName: String,
      val isAbstract: Boolean,
      val isExported: Boolean,
      val calledMethods: Map[String, List[String]],
      val calledMethodsStatic: Map[String, List[String]],
      val instantiatedClasses: List[String],
      val accessedModules: List[String],
      val accessedClassData: List[String]
  )

  object MethodInfo {
    def apply(
        encodedName: String,
        isAbstract: Boolean = false,
        isExported: Boolean = false,
        calledMethods: Map[String, List[String]] = Map.empty,
        calledMethodsStatic: Map[String, List[String]] = Map.empty,
        instantiatedClasses: List[String] = Nil,
        accessedModules: List[String] = Nil,
        accessedClassData: List[String] = Nil): MethodInfo = {
      new MethodInfo(encodedName, isAbstract, isExported, calledMethods,
          calledMethodsStatic, instantiatedClasses, accessedModules,
          accessedClassData)
    }
  }

}
