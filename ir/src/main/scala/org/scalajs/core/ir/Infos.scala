/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

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
      val ancestors: List[String], // includes this class
      val optimizerHints: OptimizerHints,
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
        optimizerHints: OptimizerHints = OptimizerHints.empty,
        methods: List[MethodInfo] = Nil): ClassInfo = {
      new ClassInfo(name, encodedName, isExported, ancestorCount,
          kind, superClass, ancestors, optimizerHints, methods)
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
      val accessedClassData: List[String],
      val optimizerHints: OptimizerHints
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
        accessedClassData: List[String] = Nil,
        optimizerHints: OptimizerHints = OptimizerHints.empty): MethodInfo = {
      new MethodInfo(encodedName, isStatic, isAbstract, isExported,
          methodsCalled, methodsCalledStatically, staticMethodsCalled,
          instantiatedClasses, accessedModules, accessedClassData,
          optimizerHints)
    }
  }

  final class OptimizerHints(val bits: Int) extends AnyVal {
    import OptimizerHints._

    def inline: Boolean = (bits & InlineMask) != 0
    def noinline: Boolean = (bits & NoinlineMask) != 0

    def withInline(value: Boolean): OptimizerHints =
      if (value) new OptimizerHints(bits | InlineMask)
      else new OptimizerHints(bits & ~InlineMask)

    def withNoinline(value: Boolean): OptimizerHints =
      if (value) new OptimizerHints(bits | NoinlineMask)
      else new OptimizerHints(bits & ~NoinlineMask)

    override def toString(): String =
      s"OptimizerHints($bits)"
  }

  object OptimizerHints {
    final val InlineShift = 0
    final val InlineMask = 1 << InlineShift

    final val NoinlineShift = 1
    final val NoinlineMask = 1 << NoinlineShift

    final val empty: OptimizerHints =
      new OptimizerHints(0)
  }

}
