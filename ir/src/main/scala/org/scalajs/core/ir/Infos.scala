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
      val isAbstract: Boolean,
      val isExported: Boolean,
      val calledMethods: Map[String, List[String]],
      val calledMethodsStatic: Map[String, List[String]],
      val instantiatedClasses: List[String],
      val accessedModules: List[String],
      val accessedClassData: List[String],
      val optimizerHints: OptimizerHints
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
        accessedClassData: List[String] = Nil,
        optimizerHints: OptimizerHints = OptimizerHints.empty): MethodInfo = {
      new MethodInfo(encodedName, isAbstract, isExported, calledMethods,
          calledMethodsStatic, instantiatedClasses, accessedModules,
          accessedClassData, optimizerHints)
    }
  }

  final class OptimizerHints(val bits: Int) extends AnyVal {
    import OptimizerHints._

    private[scalajs] def isAccessor: Boolean = (bits & AccessorMask) != 0
    private[scalajs] def hasInlineAnnot: Boolean = (bits & InlineAnnotMask) != 0

    private[scalajs] def copy(
        isAccessor: Boolean = this.isAccessor,
        hasInlineAnnot: Boolean = this.hasInlineAnnot
    ): OptimizerHints = {
      var bits: Int = 0
      if (isAccessor)
        bits |= AccessorMask
      if (hasInlineAnnot)
        bits |= InlineAnnotMask
      new OptimizerHints(bits)
    }

    override def toString(): String =
      s"OptimizerHints($bits)"
  }

  object OptimizerHints {
    private final val AccessorShift = 0
    private final val AccessorMask = 1 << AccessorShift

    private final val InlineAnnotShift = 1
    private final val InlineAnnotMask = 1 << InlineAnnotShift

    final val empty: OptimizerHints =
      new OptimizerHints(0)
  }

}
