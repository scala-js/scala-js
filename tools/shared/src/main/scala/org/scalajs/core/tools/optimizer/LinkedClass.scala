/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.collection.mutable

import org.scalajs.core.ir
import ir.Trees._
import ir.Position
import ir.Infos
import ir.ClassKind
import ir.Definitions

/** A ClassDef after linking.
 *
 *  Note that the [[version]] in the LinkedClass does not cover
 *  [[staticMethods]], [[memberMethods]], [[abstractMethods]] and
 *  [[exportedMembers]] as they have their individual versions. (The collections
 *  themselves are not versioned).
 */
final case class LinkedClass(
    // Stuff from Tree
    name: Ident,
    kind: ClassKind,
    superClass: Option[Ident],
    parents: List[Ident],
    fields: List[VarDef],
    staticMethods: List[LinkedMember[MethodDef]],
    memberMethods: List[LinkedMember[MethodDef]],
    abstractMethods: List[LinkedMember[MethodDef]],
    exportedMembers: List[LinkedMember[Tree]],
    classExports: List[Tree],
    classExportInfo: Option[Infos.MethodInfo],
    optimizerHints: OptimizerHints,
    pos: Position,

    // Actual Linking info
    ancestors: List[String],
    hasInstances: Boolean,
    hasRuntimeTypeInfo: Boolean,
    version: Option[String]) {

  // Helpers to give Info-Like access
  def encodedName: String = name.name
  def isExported: Boolean = classExports.nonEmpty

  def toInfo: Infos.ClassInfo = {
    val methodInfos = (
        staticMethods.map(_.info) ++
        memberMethods.map(_.info) ++
        abstractMethods.map(_.info) ++
        exportedMembers.map(_.info) ++
        classExportInfo
    )

    Infos.ClassInfo(encodedName, isExported, kind, superClass.fold("")(_.name),
      parents.map(_.name), methodInfos)
  }
}

object LinkedClass {

  def apply(info: Infos.ClassInfo, classDef: ClassDef,
      ancestors: List[String]): LinkedClass = {

    val memberInfoByName = Map(info.methods.map(m => m.encodedName -> m): _*)

    val fields = mutable.Buffer.empty[VarDef]
    val staticMethods = mutable.Buffer.empty[LinkedMember[MethodDef]]
    val memberMethods = mutable.Buffer.empty[LinkedMember[MethodDef]]
    val abstractMethods = mutable.Buffer.empty[LinkedMember[MethodDef]]
    val exportedMembers = mutable.Buffer.empty[LinkedMember[Tree]]
    val classExports = mutable.Buffer.empty[Tree]

    def linkedMethod(m: MethodDef) = {
      val info = memberInfoByName(m.name.name)
      new LinkedMember(info, m, None)
    }

    def linkedProperty(p: PropertyDef) = {
      val info = memberInfoByName(p.name.name)
      new LinkedMember(info, p, None)
    }

    classDef.defs.foreach {
      // Static methods
      case m: MethodDef if m.static =>
        staticMethods += linkedMethod(m)

      // Fields
      case field @ VarDef(_, _, _, _) =>
        fields += field

      // Normal methods
      case m: MethodDef if m.name.isInstanceOf[Ident] =>
        if (m.body == EmptyTree)
          abstractMethods += linkedMethod(m)
        else
          memberMethods += linkedMethod(m)

      case m: MethodDef if m.name.isInstanceOf[StringLiteral] =>
        exportedMembers += linkedMethod(m)

      case m: PropertyDef =>
        exportedMembers += linkedProperty(m)

      case e: ConstructorExportDef =>
        classExports += e

      case e: ModuleExportDef =>
        classExports += e

      case tree =>
        sys.error(s"Illegal tree in ClassDef of class ${tree.getClass}")
    }

    val classExportInfo =
      memberInfoByName.get(Definitions.ExportedConstructorsName)

    new LinkedClass(
        classDef.name,
        classDef.kind,
        classDef.superClass,
        classDef.parents,
        fields.toList,
        staticMethods.toList,
        memberMethods.toList,
        abstractMethods.toList,
        exportedMembers.toList,
        classExports.toList,
        classExportInfo,
        classDef.optimizerHints,
        classDef.pos,
        ancestors,
        hasInstances = true,
        hasRuntimeTypeInfo = true,
        version = None)
  }

  def dummyParent(encodedName: String, version: Option[String]): LinkedClass = {
    import ir.Trees.{Ident, OptimizerHints}

    implicit val pos = Position.NoPosition

    new LinkedClass(
        name = Ident(encodedName),
        kind = ClassKind.Class,
        superClass = Some(Ident(Definitions.ObjectClass)),
        parents = List(Ident(Definitions.ObjectClass)),
        fields = Nil,
        staticMethods = Nil,
        memberMethods = Nil,
        abstractMethods = Nil,
        exportedMembers = Nil,
        classExports = Nil,
        classExportInfo = None,
        optimizerHints = OptimizerHints.empty,
        pos = Position.NoPosition,
        ancestors = List(Definitions.ObjectClass, encodedName),
        hasInstances = true,
        hasRuntimeTypeInfo = true,
        version = version)
  }

}
