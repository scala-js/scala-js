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
final class LinkedClass(
    // Stuff from Tree
    val name: Ident,
    val kind: ClassKind,
    val superClass: Option[Ident],
    val interfaces: List[Ident],
    val jsName: Option[String],
    val fields: List[FieldDef],
    val staticMethods: List[LinkedMember[MethodDef]],
    val memberMethods: List[LinkedMember[MethodDef]],
    val abstractMethods: List[LinkedMember[MethodDef]],
    val exportedMembers: List[LinkedMember[Tree]],
    val classExports: List[Tree],
    val classExportInfo: Option[Infos.MethodInfo],
    val optimizerHints: OptimizerHints,
    val pos: Position,

    // Actual Linking info
    val ancestors: List[String],
    val hasInstances: Boolean,
    val hasInstanceTests: Boolean,
    val hasRuntimeTypeInfo: Boolean,
    val version: Option[String]) {

  // Helpers to give Info-Like access
  def encodedName: String = name.name
  def isExported: Boolean = classExports.nonEmpty

  def fullName: String = Definitions.decodeClassName(encodedName)

  def toInfo: Infos.ClassInfo = {
    val methodInfos = (
        staticMethods.map(_.info) ++
        memberMethods.map(_.info) ++
        abstractMethods.map(_.info) ++
        exportedMembers.map(_.info) ++
        classExportInfo
    )

    Infos.ClassInfo(encodedName, isExported, kind, superClass.map(_.name),
      interfaces.map(_.name), methodInfos)
  }

  def copy(
      name: Ident = this.name,
      kind: ClassKind = this.kind,
      superClass: Option[Ident] = this.superClass,
      interfaces: List[Ident] = this.interfaces,
      jsName: Option[String] = this.jsName,
      fields: List[FieldDef] = this.fields,
      staticMethods: List[LinkedMember[MethodDef]] = this.staticMethods,
      memberMethods: List[LinkedMember[MethodDef]] = this.memberMethods,
      abstractMethods: List[LinkedMember[MethodDef]] = this.abstractMethods,
      exportedMembers: List[LinkedMember[Tree]] = this.exportedMembers,
      classExports: List[Tree] = this.classExports,
      classExportInfo: Option[Infos.MethodInfo] = this.classExportInfo,
      optimizerHints: OptimizerHints = this.optimizerHints,
      pos: Position = this.pos,
      ancestors: List[String] = this.ancestors,
      hasInstances: Boolean = this.hasInstances,
      hasInstanceTests: Boolean = this.hasInstanceTests,
      hasRuntimeTypeInfo: Boolean = this.hasRuntimeTypeInfo,
      version: Option[String] = this.version) = {
    new LinkedClass(
        name,
        kind,
        superClass,
        interfaces,
        jsName,
        fields,
        staticMethods,
        memberMethods,
        abstractMethods,
        exportedMembers,
        classExports,
        classExportInfo,
        optimizerHints,
        pos,
        ancestors,
        hasInstances,
        hasInstanceTests,
        hasRuntimeTypeInfo,
        version)
  }
}

object LinkedClass {

  def apply(info: Infos.ClassInfo, classDef: ClassDef,
      ancestors: List[String]): LinkedClass = {

    val memberInfoByName = Map(info.methods.map(m => m.encodedName -> m): _*)

    val fields = mutable.Buffer.empty[FieldDef]
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
      case field @ FieldDef(_, _, _) =>
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
        classDef.interfaces,
        classDef.jsName,
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
        hasInstanceTests = true,
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
        interfaces = Nil,
        jsName = None,
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
        hasInstanceTests = true,
        hasRuntimeTypeInfo = true,
        version = version)
  }

}
