/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.testutils

import scala.language.implicitConversions

import org.scalajs.ir
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.ModuleInitializer

object TestIRBuilder {
  implicit val noPosition: ir.Position = ir.Position.NoPosition

  val EAF = ApplyFlags.empty
  val EMF = MemberFlags.empty
  val EOH = OptimizerHints.empty
  val NON = NoOriginalName

  val V = VoidRef
  val I = IntRef
  val O = ClassRef(ObjectClass)
  val T = ClassRef(BoxedStringClass)

  def m(name: String, paramTypeRefs: List[TypeRef], resultTypeRef: TypeRef): MethodName =
    MethodName(name, paramTypeRefs, resultTypeRef)

  def classDef(
      className: ClassName,
      kind: ClassKind = ClassKind.Class,
      jsClassCaptures: Option[List[ParamDef]] = None,
      superClass: Option[ClassName] = None,
      interfaces: List[ClassName] = Nil,
      jsSuperClass: Option[Tree] = None,
      jsNativeLoadSpec: Option[JSNativeLoadSpec] = None,
      memberDefs: List[MemberDef] = Nil,
      topLevelExportDefs: List[TopLevelExportDef] = Nil): ClassDef = {
    ClassDef(ClassIdent(className), NON, kind, jsClassCaptures,
        superClass.map(ClassIdent(_)), interfaces.map(ClassIdent(_)),
        jsSuperClass, jsNativeLoadSpec, memberDefs, topLevelExportDefs)(
        EOH)
  }

  final val MainTestClassName = ClassName("Test")

  val MainTestModuleInitializers = mainModuleInitializers("Test")

  def mainTestClassDef(mainBody: Tree): ClassDef = {
    classDef(
        MainTestClassName,
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        memberDefs = List(
            trivialCtor(MainTestClassName),
            mainMethodDef(mainBody)
        )
    )
  }

  def trivialCtor(enclosingClassName: ClassName): MethodDef = {
    val flags = MemberFlags.empty.withNamespace(MemberNamespace.Constructor)
    MethodDef(flags, MethodIdent(NoArgConstructorName), NON, Nil, NoType,
        Some(ApplyStatically(EAF.withConstructor(true),
            This()(ClassType(enclosingClassName)),
            ObjectClass, MethodIdent(NoArgConstructorName),
            Nil)(NoType)))(
        EOH, None)
  }

  def mainMethodDef(body: Tree): MethodDef = {
    val stringArrayTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)
    val stringArrayType = ArrayType(stringArrayTypeRef)
    val argsParamDef = paramDef("args", stringArrayType)
    MethodDef(MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic),
        m("main", List(stringArrayTypeRef), VoidRef), NON,
        List(argsParamDef), NoType, Some(body))(EOH, None)
  }

  def paramDef(name: LocalName, ptpe: Type): ParamDef =
    ParamDef(LocalIdent(name), NON, ptpe, mutable = false, rest = false)

  def mainModuleInitializers(moduleClassName: String): List[ModuleInitializer] =
    ModuleInitializer.mainMethodWithArgs(moduleClassName, "main") :: Nil

  implicit def string2LocalName(name: String): LocalName =
    LocalName(name)
  implicit def string2LabelName(name: String): LabelName =
    LabelName(name)
  implicit def string2FieldName(name: String): FieldName =
    FieldName(name)
  implicit def string2ClassName(name: String): ClassName =
    ClassName(name)

  implicit def string2LocalIdent(name: String): LocalIdent =
    LocalIdent(LocalName(name))
  implicit def string2LabelIdent(name: String): LabelIdent =
    LabelIdent(LabelName(name))
  implicit def string2FieldIdent(name: String): FieldIdent =
    FieldIdent(FieldName(name))
  implicit def string2ClassIdent(name: String): ClassIdent =
    ClassIdent(ClassName(name))

  implicit def methodName2MethodIdent(name: MethodName): MethodIdent =
    MethodIdent(name)

  def int(x: Int): IntLiteral = IntLiteral(x)
}
