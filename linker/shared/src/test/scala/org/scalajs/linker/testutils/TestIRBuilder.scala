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
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.ModuleInitializer

object TestIRBuilder {
  implicit val noPosition: ir.Position = ir.Position.NoPosition

  val EAF = ApplyFlags.empty
  val EMF = MemberFlags.empty
  val EOH = OptimizerHints.empty

  def classDef(
      encodedName: ClassName,
      kind: ClassKind = ClassKind.Class,
      jsClassCaptures: Option[List[ParamDef]] = None,
      superClass: Option[ClassName] = None,
      interfaces: List[ClassName] = Nil,
      jsSuperClass: Option[Tree] = None,
      jsNativeLoadSpec: Option[JSNativeLoadSpec] = None,
      memberDefs: List[MemberDef] = Nil,
      topLevelExportDefs: List[TopLevelExportDef] = Nil): ClassDef = {
    ClassDef(ClassIdent(encodedName), kind, jsClassCaptures,
        superClass.map(ClassIdent(_)), interfaces.map(ClassIdent(_)),
        jsSuperClass, jsNativeLoadSpec, memberDefs, topLevelExportDefs)(
        EOH)
  }

  final val MainTestClassDefEncodedName = ClassName("LTest$")

  val MainTestModuleInitializers = mainModuleInitializers("Test")

  def mainTestClassDef(mainBody: Tree): ClassDef = {
    classDef(
        MainTestClassDefEncodedName,
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        memberDefs = List(
            trivialCtor(MainTestClassDefEncodedName),
            mainMethodDef(mainBody)
        )
    )
  }

  def trivialCtor(enclosingClassName: ClassName): MethodDef = {
    val flags = MemberFlags.empty.withNamespace(MemberNamespace.Constructor)
    MethodDef(flags, MethodIdent(NoArgConstructorName), Nil, NoType,
        Some(ApplyStatically(EAF.withConstructor(true),
            This()(ClassType(enclosingClassName)),
            ClassRef(ObjectClass), MethodIdent(NoArgConstructorName),
            Nil)(NoType)))(
        EOH, None)
  }

  def mainMethodDef(body: Tree): MethodDef = {
    val stringArrayType = ArrayType(ArrayTypeRef(ClassRef(BoxedStringClass), 1))
    val argsParamDef = paramDef("args", stringArrayType)
    MethodDef(MemberFlags.empty, "main__AT__V", List(argsParamDef),
        NoType, Some(body))(EOH, None)
  }

  def paramDef(name: LocalName, ptpe: Type): ParamDef =
    ParamDef(LocalIdent(name), ptpe, mutable = false, rest = false)

  def mainModuleInitializers(moduleClassName: String): List[ModuleInitializer] =
    ModuleInitializer.mainMethodWithArgs(moduleClassName, "main") :: Nil

  implicit def string2LocalName(name: String): LocalName =
    LocalName(name)
  implicit def string2LabelName(name: String): LabelName =
    LabelName(name)
  implicit def string2FieldName(name: String): FieldName =
    FieldName(name)
  implicit def string2MethodName(name: String): MethodName =
    MethodName(name)
  implicit def string2ClassName(name: String): ClassName =
    ClassName(name)

  implicit def string2LocalIdent(name: String): LocalIdent =
    LocalIdent(LocalName(name))
  implicit def string2LabelIdent(name: String): LabelIdent =
    LabelIdent(LabelName(name))
  implicit def string2FieldIdent(name: String): FieldIdent =
    FieldIdent(FieldName(name))
  implicit def string2MethodIdent(name: String): MethodIdent =
    MethodIdent(MethodName(name))
  implicit def string2ClassIdent(name: String): ClassIdent =
    ClassIdent(ClassName(name))
}
