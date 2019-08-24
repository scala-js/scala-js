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

import org.scalajs.ir
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.ModuleInitializer

object TestIRBuilder {
  implicit val noPosition: ir.Position = ir.Position.NoPosition

  val EAF = ApplyFlags.empty

  val emptyOptHints: OptimizerHints = OptimizerHints.empty

  def classDef(
      encodedName: String,
      kind: ClassKind = ClassKind.Class,
      jsClassCaptures: Option[List[ParamDef]] = None,
      superClass: Option[String] = None,
      interfaces: List[String] = Nil,
      jsSuperClass: Option[Tree] = None,
      jsNativeLoadSpec: Option[JSNativeLoadSpec] = None,
      memberDefs: List[MemberDef] = Nil,
      topLevelExportDefs: List[TopLevelExportDef] = Nil): ClassDef = {
    ClassDef(Ident(encodedName), kind, jsClassCaptures,
        superClass.map(Ident(_)), interfaces.map(Ident(_)), jsSuperClass,
        jsNativeLoadSpec, memberDefs, topLevelExportDefs)(
        emptyOptHints)
  }

  def trivialCtor(enclosingClassName: String): MethodDef = {
    val flags = MemberFlags.empty.withNamespace(MemberNamespace.Constructor)
    MethodDef(flags, Ident("init___"), Nil, NoType,
        Some(ApplyStatically(EAF.withConstructor(true),
            This()(ClassType(enclosingClassName)),
            ClassRef(ObjectClass), Ident("init___"), Nil)(NoType)))(
        emptyOptHints, None)
  }

  def mainMethodDef(body: Tree): MethodDef = {
    val stringArrayType = ArrayType(ArrayTypeRef("T", 1))
    val argsParamDef = paramDef("args", stringArrayType)
    MethodDef(MemberFlags.empty, Ident("main__AT__V"), List(argsParamDef),
        NoType, Some(body))(emptyOptHints, None)
  }

  def paramDef(name: String, ptpe: Type): ParamDef =
    ParamDef(Ident(name, Some(name)), ptpe, mutable = false, rest = false)

  def mainModuleInitializers(moduleClassName: String): List[ModuleInitializer] =
    ModuleInitializer.mainMethodWithArgs(moduleClassName, "main") :: Nil
}
