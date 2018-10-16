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

object TestIRBuilder {
  implicit val noPosition: ir.Position = ir.Position.NoPosition

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
    MethodDef(static = false, Ident("init___"), Nil, NoType,
        Some(ApplyStatically(This()(ClassType(enclosingClassName)),
            ClassType(ObjectClass), Ident("init___"), Nil)(NoType)))(
        emptyOptHints, None)
  }

  def mainMethodDef(body: Tree): MethodDef = {
    val stringArrayType = ArrayType(ArrayTypeRef("T", 1))
    val argsParamDef = ParamDef(Ident("args", Some("args")), stringArrayType,
        mutable = false, rest = false)
    MethodDef(static = false, Ident("main__AT__V"), List(argsParamDef),
        NoType, Some(body))(emptyOptHints, None)
  }
}
