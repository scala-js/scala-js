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
import org.scalajs.ir.Hashers
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.interface.ModuleInitializer

object TestIRBuilder {
  implicit val noPosition: ir.Position = ir.Position.NoPosition

  val EAF = ApplyFlags.empty
  val EMF = MemberFlags.empty
  val EOH = OptimizerHints.empty
  val NON = NoOriginalName
  val UNV = ir.Version.Unversioned

  val JSCtorFlags = EMF.withNamespace(MemberNamespace.Constructor)

  val V = VoidRef
  val I = IntRef
  val Z = BooleanRef
  val O = ClassRef(ObjectClass)
  val T = ClassRef(BoxedStringClass)
  val AT = ArrayTypeRef(ClassRef(BoxedStringClass), 1)

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
    fields: List[AnyFieldDef] = Nil,
    methods: List[MethodDef] = Nil,
    jsConstructor: Option[JSConstructorDef] = None,
    jsMethodProps: List[JSMethodPropDef] = Nil,
    jsNativeMembers: List[JSNativeMemberDef] = Nil,
    topLevelExportDefs: List[TopLevelExportDef] = Nil,
    optimizerHints: OptimizerHints = EOH
  ): ClassDef = {
    val notHashed = ClassDef(ClassIdent(className), NON, kind, jsClassCaptures,
        superClass.map(ClassIdent(_)), interfaces.map(ClassIdent(_)),
        jsSuperClass, jsNativeLoadSpec, fields, methods, jsConstructor,
        jsMethodProps, jsNativeMembers, topLevelExportDefs)(
        optimizerHints)
    Hashers.hashClassDef(notHashed)
  }

  final val MainTestClassName = ClassName("Test")

  val MainTestModuleInitializers = mainModuleInitializers("Test")

  def mainTestClassDef(mainBody: Tree): ClassDef = {
    classDef(
        MainTestClassName,
        kind = ClassKind.Class,
        superClass = Some(ObjectClass),
        methods = List(
            trivialCtor(MainTestClassName),
            mainMethodDef(mainBody)
        )
    )
  }

  def trivialCtor(enclosingClassName: ClassName,
      parentClassName: ClassName = ObjectClass,
      forModuleClass: Boolean = false): MethodDef = {
    val flags = MemberFlags.empty.withNamespace(MemberNamespace.Constructor)
    val superCtorCall = trivialSuperCtorCall(enclosingClassName, parentClassName)
    val body =
      if (forModuleClass) Block(superCtorCall, StoreModule())
      else superCtorCall
    MethodDef(flags, MethodIdent(NoArgConstructorName), NON, Nil, VoidType, Some(body))(
        EOH, UNV)
  }

  def trivialSuperCtorCall(enclosingClassName: ClassName,
      parentClassName: ClassName = ObjectClass): ApplyStatically = {
    ApplyStatically(EAF.withConstructor(true),
        thisFor(enclosingClassName),
        parentClassName, MethodIdent(NoArgConstructorName),
        Nil)(VoidType)
  }

  def trivialJSCtor(forModuleClass: Boolean = false): JSConstructorDef = {
    val afterSuper =
      if (forModuleClass) StoreModule() :: Undefined() :: Nil
      else Undefined() :: Nil
    JSConstructorDef(JSCtorFlags, Nil, None,
        JSConstructorBody(Nil, JSSuperConstructorCall(Nil), afterSuper))(
        EOH, UNV)
  }

  val MainMethodName: MethodName = m("main", List(AT), VoidRef)

  def mainMethodDef(body: Tree): MethodDef = {
    val argsParamDef = paramDef("args", ArrayType(AT, nullable = true))
    MethodDef(MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic),
        MainMethodName, NON, List(argsParamDef), VoidType, Some(body))(
        EOH, UNV)
  }

  def consoleLog(expr: Tree): Tree =
    JSMethodApply(JSGlobalRef("console"), str("log"), List(expr))

  def systemOutPrintln(expr: Tree): Tree = {
    val PrintStreamClass = ClassName("java.io.PrintStream")
    val outMethodName = m("out", Nil, ClassRef(PrintStreamClass))
    val printlnMethodName = m("println", List(O), VoidRef)

    val out = ApplyStatic(EAF, "java.lang.System", outMethodName, Nil)(
        ClassType(PrintStreamClass, nullable = true))
    Apply(EAF, out, printlnMethodName, List(expr))(VoidType)
  }

  def paramDef(name: LocalName, ptpe: Type): ParamDef =
    ParamDef(LocalIdent(name), NON, ptpe, mutable = false)

  def mainModuleInitializers(moduleClassName: String): List[ModuleInitializer] =
    ModuleInitializer.mainMethodWithArgs(moduleClassName, "main") :: Nil

  val JSObjectLikeClass = ClassName("JSObject")

  val JSObjectLikeClassDef: ClassDef = {
    classDef(
      JSObjectLikeClass,
      kind = ClassKind.NativeJSClass,
      superClass = Some(ObjectClass),
      jsNativeLoadSpec = Some(JSNativeLoadSpec.Global("Object", Nil))
    )
  }

  def requiredMethods(className: ClassName, classKind: ClassKind,
      parentClassName: ClassName = ObjectClass): List[MethodDef] = {
    if (classKind == ClassKind.ModuleClass)
      List(trivialCtor(className, parentClassName, forModuleClass = true))
    else
      Nil
  }

  def requiredJSConstructor(classKind: ClassKind): Option[JSConstructorDef] = {
    if (classKind.isJSClass) Some(trivialJSCtor(forModuleClass = classKind.hasModuleAccessor))
    else None
  }

  def thisFor(cls: ClassName): VarRef =
    This()(ClassType(cls, nullable = false))

  implicit def string2LocalName(name: String): LocalName =
    LocalName(name)
  implicit def string2LabelName(name: String): LabelName =
    LabelName(name)
  implicit def string2SimpleFieldName(name: String): SimpleFieldName =
    SimpleFieldName(name)
  implicit def string2ClassName(name: String): ClassName =
    ClassName(name)

  implicit def string2LocalIdent(name: String): LocalIdent =
    LocalIdent(LocalName(name))
  implicit def string2SimpleFieldIdent(name: String): SimpleFieldIdent =
   SimpleFieldIdent(SimpleFieldName(name))
  implicit def string2ClassIdent(name: String): ClassIdent =
    ClassIdent(ClassName(name))

  implicit def localName2LocalIdent(name: LocalName): LocalIdent =
    LocalIdent(name)
  implicit def simpleFieldName2SimpleFieldIdent(name: SimpleFieldName): SimpleFieldIdent =
    SimpleFieldIdent(name)
  implicit def fieldName2FieldIdent(name: FieldName): FieldIdent =
    FieldIdent(name)
  implicit def methodName2MethodIdent(name: MethodName): MethodIdent =
    MethodIdent(name)

  def bool(x: Boolean): BooleanLiteral = BooleanLiteral(x)
  def int(x: Int): IntLiteral = IntLiteral(x)
  def str(x: String): StringLiteral = StringLiteral(x)
}
