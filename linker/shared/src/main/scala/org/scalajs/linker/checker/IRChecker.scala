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

package org.scalajs.linker.checker

// In the IR checker, we allow early returns for improved readability
// scalastyle:off return

import scala.language.implicitConversions

import scala.annotation.switch

import scala.collection.mutable

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.standard._
import org.scalajs.linker.analyzer.{Analyzer, Infos}

/** Checker for the validity of the IR. */
private final class IRChecker(unit: LinkingUnit, logger: Logger) {

  import IRChecker._

  private var errorCount: Int = 0

  /* Per-method state (setup with withPerMethodState).
   * This state is reset per-Closure as well.
   */
  private var declaredLocalVarNamesPerMethod: mutable.Set[LocalName] = _
  private var declaredLabelNamesPerMethod: mutable.Set[LabelName] = _

  private def withPerMethodState[A](body: => A): A = {
    val savedDeclaredLocalVarNamesPerMethod = declaredLocalVarNamesPerMethod
    val savedDeclaredLabelNamesPerMethod = declaredLabelNamesPerMethod
    try {
      declaredLocalVarNamesPerMethod = mutable.Set.empty
      declaredLabelNamesPerMethod = mutable.Set.empty
      body
    } finally {
      declaredLocalVarNamesPerMethod = savedDeclaredLocalVarNamesPerMethod
      declaredLabelNamesPerMethod = savedDeclaredLabelNamesPerMethod
    }
  }

  private val classes: mutable.Map[ClassName, CheckedClass] = {
    val tups = for (classDef <- unit.classDefs) yield {
      implicit val ctx = ErrorContext(classDef)
      val c = new CheckedClass(classDef)
      c.name -> c
    }
    mutable.Map(tups: _*)
  }

  def check(): Int = {
    for (classDef <- unit.classDefs) {
      implicit val ctx = ErrorContext(classDef)

      checkJSClassCaptures(classDef)
      checkClassInitializer(classDef)
      checkJSSuperClass(classDef)
      checkJSNativeLoadSpec(classDef)
      checkStaticMembers(classDef)
      checkDuplicateMembers(classDef)

      classDef.kind match {
        case ClassKind.AbstractJSType | ClassKind.NativeJSClass |
            ClassKind.NativeJSModuleClass =>
          if (classDef.fields.nonEmpty ||
              classDef.methods.exists(!_.value.flags.namespace.isStatic) ||
              classDef.exportedMembers.nonEmpty) {
            val kind =
              if (classDef.kind == ClassKind.AbstractJSType) "Abstract"
              else "Native"
            reportError(
                i"$kind JS type ${classDef.name} cannot have instance members")
          }
        case _ =>
          checkScalaClassDef(classDef)
      }
    }

    for (topLevelExport <- unit.topLevelExports) {
      val owningClass = topLevelExport.owningClass

      topLevelExport.tree match {
        case tree: TopLevelJSClassExportDef =>
          checkTopLevelJSClassExportDef(tree, owningClass)

        case tree: TopLevelModuleExportDef =>
          checkTopLevelModuleExportDef(tree, owningClass)

        case tree: TopLevelMethodExportDef =>
          checkTopLevelMethodExportDef(tree)

        case tree: TopLevelFieldExportDef =>
          checkTopLevelFieldExportDef(tree, owningClass)
      }
    }

    errorCount
  }

  private def checkJSClassCaptures(classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(classDef)

    for (classCaptures <- classDef.jsClassCaptures) {
      if (classDef.kind != ClassKind.JSClass) {
        reportError(
            i"Class ${classDef.name} which is not a non-native JS class " +
            "cannot have class captures")
      }

      classCaptures.foldLeft(Set.empty[LocalName]) {
        case (alreadyDeclared, p @ ParamDef(ident, _, tpe, mutable, rest)) =>
          implicit val ctx = ErrorContext(p)
          val name = ident.name
          if (alreadyDeclared(name))
            reportError(i"Duplicate JS class capture '$name'")
          if (tpe == NoType)
            reportError(i"The JS class capture $name cannot have type NoType")
          if (mutable)
            reportError(i"The JS class capture $name cannot be mutable")
          if (rest)
            reportError(i"The JS class capture $name cannot be a rest param")
          alreadyDeclared + name
      }
    }
  }

  private def checkClassInitializer(classDef: LinkedClass): Unit = {
    for (classInit <- classDef.methods.find(_.value.methodName.isClassInitializer)) {
      implicit val ctx = ErrorContext(classInit.value)

      if (!classDef.kind.isJSClass) {
        reportError(
            i"The non JS class ${classDef.name} cannot have a class " +
            "initializer")
      }

      if (classDef.jsClassCaptures.isDefined) {
        reportError(
            i"The non-top-level JS class ${classDef.name} cannot have a " +
            "class initializer")
      }
    }
  }

  private def checkJSSuperClass(classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(classDef)

    if (classDef.kind.isJSClass) {
      classDef.jsSuperClass.fold {
        // .get is OK: the Analyzer checks that a super class is present.
        val superClass = lookupClass(classDef.superClass.get.name)
        if (superClass.jsClassCaptures.isDefined)
          reportError(i"super class ${superClass.name} may not have jsClassCaptures")
        else if (superClass.kind == ClassKind.NativeJSClass && superClass.jsNativeLoadSpec.isEmpty)
          reportError(i"Native super class ${superClass.name} must have a native load spec")
      } { tree =>
        val env = Env.fromSignature(NoType, classDef.jsClassCaptures, Nil)
        typecheckExpect(tree, env, AnyType)
      }
    } else {
      if (classDef.jsSuperClass.isDefined)
        reportError("Only non-native JS types may have a jsSuperClass")
    }
  }

  private def checkJSNativeLoadSpec(classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(classDef)

    classDef.kind match {
      case ClassKind.NativeJSClass | ClassKind.NativeJSModuleClass =>
        () // may or may not have a native load spec
      case _ =>
        if (classDef.jsNativeLoadSpec.isDefined) {
          reportError(
              i"Non-native JS type ${classDef.name} must not have a " +
              "jsNativeLoadSpec")
        }
    }
  }

  private def checkStaticMembers(classDef: LinkedClass): Unit = {
    for {
      member <- classDef.methods
      if member.value.flags.namespace.isStatic
    } {
      val methodDef = member.value
      implicit val ctx = ErrorContext(methodDef)

      checkMethodDef(methodDef, classDef)
    }
  }

  private def checkDuplicateMembers(classDef: LinkedClass): Unit = {
    // Fields
    val scalaFields = classDef.fields.collect {
      case fieldDef: FieldDef => fieldDef
    }
    for {
      ((namespace, name), dupes) <- scalaFields.groupBy(f => (f.flags.namespace, f.name.name))
      field <- dupes.tail
    } {
      implicit val ctx = ErrorContext(field)
      reportError(
          i"Duplicate definition of ${namespace.prefixString}field " +
          i"'$name' in class '${classDef.className}'")
    }

    // Methods
    val methods = classDef.methods.map(_.value)
    for {
      ((namespace, name), dupes) <- methods.groupBy(m => (m.flags.namespace, m.name.name))
      method <- dupes.tail
    } {
      implicit val ctx: ErrorContext = ErrorContext(method)
      reportError(
          i"Duplicate definition of ${namespace.prefixString}method " +
          i"'$name' in class '${classDef.className}'")
    }

    /* JS native members
     * They are all in the public static namespace, as checked by an assertion
     * in the constructor, so we do not have to group by namespace.
     */
    for {
      (name, dupes) <- classDef.jsNativeMembers.groupBy(m => m.name.name)
      member <- dupes.tail
    } {
      implicit val ctx: ErrorContext = ErrorContext(member)
      reportError(
          i"Duplicate definition of JS native member " +
          i"'$name' in class '${classDef.className}'")
    }
  }

  private def checkScalaClassDef(classDef: LinkedClass): Unit = {
    assert(classDef.kind != ClassKind.AbstractJSType &&
        classDef.kind != ClassKind.NativeJSClass &&
        classDef.kind != ClassKind.NativeJSModuleClass)

    // Is this a normal class?
    if (classDef.kind != ClassKind.HijackedClass &&
        classDef.kind != ClassKind.Interface) {
      // Check fields
      checkFieldDefs(classDef)

      // Module classes must have exactly one constructor, without parameter
      if (classDef.kind == ClassKind.ModuleClass) {
        implicit val ctx = ErrorContext(classDef)
        val methods = classDef.methods
        if (methods.count(m => m.value.flags.namespace == MemberNamespace.Constructor) != 1)
          reportError("Module class must have exactly 1 constructor")
        if (!methods.exists(_.value.methodName == NoArgConstructorName))
          reportError("Module class must have a parameterless constructor")
      }

      val checkedClass = classes(classDef.name.name)

      // Check exported members
      for (member <- classDef.exportedMembers) {
        implicit val ctx = ErrorContext(member.value)

        member.value match {
          case m: JSMethodDef =>
            checkExportedMethodDef(m, checkedClass)

          case p: JSPropertyDef =>
            checkExportedPropertyDef(p, checkedClass)

          // Anything else is illegal
          case _ =>
            reportError("Illegal exported class member of type " +
                member.value.getClass.getName)
        }
      }
    } else {
      implicit val ctx = ErrorContext(classDef)

      def kindStr =
        if (classDef.kind == ClassKind.HijackedClass) "Hijacked classes"
        else "Interfaces"

      if (classDef.fields.nonEmpty)
        reportError(i"$kindStr may not have fields")

      if (classDef.exportedMembers.nonEmpty)
        reportError(i"$kindStr may not have exports")
    }

    // Check methods
    for {
      method <- classDef.methods
      if !method.value.flags.namespace.isStatic
    } {
      val tree = method.value
      implicit val ctx = ErrorContext(tree)

      checkMethodDef(tree, classDef)
    }
  }

  private def checkFieldDefs(classDef: LinkedClass): Unit = {
    for (fieldDef <- classDef.fields)
      checkFieldDef(fieldDef, classDef)
  }

  private def checkFieldDef(fieldDef: AnyFieldDef, classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(fieldDef)

    if (fieldDef.flags.namespace.isPrivate)
      reportError("A field cannot be private")

    fieldDef match {
      case _: FieldDef =>
        // ok
      case JSFieldDef(_, name, _) =>
        if (!classDef.kind.isJSClass)
          reportError(i"Illegal JS field '$name' in Scala class")
        typecheckExpect(name, Env.empty, AnyType)
    }

    if (fieldDef.ftpe == NoType || fieldDef.ftpe == NothingType)
      reportError(i"FieldDef cannot have type ${fieldDef.ftpe}")
  }

  private def checkMethodDef(methodDef: MethodDef,
      classDef: LinkedClass): Unit = withPerMethodState {

    val MethodDef(flags, MethodIdent(name), _, params, resultType, body) =
      methodDef
    implicit val ctx = ErrorContext(methodDef)

    val namespace = flags.namespace
    val static = namespace.isStatic
    val isConstructor = namespace == MemberNamespace.Constructor

    if (flags.isMutable)
      reportError("A method cannot have the flag Mutable")

    if (classDef.kind.isJSClass && !static) {
      reportError(i"Non exported instance method $name is illegal in JS class")
      return // things would go too badly otherwise
    }

    for (ParamDef(name, _, tpe, _, rest) <- params) {
      checkDeclareLocalVar(name)
      if (tpe == NoType)
        reportError(i"Parameter $name has type NoType")
      if (rest)
        reportError(i"Rest parameter $name is illegal in a Scala method")
    }

    if (isConstructor && classDef.kind == ClassKind.Interface)
      reportError("Interfaces cannot declare constructors")
    if (isConstructor != name.isConstructor)
      reportError("A method must have a constructor name iff it is a constructor")

    val hasStaticConstructorName = name.isStaticInitializer || name.isClassInitializer
    if ((namespace == MemberNamespace.StaticConstructor) != hasStaticConstructorName)
      reportError("A method must have a static constructor name iff it is a static constructor")

    val advertizedSig = (params.map(_.ptpe), resultType)
    val sigFromName = inferMethodType(name, static)
    if (advertizedSig != sigFromName) {
      reportError(
          i"The signature of ${classDef.name.name}.$name, which is "+
          i"$advertizedSig, does not match its name (should be $sigFromName).")
    }

    // Compute bodyEnv even for abstract defs for error checking in fromSignature
    val thisType =
      if (static) NoType
      else ClassType(classDef.name.name)
    val bodyEnv = Env.fromSignature(thisType, None, params, isConstructor)

    body.fold {
      // Abstract
      reportError(
          i"The abstract method ${classDef.name.name}.$name survived the " +
          "Analyzer (this is a bug)")
    } { body =>
      // Concrete
      if (resultType == NoType)
        typecheckStat(body, bodyEnv)
      else
        typecheckExpect(body, bodyEnv, resultType)
    }
  }

  private def checkExportedMethodDef(methodDef: JSMethodDef,
      clazz: CheckedClass): Unit = withPerMethodState {
    val JSMethodDef(flags, pName, params,  body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    val static = flags.namespace.isStatic

    if (flags.isMutable)
      reportError("An exported method cannot have the flag Mutable")
    if (flags.namespace.isPrivate)
      reportError("An exported method cannot be private")

    if (!clazz.kind.isAnyNonNativeClass) {
      reportError(i"Exported method def can only appear in a class")
      return
    }

    if (static && clazz.kind != ClassKind.JSClass)
      reportError("Exported method def in non-JS class cannot be static")

    checkExportedPropertyName(pName, clazz)
    checkJSParamDefs(params)

    def isJSConstructor = {
      !static && (pName match {
        case StringLiteral("constructor") => true
        case _                            => false
      })
    }

    if (clazz.kind.isJSClass && isJSConstructor) {
      checkJSClassConstructor(methodDef, clazz)
    } else {
      val thisType = {
        if (static) NoType
        else if (clazz.kind.isJSClass) AnyType
        else ClassType(clazz.name)
      }

      val bodyEnv = Env.fromSignature(thisType, clazz.jsClassCaptures, params)
      typecheckExpect(body, bodyEnv, AnyType)
    }
  }

  private def checkJSClassConstructor(methodDef: JSMethodDef,
      clazz: CheckedClass): Unit = {
    val JSMethodDef(static, _, params, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    val bodyStats = body match {
      case Block(stats) => stats
      case _            => body :: Nil
    }

    val (prepStats, superCallAndRest) =
      bodyStats.span(!_.isInstanceOf[JSSuperConstructorCall])

    val (superCall, restStats) = superCallAndRest match {
      case (superCall: JSSuperConstructorCall) :: restStats =>
        (superCall, restStats)
      case _ =>
        reportError(
            "A JS class constructor must contain one super constructor " +
            "call at the top-level")
        (JSSuperConstructorCall(Nil)(methodDef.pos), Nil)
    }

    val initialEnv = Env.fromSignature(NoType, clazz.jsClassCaptures,
        params, isConstructor = true)

    val preparedEnv = prepStats.foldLeft(initialEnv) { (prevEnv, stat) =>
      typecheckStat(stat, prevEnv)
    }

    for (arg <- superCall.args)
      typecheckExprOrSpread(arg, preparedEnv)

    val restEnv = preparedEnv.withThis(AnyType)
    typecheckStat(Block(restStats)(methodDef.pos), restEnv)
  }

  private def checkExportedPropertyDef(propDef: JSPropertyDef,
      clazz: CheckedClass): Unit = withPerMethodState {
    val JSPropertyDef(flags, pName, getterBody, setterArgAndBody) = propDef
    implicit val ctx = ErrorContext(propDef)

    val static = flags.namespace.isStatic

    if (flags.isMutable)
      reportError("An exported property def cannot have the flag Mutable")
    if (flags.namespace.isPrivate)
      reportError("An exported property def cannot be private")

    if (!clazz.kind.isAnyNonNativeClass) {
      reportError(i"Exported property def can only appear in a class")
      return
    }

    checkExportedPropertyName(pName, clazz)

    val thisType =
      if (static) NoType
      else if (clazz.kind.isJSClass) AnyType
      else ClassType(clazz.name)

    getterBody.foreach { getterBody =>
      val getterBodyEnv = Env.fromSignature(thisType, clazz.jsClassCaptures, Nil)
      typecheckExpect(getterBody, getterBodyEnv, AnyType)
    }

    setterArgAndBody.foreach { case (setterArg, setterBody) =>
      checkDeclareLocalVar(setterArg.name)
      if (setterArg.ptpe != AnyType)
        reportError("Setter argument of exported property def has type "+
            i"${setterArg.ptpe}, but must be Any")
      if (setterArg.rest)
        reportError(i"Rest parameter ${setterArg.name} is illegal in setter")

      val setterBodyEnv = Env.fromSignature(thisType, clazz.jsClassCaptures,
          List(setterArg))
      typecheckStat(setterBody, setterBodyEnv)
    }
  }

  private def checkExportedPropertyName(propName: Tree, clazz: CheckedClass)(
      implicit ctx: ErrorContext): Unit = {
    propName match {
      case StringLiteral(name) =>
        if (!clazz.kind.isJSClass && name.contains("__"))
          reportError("Exported method def name cannot contain __")

      case _ =>
        if (!clazz.kind.isJSClass)
          reportError("Only JS classes may contain members with computed names")
        typecheckExpect(propName, Env.empty, AnyType)
    }
  }

  private def checkTopLevelJSClassExportDef(
      classExportDef: TopLevelJSClassExportDef, owningClass: ClassName): Unit = {
    implicit val ctx = ErrorContext(classExportDef)

    val clazz = lookupClass(owningClass)

    if (clazz.kind != ClassKind.JSClass)
      reportError(i"Exported JS class def can only appear in a JS class")
  }

  private def checkTopLevelModuleExportDef(
      topLevelModuleDef: TopLevelModuleExportDef,
      owningClass: ClassName): Unit = {
    implicit val ctx = ErrorContext(topLevelModuleDef)

    val clazz = lookupClass(owningClass)

    if (!clazz.kind.hasModuleAccessor) {
      reportError(
          "Top-level module export def can only appear in a module class")
    }
  }

  private def checkTopLevelMethodExportDef(
      topLevelMethodExportDef: TopLevelMethodExportDef): Unit = withPerMethodState {

    val JSMethodDef(flags, pName, params,  body) = topLevelMethodExportDef.methodDef
    implicit val ctx = ErrorContext(topLevelMethodExportDef.methodDef)

    if (flags.isMutable)
      reportError("Top level export method cannot have the flag Mutable")
    if (flags.namespace != MemberNamespace.PublicStatic)
      reportError("Top level export must be public and static")

    pName match {
      case StringLiteral(name) => // ok

      case _ =>
        reportError("Top level exports may not have computed names")
    }

    checkJSParamDefs(params)

    val bodyEnv = Env.fromSignature(NoType, None, params)
    typecheckExpect(body, bodyEnv, AnyType)
  }

  private def checkTopLevelFieldExportDef(
      topLevelFieldExportDef: TopLevelFieldExportDef,
      owningClass: ClassName): Unit = {
    implicit val ctx = ErrorContext(topLevelFieldExportDef)

    val clazz = lookupClass(owningClass)

    if (!clazz.kind.isAnyNonNativeClass) {
      reportError("non-native classes may not have field exports")
    }

    val field = topLevelFieldExportDef.field

    clazz.lookupStaticField(field.name).fold {
      reportError(i"Cannot export non-existent static field '$field'")
    } { checkedField =>
      val tpe = checkedField.tpe
      if (tpe != AnyType)
        reportError(i"Cannot export field '$field' of type $tpe")
    }
  }

  private def typecheckStat(tree: Tree, env: Env): Env = {
    implicit val ctx = ErrorContext(tree)

    tree match {
      case VarDef(ident, _, vtpe, mutable, rhs) =>
        checkDeclareLocalVar(ident)
        typecheckExpect(rhs, env, vtpe)
        env.withLocal(LocalDef(ident.name, vtpe, mutable)(tree.pos))

      case Skip() =>
        env

      case Assign(select, rhs) =>
        select match {
          case Select(This(), className, FieldIdent(_))
              if env.inConstructor && env.thisTpe == ClassType(className) =>
            // ok
          case Select(receiver, className, FieldIdent(name)) =>
            val c = lookupClass(className)
            for {
              f <- c.lookupField(name)
              if !f.flags.isMutable
            } {
              reportError(i"Assignment to immutable field $name.")
            }
          case SelectStatic(className, FieldIdent(name)) =>
            val c = lookupClass(className)
            for {
              f <- c.lookupStaticField(name)
              if !f.flags.isMutable
            } {
              reportError(i"Assignment to immutable static field $name.")
            }
          case VarRef(LocalIdent(name)) if !env.locals(name).mutable =>
            reportError(i"Assignment to immutable variable $name.")
          case _ =>
        }
        val lhsTpe = typecheckExpr(select, env)
        typecheckExpect(rhs, env, lhsTpe)
        env

      case StoreModule(className, value) =>
        val clazz = lookupClass(className)
        if (!clazz.kind.hasModuleAccessor)
          reportError("StoreModule of non-module class $className")
        val expectedType =
          if (clazz.kind == ClassKind.JSModuleClass) AnyType
          else ClassType(className)
        typecheckExpect(value, env, expectedType)
        env

      case Block(stats) =>
        stats.foldLeft(env) { (prevEnv, stat) =>
          typecheckStat(stat, prevEnv)
        }
        env

      case Labeled(label, NoType, body) =>
        checkDeclareLabel(label)
        typecheckStat(body, env.withLabeledReturnType(label.name, AnyType))
        env

      case If(cond, thenp, elsep) =>
        typecheckExpect(cond, env, BooleanType)
        typecheckStat(thenp, env)
        typecheckStat(elsep, env)
        env

      case While(cond, body) =>
        typecheckExpect(cond, env, BooleanType)
        typecheckStat(body, env)
        env

      case DoWhile(body, cond) =>
        typecheckStat(body, env)
        typecheckExpect(cond, env, BooleanType)
        env

      case ForIn(obj, keyVar, _, body) =>
        typecheckExpr(obj, env)
        val bodyEnv =
          env.withLocal(LocalDef(keyVar.name, AnyType, false)(keyVar.pos))
        typecheckStat(body, bodyEnv)
        env

      case TryCatch(block, errVar, _, handler) =>
        typecheckStat(block, env)
        val handlerEnv =
          env.withLocal(LocalDef(errVar.name, AnyType, false)(errVar.pos))
        typecheckStat(handler, handlerEnv)
        env

      case TryFinally(block, finalizer) =>
        typecheckStat(block, env)
        typecheckStat(finalizer, env)
        env

      case Match(selector, cases, default) =>
        typecheckExpect(selector, env, IntType)
        // The alternatives are IntLiterals, no point typechecking them
        for ((_, body) <- cases)
          typecheckStat(body, env)
        typecheckStat(default, env)
        env

      case Debugger() =>
        env

      case JSDelete(qualifier, item) =>
        typecheckExpr(qualifier, env)
        typecheckExpr(item, env)
        env

      case _ =>
        typecheck(tree, env)
        env
    }
  }

  private def typecheckExpect(tree: Tree, env: Env, expectedType: Type)(
      implicit ctx: ErrorContext): Unit = {
    val tpe = typecheckExpr(tree, env)
    if (!isSubtype(tpe, expectedType))
      reportError(i"$expectedType expected but $tpe found "+
          i"for tree of type ${tree.getClass.getName}")
  }

  private def typecheckExpr(tree: Tree, env: Env): Type = {
    implicit val ctx = ErrorContext(tree)
    if (tree.tpe == NoType)
      reportError(i"Expression tree has type NoType")
    typecheck(tree, env)
  }

  private def typecheckExprOrSpread(tree: TreeOrJSSpread, env: Env): Unit = {
    tree match {
      case JSSpread(items) =>
        typecheckExpr(items, env)
      case tree: Tree =>
        typecheckExpr(tree, env)
    }
  }

  private def typecheck(tree: Tree, env: Env): Type = {
    implicit val ctx = ErrorContext(tree)

    def checkApplyGeneric(methodName: MethodName, methodFullName: String,
        args: List[Tree], tpe: Type, isStatic: Boolean): Unit = {
      val (methodParams, resultType) = inferMethodType(methodName, isStatic)
      if (args.size != methodParams.size)
        reportError(i"Arity mismatch: ${methodParams.size} expected but "+
            i"${args.size} found")
      for ((actual, formal) <- args zip methodParams) {
        typecheckExpect(actual, env, formal)
      }
      if (tpe != resultType)
        reportError(i"Call to $methodFullName of type $resultType "+
            i"typed as ${tree.tpe}")
    }

    tree match {
      // Control flow constructs

      case Block(statsAndExpr) =>
        val stats :+ expr = statsAndExpr
        val envAfterStats = stats.foldLeft(env) { (prevEnv, stat) =>
          typecheckStat(stat, prevEnv)
        }
        typecheckExpr(expr, envAfterStats)

      case Labeled(label, tpe, body) =>
        checkDeclareLabel(label)
        typecheckExpect(body, env.withLabeledReturnType(label.name, tpe), tpe)

      case Return(expr, label) =>
        env.returnTypes.get(label.name).fold[Unit] {
          reportError(i"Cannot return to label $label.")
          typecheckExpr(expr, env)
        } { returnType =>
          typecheckExpect(expr, env, returnType)
        }

      case If(cond, thenp, elsep) =>
        val tpe = tree.tpe
        typecheckExpect(cond, env, BooleanType)
        typecheckExpect(thenp, env, tpe)
        typecheckExpect(elsep, env, tpe)

      case While(BooleanLiteral(true), body) if tree.tpe == NothingType =>
        typecheckStat(body, env)

      case TryCatch(block, errVar, _, handler) =>
        val tpe = tree.tpe
        typecheckExpect(block, env, tpe)
        val handlerEnv =
          env.withLocal(LocalDef(errVar.name, AnyType, false)(errVar.pos))
        typecheckExpect(handler, handlerEnv, tpe)

      case TryFinally(block, finalizer) =>
        val tpe = tree.tpe
        typecheckExpect(block, env, tpe)
        typecheckStat(finalizer, env)

      case Throw(expr) =>
        typecheckExpr(expr, env)

      case Match(selector, cases, default) =>
        val tpe = tree.tpe
        typecheckExpect(selector, env, IntType)
        // The alternatives are IntLiterals, no point typechecking them
        for ((_, body) <- cases)
          typecheckExpect(body, env, tpe)
        typecheckExpect(default, env, tpe)

      // Scala expressions

      case New(className, ctor, args) =>
        val clazz = lookupClass(className)
        if (clazz.kind != ClassKind.Class)
          reportError(i"new $className which is not a class")
        checkApplyGeneric(ctor.name, i"$className.$ctor", args, NoType,
            isStatic = false)

      case LoadModule(className) =>
        val clazz = lookupClass(className)
        if (clazz.kind != ClassKind.ModuleClass)
          reportError("LoadModule of non-module class $className")

      case Select(qualifier, className, FieldIdent(item)) =>
        val c = lookupClass(className)
        val kind = c.kind
        if (!kind.isClass) {
          reportError(i"Cannot select $item of non-class $className")
          typecheckExpr(qualifier, env)
        } else {
          typecheckExpect(qualifier, env, ClassType(className))

          /* Actually checking the field is done only if the class has
           * instances (including instances of subclasses).
           *
           * This is necessary because the BaseLinker can completely get rid
           * of all the fields of a class that has no instance. Obviously in
           * such cases, the only value that `qualifier` can assume is
           * `null`, and the `Select` will fail with an NPE. But the IR is
           * still valid per se.
           *
           * See #3060.
           */
          if (c.hasInstances) {
            c.lookupField(item).fold[Unit] {
              reportError(i"Class $className does not have a field $item")
            } { fieldDef =>
              if (fieldDef.tpe != tree.tpe)
                reportError(i"Select $className.$item of type "+
                    i"${fieldDef.tpe} typed as ${tree.tpe}")
            }
          }
        }

      case SelectStatic(className, FieldIdent(item)) =>
        val checkedClass = lookupClass(className)
        if (checkedClass.kind.isJSType) {
          reportError(i"Cannot select static $item of JS type $className")
        } else {
          checkedClass.lookupStaticField(item).fold[Unit] {
            reportError(i"Class $className does not have a static field $item")
          } { fieldDef =>
            if (fieldDef.tpe != tree.tpe)
              reportError(i"SelectStatic $className.$item of type "+
                  i"${fieldDef.tpe} typed as ${tree.tpe}")
          }
        }

      case SelectJSNativeMember(className, MethodIdent(member)) =>
        val checkedClass = lookupClass(className)
        if (!checkedClass.hasJSNativeMember(member))
          reportError(i"Class $className does not have JS native member $member")

      case Apply(flags, receiver, MethodIdent(method), args) =>
        if (flags.isPrivate)
          reportError("Illegal flag for Apply: Private")
        val receiverType = typecheckExpr(receiver, env)
        val fullCheck = receiverType match {
          case ClassType(className) =>
            /* For class types, we only perform full checks if the class has
             * instances. This is necessary because the BaseLinker can
             * completely get rid of all the method *definitions* for the call
             * method. In that case, the classes references in the *signature*
             * of the method might not have been made reachable, and hence
             * inferring the type signature might fail. Obviously in such cases,
             * the only value that `receiver` can assume is `null`, and the
             * `Apply` will fail with an NPE, so the types of the arguments are
             * irreleant.
             */
            lookupClass(className).hasInstances
          case NullType | NothingType =>
            // By a similar argument, we must not perform full checks here
            false
          case _ =>
            true
        }
        if (fullCheck) {
          checkApplyGeneric(method, i"$receiverType.$method", args, tree.tpe,
              isStatic = false)
        } else {
          for (arg <- args)
            typecheckExpr(arg, env)
        }

      case ApplyStatically(_, receiver, className, MethodIdent(method), args) =>
        typecheckExpect(receiver, env, ClassType(className))
        checkApplyGeneric(method, i"$className.$method", args, tree.tpe,
            isStatic = false)

      case ApplyStatic(_, className, MethodIdent(method), args) =>
        val clazz = lookupClass(className)
        checkApplyGeneric(method, i"$className.$method", args, tree.tpe,
            isStatic = true)

      case ApplyDynamicImport(_, className, MethodIdent(method), args) =>
        val clazz = lookupClass(className)
        val methodFullName = i"$className.$method"

        checkApplyGeneric(method, methodFullName, args, AnyType, isStatic = true)

        val resultType = method.resultTypeRef
        if (resultType != ClassRef(ObjectClass)) {
          reportError(i"illegal dynamic import call to $methodFullName with " +
              i"non-object result type: $resultType")
        }

      case UnaryOp(op, lhs) =>
        import UnaryOp._
        val expectedArgType = (op: @switch) match {
          case Boolean_! =>
            BooleanType
          case CharToInt =>
            CharType
          case ByteToInt =>
            ByteType
          case ShortToInt =>
            ShortType
          case IntToLong | IntToDouble | IntToChar | IntToByte | IntToShort =>
            IntType
          case LongToInt | LongToDouble =>
            LongType
          case FloatToDouble =>
            FloatType
          case DoubleToInt | DoubleToFloat | DoubleToLong =>
            DoubleType
        }
        typecheckExpect(lhs, env, expectedArgType)

      case BinaryOp(op, lhs, rhs) =>
        import BinaryOp._
        val expectedLhsType = (op: @switch) match {
          case === | !== | String_+ =>
            AnyType
          case Boolean_== | Boolean_!= | Boolean_| | Boolean_& =>
            BooleanType
          case Int_+ | Int_- | Int_* | Int_/ | Int_% |
              Int_| | Int_& | Int_^ | Int_<< | Int_>>> | Int_>> |
              Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>= =>
            IntType
          case Long_+ | Long_- | Long_* | Long_/ | Long_% |
              Long_| | Long_& | Long_^ | Long_<< | Long_>>> | Long_>> |
              Long_== | Long_!= | Long_< | Long_<= | Long_> | Long_>= =>
            LongType
          case Float_+ | Float_- | Float_* | Float_/ | Float_% =>
            FloatType
          case Double_+ | Double_- | Double_* | Double_/ | Double_% |
              Double_== | Double_!= |
              Double_< | Double_<= | Double_> | Double_>= =>
            DoubleType
        }
        val expectedRhsType = (op: @switch) match {
          case Long_<< | Long_>>> | Long_>> => IntType
          case _                            => expectedLhsType
        }
        typecheckExpect(lhs, env, expectedLhsType)
        typecheckExpect(rhs, env, expectedRhsType)

      case NewArray(typeRef, lengths) =>
        checkArrayTypeRef(typeRef)
        for (length <- lengths)
          typecheckExpect(length, env, IntType)

      case ArrayValue(typeRef, elems) =>
        checkArrayTypeRef(typeRef)
        val elemType = arrayElemType(typeRef)
        for (elem <- elems)
          typecheckExpect(elem, env, elemType)

      case ArrayLength(array) =>
        val arrayType = typecheckExpr(array, env)
        if (!arrayType.isInstanceOf[ArrayType])
          reportError(i"Array type expected but $arrayType found")

      case ArraySelect(array, index) =>
        typecheckExpect(index, env, IntType)
        typecheckExpr(array, env) match {
          case arrayType: ArrayType =>
            if (tree.tpe != arrayElemType(arrayType))
              reportError(i"Array select of array type $arrayType typed as ${tree.tpe}")
          case arrayType =>
            reportError(i"Array type expected but $arrayType found")
        }

      case IsInstanceOf(expr, testType) =>
        typecheckExpr(expr, env)
        checkIsAsInstanceTargetType(testType)

      case AsInstanceOf(expr, tpe) =>
        typecheckExpr(expr, env)
        checkIsAsInstanceTargetType(tpe)

      case GetClass(expr) =>
        typecheckExpr(expr, env)

      case Clone(expr) =>
        typecheckExpect(expr, env, ClassType(CloneableClass))

      case IdentityHashCode(expr) =>
        typecheckExpr(expr, env)

      // JavaScript expressions

      case JSNew(ctor, args) =>
        typecheckExpr(ctor, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSPrivateSelect(qualifier, className, field) =>
        typecheckExpr(qualifier, env)
        val checkedClass = lookupClass(className)
        if (!checkedClass.kind.isJSClass && checkedClass.kind != ClassKind.AbstractJSType) {
          reportError(i"Cannot select JS private field $field of non-JS class $className")
        } else {
          if (checkedClass.lookupField(field.name).isEmpty)
            reportError(i"JS class $className does not have a field $field")
          /* The declared type of the field is irrelevant here. It is only
           * relevant for its initialization value. The type of the selection
           * is always `any`.
           */
        }

      case JSSelect(qualifier, item) =>
        typecheckExpr(qualifier, env)
        typecheckExpr(item, env)

      case JSFunctionApply(fun, args) =>
        typecheckExpr(fun, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSMethodApply(receiver, method, args) =>
        typecheckExpr(receiver, env)
        typecheckExpr(method, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSSuperSelect(superClass, qualifier, item) =>
        typecheckExpr(superClass, env)
        typecheckExpr(qualifier, env)
        typecheckExpr(item, env)

      case JSSuperMethodCall(superClass, receiver, method, args) =>
        typecheckExpr(superClass, env)
        typecheckExpr(receiver, env)
        typecheckExpr(method, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSImportCall(arg) =>
        typecheckExpr(arg, env)

      case LoadJSConstructor(className) =>
        val clazz = lookupClass(className)
        val valid = clazz.kind match {
          case ClassKind.JSClass       => true
          case ClassKind.NativeJSClass => true
          case _                       => false
        }
        if (!valid)
          reportError(i"JS class type expected but $className found")
        else if (clazz.jsClassCaptures.nonEmpty)
          reportError(i"Cannot load JS constructor of non-top-level class $className")
        else if (clazz.kind == ClassKind.NativeJSClass && clazz.jsNativeLoadSpec.isEmpty)
          reportError(i"Cannot load JS constructor of native JS class $className without native load spec")

      case LoadJSModule(className) =>
        val clazz = lookupClass(className)
        val valid = clazz.kind match {
          case ClassKind.JSModuleClass       => true
          case ClassKind.NativeJSModuleClass => true
          case _                             => false
        }
        if (!valid)
          reportError(i"JS module class type expected but $className found")
        else if (clazz.kind == ClassKind.NativeJSModuleClass && clazz.jsNativeLoadSpec.isEmpty)
          reportError(i"Cannot load JS module of native JS module class $className without native load spec")

      case JSUnaryOp(op, lhs) =>
        typecheckExpr(lhs, env)

      case JSBinaryOp(op, lhs, rhs) =>
        typecheckExpr(lhs, env)
        typecheckExpr(rhs, env)

      case JSArrayConstr(items) =>
        for (item <- items)
          typecheckExprOrSpread(item, env)

      case JSObjectConstr(fields) =>
        for ((key, value) <- fields) {
          typecheckExpr(key, env)
          typecheckExpr(value, env)
        }

      case JSGlobalRef(_) =>

      case JSTypeOfGlobalRef(_) =>

      case JSLinkingInfo() =>

      // Literals

      case ClassOf(typeRef) =>
        typeRef match {
          case NullRef | NothingRef =>
            reportError(i"Invalid classOf[$typeRef]")
          case typeRef: ArrayTypeRef =>
            checkArrayTypeRef(typeRef)
          case _ =>
            // ok
        }

      case _: Literal =>

      // Atomic expressions

      case VarRef(LocalIdent(name)) =>
        env.locals.get(name).fold[Unit] {
          reportError(i"Cannot find variable $name in scope")
        } { localDef =>
          if (tree.tpe != localDef.tpe)
            reportError(i"Variable $name of type ${localDef.tpe} "+
                i"typed as ${tree.tpe}")
        }

      case This() =>
        if (!isSubtype(env.thisTpe, tree.tpe))
          reportError(i"this of type ${env.thisTpe} typed as ${tree.tpe}")

      case Closure(arrow, captureParams, params, body, captureValues) =>
        /* Check compliance of captureValues wrt. captureParams in the current
         * method state, i.e., outside `withPerMethodState`.
         */
        if (captureParams.size != captureValues.size)
          reportError("Mismatched size for captures: "+
              i"${captureParams.size} params vs ${captureValues.size} values")

        for ((ParamDef(_, _, ctpe, _, _), value) <- captureParams zip captureValues) {
          typecheckExpect(value, env, ctpe)
        }

        // Then check the closure params and body in its own per-method state
        withPerMethodState {
          for (ParamDef(name, _, ctpe, mutable, rest) <- captureParams) {
            checkDeclareLocalVar(name)
            if (mutable)
              reportError(i"Capture parameter $name cannot be mutable")
            if (rest)
              reportError(i"Capture parameter $name cannot be a rest parameter")
            if (ctpe == NoType)
              reportError(i"Parameter $name has type NoType")
          }

          checkJSParamDefs(params)

          val thisType = if (arrow) NoType else AnyType
          val bodyEnv = Env.fromSignature(thisType, None, captureParams ++ params)
          typecheckExpect(body, bodyEnv, AnyType)
        }

      case CreateJSClass(className, captureValues) =>
        val clazz = lookupClass(className)
        clazz.jsClassCaptures.fold {
          reportError(i"Invalid CreateJSClass of top-level class $className")
        } { captureParams =>
          if (captureParams.size != captureValues.size) {
            reportError("Mismatched size for class captures: " +
                i"${captureParams.size} params vs ${captureValues.size} values")
          }

          for ((ParamDef(_, _, ctpe, _, _), value) <- captureParams.zip(captureValues))
            typecheckExpect(value, env, ctpe)
        }

      case _ =>
        reportError(i"Invalid expression tree")
    }

    tree.tpe
  }

  /** Check the parameters for a method with JS calling conventions. */
  private def checkJSParamDefs(params: List[ParamDef])(
      implicit ctx: ErrorContext): Unit = {
    for (ParamDef(name, _, ptpe, _, _) <- params) {
      checkDeclareLocalVar(name)
      if (ptpe == NoType)
        reportError(i"Parameter $name has type NoType")
      else if (ptpe != AnyType)
        reportError(i"Parameter $name has type $ptpe but must be any")
    }

    if (params.nonEmpty) {
      for (ParamDef(name, _, _, _, rest) <- params.init) {
        if (rest)
          reportError(i"Non-last rest parameter $name is illegal")
      }
    }
  }

  private def checkDeclareLocalVar(ident: LocalIdent)(
      implicit ctx: ErrorContext): Unit = {
    if (!declaredLocalVarNamesPerMethod.add(ident.name))
      reportError(i"Duplicate local variable name ${ident.name}.")
  }

  private def checkDeclareLabel(label: LabelIdent)(
      implicit ctx: ErrorContext): Unit = {
    if (!declaredLabelNamesPerMethod.add(label.name))
      reportError(i"Duplicate label named ${label.name}.")
  }

  private def checkIsAsInstanceTargetType(tpe: Type)(
      implicit ctx: ErrorContext): Unit = {
    tpe match {
      case ClassType(className) =>
        val kind = lookupClass(className).kind
        if (kind.isJSType) {
          reportError(
              i"JS type $className is not a valid target type for " +
              "Is/AsInstanceOf")
        }

      case NoType | NullType | NothingType | _:RecordType =>
        reportError(i"$tpe is not a valid target type for Is/AsInstanceOf")

      case tpe: ArrayType =>
        checkArrayType(tpe)

      case _ =>
        // ok
    }
  }

  private def checkArrayType(tpe: ArrayType)(
      implicit ctx: ErrorContext): Unit = {
    checkArrayTypeRef(tpe.arrayTypeRef)
  }

  private def checkArrayTypeRef(typeRef: ArrayTypeRef)(
      implicit ctx: ErrorContext): Unit = {
    typeRef.base match {
      case VoidRef | NullRef | NothingRef =>
        reportError(i"Invalid array type $typeRef")
      case _ =>
        // ok
    }
  }

  private def inferMethodType(methodName: MethodName, isStatic: Boolean)(
      implicit ctx: ErrorContext): (List[Type], Type) = {

    val paramTypes = methodName.paramTypeRefs.map(typeRefToType)
    val resultType = typeRefToType(methodName.resultTypeRef)
    (paramTypes, resultType)
  }

  private def typeRefToType(typeRef: TypeRef)(
      implicit ctx: ErrorContext): Type = {
    typeRef match {
      case PrimRef(tpe)               => tpe
      case ClassRef(className)        => classNameToType(className)
      case arrayTypeRef: ArrayTypeRef => ArrayType(arrayTypeRef)
    }
  }

  private def classNameToType(className: ClassName)(
      implicit ctx: ErrorContext): Type = {
    if (className == ObjectClass) {
      AnyType
    } else {
      val kind = lookupClass(className).kind
      if (kind.isJSType) AnyType
      else ClassType(className)
    }
  }

  private def arrayElemType(arrayType: ArrayType)(
      implicit ctx: ErrorContext): Type = {
    arrayElemType(arrayType.arrayTypeRef)
  }

  private def arrayElemType(arrayTypeRef: ArrayTypeRef)(
      implicit ctx: ErrorContext): Type = {
    val ArrayTypeRef(base, dimensions) = arrayTypeRef
    if (dimensions == 1)
      typeRefToType(base)
    else
      ArrayType(ArrayTypeRef(base, dimensions - 1))
  }

  private def reportError(msg: String)(implicit ctx: ErrorContext): Unit = {
    logger.error(s"$ctx: $msg")
    errorCount += 1
  }

  private def lookupClass(className: ClassName)(
      implicit ctx: ErrorContext): CheckedClass = {
    classes.getOrElseUpdate(className, {
      reportError(i"Cannot find class $className")
      new CheckedClass(className, ClassKind.Class, None, Some(ObjectClass),
          Set(ObjectClass), hasInstances = true, None, Nil, Set.empty)
    })
  }

  private def lookupClass(classType: ClassType)(
      implicit ctx: ErrorContext): CheckedClass = {
    lookupClass(classType.className)
  }

  private def lookupClass(classRef: ClassRef)(
      implicit ctx: ErrorContext): CheckedClass = {
    lookupClass(classRef.className)
  }

  private def isSubclass(lhs: ClassName, rhs: ClassName)(
      implicit ctx: ErrorContext): Boolean = {
    lookupClass(lhs).ancestors.contains(rhs)
  }

  private def isSubtype(lhs: Type, rhs: Type)(
      implicit ctx: ErrorContext): Boolean = {
    Types.isSubtype(lhs, rhs)(isSubclass)
  }

  private class Env(
      /** Type of `this`. Can be NoType. */
      val thisTpe: Type,
      /** Local variables in scope (including through closures). */
      val locals: Map[LocalName, LocalDef],
      /** Return types by label. */
      val returnTypes: Map[LabelName, Type],
      /** Whether we're in a constructor of the class */
      val inConstructor: Boolean
  ) {
    import Env._

    def withThis(thisTpe: Type): Env =
      new Env(thisTpe, this.locals, this.returnTypes, this.inConstructor)

    def withLocal(localDef: LocalDef)(implicit ctx: ErrorContext): Env = {
      new Env(thisTpe, locals + (localDef.name -> localDef), returnTypes,
          this.inConstructor)
    }

    def withLabeledReturnType(label: LabelName, returnType: Type): Env =
      new Env(this.thisTpe, this.locals,
          returnTypes + (label -> returnType), this.inConstructor)

    def withInConstructor(inConstructor: Boolean): Env =
      new Env(this.thisTpe, this.locals, this.returnTypes, inConstructor)
  }

  private object Env {
    val empty: Env = new Env(NoType, Map.empty, Map.empty, false)

    def fromSignature(thisType: Type, jsClassCaptures: Option[List[ParamDef]],
        params: List[ParamDef], isConstructor: Boolean = false): Env = {
      val allParams = jsClassCaptures.getOrElse(Nil) ::: params
      val paramLocalDefs =
        for (p @ ParamDef(ident, _, tpe, mutable, _) <- allParams)
          yield ident.name -> LocalDef(ident.name, tpe, mutable)(p.pos)
      new Env(thisType, paramLocalDefs.toMap, Map.empty, isConstructor)
    }
  }

  private class CheckedClass(
      val name: ClassName,
      val kind: ClassKind,
      val jsClassCaptures: Option[List[ParamDef]],
      val superClassName: Option[ClassName],
      val ancestors: Set[ClassName],
      val hasInstances: Boolean,
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      _fields: List[CheckedField],
      val jsNativeMembers: Set[MethodName])(
      implicit ctx: ErrorContext) {

    val fields = _fields.filter(!_.flags.namespace.isStatic).map(f => f.name -> f).toMap
    val staticFields = _fields.filter(_.flags.namespace.isStatic).map(f => f.name -> f).toMap

    lazy val superClass = superClassName.map(classes)

    def this(classDef: LinkedClass)(implicit ctx: ErrorContext) = {
      this(classDef.name.name, classDef.kind,
          classDef.jsClassCaptures,
          classDef.superClass.map(_.name),
          classDef.ancestors.toSet,
          classDef.hasInstances,
          classDef.jsNativeLoadSpec,
          CheckedClass.checkedFieldsOf(classDef),
          classDef.jsNativeMembers.map(_.name.name).toSet)
    }

    def lookupField(name: FieldName): Option[CheckedField] =
      fields.get(name)

    def lookupStaticField(name: FieldName): Option[CheckedField] =
      staticFields.get(name)

    def hasJSNativeMember(name: MethodName): Boolean =
      jsNativeMembers.contains(name)
  }

  private object CheckedClass {
    private def checkedFieldsOf(classDef: LinkedClass): List[CheckedField] = {
      classDef.fields.collect {
        case FieldDef(flags, FieldIdent(name), _, tpe) =>
          new CheckedField(flags, name, tpe)
      }
    }
  }

  private class CheckedField(val flags: MemberFlags, val name: FieldName,
      val tpe: Type)
}

object IRChecker {
  /** Checks that the IR in a [[frontend.LinkingUnit LinkingUnit]] is correct.
   *
   *  @return Count of IR checking errors (0 in case of success)
   */
  def check(unit: LinkingUnit, logger: Logger): Int = {
    new IRChecker(unit, logger).check()
  }

  /** A string interpolator that displays IR concepts in a nice way. */
  private implicit final class InfoStringContext(
      private val self: StringContext)
      extends AnyVal {

    def i(args: Any*): String =
      self.s(args.map(format(_)): _*)

    private def format(arg: Any): String = {
      arg match {
        case arg: Name       => arg.nameString
        case arg: MethodName => arg.displayName
        case arg: IRNode     => arg.show
        case arg: TypeRef    => arg.displayName
        case arg: Type       => arg.show()
        case _               => arg.toString()
      }
    }
  }

  /** The context in which to report IR check errors.
   *
   *  The way this class is written is optimized for the happy path, where no
   *  error occurs. In that case, `toString()` is never called, and we avoid
   *  any kind of allocation.
   *
   *  The parameter is an `Any` for that reason. It should be an
   *  `Either[IRNode, LinkedClass]`, but that would also require an allocation
   *  of the `Left` or `Right` (in fact, we'd love to type it as
   *  `IRNode | LinkedClass`). `ErrorContext` is also made an `AnyVal` for the
   *  same reasons, again.
   *
   *  If `toString()` is called, we're in a bad situation anyway, because the
   *  IR is invalid, so all bets are off and we can be slow and allocate stuff;
   *  we don't care.
   */
  private final class ErrorContext private (private val nodeOrLinkedClass: Any)
      extends AnyVal {

    override def toString(): String = {
      val (pos, name) = nodeOrLinkedClass match {
        case tree: IRNode             => (tree.pos, tree.getClass.getSimpleName)
        case linkedClass: LinkedClass => (linkedClass.pos, "ClassDef")
      }
      s"${pos.source}(${pos.line+1}:${pos.column+1}:$name)"
    }
  }

  private object ErrorContext {
    implicit def node2errorContext(node: IRNode): ErrorContext =
      ErrorContext(node)

    def apply(node: IRNode): ErrorContext =
      new ErrorContext(node)

    def apply(linkedClass: LinkedClass): ErrorContext =
      new ErrorContext(linkedClass)
  }

  private final case class LocalDef(name: LocalName, tpe: Type,
      mutable: Boolean)(
      val pos: Position)
}
