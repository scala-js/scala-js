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
import Definitions._
import Trees._
import Types._

import org.scalajs.logging._

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
      checkJSNativeLoadSpec(classDef)
      checkStaticMembers(classDef)

      classDef.kind match {
        case ClassKind.AbstractJSType | ClassKind.NativeJSClass |
            ClassKind.NativeJSModuleClass =>
          if (classDef.fields.nonEmpty ||
              classDef.methods.exists(!_.value.flags.namespace.isStatic) ||
              classDef.exportedMembers.nonEmpty ||
              classDef.topLevelExports.nonEmpty) {
            val kind =
              if (classDef.kind == ClassKind.AbstractJSType) "Abstract"
              else "Native"
            reportError(
                s"$kind JS type ${classDef.name} cannot have instance members")
          }
        case _ =>
          checkScalaClassDef(classDef)
      }
    }
    errorCount
  }

  private def checkJSClassCaptures(classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(classDef)

    for (classCaptures <- classDef.jsClassCaptures) {
      if (classDef.kind != ClassKind.JSClass) {
        reportError(
            s"Class ${classDef.name} which is not a non-native JS class " +
            "cannot have class captures")
      }

      classCaptures.foldLeft(Set.empty[LocalName]) {
        case (alreadyDeclared, p @ ParamDef(ident, tpe, mutable, rest)) =>
          implicit val ctx = ErrorContext(p)
          val name = ident.name
          if (alreadyDeclared(name))
            reportError(s"Duplicate JS class capture '$name'")
          if (tpe == NoType)
            reportError(s"The JS class capture $name cannot have type NoType")
          if (mutable)
            reportError(s"The JS class capture $name cannot be mutable")
          if (rest)
            reportError(s"The JS class capture $name cannot be a rest param")
          alreadyDeclared + name
      }

      def isStaticInit(methodDef: Versioned[MethodDef]): Boolean = {
        val m = methodDef.value
        m.flags.namespace == MemberNamespace.PublicStatic &&
        m.encodedName == StaticInitializerName
      }

      for (staticInit <- classDef.methods.find(isStaticInit)) {
        implicit val ctx = ErrorContext(staticInit.value)
        reportError(
            s"The non-top-level JS class ${classDef.name} cannot have a " +
            "static initializer")
      }
    }
  }

  private def checkJSNativeLoadSpec(classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(classDef)

    classDef.kind match {
      case ClassKind.NativeJSClass | ClassKind.NativeJSModuleClass =>
        if (classDef.jsNativeLoadSpec.isEmpty) {
          reportError(
              s"Native JS type ${classDef.name} must have a jsNativeLoadSpec")
        }
      case _ =>
        if (classDef.jsNativeLoadSpec.isDefined) {
          reportError(
              s"Non-native JS type ${classDef.name} must not have a " +
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

  private def checkScalaClassDef(classDef: LinkedClass): Unit = {
    assert(classDef.kind != ClassKind.AbstractJSType &&
        classDef.kind != ClassKind.NativeJSClass &&
        classDef.kind != ClassKind.NativeJSModuleClass)

    // Is this a normal class?
    if (classDef.kind != ClassKind.HijackedClass &&
        classDef.kind != ClassKind.Interface) {
      // Check fields
      checkFieldDefs(classDef)

      /* Check for static field collisions.
       * TODO #2627 We currently cannot check instance field collisions because
       * of #2382.
       */
      val staticFieldDefs = classDef.fields.collect {
        case fieldDef: FieldDef if fieldDef.flags.namespace.isStatic => fieldDef
      }
      for {
        fieldsWithSameName <- staticFieldDefs.groupBy(_.name.name).values
        duplicate <- fieldsWithSameName.tail
      } {
        implicit val ctx = ErrorContext(duplicate)
        reportError(s"Duplicate static field with name '${duplicate.name}'")
      }

      // Module classes must have exactly one constructor, without parameter
      if (classDef.kind == ClassKind.ModuleClass) {
        implicit val ctx = ErrorContext(classDef)
        val methods = classDef.methods
        if (methods.count(m => m.value.flags.namespace == MemberNamespace.Constructor) != 1)
          reportError(s"Module class must have exactly 1 constructor")
        if (!methods.exists(_.value.encodedName == NoArgConstructorName))
          reportError(s"Module class must have a parameterless constructor")
      }

      // Check exported members
      for (member <- classDef.exportedMembers) {
        implicit val ctx = ErrorContext(member.value)

        member.value match {
          case m: JSMethodDef =>
            checkExportedMethodDef(m, classDef, isTopLevel = false)

          case p: JSPropertyDef =>
            checkExportedPropertyDef(p, classDef)

          // Anything else is illegal
          case _ =>
            reportError("Illegal exported class member of type " +
                member.value.getClass.getName)
        }
      }

      // Check top-level exports
      for (topLevelExport <- classDef.topLevelExports) {
        val tree = topLevelExport.value
        implicit val ctx = ErrorContext(tree)

        tree match {
          case tree: TopLevelJSClassExportDef =>
            checkTopLevelJSClassExportDef(tree, classDef)

          case tree: TopLevelModuleExportDef =>
            checkTopLevelModuleExportDef(tree, classDef)

          case TopLevelMethodExportDef(methodDef) =>
            checkExportedMethodDef(methodDef, classDef, isTopLevel = true)

          case TopLevelFieldExportDef(_, field) =>
            lookupClass(classDef.name.name).lookupStaticField(field.name).fold {
              reportError(s"Cannot export non-existent static field '$field'")
            } { checkedField =>
              val tpe = checkedField.tpe
              if (tpe != AnyType)
                reportError(s"Cannot export field '$field' of type $tpe")
            }
        }
      }
    } else {
      implicit val ctx = ErrorContext(classDef)

      def kindStr =
        if (classDef.kind == ClassKind.HijackedClass) "Hijacked classes"
        else "Interfaces"

      if (classDef.fields.nonEmpty)
        reportError(s"$kindStr may not have fields")

      if (classDef.exportedMembers.nonEmpty || classDef.topLevelExports.nonEmpty)
        reportError(s"$kindStr may not have exports")
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
          reportError(s"Illegal JS field '$name' in Scala class")
        typecheckExpect(name, Env.empty, AnyType)
    }

    if (fieldDef.ftpe == NoType)
      reportError(s"FieldDef cannot have type NoType")
  }

  private def checkMethodDef(methodDef: MethodDef,
      classDef: LinkedClass): Unit = withPerMethodState {

    val MethodDef(flags, MethodIdent(name, _), params, resultType, body) =
      methodDef
    implicit val ctx = ErrorContext(methodDef)

    val namespace = flags.namespace
    val static = namespace.isStatic
    val isConstructor = namespace == MemberNamespace.Constructor

    if (flags.isMutable)
      reportError("A method cannot have the flag Mutable")

    if (classDef.kind.isJSClass && !static) {
      reportError(s"Non exported instance method $name is illegal in JS class")
      return // things would go too badly otherwise
    }

    for (ParamDef(name, tpe, _, rest) <- params) {
      checkDeclareLocalVar(name)
      if (tpe == NoType)
        reportError(s"Parameter $name has type NoType")
      if (rest)
        reportError(s"Rest parameter $name is illegal in a Scala method")
    }

    if (isConstructor && classDef.kind == ClassKind.Interface)
      reportError("Interfaces cannot declare constructors")
    if (isConstructor != isConstructorName(name))
      reportError("A method must have a constructor name iff it is a constructor")

    if ((namespace == MemberNamespace.StaticConstructor) != (name == StaticInitializerName))
      reportError("A method must have a static initializer name iff it is a static constructor")

    val advertizedSig = (params.map(_.ptpe), resultType)
    val sigFromName = inferMethodType(name, static)
    if (advertizedSig != sigFromName) {
      reportError(
          s"The signature of ${classDef.name.name}.$name, which is "+
          s"$advertizedSig, does not match its name (should be $sigFromName).")
    }

    // Compute bodyEnv even for abstract defs for error checking in fromSignature
    val thisType =
      if (static) NoType
      else ClassType(classDef.name.name)
    val bodyEnv = Env.fromSignature(thisType, None, params, resultType,
        isConstructor)

    body.fold {
      // Abstract
      reportError(
          s"The abstract method ${classDef.name.name}.$name survived the " +
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
      classDef: LinkedClass, isTopLevel: Boolean): Unit = withPerMethodState {
    val JSMethodDef(flags, pName, params,  body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    val static = flags.namespace.isStatic

    if (flags.isMutable)
      reportError("An exported method cannot have the flag Mutable")
    if (flags.namespace.isPrivate)
      reportError("An exported method cannot be private")

    if (!isTopLevel && !classDef.kind.isAnyNonNativeClass) {
      reportError(s"Exported method def can only appear in a class")
      return
    }

    if (!isTopLevel && static && classDef.kind != ClassKind.JSClass)
      reportError("Exported method def in non-JS class cannot be static")

    if (isTopLevel && !static)
      reportError("Top level export must be static")

    checkExportedPropertyName(pName, classDef, isTopLevel)
    checkJSParamDefs(params)

    def isJSConstructor = {
      !static && (pName match {
        case StringLiteral("constructor") => true
        case _                            => false
      })
    }

    if (classDef.kind.isJSClass && isJSConstructor) {
      checkJSClassConstructor(methodDef, classDef)
    } else {
      val thisType = {
        if (static) NoType
        else if (classDef.kind.isJSClass) AnyType
        else ClassType(classDef.name.name)
      }

      val bodyEnv = Env.fromSignature(thisType, classDef.jsClassCaptures,
          params, AnyType)
      typecheckExpect(body, bodyEnv, AnyType)
    }
  }

  private def checkJSClassConstructor(methodDef: JSMethodDef,
      classDef: LinkedClass): Unit = {
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

    val initialEnv = Env.fromSignature(NoType, classDef.jsClassCaptures,
        params, NoType, isConstructor = true)

    val preparedEnv = prepStats.foldLeft(initialEnv) { (prevEnv, stat) =>
      typecheckStat(stat, prevEnv)
    }

    for (arg <- superCall.args)
      typecheckExprOrSpread(arg, preparedEnv)

    val restEnv = preparedEnv.withThis(AnyType)
    typecheckStat(Block(restStats)(methodDef.pos), restEnv)
  }

  private def checkExportedPropertyDef(propDef: JSPropertyDef,
      classDef: LinkedClass): Unit = withPerMethodState {
    val JSPropertyDef(flags, pName, getterBody, setterArgAndBody) = propDef
    implicit val ctx = ErrorContext(propDef)

    val static = flags.namespace.isStatic

    if (flags.isMutable)
      reportError("An exported property def cannot have the flag Mutable")
    if (flags.namespace.isPrivate)
      reportError("An exported property def cannot be private")

    if (!classDef.kind.isAnyNonNativeClass) {
      reportError(s"Exported property def can only appear in a class")
      return
    }

    checkExportedPropertyName(pName, classDef, isTopLevel = false)

    val thisType =
      if (static) NoType
      else if (classDef.kind.isJSClass) AnyType
      else ClassType(classDef.name.name)

    getterBody.foreach { getterBody =>
      val getterBodyEnv = Env.fromSignature(thisType, classDef.jsClassCaptures,
          Nil, AnyType)
      typecheckExpect(getterBody, getterBodyEnv, AnyType)
    }

    setterArgAndBody.foreach { case (setterArg, setterBody) =>
      checkDeclareLocalVar(setterArg.name)
      if (setterArg.ptpe != AnyType)
        reportError("Setter argument of exported property def has type "+
            s"${setterArg.ptpe}, but must be Any")
      if (setterArg.rest)
        reportError(s"Rest parameter ${setterArg.name} is illegal in setter")

      val setterBodyEnv = Env.fromSignature(thisType, classDef.jsClassCaptures,
          List(setterArg), NoType)
      typecheckStat(setterBody, setterBodyEnv)
    }
  }

  private def checkExportedPropertyName(propName: Tree,
      classDef: LinkedClass, isTopLevel: Boolean)(
      implicit ctx: ErrorContext): Unit = {
    propName match {
      case StringLiteral(name) =>
        if (!classDef.kind.isJSClass && name.contains("__"))
          reportError("Exported method def name cannot contain __")

      case _ =>
        if (isTopLevel || !classDef.kind.isJSClass)
          reportError("Only JS classes may contain members with computed names")
        typecheckExpect(propName, Env.empty, AnyType)
    }
  }

  private def checkTopLevelJSClassExportDef(
      classExportDef: TopLevelJSClassExportDef, classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(classExportDef)

    if (classDef.kind != ClassKind.JSClass)
      reportError(s"Exported JS class def can only appear in a JS class")
  }

  private def checkTopLevelModuleExportDef(
      topLevelModuleDef: TopLevelModuleExportDef,
      classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(topLevelModuleDef)

    if (!classDef.kind.hasModuleAccessor) {
      reportError(
          "Top-level module export def can only appear in a module class")
    }
  }

  private def typecheckStat(tree: Tree, env: Env): Env = {
    implicit val ctx = ErrorContext(tree)

    tree match {
      case VarDef(ident, vtpe, mutable, rhs) =>
        checkDeclareLocalVar(ident)
        typecheckExpect(rhs, env, vtpe)
        env.withLocal(LocalDef(ident.name, vtpe, mutable)(tree.pos))

      case Skip() =>
        env

      case Assign(select, rhs) =>
        select match {
          case Select(This(), ClassRef(cls), FieldIdent(_, _))
              if env.inConstructor && env.thisTpe == ClassType(cls) =>
            // ok
          case Select(receiver, ClassRef(cls), FieldIdent(name, _)) =>
            val c = lookupClass(cls)
            for {
              f <- c.lookupField(name)
              if !f.flags.isMutable
            } {
              reportError(s"Assignment to immutable field $name.")
            }
          case SelectStatic(ClassRef(cls), FieldIdent(name, _)) =>
            val c = lookupClass(cls)
            for {
              f <- c.lookupStaticField(name)
              if !f.flags.isMutable
            } {
              reportError(s"Assignment to immutable static field $name.")
            }
          case VarRef(LocalIdent(name, _)) if !env.locals(name).mutable =>
            reportError(s"Assignment to immutable variable $name.")
          case _ =>
        }
        val lhsTpe = typecheckExpr(select, env)
        typecheckExpect(rhs, env, lhsTpe)
        env

      case StoreModule(cls, value) =>
        val clazz = lookupClass(cls)
        if (!clazz.kind.hasModuleAccessor)
          reportError("StoreModule of non-module class $cls")
        val expectedType =
          if (clazz.kind == ClassKind.JSModuleClass) AnyType
          else ClassType(cls.className)
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

      case ForIn(obj, keyVar, body) =>
        typecheckExpr(obj, env)
        val bodyEnv =
          env.withLocal(LocalDef(keyVar.name, AnyType, false)(keyVar.pos))
        typecheckStat(body, bodyEnv)
        env

      case TryCatch(block, errVar, handler) =>
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
      reportError(s"$expectedType expected but $tpe found "+
          s"for tree of type ${tree.getClass.getName}")
  }

  private def typecheckExpr(tree: Tree, env: Env): Type = {
    implicit val ctx = ErrorContext(tree)
    if (tree.tpe == NoType)
      reportError(s"Expression tree has type NoType")
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
        reportError(s"Arity mismatch: ${methodParams.size} expected but "+
            s"${args.size} found")
      for ((actual, formal) <- args zip methodParams) {
        typecheckExpect(actual, env, formal)
      }
      if (tpe != resultType)
        reportError(s"Call to $methodFullName of type $resultType "+
            s"typed as ${tree.tpe}")
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
          reportError(s"Cannot return to label $label.")
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

      case TryCatch(block, errVar, handler) =>
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

      case New(cls, ctor, args) =>
        val clazz = lookupClass(cls)
        if (!clazz.kind.isClass)
          reportError(s"new $cls which is not a class")
        checkApplyGeneric(ctor.name, s"$cls.$ctor", args, NoType,
            isStatic = false)

      case LoadModule(cls) =>
        val clazz = lookupClass(cls)
        if (clazz.kind != ClassKind.ModuleClass)
          reportError("LoadModule of non-module class $cls")

      case Select(qualifier, ClassRef(cls), FieldIdent(item, _)) =>
        val c = lookupClass(cls)
        val kind = c.kind
        if (!kind.isClass) {
          reportError(s"Cannot select $item of non-class $cls")
          typecheckExpr(qualifier, env)
        } else {
          typecheckExpect(qualifier, env, ClassType(cls))

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
              reportError(s"Class $cls does not have a field $item")
            } { fieldDef =>
              if (fieldDef.tpe != tree.tpe)
                reportError(s"Select $cls.$item of type "+
                    s"${fieldDef.tpe} typed as ${tree.tpe}")
            }
          }
        }

      case SelectStatic(ClassRef(cls), FieldIdent(item, _)) =>
        val checkedClass = lookupClass(cls)
        if (checkedClass.kind.isJSType) {
          reportError(s"Cannot select static $item of JS type $cls")
        } else {
          checkedClass.lookupStaticField(item).fold[Unit] {
            reportError(s"Class $cls does not have a static field $item")
          } { fieldDef =>
            if (fieldDef.tpe != tree.tpe)
              reportError(s"SelectStatic $cls.$item of type "+
                  s"${fieldDef.tpe} typed as ${tree.tpe}")
          }
        }

      case Apply(flags, receiver, MethodIdent(method, _), args) =>
        if (flags.isPrivate)
          reportError(s"Illegal flag for Apply: Private")
        val receiverType = typecheckExpr(receiver, env)
        val fullCheck = receiverType match {
          case ClassType(cls) =>
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
            lookupClass(cls).hasInstances
          case NullType | NothingType =>
            // By a similar argument, we must not perform full checks here
            false
          case _ =>
            true
        }
        if (fullCheck) {
          checkApplyGeneric(method, s"$receiverType.$method", args, tree.tpe,
              isStatic = false)
        } else {
          for (arg <- args)
            typecheckExpr(arg, env)
        }

      case ApplyStatically(_, receiver, cls, MethodIdent(method, _), args) =>
        typecheckExpect(receiver, env, ClassType(cls.className))
        checkApplyGeneric(method, s"$cls.$method", args, tree.tpe,
            isStatic = false)

      case ApplyStatic(_, cls, MethodIdent(method, _), args) =>
        val clazz = lookupClass(cls)
        checkApplyGeneric(method, s"$cls.$method", args, tree.tpe,
            isStatic = true)

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
          reportError(s"Array type expected but $arrayType found")

      case ArraySelect(array, index) =>
        typecheckExpect(index, env, IntType)
        typecheckExpr(array, env) match {
          case arrayType: ArrayType =>
            if (tree.tpe != arrayElemType(arrayType))
              reportError(s"Array select of array type $arrayType typed as ${tree.tpe}")
          case arrayType =>
            reportError(s"Array type expected but $arrayType found")
        }

      case IsInstanceOf(expr, testType) =>
        typecheckExpr(expr, env)
        checkIsAsInstanceTargetType(testType)

      case AsInstanceOf(expr, tpe) =>
        typecheckExpr(expr, env)
        checkIsAsInstanceTargetType(tpe)

      case GetClass(expr) =>
        typecheckExpr(expr, env)

      // JavaScript expressions

      case JSNew(ctor, args) =>
        typecheckExpr(ctor, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSPrivateSelect(qualifier, cls, field) =>
        typecheckExpr(qualifier, env)
        val checkedClass = lookupClass(cls)
        if (!checkedClass.kind.isJSClass && checkedClass.kind != ClassKind.AbstractJSType) {
          reportError(s"Cannot select JS private field $field of non-JS class $cls")
        } else {
          if (checkedClass.lookupField(field.name).isEmpty)
            reportError(s"JS class $cls does not have a field $field")
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

      case LoadJSConstructor(cls) =>
        val clazz = lookupClass(cls)
        val valid = clazz.kind match {
          case ClassKind.JSClass       => true
          case ClassKind.JSModuleClass => true
          case ClassKind.NativeJSClass => true
          case _                       => false
        }
        if (!valid)
          reportError(s"JS class type expected but $cls found")
        else if (clazz.jsClassCaptures.nonEmpty)
          reportError(s"Cannot load JS constructor of non-top-level class $cls")

      case LoadJSModule(cls) =>
        val clazz = lookupClass(cls)
        val valid = clazz.kind match {
          case ClassKind.JSModuleClass       => true
          case ClassKind.NativeJSModuleClass => true
          case _                             => false
        }
        if (!valid)
          reportError(s"JS module class type expected but $cls found")

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

      case JSLinkingInfo() =>

      // Literals

      case ClassOf(typeRef) =>
        typeRef match {
          case NullRef | NothingRef =>
            reportError(s"Invalid classOf[$typeRef]")
          case typeRef: ArrayTypeRef =>
            checkArrayTypeRef(typeRef)
          case _ =>
            // ok
        }

      case _: Literal =>

      // Atomic expressions

      case VarRef(LocalIdent(name, _)) =>
        env.locals.get(name).fold[Unit] {
          reportError(s"Cannot find variable $name in scope")
        } { localDef =>
          if (tree.tpe != localDef.tpe)
            reportError(s"Variable $name of type ${localDef.tpe} "+
                s"typed as ${tree.tpe}")
        }

      case This() =>
        if (!isSubtype(env.thisTpe, tree.tpe))
          reportError(s"this of type ${env.thisTpe} typed as ${tree.tpe}")

      case Closure(arrow, captureParams, params, body, captureValues) =>
        /* Check compliance of captureValues wrt. captureParams in the current
         * method state, i.e., outside `withPerMethodState`.
         */
        if (captureParams.size != captureValues.size)
          reportError("Mismatched size for captures: "+
              s"${captureParams.size} params vs ${captureValues.size} values")

        for ((ParamDef(_, ctpe, _, _), value) <- captureParams zip captureValues) {
          typecheckExpect(value, env, ctpe)
        }

        // Then check the closure params and body in its own per-method state
        withPerMethodState {
          for (ParamDef(name, ctpe, mutable, rest) <- captureParams) {
            checkDeclareLocalVar(name)
            if (mutable)
              reportError(s"Capture parameter $name cannot be mutable")
            if (rest)
              reportError(s"Capture parameter $name cannot be a rest parameter")
            if (ctpe == NoType)
              reportError(s"Parameter $name has type NoType")
          }

          checkJSParamDefs(params)

          val thisType = if (arrow) NoType else AnyType
          val bodyEnv = Env.fromSignature(
              thisType, None, captureParams ++ params, AnyType)
          typecheckExpect(body, bodyEnv, AnyType)
        }

      case CreateJSClass(cls, captureValues) =>
        val clazz = lookupClass(cls)
        clazz.jsClassCaptures.fold {
          reportError(s"Invalid CreateJSClass of top-level class $cls")
        } { captureParams =>
          if (captureParams.size != captureValues.size) {
            reportError("Mismatched size for class captures: " +
                s"${captureParams.size} params vs ${captureValues.size} values")
          }

          for ((ParamDef(_, ctpe, _, _), value) <- captureParams.zip(captureValues))
            typecheckExpect(value, env, ctpe)
        }

      case _ =>
        reportError(s"Invalid expression tree")
    }

    tree.tpe
  }

  /** Check the parameters for a method with JS calling conventions. */
  private def checkJSParamDefs(params: List[ParamDef])(
      implicit ctx: ErrorContext): Unit = {
    for (ParamDef(name, ptpe, _, _) <- params) {
      checkDeclareLocalVar(name)
      if (ptpe == NoType)
        reportError(s"Parameter $name has type NoType")
      else if (ptpe != AnyType)
        reportError(s"Parameter $name has type $ptpe but must be any")
    }

    if (params.nonEmpty) {
      for (ParamDef(name, _, _, rest) <- params.init) {
        if (rest)
          reportError(s"Non-last rest parameter $name is illegal")
      }
    }
  }

  private def checkDeclareLocalVar(ident: LocalIdent)(
      implicit ctx: ErrorContext): Unit = {
    if (!declaredLocalVarNamesPerMethod.add(ident.name))
      reportError(s"Duplicate local variable name ${ident.name}.")
  }

  private def checkDeclareLabel(label: LabelIdent)(
      implicit ctx: ErrorContext): Unit = {
    if (!declaredLabelNamesPerMethod.add(label.name))
      reportError(s"Duplicate label named ${label.name}.")
  }

  private def checkIsAsInstanceTargetType(tpe: Type)(
      implicit ctx: ErrorContext): Unit = {
    tpe match {
      case ClassType(className) =>
        val kind = lookupClass(className).kind
        if (kind.isJSType) {
          reportError(
              s"JS type $className is not a valid target type for " +
              "Is/AsInstanceOf")
        }

      case NoType | NullType | NothingType | _:RecordType =>
        reportError(s"$tpe is not a valid target type for Is/AsInstanceOf")

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
        reportError(s"Invalid array type $typeRef")
      case _ =>
        // ok
    }
  }

  private def inferMethodType(encodedName: MethodName, isStatic: Boolean)(
      implicit ctx: ErrorContext): (List[Type], Type) = {

    val (_, paramTypeRefs, resultTypeRef) = decodeMethodName(encodedName)
    val paramTypes = paramTypeRefs.map(typeRefToType)

    val resultType = resultTypeRef.fold[Type] {
      if (isConstructorName(encodedName)) NoType
      else if (encodedName == StaticInitializerName) NoType
      else AnyType // reflective proxy
    } { typeRef =>
      typeRefToType(typeRef)
    }

    (paramTypes, resultType)
  }

  private def typeRefToType(typeRef: TypeRef)(
      implicit ctx: ErrorContext): Type = {
    typeRef match {
      case PrimRef(tpe)               => tpe
      case ClassRef(encodedName)      => classNameToType(encodedName)
      case arrayTypeRef: ArrayTypeRef => ArrayType(arrayTypeRef)
    }
  }

  private def classNameToType(encodedName: ClassName)(
      implicit ctx: ErrorContext): Type = {
    if (encodedName == ObjectClass) {
      AnyType
    } else {
      val kind = lookupClass(encodedName).kind
      if (kind.isJSType) AnyType
      else ClassType(encodedName)
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
      reportError(s"Cannot find class $className")
      new CheckedClass(className, ClassKind.Class, None,
          Some(ObjectClass), Set(ObjectClass), hasInstances = true, None, Nil)
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
        params: List[ParamDef], resultType: Type,
        isConstructor: Boolean = false): Env = {
      val allParams = jsClassCaptures.getOrElse(Nil) ::: params
      val paramLocalDefs =
        for (p @ ParamDef(ident, tpe, mutable, _) <- allParams)
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
      _fields: List[CheckedField])(
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
          CheckedClass.checkedFieldsOf(classDef))
    }

    def lookupField(name: FieldName): Option[CheckedField] =
      fields.get(name)

    def lookupStaticField(name: FieldName): Option[CheckedField] =
      staticFields.get(name)
  }

  private object CheckedClass {
    private def checkedFieldsOf(classDef: LinkedClass): List[CheckedField] = {
      classDef.fields.collect {
        case FieldDef(flags, FieldIdent(name, _), tpe) =>
          new CheckedField(flags, name, tpe)
      }
    }
  }

  private class CheckedField(val flags: MemberFlags, val name: FieldName,
      val tpe: Type)
}

object IRChecker {
  /** Checks that the IR in a [[standard.LinkingUnit LinkingUnit]] is correct.
   *
   *  @return Count of IR checking errors (0 in case of success)
   */
  def check(unit: LinkingUnit, logger: Logger): Int = {
    new IRChecker(unit, logger).check()
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
  private final class ErrorContext private (
      val __private_nodeOrLinkedClass: Any)
      extends AnyVal {

    @inline private def nodeOrLinkedClass: Any = __private_nodeOrLinkedClass

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

  private case class LocalDef(name: LocalName, tpe: Type, mutable: Boolean)(
      val pos: Position)
}
