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

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.logging._

import org.scalajs.linker.checker.ErrorReporter._
import org.scalajs.linker.standard.LinkedClass

/** Checker for the validity of the IR. */
private final class ClassDefChecker(classDef: ClassDef,
    previousPhase: CheckingPhase, reporter: ErrorReporter) {
  import ClassDefChecker._

  import reporter.reportError

  private val featureSet = FeatureSet.allowedAfter(previousPhase)

  private[this] val isJLObject = classDef.name.name == ObjectClass

  private[this] val instanceThisType: Type = {
    val cls = classDef.name.name
    if (classDef.kind.isJSType)
      AnyType
    else if (classDef.kind == ClassKind.HijackedClass)
      BoxedClassToPrimType.getOrElse(cls, ClassType(cls, nullable = false)) // getOrElse not to crash on invalid input
    else
      ClassType(cls, nullable = false)
  }

  private[this] val fields =
    Array.fill(MemberNamespace.Count)(mutable.Map.empty[FieldName, Type])

  private[this] val methods =
    Array.fill(MemberNamespace.Count)(mutable.Set.empty[MethodName])

  private[this] val jsNativeMembers =
    mutable.Set.empty[MethodName] // always public static

  /* Per-method state (set-up with withPerMethodState).
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

  def checkClassDef(): Unit = {
    implicit val ctx = ErrorContext(classDef)

    //// Checks for fields in ClassDef in order.

    // `name` / `originalName` do not need checking.
    checkKind()
    checkJSClassCaptures()
    checkSuperClass()
    checkInterfaces()
    checkJSSuperClass()
    checkJSNativeLoadSpec()

    /* These checks also populate the lookup maps on the instance
     * (fields, methods, jsNativeMembers).
     */
    classDef.fields.foreach(checkFieldDef(_))
    classDef.methods.foreach(checkMethodDef(_))
    classDef.jsConstructor.foreach(checkJSConstructorDef(_))
    classDef.jsMethodProps.foreach {
      case jsMethodDef: JSMethodDef             => checkJSMethodDef(jsMethodDef)
      case jsPropertyDef: JSPropertyDef         => checkJSPropertyDef(jsPropertyDef)
    }
    classDef.jsNativeMembers.foreach(checkJSNativeMemberDef(_))

    // top level exports need the lookup maps to be populated.
    classDef.topLevelExportDefs.foreach(checkTopLevelExportDef(_))

    //// Additional checks

    /* After the base linker, we may have DCE'ed away the constructors of
     * module classes and JS classes that are never instantiated. The classes
     * may still exist because their class data are accessed.
     */
    if (!featureSet.supports(FeatureSet.OptionalConstructors)) {
      /* Check that we have exactly 1 constructor in a module class. This goes
       * together with `checkMethodDef`, which checks that a constructor in a
       * module class must be 0-arg.
       */
      if (classDef.kind == ClassKind.ModuleClass &&
          methods(MemberNamespace.Constructor.ordinal).size != 1) {
        reportError("Module class must have exactly 1 constructor")
      }

      if (classDef.kind.isJSClass && classDef.jsConstructor.isEmpty)
        reportError("JS classes and module classes must have a constructor")
    }
  }

  private def checkKind()(implicit ctx: ErrorContext): Unit = {
    val className = classDef.name.name

    if ((isJLObject || className == ThrowableClass) && classDef.kind != ClassKind.Class) {
      reportError(i"$className must be a Class")
    } else {
      val isHijacked = HijackedClasses.contains(className)
      if (isHijacked && classDef.kind != ClassKind.HijackedClass)
        reportError(i"$className must be a HijackedClass")
      else if (!isHijacked && classDef.kind == ClassKind.HijackedClass)
        reportError(i"$className must not be a HijackedClass")
    }
  }

  private def checkJSClassCaptures()(implicit ctx: ErrorContext): Unit = {
    for (classCaptures <- classDef.jsClassCaptures) {
      if (classDef.kind != ClassKind.JSClass) {
        reportError(
            i"Class ${classDef.name} which is not a non-native JS class " +
            "cannot have class captures")
      }

      val alreadyDeclared = mutable.Set.empty[LocalName]
      for {
        p @ ParamDef(ident, _, tpe, mutable) <- classCaptures
      } {
        implicit val ctx = ErrorContext(p)
        val name = ident.name
        if (name.isThis)
          reportError(i"Illegal JS class capture with name '$name'")
        if (!alreadyDeclared.add(name))
          reportError(i"Duplicate JS class capture '$name'")
        if (tpe == VoidType)
          reportError(i"The JS class capture $name cannot have type VoidType")
        if (mutable)
          reportError(i"The JS class capture $name cannot be mutable")
      }
    }
  }

  private def checkSuperClass()(implicit ctx: ErrorContext): Unit = {
    classDef.kind match {
      case _ if isJLObject =>
        if (classDef.superClass.isDefined)
          reportError("java.lang.Object cannot have a superClass")

      case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass |
          ClassKind.JSClass | ClassKind.JSModuleClass |
          ClassKind.NativeJSClass | ClassKind.NativeJSModuleClass =>
        if (classDef.superClass.isEmpty)
          reportError("missing superClass")
        else if (classDef.className == ThrowableClass && classDef.superClass.get.name != ObjectClass)
          reportError("the superClass of java.lang.Throwable must be java.lang.Object")

      case ClassKind.Interface =>
        if (classDef.superClass.isDefined)
          reportError("interfaces may not have a superClass")

      case ClassKind.AbstractJSType =>
        // Either is OK.
    }
  }

  private def checkInterfaces()(implicit ctx: ErrorContext): Unit = {
    if (isJLObject && classDef.interfaces.nonEmpty)
      reportError("java.lang.Object may not implement any interfaces")
  }

  private def checkJSSuperClass()(implicit ctx: ErrorContext): Unit = {
    if (!classDef.kind.isJSClass && classDef.jsSuperClass.isDefined)
      reportError("Only non-native JS types may have a jsSuperClass")

    classDef.jsSuperClass.foreach(checkTree(_, Env.fromParams(classDef.jsClassCaptures.getOrElse(Nil))))
  }

  private def checkJSNativeLoadSpec()(implicit ctx: ErrorContext): Unit = {
    /* Note that native JS classes may be missing a jsNativeLoadSpec in case they are nested.
     * See #4402 for details.
     */
    if (!classDef.kind.isNativeJSClass && classDef.jsNativeLoadSpec.isDefined)
      reportError("Only native JS classes may have a jsNativeLoadSpec")
  }

  private def checkFieldDef(fieldDef: AnyFieldDef): Unit = {
    implicit val ctx = ErrorContext(fieldDef)

    val namespace = fieldDef.flags.namespace

    if (namespace.isPrivate)
      reportError("A field cannot be private")
    if (namespace.isConstructor)
      reportError("A field cannot be a constuctor")

    fieldDef match {
      case FieldDef(_, FieldIdent(name), _, ftpe) =>
        if (!classDef.kind.isAnyNonNativeClass)
          reportError("illegal FieldDef (only non native classes may contain fields)")
        if (name.className != classDef.className)
          reportError(i"illegal FieldDef with name $name in class ${classDef.className}")
        if (fields(namespace.ordinal).put(name, ftpe).isDefined)
          reportError(i"duplicate ${namespace.prefixString}field '$name'")

      case JSFieldDef(_, name, _) =>
        if (!classDef.kind.isJSClass)
          reportError("illegal JSFieldDef in non JS class")
        checkTree(name, Env.empty)
    }

    fieldDef.ftpe match {
      case VoidType | NothingType | AnyNotNullType | ClassType(_, false) |
          ArrayType(_, false) | _:RecordType =>
        reportError(i"FieldDef cannot have type ${fieldDef.ftpe}")
      case _ =>
        // ok
    }
  }

  private def checkMethodDef(methodDef: MethodDef): Unit = withPerMethodState {
    val MethodDef(flags, MethodIdent(name), _, params, _, optBody) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    val namespace = flags.namespace
    val static = namespace.isStatic
    val isConstructor = namespace == MemberNamespace.Constructor

    if (flags.isMutable)
      reportError("A method cannot have the flag Mutable")

    checkMethodNameNamespace(name, namespace)

    if (!methods(namespace.ordinal).add(name))
      reportError(i"duplicate ${namespace.prefixString}method '$name'")

    if (optBody.isEmpty && namespace != MemberNamespace.Public)
      reportError("Abstract methods may only be in the public namespace")

    // ClassInitializer
    if (name.isClassInitializer) {
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

    classDef.kind match {
      case ClassKind.ModuleClass =>
        if (isConstructor && name != NoArgConstructorName)
          reportError("Module class must have a parameterless constructor")

      case ClassKind.Class | ClassKind.HijackedClass =>
        // all namespaces are allowed (except for class initializers as checked above)

      case ClassKind.Interface =>
        if (isConstructor)
          reportError("Interfaces cannot declare constructors")

      case ClassKind.JSClass | ClassKind.JSModuleClass =>
        if (!static)
          reportError("Non exported instance method is illegal in JS class")

      case ClassKind.NativeJSClass | ClassKind.NativeJSModuleClass | ClassKind.AbstractJSType =>
        if (!static)
          reportError("illegal instance member")
    }


    // Params
    for (ParamDef(name, _, tpe, _) <- params) {
      checkDeclareLocalVar(name)
      if (tpe == VoidType)
        reportError(i"Parameter $name has type VoidType")
    }

    // Body
    for (body <- optBody) {
      val bodyEnv = Env.fromParams(params)
        .withMaybeThisType(!static, instanceThisType)

      if (isConstructor)
        checkConstructorBody(body, bodyEnv)
      else
        checkTree(body, bodyEnv)
    }
  }

  private def checkJSConstructorDef(ctorDef: JSConstructorDef): Unit = withPerMethodState {
    val JSConstructorDef(flags, params, restParam, body) = ctorDef
    implicit val ctx = ErrorContext(ctorDef)

    if (flags.isMutable)
      reportError("A JS constructor cannot have the flag Mutable")
    if (flags.namespace != MemberNamespace.Constructor)
      reportError("A JS constructor must be in the constructor namespace")

    if (!classDef.kind.isJSClass)
      reportError("JS constructor defs can only appear in JS classes")

    checkJSParamDefs(params, restParam)

    val startEnv = Env.fromParams(classDef.jsClassCaptures.getOrElse(Nil) ++ params ++ restParam)
      .withHasNewTarget(true)

    val envJustBeforeSuper = checkBlockStats(body.beforeSuper, startEnv)
    checkTreeOrSpreads(body.superCall.args, envJustBeforeSuper)
    val envJustAfterSuper = envJustBeforeSuper.withThisType(instanceThisType)
    val afterSuperStoreModulesHandled =
      handleStoreModulesAfterSuperCtorCall(body.afterSuper)
    checkBlockStats(afterSuperStoreModulesHandled, envJustAfterSuper)
  }

  private def checkJSMethodDef(methodDef: JSMethodDef): Unit = withPerMethodState {
    val JSMethodDef(flags, pName, params, restParam, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    val static = flags.namespace.isStatic

    if (flags.isMutable)
      reportError("An exported method cannot have the flag Mutable")
    if (flags.namespace.isPrivate)
      reportError("An exported method cannot be private")
    if (flags.namespace.isConstructor)
      reportError("An exported method cannot be in the constructor namespace")

    if (!classDef.kind.isAnyNonNativeClass)
      reportError("Exported method def can only appear in a class")
    else if (static && classDef.kind != ClassKind.JSClass)
      reportError("Exported method def in non-JS class cannot be static")

    checkExportedPropertyName(pName)
    checkJSParamDefs(params, restParam)

    val env = Env.fromParams(classDef.jsClassCaptures.getOrElse(Nil) ++ params ++ restParam)
      .withMaybeThisType(!static, instanceThisType)

    checkTree(body, env)
  }

  private def checkJSPropertyDef(propDef: JSPropertyDef): Unit = {
    val JSPropertyDef(flags, pName, getterBody, setterArgAndBody) = propDef
    implicit val ctx = ErrorContext(propDef)

    val static = flags.namespace.isStatic

    if (flags.isMutable)
      reportError("An exported property def cannot have the flag Mutable")
    if (flags.namespace.isPrivate)
      reportError("An exported property def cannot be private")
    if (flags.namespace.isConstructor)
      reportError("An exported property def cannot be in the constructor namespace")

    if (!classDef.kind.isAnyNonNativeClass)
      reportError("Exported property def can only appear in a class")

    checkExportedPropertyName(pName)

    val jsClassCaptures = classDef.jsClassCaptures.getOrElse(Nil)

    getterBody.foreach { body =>
      withPerMethodState {
        val bodyEnv = Env.fromParams(jsClassCaptures)
          .withMaybeThisType(!static, instanceThisType)
        checkTree(body, bodyEnv)
      }
    }

    setterArgAndBody.foreach { case (setterArg, body) =>
      withPerMethodState {
        checkJSParamDefs(setterArg :: Nil, None)
        val bodyEnv = Env.fromParams(jsClassCaptures :+ setterArg)
          .withMaybeThisType(!static, instanceThisType)
        checkTree(body, bodyEnv)
      }
    }
  }

  private def checkJSNativeMemberDef(jsNativeMemberDef: JSNativeMemberDef): Unit = {
    val JSNativeMemberDef(flags, MethodIdent(name), _) = jsNativeMemberDef
    implicit val ctx = ErrorContext(jsNativeMemberDef)

    val namespace = flags.namespace

    if (flags.isMutable)
      reportError("A js native def cannot have the flag Mutable")
    if (namespace != MemberNamespace.PublicStatic)
      reportError("A js native def must be in the public static namespace")

    checkMethodNameNamespace(name, namespace)

    if (!jsNativeMembers.add(name))
      reportError(i"duplicate js native member def $name")
  }

  private def checkExportedPropertyName(propName: Tree)(
      implicit ctx: ErrorContext): Unit = {
    propName match {
      case StringLiteral(name) =>
        if (!classDef.kind.isJSClass && name.contains("__"))
          reportError("Exported method def name cannot contain __")

      case _ =>
        if (!classDef.kind.isJSClass)
          reportError("Only JS classes may contain members with computed names")
        checkTree(propName, Env.empty)
    }
  }

  private def checkTopLevelExportDef(topLevelExportDef: TopLevelExportDef): Unit = {
    implicit val ctx = ErrorContext(topLevelExportDef)

    topLevelExportDef match {
      case _: TopLevelJSClassExportDef =>
        if (classDef.kind != ClassKind.JSClass)
          reportError("Exported JS class def can only appear in a JS class")

      case _: TopLevelModuleExportDef =>
        if (!classDef.kind.hasModuleAccessor)
          reportError("Top-level module export def can only appear in a module class")

      case TopLevelMethodExportDef(_, methodDef) =>
        checkTopLevelMethodExportDef(methodDef)

      case topLevelExportDef: TopLevelFieldExportDef =>
        checkTopLevelFieldExportDef(topLevelExportDef)
    }
  }

  private def checkTopLevelMethodExportDef(methodDef: JSMethodDef): Unit = withPerMethodState {
    val JSMethodDef(flags, pName, params,  restParam, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    if (flags.isMutable)
      reportError("Top level export method cannot have the flag Mutable")
    if (flags.namespace != MemberNamespace.PublicStatic)
      reportError("Top level export must be public and static")

    if (!pName.isInstanceOf[StringLiteral])
      reportError("Top level exports may not have computed names")

    checkJSParamDefs(params, restParam)
    checkTree(body, Env.fromParams(params ++ restParam))
  }

  private def checkTopLevelFieldExportDef(
      topLevelFieldExportDef: TopLevelFieldExportDef): Unit = {
    implicit val ctx = ErrorContext(topLevelFieldExportDef)

    if (!classDef.kind.isAnyNonNativeClass)
      reportError("native classes may not have field exports")

    val field = topLevelFieldExportDef.field

    fields(MemberNamespace.PublicStatic.ordinal).get(field.name).fold {
      reportError(i"Cannot export non-existent static field '$field'")
    } { tpe =>
      if (tpe != AnyType)
        reportError(i"Cannot export field '$field' of type $tpe")
    }
  }

  private def checkMethodNameNamespace(name: MethodName, namespace: MemberNamespace)(
      implicit ctx: ErrorContext): Unit = {
    if (name.isReflectiveProxy) {
      if (featureSet.supports(FeatureSet.ReflectiveProxies)) {
        if (namespace != MemberNamespace.Public)
          reportError("reflective profixes are only allowed in the public namespace")
      } else {
        reportError("illegal reflective proxy")
      }
    }

    if (name.isConstructor != (namespace == MemberNamespace.Constructor))
      reportError("a member can have a constructor name iff it is in the constructor namespace")

    if ((name.isStaticInitializer || name.isClassInitializer) != (namespace == MemberNamespace.StaticConstructor))
      reportError("a member can have a static constructor name iff it is in the static constructor namespace")

    if ((name.resultTypeRef :: name.paramTypeRefs).exists(_.isInstanceOf[TransientTypeRef])) {
      if (featureSet.supports(FeatureSet.TransientTypeRefs)) {
        if (namespace == MemberNamespace.Public)
          reportError(i"Illegal transient type ref in public method $name")
      } else {
        reportError(i"Illegal transient type ref in method name $name")
      }
    }
  }

  private def checkCaptureParamDefs(params: List[ParamDef])(
      implicit ctx: ErrorContext): Unit = {
    for (ParamDef(name, _, ctpe, mutable) <- params) {
      checkDeclareLocalVar(name)
      if (mutable)
        reportError(i"Capture parameter $name cannot be mutable")
      if (ctpe == VoidType)
        reportError(i"Parameter $name has type VoidType")
    }
  }

  private def checkJSParamDefs(params: List[ParamDef], restParam: Option[ParamDef])(
      implicit ctx: ErrorContext): Unit = {
    for (ParamDef(name, _, ptpe, _) <- params ++ restParam) {
      checkDeclareLocalVar(name)
      if (ptpe != AnyType)
        reportError(i"Parameter $name has type $ptpe but must be any")
    }
  }

  private def checkTypedParamDefs(params: List[ParamDef])(
      implicit ctx: ErrorContext): Unit = {
    for (ParamDef(name, _, ctpe, _) <- params) {
      checkDeclareLocalVar(name)
      if (ctpe == VoidType)
        reportError(i"Parameter $name has type VoidType")
    }
  }

  private def checkConstructorBody(body: Tree, bodyEnv: Env): Unit = {
    /* If the enclosing class is `jl.Object`, the `body` cannot contain any
     * delegate constructor call.
     *
     * Otherwise:
     *
     * - Let `stats` be the list of flattened statements in `body`.
     * - There exists a unique `stat` in the `stats` list that is an
     *   `ApplyStatically(_, This(), cls, someConstructor, args)`, called the
     *   delegate constructor call.
     * - There is no such `ApplyStatically` anywhere else in the body.
     * - `cls` must be the enclosing class or its direct superclass.
     * - In the statements before the delegate constructor call, and within
     *   `args`, `This()` cannot be used except in `Assign(Select(This(), _), _)`,
     *   i.e., to assign to a field (but not read from one).
     * - After the delegate constructor call, `This` can be used without
     *   restriction. Moreover, we can have `StoreModule`s at the top-level.
     *
     * After the optimizer, there may be no delegate constructor call at all.
     * This frequently happens as the optimizer inlines super constructor
     * calls. If there is one, `cls` can be any class (it must still be some
     * class in the superclass chain for the types to align, but this is not
     * checked here).
     */

    implicit val ctx = ErrorContext(body)

    val bodyStats = body match {
      case Block(stats) => stats
      case Skip()       => Nil
      case _            => body :: Nil
    }

    if (isJLObject) {
      checkBlockStats(bodyStats, bodyEnv)
    } else {
      val (beforeDelegateCtor, rest) = bodyStats.span {
        case ApplyStatically(_, This(), _, MethodIdent(ctor), _) =>
          !ctor.isConstructor
        case _ =>
          true
      }

      if (rest.isEmpty) {
        if (!featureSet.supports(FeatureSet.RelaxedCtorBodies))
          reportError(i"Constructor must contain a delegate constructor call")

        val bodyStatsStoreModulesHandled =
          handleStoreModulesAfterSuperCtorCall(bodyStats)
        checkBlockStats(bodyStatsStoreModulesHandled, bodyEnv)
      } else {
        val (delegateCtorCall: ApplyStatically) :: afterDelegateCtor = rest
        val ApplyStatically(_, receiver, cls, MethodIdent(ctor), args) = delegateCtorCall

        val initEnv = bodyEnv.withIsThisRestricted(true)
        val envJustBeforeDelegate = checkBlockStats(beforeDelegateCtor, initEnv)

        checkApplyArgs(ctor, args, envJustBeforeDelegate)

        val unrestrictedEnv = envJustBeforeDelegate.withIsThisRestricted(false)

        checkTree(receiver, unrestrictedEnv) // check that the This itself is valid

        if (!featureSet.supports(FeatureSet.RelaxedCtorBodies)) {
          if (!(cls == classDef.className || classDef.superClass.exists(_.name == cls))) {
            implicit val ctx = ErrorContext(delegateCtorCall)
            reportError(
                i"Invalid target class $cls for delegate constructor call; " +
                i"expected ${classDef.className}" +
                classDef.superClass.fold("")(s => i" or ${s.name}"))
          }
        }

        val afterDelegateCtorStoreModulesHandled =
          handleStoreModulesAfterSuperCtorCall(afterDelegateCtor)
        checkBlockStats(afterDelegateCtorStoreModulesHandled, unrestrictedEnv)
      }
    }
  }

  private def handleStoreModulesAfterSuperCtorCall(trees: List[Tree])(
      implicit ctx: ErrorContext): List[Tree] = {

    if (classDef.kind.hasModuleAccessor) {
      if (featureSet.supports(FeatureSet.RelaxedCtorBodies)) {
        /* If the super constructor call was inlined, the StoreModule can be anywhere.
         * Moreover, the optimizer can remove StoreModules altogether in many cases.
         */
        trees.filter(!_.isInstanceOf[StoreModule])
      } else {
        /* Before the optimizer, there must be a StoreModule and it must come
         * right after the super constructor call.
         */
        trees match {
          case StoreModule() :: rest =>
            rest
          case _ =>
            reportError(i"Missing StoreModule right after the super constructor call")
            trees
        }
      }
    } else {
      trees
    }
  }

  private def checkBlockStats(stats: List[Tree], env: Env): Env = {
    stats.foldLeft(env) { (prevEnv, stat) =>
      checkTree(stat, prevEnv)
    }
  }

  private def checkTreeOrSpreads(trees: List[TreeOrJSSpread], env: Env): Unit = {
    trees.foreach {
      case JSSpread(items) => checkTree(items, env)
      case tree: Tree      => checkTree(tree, env)
    }
  }

  private def checkTrees(trees: List[Tree], env: Env): Unit =
    trees.foreach(checkTree(_, env))

  private def checkApplyArgs(methodName: MethodName, args: List[Tree], env: Env)(
      implicit ctx: ErrorContext): Unit = {
    val paramRefsCount = methodName.paramTypeRefs.size
    if (args.size != paramRefsCount)
      reportError(i"Arity mismatch: $paramRefsCount expected but ${args.size} found")
    checkTrees(args, env)
  }

  private def checkTree(tree: Tree, env: Env): Env = {
    implicit val ctx = ErrorContext(tree)

    def checkApplyGeneric(methodName: MethodName, args: List[Tree]): Unit =
      checkApplyArgs(methodName, args, env)

    val newEnv = tree match {
      case VarDef(ident, _, vtpe, mutable, _) =>
        env.withLocal(LocalDef(ident.name, vtpe, mutable))
      case _ =>
        env
    }

    tree match {
      case VarDef(ident, _, vtpe, mutable, rhs) =>
        checkDeclareLocalVar(ident)
        checkTree(rhs, env)

      case Skip() =>

      case Block(stats) =>
        checkBlockStats(stats, env)

      case Labeled(label, _, body) =>
        checkDeclareLabel(label)
        checkTree(body, env.withLabel(label))

      case Assign(lhs, rhs) =>
        lhs match {
          case Select(This(), field) if env.isThisRestricted =>
            if (featureSet.supports(FeatureSet.RelaxedCtorBodies) || field.name.className == classDef.className)
              checkTree(lhs, env.withIsThisRestricted(false))
            else
              checkTree(lhs, env)
          case _ =>
            checkTree(lhs, env)
        }
        checkTree(rhs, env)

        lhs match {
          case VarRef(name) =>
            if (env.locals.get(name).exists(!_.mutable))
              reportError(i"Assignment to immutable variable $name.")

          case JSGlobalRef(JSGlobalRef.FileLevelThis) =>
            reportError(i"Assignment to global this.")

          case RecordSelect(record, SimpleFieldIdent(fieldName)) =>
            record.tpe match {
              case RecordType(fields) =>
                if (fields.find(_.name == fieldName).exists(!_.mutable))
                  reportError(i"assignment to immutable record field $fieldName")

              case _ =>
                // ok (NothingType) or IRChecker will complain.
            }

            @tailrec
            def check(lhs: Tree): Unit = lhs match {
              case RecordSelect(inner, _) => check(inner)
              case _: VarRef              => // ok

              case lhs =>
                reportError(i"Assignment to RecordSelect of illegal tree: ${lhs.getClass.getName}")
            }

            check(record)

          case _:Select | _:JSPrivateSelect | _:SelectStatic |
              _:ArraySelect | _:JSSelect | _:JSSuperSelect |
              _:JSGlobalRef =>
        }

      case Return(expr, label) =>
        if (!env.returnLabels.contains(label))
          reportError(i"unknown label $label.")

        checkTree(expr, env)

      case If(cond, thenp, elsep) =>
        checkTree(cond, env)
        checkTree(thenp, env)
        checkTree(elsep, env)

      case LinkTimeIf(cond, thenp, elsep) =>
        if (!featureSet.supports(FeatureSet.LinkTimeNodes))
          reportError(i"Illegal link-time if after desugaring")
        checkLinkTimeTree(cond, BooleanType)
        checkTree(thenp, env)
        checkTree(elsep, env)

      case While(cond, body) =>
        checkTree(cond, env)
        checkTree(body, env)

      case ForIn(obj, keyVar, _, body) =>
        checkTree(obj, env)
        checkDeclareLocalVar(keyVar)
        val bodyEnv = env.withLocal(LocalDef(keyVar.name, AnyType, false))
        checkTree(body, bodyEnv)

      case TryCatch(block, errVar, _, handler) =>
        checkTree(block, env)
        checkDeclareLocalVar(errVar)
        val handlerEnv =
          env.withLocal(LocalDef(errVar.name, AnyType, false))
        checkTree(handler, handlerEnv)

      case TryFinally(block, finalizer) =>
        checkTree(block, env)
        checkTree(finalizer, env)

      case Match(selector, cases, default) =>
        checkTree(selector, env)
        for ((alts, body) <- cases) {
          checkTrees(alts, env)
          checkTree(body, env)
        }

        checkTree(default, env)

      case JSAwait(arg) =>
        checkTree(arg, env)

      case Debugger() =>

      // Scala expressions

      case New(_, ctor, args) =>
        checkApplyGeneric(ctor.name, args)

      case _: LoadModule =>

      case StoreModule() =>
        reportError(i"Illegal StoreModule")

      case Select(qualifier, _) =>
        checkTree(qualifier, env)

      case _: SelectStatic =>

      case _: SelectJSNativeMember =>

      case Apply(flags, receiver, MethodIdent(method), args) =>
        if (flags.isPrivate)
          reportError("invalid flag Private for Apply")
        checkTree(receiver, env)
        checkApplyGeneric(method, args)

      case ApplyStatically(_, receiver, _, MethodIdent(method), args) =>
        if (method.isConstructor)
          reportError(i"Illegal constructor call")
        checkTree(receiver, env)
        checkApplyGeneric(method, args)

      case ApplyStatic(_, _, MethodIdent(method), args) =>
        checkApplyGeneric(method, args)

      case ApplyDynamicImport(flags, className, MethodIdent(method), args) =>
        if (flags.isPrivate)
          reportError("invalid flag Private for ApplyDynamicImport")
        if (flags.isConstructor)
          reportError("invalid flag Constructor for ApplyDynamicImport")

        checkApplyGeneric(method, args)

      case ApplyTypedClosure(flags, fun, args) =>
        if (!featureSet.supports(FeatureSet.TypedClosures))
          reportError(i"Illegal node ApplyTypedClosure")

        if (flags.isPrivate)
          reportError("invalid flag Private for ApplyTypedClosure")
        if (flags.isConstructor)
          reportError("invalid flag Constructor for ApplyTypedClosure")

        checkTree(fun, env)
        checkAppliedClosureType(fun.tpe)
        checkTrees(args, env)

        fun.tpe match {
          case ClosureType(paramTypes, resultType, _) =>
            if (args.size != paramTypes.size)
              reportError(i"Arity mismatch: ${paramTypes.size} expected but ${args.size} found")
          case _ =>
            () // OK, notably for NothingType
        }

      case NewLambda(descriptor, fun) =>
        if (!featureSet.supports(FeatureSet.NewLambda))
          reportError(i"Illegal NewLambda after desugaring")

        fun match {
          case fun: Closure if fun.flags.typed =>
            checkClosure(fun, env)
          case _ =>
            reportError(i"The argument to a NewLambda must be a typed closure")
            checkTree(fun, env)
        }

      case UnaryOp(_, lhs) =>
        checkTree(lhs, env)

      case BinaryOp(_, lhs, rhs) =>
        checkTree(lhs, env)
        checkTree(rhs, env)

      case NewArray(typeRef, length) =>
        checkArrayTypeRef(typeRef)
        checkTree(length, env)

      case ArrayValue(typeRef, elems) =>
        checkArrayTypeRef(typeRef)
        checkTrees(elems, env)

      case ArraySelect(array, index) =>
        checkTree(array, env)
        checkTree(index, env)

      case RecordSelect(record, _) =>
        if (!featureSet.supports(FeatureSet.Records))
          reportError("invalid use of records")
        checkTree(record, env)

      case RecordValue(_, elems) =>
        if (!featureSet.supports(FeatureSet.Records))
          reportError("invalid use of records")
        checkTrees(elems, env)

      case IsInstanceOf(expr, testType) =>
        checkTree(expr, env)
        testType match {
          case VoidType | NullType | NothingType | AnyType |
              ClassType(_, true) | ArrayType(_, true) | _:ClosureType | _:RecordType =>
            reportError(i"$testType is not a valid test type for IsInstanceOf")
          case testType: ArrayType =>
            checkArrayType(testType)
          case _ =>
            // ok
        }

      case AsInstanceOf(expr, tpe) =>
        checkTree(expr, env)
        tpe match {
          case VoidType | NullType | NothingType | AnyNotNullType |
              ClassType(_, false) | ArrayType(_, false) | _:ClosureType | _:RecordType =>
            reportError(i"$tpe is not a valid target type for AsInstanceOf")
          case tpe: ArrayType =>
            checkArrayType(tpe)
          case _ =>
            // ok
        }

      case LinkTimeProperty(name) =>
        if (!featureSet.supports(FeatureSet.LinkTimeNodes))
          reportError(i"Illegal link-time property '$name' after desugaring")

        tree.tpe match {
          case BooleanType | IntType | StringType =>
            () // ok
          case tpe =>
            reportError(i"$tpe is not a valid type for LinkTimeProperty")
        }

      // JavaScript expressions

      case JSNew(ctor, args) =>
        checkTree(ctor, env)
        checkTreeOrSpreads(args, env)

      case JSPrivateSelect(qualifier, _) =>
        checkTree(qualifier, env)

      case JSSelect(qualifier, item) =>
        checkTree(qualifier, env)
        checkTree(item, env)

      case JSFunctionApply(fun, args) =>
        checkTree(fun, env)
        checkTreeOrSpreads(args, env)

      case JSMethodApply(receiver, method, args) =>
        checkTree(receiver, env)
        checkTree(method, env)
        checkTreeOrSpreads(args, env)

      case JSSuperSelect(superClass, qualifier, item) =>
        checkTree(superClass, env)
        checkTree(qualifier, env)
        checkTree(item, env)

      case JSSuperMethodCall(superClass, receiver, method, args) =>
        checkTree(superClass, env)
        checkTree(receiver, env)
        checkTree(method, env)
        checkTreeOrSpreads(args, env)

      case JSSuperConstructorCall(args) =>
        reportError("illegal JSSuperConstructorCall")
        checkTreeOrSpreads(args, env)

      case JSImportCall(arg) =>
        checkTree(arg, env)

      case JSNewTarget() =>
        if (!env.hasNewTarget)
          reportError("Cannot refer to `new.target` outside of a JS class constructor or non-arrow function")

      case JSImportMeta() =>

      case LoadJSConstructor(_) =>

      case LoadJSModule(_) =>

      case JSDelete(qualifier, item) =>
        checkTree(qualifier, env)
        checkTree(item, env)

      case JSUnaryOp(_, lhs) =>
        checkTree(lhs, env)

      case JSBinaryOp(_, lhs, rhs) =>
        checkTree(lhs, env)
        checkTree(rhs, env)

      case JSArrayConstr(items) =>
        checkTreeOrSpreads(items, env)

      case JSObjectConstr(fields) =>
        for ((key, value) <- fields) {
          checkTree(key, env)
          checkTree(value, env)
        }

      case JSGlobalRef(_) =>

      case JSTypeOfGlobalRef(_) =>

      // Literals

      case ClassOf(typeRef) =>
        typeRef match {
          case NullRef | NothingRef =>
            reportError(i"Invalid classOf[$typeRef]")
          case typeRef: ArrayTypeRef =>
            checkArrayTypeRef(typeRef)
          case typeRef: TransientTypeRef =>
            reportError(i"Illegal special type ref in classOf[$typeRef]")
          case _ =>
            // ok
        }

      case _: Literal =>

      // Atomic expressions

      case VarRef(name) =>
        env.locals.get(name).fold[Unit] {
          reportError(i"Cannot find variable $name in scope")
        } { localDef =>
          if (tree.tpe != localDef.tpe)
            reportError(i"Variable $name of type ${localDef.tpe} typed as ${tree.tpe}")
        }
        if (env.isThisRestricted && name.isThis)
          reportError(i"Restricted use of `this` before the super constructor call")

      case tree: Closure =>
        if (tree.flags.typed && !featureSet.supports(FeatureSet.TypedClosures))
          reportError(i"Illegal typed closure outside of a NewLambda")
        checkClosure(tree, env)

      case CreateJSClass(className, captureValues) =>
        checkTrees(captureValues, env)

      case Transient(transient) =>
        if (!featureSet.supports(FeatureSet.OptimizedTransients))
          reportError(i"invalid transient tree of class ${transient.getClass().getName()}")

        transient.traverse(new Traversers.Traverser {
          override def traverse(tree: Tree): Unit = checkTree(tree, env)
        })
    }

    newEnv
  }

  private def checkClosure(tree: Closure, env: Env): Unit = {
    implicit val ctx = ErrorContext(tree)

    val Closure(flags, captureParams, params, restParam, resultType, body, captureValues) = tree

    if (flags.typed && !flags.arrow)
      reportError(i"A typed closure must have the 'arrow' flag")
    if (flags.typed && flags.async)
      reportError(i"A typed closure cannot have the 'async' flag")

    /* Check compliance of captureValues wrt. captureParams in the current
     * method state, i.e., outside `withPerMethodState`.
     */
    if (captureParams.size != captureValues.size) {
      reportError(
          "Mismatched size for captures: "+
          i"${captureParams.size} params vs ${captureValues.size} values")
    }

    checkTrees(captureValues, env)

    // Then check the closure params and body in its own per-method state
    withPerMethodState {
      checkCaptureParamDefs(captureParams)

      if (flags.typed) {
        checkTypedParamDefs(params)
        if (restParam.isDefined)
          reportError(i"A typed closure may not have a rest param")
      } else {
        checkJSParamDefs(params, restParam)
        if (resultType != AnyType)
          reportError(i"A JS closure must have result type 'any' but found '$resultType'")
      }

      val bodyEnv = Env
        .fromParams(captureParams ++ params ++ restParam)
        .withHasNewTarget(!flags.arrow)
        .withMaybeThisType(!flags.arrow, AnyType)
      checkTree(body, bodyEnv)
    }
  }

  private def checkLinkTimeTree(tree: Tree, expectedType: PrimType): Unit = {
    implicit val ctx = ErrorContext(tree)

    /* For link-time trees, we need to check the types. Having a well-typed
     * condition is required for `LinkTimeIf` to be resolved, and that happens
     * before IR checking. Fortunately, only trivial primitive types can appear
     * in link-time trees, and it is therefore possible to check them now.
     */
    if (tree.tpe != expectedType)
      reportError(i"$expectedType expected but ${tree.tpe} found in link-time tree")

    /* Unlike the evaluation algorithm, at this time we allow LinkTimeProperty's
     * that are not actually available. We only check that their declared type
     * matches the expected type. If it does not exist or does not have the
     * type it was declared with, that constitutes a *linking error*, but it
     * does not make the ClassDef invalid.
     */

    tree match {
      case _:IntLiteral | _:BooleanLiteral | _:StringLiteral | _:LinkTimeProperty =>
        () // ok

      case UnaryOp(op, lhs) =>
        import UnaryOp._
        op match {
          case Boolean_! =>
            checkLinkTimeTree(lhs, BooleanType)
          case _ =>
            reportError(i"illegal unary op $op in link-time tree")
        }

      case BinaryOp(op, lhs, rhs) =>
        import BinaryOp._
        op match {
          case Boolean_== | Boolean_!= | Boolean_| | Boolean_& =>
            checkLinkTimeTree(lhs, BooleanType)
            checkLinkTimeTree(rhs, BooleanType)
          case Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>= =>
            checkLinkTimeTree(lhs, IntType)
            checkLinkTimeTree(rhs, IntType)
          case _ =>
            reportError(i"illegal binary op $op in link-time tree")
        }

      case LinkTimeIf(cond, thenp, elsep) =>
        checkLinkTimeTree(cond, BooleanType)
        checkLinkTimeTree(thenp, expectedType)
        checkLinkTimeTree(elsep, expectedType)

      case _ =>
        reportError(i"illegal tree of class ${tree.getClass().getName()} in link-time tree")
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

  private def checkAppliedClosureType(tpe: Type)(
      implicit ctx: ErrorContext): Unit = tpe match {
    case tpe: ClosureType       => checkClosureType(tpe)
    case NothingType | NullType => // ok
    case _                      => reportError(s"Closure type expected but $tpe found")
  }

  private def checkClosureType(tpe: ClosureType)(
      implicit ctx: ErrorContext): Unit = {
    for (paramType <- tpe.paramTypes) {
      paramType match {
        case paramType: ArrayType   => checkArrayType(paramType)
        case paramType: ClosureType => checkClosureType(paramType)
        case VoidType               => reportError(i"Illegal parameter type $paramType")
        case _                      => () // ok
      }
    }
  }

  private def checkDeclareLocalVar(ident: LocalIdent)(
      implicit ctx: ErrorContext): Unit = {
    if (ident.name.isThis)
      reportError(i"Illegal definition of a variable with name ${ident.name}")
    if (!declaredLocalVarNamesPerMethod.add(ident.name))
      reportError(i"Duplicate local variable name ${ident.name}.")
  }

  private def checkDeclareLabel(label: LabelName)(
      implicit ctx: ErrorContext): Unit = {
    if (!declaredLabelNamesPerMethod.add(label))
      reportError(i"Duplicate label named $label.")
  }
}

object ClassDefChecker {
  /** Checks that the IR in a ClassDef is correct.
   *
   *  @return Count of IR checking errors (0 in case of success)
   */
  def check(classDef: ClassDef, previousPhase: CheckingPhase, logger: Logger): Int = {
    val reporter = new LoggerErrorReporter(logger)
    new ClassDefChecker(classDef, previousPhase, reporter).checkClassDef()
    reporter.errorCount
  }

  def check(linkedClass: LinkedClass, previousPhase: CheckingPhase, logger: Logger): Int = {
    // Rebuild a ClassDef out of the LinkedClass
    import linkedClass._
    implicit val pos = linkedClass.pos
    val classDef = ClassDef(
      name,
      OriginalName.NoOriginalName,
      kind,
      jsClassCaptures,
      superClass,
      interfaces,
      jsSuperClass,
      jsNativeLoadSpec,
      fields,
      methods,
      jsConstructorDef,
      exportedMembers,
      jsNativeMembers,
      topLevelExportDefs = Nil
    )(optimizerHints)

    check(classDef, previousPhase, logger)
  }

  private class Env(
      /** Whether there is a valid `new.target` in scope. */
      val hasNewTarget: Boolean,
      /** Local variables in scope (including through closures). */
      val locals: Map[LocalName, LocalDef],
      /** Return types by label. */
      val returnLabels: Set[LabelName],
      /** Whether usages of `this` are restricted in this scope. */
      val isThisRestricted: Boolean
  ) {
    import Env._

    def withHasNewTarget(hasNewTarget: Boolean): Env =
      copy(hasNewTarget = hasNewTarget)

    def withThisType(thisType: Type): Env =
      withLocal(LocalDef(LocalName.This, thisType, mutable = false))

    def withMaybeThisType(hasThis: Boolean, thisType: Type): Env =
      if (hasThis) withThisType(thisType)
      else this

    def withLocal(localDef: LocalDef): Env =
      copy(locals = locals + (localDef.name -> localDef))

    def withLabel(label: LabelName): Env =
      copy(returnLabels = returnLabels + label)

    def withIsThisRestricted(isThisRestricted: Boolean): Env =
      copy(isThisRestricted = isThisRestricted)

    private def copy(
      hasNewTarget: Boolean = hasNewTarget,
      locals: Map[LocalName, LocalDef] = locals,
      returnLabels: Set[LabelName] = returnLabels,
      isThisRestricted: Boolean = isThisRestricted
    ): Env = {
      new Env(hasNewTarget, locals, returnLabels, isThisRestricted)
    }
  }

  private object Env {
    val empty: Env = {
      new Env(
        hasNewTarget = false,
        locals = Map.empty,
        returnLabels = Set.empty,
        isThisRestricted = false
      )
    }

    def fromParams(params: List[ParamDef]): Env = {
      val paramLocalDefs =
        for (p @ ParamDef(ident, _, tpe, mutable) <- params)
          yield ident.name -> LocalDef(ident.name, tpe, mutable)

      new Env(
        hasNewTarget = false,
        paramLocalDefs.toMap,
        Set.empty,
        isThisRestricted = false
      )
    }
  }

  private final case class LocalDef(name: LocalName, tpe: Type,
      mutable: Boolean)
}
