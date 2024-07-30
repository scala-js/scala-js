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

import scala.annotation.switch

import scala.collection.mutable

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.linker.checker.ErrorReporter._
import org.scalajs.linker.standard.LinkedClass
import org.scalajs.linker.standard.LinkTimeProperties

/** Checker for the validity of the IR. */
private final class ClassDefChecker(classDef: ClassDef,
    postBaseLinker: Boolean, postOptimizer: Boolean,
    reporter: ErrorReporter, linkTimeProperties: LinkTimeProperties) {
  import ClassDefChecker._

  import reporter.reportError

  private[this] val isJLObject = classDef.name.name == ObjectClass

  private[this] val instanceThisType: Type = {
    val cls = classDef.name.name
    if (classDef.kind.isJSType)
      AnyType
    else if (classDef.kind == ClassKind.HijackedClass)
      BoxedClassToPrimType.getOrElse(cls, ClassType(cls)) // getOrElse not to crash on invalid ClassDef
    else
      ClassType(cls)
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
    if (!postBaseLinker) {
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

    if (isJLObject && classDef.kind != ClassKind.Class) {
      reportError("java.lang.Object must be a Class")
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
        if (!alreadyDeclared.add(name))
            reportError(i"Duplicate JS class capture '$name'")
        if (tpe == NoType)
          reportError(i"The JS class capture $name cannot have type NoType")
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

    if (fieldDef.ftpe == NoType || fieldDef.ftpe == NothingType)
      reportError(i"FieldDef cannot have type ${fieldDef.ftpe}")
  }

  private def checkMethodDef(methodDef: MethodDef): Unit = withPerMethodState {
    val MethodDef(flags, MethodIdent(name), _, params, _, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    val namespace = flags.namespace
    val static = namespace.isStatic
    val isConstructor = namespace == MemberNamespace.Constructor

    if (flags.isMutable)
      reportError("A method cannot have the flag Mutable")

    checkMethodNameNamespace(name, namespace)

    if (!methods(namespace.ordinal).add(name))
      reportError(i"duplicate ${namespace.prefixString}method '$name'")

    if (body.isEmpty && namespace != MemberNamespace.Public)
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
      if (tpe == NoType)
        reportError(i"Parameter $name has type NoType")
    }

    // Body
    val thisType = if (static) NoType else instanceThisType
    val bodyEnv = Env.fromParams(params)
      .withThisType(thisType)
      .withInConstructor(isConstructor)
    body.foreach(checkTree(_, bodyEnv))
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
      .withInConstructor(true)

    val envJustBeforeSuper = body.beforeSuper.foldLeft(startEnv) { (prevEnv, stat) =>
      checkTree(stat, prevEnv)
    }
    checkTreeOrSpreads(body.superCall.args, envJustBeforeSuper)
    val envJustAfterSuper = envJustBeforeSuper.withThisType(instanceThisType)
    body.afterSuper.foldLeft(envJustAfterSuper) { (prevEnv, stat) =>
      checkTree(stat, prevEnv)
    }
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

    val thisType = if (static) NoType else instanceThisType
    val env =
      Env.fromParams(classDef.jsClassCaptures.getOrElse(Nil) ++ params ++ restParam).withThisType(thisType)

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
    val thisType = if (static) NoType else instanceThisType

    getterBody.foreach { body =>
      withPerMethodState {
        val bodyEnv = Env.fromParams(jsClassCaptures).withThisType(thisType)
        checkTree(body, bodyEnv)
      }
    }

    setterArgAndBody.foreach { case (setterArg, body) =>
      withPerMethodState {
        checkJSParamDefs(setterArg :: Nil, None)
        val bodyEnv = Env.fromParams(jsClassCaptures :+ setterArg).withThisType(thisType)
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
      if (postBaseLinker) {
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
  }

  private def checkJSParamDefs(params: List[ParamDef], restParam: Option[ParamDef])(
      implicit ctx: ErrorContext): Unit = {
    for (ParamDef(name, _, ptpe, _) <- params ++ restParam) {
      checkDeclareLocalVar(name)
      if (ptpe != AnyType)
        reportError(i"Parameter $name has type $ptpe but must be any")
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

  private def checkTree(tree: Tree, env: Env): Env = {
    implicit val ctx = ErrorContext(tree)

    def checkApplyGeneric(methodName: MethodName, args: List[Tree]): Unit = {
      val paramRefsCount = methodName.paramTypeRefs.size
      if (args.size != paramRefsCount)
        reportError(i"Arity mismatch: $paramRefsCount expected but ${args.size} found")
      checkTrees(args, env)
    }

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
        stats.foldLeft(env) { (prevEnv, stat) =>
          checkTree(stat, prevEnv)
        }

      case Labeled(label, _, body) =>
        checkDeclareLabel(label)
        checkTree(body, env.withLabel(label.name))

      case Assign(lhs, rhs) =>
        checkTree(lhs, env)
        checkTree(rhs, env)

        lhs match {
          case VarRef(LocalIdent(name)) =>
            if (env.locals.get(name).exists(!_.mutable))
              reportError(i"Assignment to immutable variable $name.")

          case _:Select | _:JSPrivateSelect | _:SelectStatic |
              _:ArraySelect | _:RecordSelect | _:JSSelect | _:JSSuperSelect |
              _:JSGlobalRef =>
        }

      case Return(expr, label) =>
        if (!env.returnLabels.contains(label.name))
          reportError(i"unknown label $label.")

        checkTree(expr, env)

      case If(cond, thenp, elsep) =>
        checkTree(cond, env)
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

      case Throw(expr) =>
        checkTree(expr, env)

      case Match(selector, cases, default) =>
        checkTree(selector, env)
        for ((alts, body) <- cases) {
          checkTrees(alts, env)
          checkTree(body, env)
        }

        checkTree(default, env)

      case Debugger() =>

      // Scala expressions

      case New(_, ctor, args) =>
        checkApplyGeneric(ctor.name, args)

      case _: LoadModule =>

      case StoreModule() =>
        if (!classDef.kind.hasModuleAccessor)
          reportError(i"Illegal StoreModule inside class of kind ${classDef.kind}")
        if (!env.inConstructor)
          reportError(i"Illegal StoreModule outside of constructor")
        if (env.thisType == NoType) // can happen before JSSuperConstructorCall in JSModuleClass
          reportError(i"Cannot find `this` in scope for StoreModule()")

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

      case UnaryOp(_, lhs) =>
        checkTree(lhs, env)

      case BinaryOp(_, lhs, rhs) =>
        checkTree(lhs, env)
        checkTree(rhs, env)

      case NewArray(typeRef, lengths) =>
        if (lengths.isEmpty)
          reportError("NewArray must have non-0 dimensions")
        if (lengths.size > typeRef.dimensions)
          reportError("NewArray dimensions may not exceed its type")

        checkArrayTypeRef(typeRef)
        checkTrees(lengths, env)

      case ArrayValue(typeRef, elems) =>
        checkArrayTypeRef(typeRef)
        checkTrees(elems, env)

      case ArrayLength(array) =>
        checkArrayReceiverType(array.tpe)
        checkTree(array, env)

      case ArraySelect(array, index) =>
        checkArrayReceiverType(array.tpe)
        checkTree(array, env)
        checkTree(index, env)

      case RecordSelect(record, _) =>
        checkAllowTransients()
        checkTree(record, env)

      case RecordValue(_, elems) =>
        checkAllowTransients()
        checkTrees(elems, env)

      case IsInstanceOf(expr, testType) =>
        checkTree(expr, env)
        checkIsAsInstanceTargetType(testType)

      case AsInstanceOf(expr, tpe) =>
        checkTree(expr, env)
        checkIsAsInstanceTargetType(tpe)

      case GetClass(expr) =>
        checkTree(expr, env)

      case Clone(expr) =>
        checkTree(expr, env)

      case IdentityHashCode(expr) =>
        checkTree(expr, env)

      case WrapAsThrowable(expr) =>
        checkTree(expr, env)

      case UnwrapFromThrowable(expr) =>
        checkTree(expr, env)

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
            reportError(i"Variable $name of type ${localDef.tpe} typed as ${tree.tpe}")
        }

      case This() =>
        if (env.thisType == NoType)
          reportError(i"Cannot find `this` in scope")
        else if (tree.tpe != env.thisType)
          reportError(i"`this` of type ${env.thisType} typed as ${tree.tpe}")

      case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
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
          for (ParamDef(name, _, ctpe, mutable) <- captureParams) {
            checkDeclareLocalVar(name)
            if (mutable)
              reportError(i"Capture parameter $name cannot be mutable")
            if (ctpe == NoType)
              reportError(i"Parameter $name has type NoType")
          }

          checkJSParamDefs(params, restParam)

          val bodyEnv = Env
            .fromParams(captureParams ++ params ++ restParam)
            .withHasNewTarget(!arrow)
            .withThisType(if (arrow) NoType else AnyType)
          checkTree(body, bodyEnv)
        }

      case CreateJSClass(className, captureValues) =>
        checkTrees(captureValues, env)

      case Transient(transient) =>
        checkAllowTransients()
        transient.traverse(new Traversers.Traverser {
          override def traverse(tree: Tree): Unit = checkTree(tree, env)
        })

      case LinkTimeIf(cond, thenp, elsep) =>
        if (cond.tpe != BooleanType)
          reportError(i"Link-time condition must be typed as boolean, but ${cond.tpe} is found.")
        checkLinkTimeTree(cond)
        checkTree(thenp, env)
        checkTree(elsep, env)
    }

    newEnv
  }

  private def checkAllowTransients()(implicit ctx: ErrorContext): Unit = {
    if (!postOptimizer)
      reportError("invalid transient tree")
  }

  private def checkIsAsInstanceTargetType(tpe: Type)(
      implicit ctx: ErrorContext): Unit = {
    tpe match {
      case NoType | NullType | NothingType | _:RecordType =>
        reportError(i"$tpe is not a valid target type for Is/AsInstanceOf")

      case tpe: ArrayType =>
        checkArrayType(tpe)

      case _ =>
        // ok
    }
  }

  private def checkArrayReceiverType(tpe: Type)(
      implicit ctx: ErrorContext): Unit = tpe match {
    case tpe: ArrayType         => checkArrayType(tpe)
    case NullType | NothingType => // ok
    case _                      => reportError(i"Array type expected but $tpe found")
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

  private def checkLinkTimeTree(tree: LinkTimeTree): Unit = {
    implicit val ctx = ErrorContext(tree)
    import LinkTimeOp._
    tree match {
      case LinkTimeTree.BinaryOp(op, lhs, rhs) =>
        if (lhs.tpe != rhs.tpe)
          reportError(i"Type mismatch for binary operation: ${lhs.tpe} and ${rhs.tpe}.")
        op match {
          case Boolean_!= | Boolean_== | Boolean_&& | Boolean_|| =>
            if (lhs.tpe != BooleanType)
              reportError(i"Invalid operand type for Boolean operation: ${lhs.tpe}.")
          case Int_!= | Int_== | Int_< | Int_<= | Int_> | Int_>= =>
            if (lhs.tpe != IntType)
              reportError(i"Invalid operand type for Integer operation: ${lhs.tpe}.")
        }
        checkLinkTimeTree(lhs)
        checkLinkTimeTree(rhs)
      case prop: LinkTimeTree.Property =>
        if (!linkTimeProperties.exist(prop.name, prop.tpe)) {
          reportError(i"link-time property '${prop.name}' of ${prop.tpe} not found.")
        }
      case _ =>
    }
  }
}

object ClassDefChecker {
  /** Checks that the IR in a ClassDef is correct.
   *
   *  @return Count of IR checking errors (0 in case of success)
   */
  def check(classDef: ClassDef, postBaseLinker: Boolean, postOptimizer: Boolean,
      logger: Logger, linkTimeProperties: LinkTimeProperties): Int = {
    val reporter = new LoggerErrorReporter(logger)
    new ClassDefChecker(classDef, postBaseLinker, postOptimizer, reporter,
        linkTimeProperties).checkClassDef()
    reporter.errorCount
  }

  def check(linkedClass: LinkedClass, postOptimizer: Boolean,
      logger: Logger, linkTimeProperties: LinkTimeProperties): Int = {
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

    check(classDef, postBaseLinker = true, postOptimizer, logger, linkTimeProperties)
  }

  private class Env(
      /** Whether there is a valid `new.target` in scope. */
      val hasNewTarget: Boolean,
      /** The type of `this` in scope, or `NoType` if there is no `this` in scope. */
      val thisType: Type,
      /** Local variables in scope (including through closures). */
      val locals: Map[LocalName, LocalDef],
      /** Return types by label. */
      val returnLabels: Set[LabelName],
      /** Whether we are in a constructor of the class. */
      val inConstructor: Boolean
  ) {
    import Env._

    def withHasNewTarget(hasNewTarget: Boolean): Env =
      copy(hasNewTarget = hasNewTarget)

    def withThisType(thisType: Type): Env =
      copy(thisType = thisType)

    def withLocal(localDef: LocalDef): Env =
      copy(locals = locals + (localDef.name -> localDef))

    def withLabel(label: LabelName): Env =
      copy(returnLabels = returnLabels + label)

    def withInConstructor(inConstructor: Boolean): Env =
      copy(inConstructor = inConstructor)

    private def copy(
      hasNewTarget: Boolean = hasNewTarget,
      thisType: Type = thisType,
      locals: Map[LocalName, LocalDef] = locals,
      returnLabels: Set[LabelName] = returnLabels,
      inConstructor: Boolean = inConstructor
    ): Env = {
      new Env(hasNewTarget, thisType, locals, returnLabels, inConstructor)
    }
  }

  private object Env {
    val empty: Env =
      new Env(hasNewTarget = false, thisType = NoType, Map.empty, Set.empty, inConstructor = false)

    def fromParams(params: List[ParamDef]): Env = {
      val paramLocalDefs =
        for (p @ ParamDef(ident, _, tpe, mutable) <- params)
          yield ident.name -> LocalDef(ident.name, tpe, mutable)

      new Env(
        hasNewTarget = false,
        thisType = NoType,
        paramLocalDefs.toMap,
        Set.empty,
        inConstructor = false
      )
    }
  }

  private final case class LocalDef(name: LocalName, tpe: Type,
      mutable: Boolean)
}
