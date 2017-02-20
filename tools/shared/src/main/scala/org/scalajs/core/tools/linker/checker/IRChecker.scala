/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.checker

import scala.language.implicitConversions

import scala.annotation.switch

import scala.collection.mutable

import org.scalajs.core.ir._
import Definitions._
import Trees._
import Types._

import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.linker.{LinkingUnit, LinkedClass}

/** Checker for the validity of the IR. */
private final class IRChecker(unit: LinkingUnit, logger: Logger) {
  import IRChecker._

  private var errorCount: Int = 0

  /* Per-method state (setup with withPerMethodState).
   * This state is reset per-Closure as well.
   */
  private var declaredLabelNamesPerMethod: mutable.Set[String] = _

  private def withPerMethodState[A](body: => A): A = {
    val savedDeclaredLabelNamesPerMethod = declaredLabelNamesPerMethod
    try {
      declaredLabelNamesPerMethod = mutable.Set.empty
      body
    } finally {
      declaredLabelNamesPerMethod = savedDeclaredLabelNamesPerMethod
    }
  }

  private val classes: mutable.Map[String, CheckedClass] = {
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
      checkJSNativeLoadSpec(classDef)
      checkStaticMembers(classDef)
      classDef.kind match {
        case ClassKind.AbstractJSType | ClassKind.NativeJSClass |
            ClassKind.NativeJSModuleClass =>
          if (classDef.fields.nonEmpty ||
              classDef.memberMethods.nonEmpty ||
              classDef.abstractMethods.nonEmpty ||
              classDef.exportedMembers.nonEmpty ||
              classDef.classExports.nonEmpty) {
            reportError(s"Raw JS type ${classDef.name} cannot "+
                "have instance members")
          }
        case _ =>
          checkScalaClassDef(classDef)
      }
    }
    errorCount
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
    for (member <- classDef.staticMethods) {
      val methodDef = member.tree
      implicit val ctx = ErrorContext(methodDef)

      assert(methodDef.static, "Found non-static member in static defs")

      if (!methodDef.name.isInstanceOf[Ident])
        reportError(s"Static method ${methodDef.name} cannot be exported")
      else
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
      for (field <- classDef.fields) {
        implicit val ctx = ErrorContext(field)
        checkFieldDef(field, classDef)
      }

      /* Check for static field collisions.
       * TODO #2627 We currently cannot check instance field collisions because
       * of #2382.
       */
      val staticFieldDefs = classDef.fields.filter(_.static)
      for {
        fieldsWithSameName <- staticFieldDefs.groupBy(_.name.encodedName).values
        duplicate <- fieldsWithSameName.tail
      } {
        implicit val ctx = ErrorContext(duplicate)
        reportError(s"Duplicate static field with name '${duplicate.name}'")
      }

      // Check exported members
      for (member <- classDef.exportedMembers) {
        implicit val ctx = ErrorContext(member.tree)

        member.tree match {
          case m: MethodDef =>
            checkExportedMethodDef(m, classDef, isTopLevel = false)

          case p: PropertyDef =>
            checkExportedPropertyDef(p, classDef)

          // Anything else is illegal
          case _ =>
            reportError("Illegal exported class member of type " +
                member.tree.getClass.getName)
        }
      }

      // Check classExports
      for (tree <- classDef.classExports) {
        implicit val ctx = ErrorContext(tree)

        tree match {
          case tree: ConstructorExportDef =>
            checkConstructorExportDef(tree, classDef)

          case tree: JSClassExportDef =>
            checkJSClassExportDef(tree, classDef)

          case tree: ModuleExportDef =>
            checkModuleExportDef(tree, classDef)

          case tree: TopLevelModuleExportDef =>
            checkTopLevelModuleExportDef(tree, classDef)

          case TopLevelMethodExportDef(methodDef) =>
            checkExportedMethodDef(methodDef, classDef, isTopLevel = true)

          case TopLevelFieldExportDef(fullName, field) =>
            lookupClass(classDef.name.name).lookupStaticField(field.name).fold {
              reportError(s"Cannot export non-existent static field '$field'")
            } { checkedField =>
              val tpe = checkedField.tpe
              if (tpe != AnyType)
                reportError(s"Cannot export field '$field' of type $tpe")
            }

          // Anything else is illegal
          case _ =>
            reportError("Illegal class export of type " +
                tree.getClass.getName)
        }
      }
    } else {
      implicit val ctx = ErrorContext(classDef)

      def kindStr =
        if (classDef.kind == ClassKind.HijackedClass) "Hijacked classes"
        else "Interfaces"

      if (classDef.fields.nonEmpty)
        reportError(s"$kindStr may not have fields")

      if (classDef.exportedMembers.nonEmpty || classDef.classExports.nonEmpty)
        reportError(s"$kindStr may not have exports")
    }

    // Check methods
    for (method <- classDef.memberMethods ++ classDef.abstractMethods) {
      val tree = method.tree
      implicit val ctx = ErrorContext(tree)

      assert(!tree.static, "Member or abstract method may not be static")
      assert(tree.name.isInstanceOf[Ident],
          "Normal method must have Ident as name")

      checkMethodDef(tree, classDef)
    }
  }

  private def checkFieldDef(fieldDef: FieldDef, classDef: LinkedClass): Unit = {
    val FieldDef(static, name, tpe, mutable) = fieldDef
    implicit val ctx = ErrorContext(fieldDef)

    name match {
      case _: Ident =>
        // ok
      case _: StringLiteral =>
        if (!classDef.kind.isJSClass)
          reportError(s"FieldDef '$name' cannot have a string literal name")
      case ComputedName(tree, _) =>
        if (!classDef.kind.isJSClass)
          reportError(s"FieldDef '$name' cannot have a computed name")
        typecheckExpect(tree, Env.empty, AnyType)
    }

    if (tpe == NoType)
      reportError(s"FieldDef cannot have type NoType")
  }

  private def checkMethodDef(methodDef: MethodDef,
      classDef: LinkedClass): Unit = withPerMethodState {

    val MethodDef(static, Ident(name, _), params, resultType, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    if (classDef.kind.isJSClass && !static) {
      reportError(s"Non exported instance method $name is illegal in JS class")
      return // things would go too badly otherwise
    }

    for (ParamDef(name, tpe, _, rest) <- params) {
      if (tpe == NoType)
        reportError(s"Parameter $name has type NoType")
      if (rest)
        reportError(s"Rest parameter $name is illegal in a Scala method")
    }

    val isConstructor = isConstructorName(name)

    if (isConstructor && classDef.kind == ClassKind.Interface)
      reportError("Interfaces cannot declare constructors")

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
    val bodyEnv = Env.fromSignature(thisType, params, resultType, isConstructor)

    body.fold {
      // Abstract
      if (static)
        reportError(s"Static method ${classDef.name.name}.$name cannot be abstract")
      else if (isConstructor)
        reportError(s"Constructor ${classDef.name.name}.$name cannot be abstract")
    } { body =>
      // Concrete
      if (resultType == NoType)
        typecheckStat(body, bodyEnv)
      else
        typecheckExpect(body, bodyEnv, resultType)
    }
  }

  private def checkExportedMethodDef(methodDef: MethodDef,
      classDef: LinkedClass, isTopLevel: Boolean): Unit = withPerMethodState {
    val MethodDef(static, pName, params, resultType, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    if (!isTopLevel && !classDef.kind.isAnyScalaJSDefinedClass) {
      reportError(s"Exported method def can only appear in a class")
      return
    }

    if (!isTopLevel && static && classDef.kind != ClassKind.JSClass)
      reportError("Exported method def in non-JS class cannot be static")

    if (isTopLevel && !static)
      reportError("Top level export must be static")

    checkExportedPropertyName(pName, classDef, isTopLevel)

    for (ParamDef(name, tpe, _, _) <- params) {
      if (tpe == NoType)
        reportError(s"Parameter $name has type NoType")
      else if (tpe != AnyType)
        reportError(s"Parameter $name of exported method def has type $tpe, "+
            "but must be Any")
    }

    if (params.nonEmpty) {
      for (ParamDef(name, _, _, rest) <- params.init) {
        if (rest)
          reportError(s"Non-last rest parameter $name is illegal")
      }
    }

    def isJSConstructor = {
      !static && (pName match {
        case StringLiteral("constructor") => true
        case _                            => false
      })
    }

    if (classDef.kind.isJSClass && isJSConstructor) {
      checkJSClassConstructor(methodDef, classDef)
    } else {
      if (resultType != AnyType) {
        reportError(s"Result type of exported method def is $resultType, "+
            "but must be Any")
      }

      val thisType = {
        if (static) NoType
        else if (classDef.kind.isJSClass) AnyType
        else ClassType(classDef.name.name)
      }

      body.fold {
        reportError("Exported method cannot be abstract")
      } { body =>
        val bodyEnv = Env.fromSignature(thisType, params, resultType)
        typecheckExpect(body, bodyEnv, resultType)
      }
    }
  }

  private def checkJSClassConstructor(methodDef: MethodDef,
      classDef: LinkedClass): Unit = {
    val MethodDef(static, _, params, resultType, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    if (resultType != NoType) {
      reportError(s"Result type of JS class constructor is $resultType, "+
          "but must be NoType")
    }

    body.fold {
      reportError("JS class constructor cannot be abstract.")
    } { body =>
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

      val initialEnv = Env.fromSignature(NoType, params, NoType,
          isConstructor = true)

      val preparedEnv = (initialEnv /: prepStats) { (prevEnv, stat) =>
        typecheckStat(stat, prevEnv)
      }

      for (arg <- superCall.args)
        typecheckExprOrSpread(arg, preparedEnv)

      val restEnv = preparedEnv.withThis(AnyType)
      typecheckStat(Block(restStats)(methodDef.pos), restEnv)
    }
  }

  private def checkExportedPropertyDef(propDef: PropertyDef,
      classDef: LinkedClass): Unit = withPerMethodState {
    val PropertyDef(static, pName, getterBody, setterArgAndBody) = propDef
    implicit val ctx = ErrorContext(propDef)

    if (!classDef.kind.isAnyScalaJSDefinedClass) {
      reportError(s"Exported property def can only appear in a class")
      return
    }

    checkExportedPropertyName(pName, classDef, isTopLevel = false)

    val thisType =
      if (static) NoType
      else if (classDef.kind.isJSClass) AnyType
      else ClassType(classDef.name.name)

    getterBody.foreach { getterBody =>
      val getterBodyEnv = Env.fromSignature(thisType, Nil, AnyType)
      typecheckExpect(getterBody, getterBodyEnv, AnyType)
    }

    setterArgAndBody.foreach { case (setterArg, setterBody) =>
      if (setterArg.ptpe != AnyType)
        reportError("Setter argument of exported property def has type "+
            s"${setterArg.ptpe}, but must be Any")
      if (setterArg.rest)
        reportError(s"Rest parameter ${setterArg.name} is illegal in setter")

      val setterBodyEnv = Env.fromSignature(thisType, List(setterArg), NoType)
      typecheckStat(setterBody, setterBodyEnv)
    }
  }

  private def checkExportedPropertyName(pName: PropertyName,
      classDef: LinkedClass, isTopLevel: Boolean)(
      implicit ctx: ErrorContext): Unit = {
    pName match {
      case _: Ident =>
        throw new AssertionError("Exported method may not have Ident as name")

      case StringLiteral(name) =>
        if (name.contains("__") && name != Definitions.ClassExportsName)
          reportError("Exported method def name cannot contain __")

      case ComputedName(tree, _) =>
        if (isTopLevel || !classDef.kind.isJSClass)
          reportError("Only JS classes may contain members with computed names")
        typecheckExpect(tree, Env.empty, AnyType)
    }
  }

  private def checkConstructorExportDef(ctorDef: ConstructorExportDef,
      classDef: LinkedClass): Unit = withPerMethodState {
    val ConstructorExportDef(_, params, body) = ctorDef
    implicit val ctx = ErrorContext(ctorDef)

    if (!classDef.kind.isClass) {
      reportError(s"Exported constructor def can only appear in a class")
      return
    }

    for (ParamDef(name, tpe, _, _) <- params) {
      if (tpe == NoType)
        reportError(s"Parameter $name has type NoType")
      else if (tpe != AnyType)
        reportError(s"Parameter $name of exported constructor def has type "+
            s"$tpe, but must be Any")
    }

    if (params.nonEmpty) {
      for (ParamDef(name, _, _, rest) <- params.init) {
        if (rest)
          reportError(s"Non-last rest parameter $name is illegal")
      }
    }

    val thisType = ClassType(classDef.name.name)
    val bodyEnv = Env.fromSignature(thisType, params, NoType)
    typecheckStat(body, bodyEnv)
  }

  private def checkJSClassExportDef(classExportDef: JSClassExportDef,
      classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(classExportDef)

    if (classDef.kind != ClassKind.JSClass)
      reportError(s"Exported JS class def can only appear in a JS class")
  }

  private def checkModuleExportDef(moduleDef: ModuleExportDef,
      classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(moduleDef)

    if (!classDef.kind.hasModuleAccessor)
      reportError(s"Exported module def can only appear in a module class")
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
        typecheckExpect(rhs, env, vtpe)
        env.withLocal(LocalDef(ident.name, vtpe, mutable)(tree.pos))

      case Skip() =>
        env

      case Assign(select, rhs) =>
        select match {
          case Select(This(), Ident(_, _)) if env.inConstructor =>
            // ok
          case Select(receiver, Ident(name, _)) =>
            receiver.tpe match {
              case ClassType(clazz) =>
                for {
                  c <- tryLookupClass(clazz)
                  f <- c.lookupField(name)
                  if !f.mutable
                } reportError(s"Assignment to immutable field $name.")
              case _ =>
            }
          case SelectStatic(ClassType(cls), Ident(name, _)) =>
            for {
              c <- tryLookupClass(cls)
              f <- c.lookupStaticField(name)
              if !f.mutable
            } {
              reportError(s"Assignment to immutable static field $name.")
            }
          case VarRef(Ident(name, _)) if !env.locals(name).mutable =>
            reportError(s"Assignment to immutable variable $name.")
          case _ =>
        }
        val lhsTpe = typecheckExpr(select, env)
        val expectedRhsTpe = select match {
          case _:JSDotSelect | _:JSBracketSelect | _:JSSuperBracketSelect =>
            AnyType
          case _ =>
            lhsTpe
        }
        typecheckExpect(rhs, env, expectedRhsTpe)
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
        (env /: stats) { (prevEnv, stat) =>
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

      case While(cond, body, label) =>
        label.foreach(checkDeclareLabel)
        typecheckExpect(cond, env, BooleanType)
        typecheckStat(body, env)
        env

      case DoWhile(body, cond, label) =>
        label.foreach(checkDeclareLabel)
        typecheckStat(body, env)
        typecheckExpect(cond, env, BooleanType)
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
        typecheckExpr(selector, env)
        for ((alts, body) <- cases) {
          alts.foreach(typecheckExpr(_, env))
          typecheckStat(body, env)
        }
        typecheckStat(default, env)
        env

      case Debugger() =>
        env

      case JSDelete(JSDotSelect(obj, prop)) =>
        typecheckExpr(obj, env)
        env

      case JSDelete(JSBracketSelect(obj, prop)) =>
        typecheckExpr(obj, env)
        typecheckExpr(prop, env)
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

  private def typecheckExprOrSpread(tree: Tree, env: Env): Type = {
    tree match {
      case JSSpread(items) =>
        typecheckExpr(items, env)
        AnyType
      case _ =>
        typecheckExpr(tree, env)
    }
  }

  private def typecheck(tree: Tree, env: Env): Type = {
    implicit val ctx = ErrorContext(tree)

    def checkApplyGeneric(methodName: String, methodFullName: String,
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
        val envAfterStats = (env /: stats) { (prevEnv, stat) =>
          typecheckStat(stat, prevEnv)
        }
        typecheckExpr(expr, envAfterStats)

      case Labeled(label, tpe, body) =>
        checkDeclareLabel(label)
        typecheckExpect(body, env.withLabeledReturnType(label.name, tpe), tpe)

      case Return(expr, label) =>
        env.returnTypes.get(label.map(_.name)).fold[Unit] {
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

      case While(BooleanLiteral(true), body, label) if tree.tpe == NothingType =>
        label.foreach(checkDeclareLabel)
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

      case Continue(label) =>
        /* Here we could check that it is indeed legal to break to the
         * specified label. However, if we do anything illegal here, it will
         * result in a SyntaxError in JavaScript anyway, so we do not really
         * care.
         */

      case Match(selector, cases, default) =>
        val tpe = tree.tpe
        typecheckExpr(selector, env)
        for ((alts, body) <- cases) {
          alts.foreach(typecheckExpr(_, env))
          typecheckExpect(body, env, tpe)
        }
        typecheckExpect(default, env, tpe)

      // Scala expressions

      case New(cls, ctor, args) =>
        val clazz = lookupClass(cls)
        if (!clazz.kind.isClass)
          reportError(s"new $cls which is not a class")
        checkApplyGeneric(ctor.name, s"$cls.$ctor", args, NoType,
            isStatic = false)

      case LoadModule(cls) =>
        if (!cls.className.endsWith("$"))
          reportError("LoadModule of non-module class $cls")

      case Select(qualifier, Ident(item, _)) =>
        val qualType = typecheckExpr(qualifier, env)
        qualType match {
          case ClassType(cls) =>
            val maybeClass = tryLookupClass(cls)
            val kind = maybeClass.fold(_.kind, _.kind)
            if (!kind.isClass) {
              reportError(s"Cannot select $item of non-class $cls")
            } else {
              maybeClass.foreach {
                _.lookupField(item).fold[Unit] {
                  reportError(s"Class $cls does not have a field $item")
                } { fieldDef =>
                  if (fieldDef.tpe != tree.tpe)
                    reportError(s"Select $cls.$item of type "+
                      s"${fieldDef.tpe} typed as ${tree.tpe}")
                }
              }
            }
          case NullType | NothingType =>
            // always ok
          case _ =>
            reportError(s"Cannot select $item of non-class type $qualType")
        }

      case SelectStatic(ClassType(cls), Ident(item, _)) =>
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

      case Apply(receiver, Ident(method, _), args) =>
        val receiverType = typecheckExpr(receiver, env)
        checkApplyGeneric(method, s"$receiverType.$method", args, tree.tpe,
            isStatic = false)

      case ApplyStatically(receiver, cls, Ident(method, _), args) =>
        typecheckExpect(receiver, env, cls)
        checkApplyGeneric(method, s"$cls.$method", args, tree.tpe,
            isStatic = false)

      case ApplyStatic(cls, Ident(method, _), args) =>
        val clazz = lookupClass(cls)
        checkApplyGeneric(method, s"$cls.$method", args, tree.tpe,
            isStatic = true)

      case UnaryOp(op, lhs) =>
        import UnaryOp._
        (op: @switch) match {
          case IntToLong =>
            typecheckExpect(lhs, env, IntType)
          case LongToInt | LongToDouble =>
            typecheckExpect(lhs, env, LongType)
          case DoubleToInt | DoubleToFloat | DoubleToLong =>
            typecheckExpect(lhs, env, DoubleType)
          case Boolean_! =>
            typecheckExpect(lhs, env, BooleanType)
        }

      case BinaryOp(op, lhs, rhs) =>
        import BinaryOp._
        (op: @switch) match {
          case === | !== | String_+ =>
            typecheckExpr(lhs, env)
            typecheckExpr(rhs, env)
          case Int_+ | Int_- | Int_* | Int_/ | Int_% |
              Int_| | Int_& | Int_^ | Int_<< | Int_>>> | Int_>> =>
            typecheckExpect(lhs, env, IntType)
            typecheckExpect(rhs, env, IntType)
          case Float_+ | Float_- | Float_* | Float_/ | Float_% =>
            typecheckExpect(lhs, env, FloatType)
            typecheckExpect(rhs, env, FloatType)
          case Long_+ | Long_- | Long_* | Long_/ | Long_% |
              Long_| | Long_& | Long_^ |
              Long_== | Long_!= | Long_< | Long_<= | Long_> | Long_>= =>
            typecheckExpect(lhs, env, LongType)
            typecheckExpect(rhs, env, LongType)
          case Long_<< | Long_>>> | Long_>> =>
            typecheckExpect(lhs, env, LongType)
            typecheckExpect(rhs, env, IntType)
          case Double_+ | Double_- | Double_* | Double_/ | Double_% |
              Num_== | Num_!= | Num_< | Num_<= | Num_> | Num_>= =>
            typecheckExpect(lhs, env, DoubleType)
            typecheckExpect(rhs, env, DoubleType)
          case Boolean_== | Boolean_!= | Boolean_| | Boolean_& =>
            typecheckExpect(lhs, env, BooleanType)
            typecheckExpect(rhs, env, BooleanType)
        }

      case NewArray(tpe, lengths) =>
        for (length <- lengths)
          typecheckExpect(length, env, IntType)

      case ArrayValue(tpe, elems) =>
        val elemType = arrayElemType(tpe)
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

      case IsInstanceOf(expr, cls) =>
        typecheckExpr(expr, env)

      case AsInstanceOf(expr, cls) =>
        typecheckExpr(expr, env)

      case Unbox(expr, _) =>
        typecheckExpr(expr, env)

      case GetClass(expr) =>
        typecheckExpr(expr, env)

      // JavaScript expressions

      case JSNew(ctor, args) =>
        typecheckExpr(ctor, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSDotSelect(qualifier, item) =>
        typecheckExpr(qualifier, env)

      case JSBracketSelect(qualifier, item) =>
        typecheckExpr(qualifier, env)
        typecheckExpr(item, env)

      case JSFunctionApply(fun, args) =>
        typecheckExpr(fun, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSDotMethodApply(receiver, method, args) =>
        typecheckExpr(receiver, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSBracketMethodApply(receiver, method, args) =>
        typecheckExpr(receiver, env)
        typecheckExpr(method, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSSuperBracketSelect(cls, qualifier, item) =>
        if (!lookupClass(cls).kind.isJSClass)
          reportError(s"JS class type expected but $cls found")
        typecheckExpr(qualifier, env)
        typecheckExpr(item, env)

      case JSSuperBracketCall(cls, receiver, method, args) =>
        if (!lookupClass(cls).kind.isJSClass)
          reportError(s"JS class type expected but $cls found")
        typecheckExpr(receiver, env)
        typecheckExpr(method, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

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
        for ((_, value) <- fields)
          typecheckExpr(value, env)

      case JSLinkingInfo() =>

      // Literals

      case _: Literal =>

      // Atomic expressions

      case VarRef(Ident(name, _)) =>
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

      case Closure(captureParams, params, body, captureValues) =>
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
            if (mutable)
              reportError(s"Capture parameter $name cannot be mutable")
            if (rest)
              reportError(s"Capture parameter $name cannot be a rest parameter")
            if (ctpe == NoType)
              reportError(s"Parameter $name has type NoType")
          }

          for (ParamDef(name, ptpe, mutable, rest) <- params) {
            if (ptpe == NoType)
              reportError(s"Parameter $name has type NoType")
            else if (ptpe != AnyType)
              reportError(s"Closure parameter $name has type $ptpe instead of any")
            if (rest)
              reportError(s"Closure parameter $name cannot be a rest parameter")
          }

          val bodyEnv = Env.fromSignature(
              AnyType, captureParams ++ params, AnyType)
          typecheckExpect(body, bodyEnv, AnyType)
        }

      case _ =>
        reportError(s"Invalid expression tree")
    }

    tree.tpe
  }

  private def checkDeclareLabel(label: Ident)(
      implicit ctx: ErrorContext): Unit = {
    if (!declaredLabelNamesPerMethod.add(label.name))
      reportError(s"Duplicate label named ${label.name}.")
  }

  private def inferMethodType(encodedName: String, isStatic: Boolean)(
      implicit ctx: ErrorContext): (List[Type], Type) = {

    val (_, paramRefTypes, resultRefType) = decodeMethodName(encodedName)
    val paramTypes = paramRefTypes.map(refTypeToType)

    val resultType = resultRefType.fold[Type] {
      if (isConstructorName(encodedName)) NoType
      else if (encodedName == StaticInitializerName) NoType
      else AnyType // reflective proxy
    } { refType =>
      refTypeToType(refType)
    }

    (paramTypes, resultType)
  }

  private def refTypeToType(refType: ReferenceType)(
      implicit ctx: ErrorContext): Type = {
    refType match {
      case arrayType: ArrayType   => arrayType
      case ClassType(encodedName) => classNameToType(encodedName)
    }
  }

  private def classNameToType(encodedName: String)(
      implicit ctx: ErrorContext): Type = {
    if (encodedName.length == 1) {
      (encodedName.charAt(0): @switch) match {
        case 'V'                   => NoType
        case 'Z'                   => BooleanType
        case 'C' | 'B' | 'S' | 'I' => IntType
        case 'J'                   => LongType
        case 'F'                   => FloatType
        case 'D'                   => DoubleType
        case 'O'                   => AnyType
        case 'T'                   => ClassType(StringClass) // NOT StringType
      }
    } else if (encodedName == "sr_Nothing$") {
      NothingType
    } else if (encodedName == "sr_Null$") {
      NullType
    } else {
      val kind = tryLookupClass(encodedName).fold(_.kind, _.kind)
      if (kind.isJSType) AnyType
      else ClassType(encodedName)
    }
  }

  private def arrayElemType(arrayType: ArrayType)(
      implicit ctx: ErrorContext): Type = {
    if (arrayType.dimensions == 1) classNameToType(arrayType.baseClassName)
    else ArrayType(arrayType.baseClassName, arrayType.dimensions-1)
  }

  private def reportError(msg: String)(implicit ctx: ErrorContext): Unit = {
    logger.error(s"$ctx: $msg")
    errorCount += 1
  }

  private def lookupInfo(className: String)(
      implicit ctx: ErrorContext): Infos.ClassInfo = {
    unit.infos.getOrElse(className, {
      reportError(s"Cannot find info for class $className")
      Infos.ClassInfo(className)
    })
  }

  private def tryLookupClass(className: String)(
       implicit ctx: ErrorContext): Either[Infos.ClassInfo, CheckedClass] = {
    classes.get(className).fold[Either[Infos.ClassInfo, CheckedClass]](
        Left(lookupInfo(className)))(Right(_))
  }

  private def lookupClass(className: String)(
      implicit ctx: ErrorContext): CheckedClass = {
    classes.getOrElseUpdate(className, {
      reportError(s"Cannot find class $className")
      new CheckedClass(className, ClassKind.Class,
          Some(ObjectClass), Set(ObjectClass), None, Nil)
    })
  }

  private def lookupClass(classType: ClassType)(
      implicit ctx: ErrorContext): CheckedClass = {
    lookupClass(classType.className)
  }

  private def isSubclass(lhs: String, rhs: String)(
      implicit ctx: ErrorContext): Boolean = {
    tryLookupClass(lhs).fold({ info =>
      val parents = info.superClass ++: info.interfaces
      parents.exists(_ == rhs) || parents.exists(isSubclass(_, rhs))
    }, { lhsClass =>
      lhsClass.ancestors.contains(rhs)
    })
  }

  private def isSubtype(lhs: Type, rhs: Type)(
      implicit ctx: ErrorContext): Boolean = {
    Types.isSubtype(lhs, rhs)(isSubclass)
  }

  private class Env(
      /** Type of `this`. Can be NoType. */
      val thisTpe: Type,
      /** Local variables in scope (including through closures). */
      val locals: Map[String, LocalDef],
      /** Return types by label. */
      val returnTypes: Map[Option[String], Type],
      /** Whether we're in a constructor of the class */
      val inConstructor: Boolean
  ) {
    import Env._

    def withThis(thisTpe: Type): Env =
      new Env(thisTpe, this.locals, this.returnTypes, this.inConstructor)

    def withLocal(localDef: LocalDef)(implicit ctx: ErrorContext): Env = {
      if (locals.contains(localDef.name))
        reportDuplicateLocalVarName(localDef.name)
      new Env(thisTpe, locals + (localDef.name -> localDef), returnTypes,
          this.inConstructor)
    }

    def withReturnType(returnType: Type): Env =
      new Env(this.thisTpe, this.locals,
          returnTypes + (None -> returnType), this.inConstructor)

    def withLabeledReturnType(label: String, returnType: Type): Env =
      new Env(this.thisTpe, this.locals,
          returnTypes + (Some(label) -> returnType), this.inConstructor)

    def withInConstructor(inConstructor: Boolean): Env =
      new Env(this.thisTpe, this.locals, this.returnTypes, inConstructor)
  }

  private object Env {
    val empty: Env = new Env(NoType, Map.empty, Map.empty, false)

    def fromSignature(thisType: Type, params: List[ParamDef],
        resultType: Type, isConstructor: Boolean = false): Env = {
      val paramLocalDefs = params.foldLeft(Map.empty[String, LocalDef]) {
        case (prevParams, p @ ParamDef(ident, tpe, mutable, _)) =>
          implicit val ctx = ErrorContext(p)
          val name = ident.name
          if (prevParams.contains(name))
            reportDuplicateLocalVarName(name)
          prevParams + (name -> LocalDef(name, tpe, mutable)(p.pos))
      }
      new Env(thisType, paramLocalDefs,
          Map(None -> (if (resultType == NoType) AnyType else resultType)),
          isConstructor)
    }

    private def reportDuplicateLocalVarName(name: String)(
        implicit ctx: ErrorContext): Unit = {
      reportError(s"Duplicate local variable name $name.")
    }
  }

  private class CheckedClass(
      val name: String,
      val kind: ClassKind,
      val superClassName: Option[String],
      val ancestors: Set[String],
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      _fields: TraversableOnce[CheckedField])(
      implicit ctx: ErrorContext) {

    val fields = _fields.filter(!_.static).map(f => f.name -> f).toMap
    val staticFields = _fields.filter(_.static).map(f => f.name -> f).toMap

    lazy val superClass = superClassName.map(classes)

    def this(classDef: LinkedClass)(implicit ctx: ErrorContext) = {
      this(classDef.name.name, classDef.kind,
          classDef.superClass.map(_.name),
          classDef.ancestors.toSet,
          classDef.jsNativeLoadSpec,
          if (classDef.kind.isJSClass) Nil
          else classDef.fields.map(CheckedClass.checkedField))
    }

    def isAncestorOfHijackedClass: Boolean =
      AncestorsOfHijackedClasses.contains(name)

    def lookupField(name: String): Option[CheckedField] =
      fields.get(name).orElse(superClass.flatMap(_.lookupField(name)))

    def lookupStaticField(name: String): Option[CheckedField] =
      staticFields.get(name)
  }

  private object CheckedClass {
    private def checkedField(fieldDef: FieldDef) = {
      val FieldDef(static, Ident(name, _), tpe, mutable) = fieldDef
      new CheckedField(static, name, tpe, mutable)
    }
  }

  private class CheckedField(val static: Boolean, val name: String,
      val tpe: Type, val mutable: Boolean)
}

object IRChecker {
  /** Checks that the IR in a [[LinkingUnit]] is correct.
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
   *  `Either[Tree, LinkedClass]`, but that would also require an allocation of
   *  the `Left` or `Right` (in fact, we'd love to type it as
   *  `Tree | LinkedClass`). `ErrorContext` is also made an `AnyVal` for the
   *  same reasons, again.
   *
   *  If `toString()` is called, we're in a bad situation anyway, because the
   *  IR is invalid, so all bets are off and we can be slow and allocate stuff;
   *  we don't care.
   */
  private final class ErrorContext private (val treeOrLinkedClass: Any)
      extends AnyVal {

    override def toString(): String = {
      val (pos, name) = treeOrLinkedClass match {
        case tree: Tree               => (tree.pos, tree.getClass.getSimpleName)
        case linkedClass: LinkedClass => (linkedClass.pos, "ClassDef")
      }
      s"${pos.source}(${pos.line+1}:${pos.column+1}:$name)"
    }
  }

  private object ErrorContext {
    implicit def tree2errorContext(tree: Tree): ErrorContext =
      ErrorContext(tree)

    def apply(tree: Tree): ErrorContext =
      new ErrorContext(tree)

    def apply(linkedClass: LinkedClass): ErrorContext =
      new ErrorContext(linkedClass)
  }

  private case class LocalDef(name: String, tpe: Type, mutable: Boolean)(
      val pos: Position)

  /** Enable the right-biased API of Either from 2.12 in earlier versions. */
  private implicit class RightBiasedEither[A, B](val self: Either[A, B])
      extends AnyVal {

    def foreach[U](f: B => U): Unit = self match {
      case Left(_)  =>
      case Right(r) => f(r)
    }
  }
}
