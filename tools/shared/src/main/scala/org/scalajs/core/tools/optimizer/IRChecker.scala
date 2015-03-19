/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.language.implicitConversions

import scala.annotation.switch

import scala.collection.mutable

import org.scalajs.core.ir._
import Definitions._
import Trees._
import Types._

import org.scalajs.core.tools.logging._

/** Checker for the validity of the IR. */
class IRChecker(unit: LinkingUnit, logger: Logger) {
  import IRChecker._

  private var _errorCount: Int = 0
  def errorCount: Int = _errorCount

  private val classes: mutable.Map[String, CheckedClass] = {
    val tups = for (classDef <- unit.classDefs) yield {
      implicit val ctx = ErrorContext(classDef)
      val c = new CheckedClass(classDef)
      c.name -> c
    }
    mutable.Map(tups: _*)
  }

  def check(): Boolean = {
    for (classDef <- unit.classDefs) {
      implicit val ctx = ErrorContext(classDef)
      checkStaticMembers(classDef)
      classDef.kind match {
        case ClassKind.RawJSType =>
          if (classDef.fields.nonEmpty ||
              classDef.memberMethods.nonEmpty ||
              classDef.abstractMethods.nonEmpty ||
              classDef.exportedMembers.nonEmpty ||
              classDef.classExports.nonEmpty) {
            reportError(s"Raw JS type ${classDef.name} cannot "+
                "have instance members")
          }
        case ClassKind.Interface =>
          if (classDef.fields.nonEmpty ||
              classDef.memberMethods.nonEmpty ||
              classDef.exportedMembers.nonEmpty ||
              classDef.classExports.nonEmpty) {
            reportError(s"Interface ${classDef.name} cannot "+
                "have concrete instance members")
          }
        case _ =>
          checkScalaClassDef(classDef)
      }
    }
    errorCount == 0
  }

  def checkStaticMembers(classDef: LinkedClass): Unit = {
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

  def checkScalaClassDef(classDef: LinkedClass): Unit = {
    assert(classDef.kind != ClassKind.RawJSType &&
        classDef.kind != ClassKind.Interface)

    // Is this a normal class?
    if (classDef.kind != ClassKind.HijackedClass) {
      // Check fields
      for (field <- classDef.fields) {
        implicit val ctx = ErrorContext(field)
        checkFieldDef(field, classDef)
      }

      // Check exported members
      for (member <- classDef.exportedMembers) {
        implicit val ctx = ErrorContext(member.tree)

        member.tree match {
          case m: MethodDef =>
            assert(m.name.isInstanceOf[StringLiteral],
              "Exported method must have StringLiteral as name")
            checkExportedMethodDef(m, classDef)
          case p: PropertyDef =>
            assert(p.name.isInstanceOf[StringLiteral],
              "Exported property must have StringLiteral as name")
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
          case member @ ConstructorExportDef(_, _, _) =>
            checkConstructorExportDef(member, classDef)
          case member @ ModuleExportDef(_) =>
            checkModuleExportDef(member, classDef)
          // Anything else is illegal
          case _ =>
            reportError("Illegal class export of type " +
                tree.getClass.getName)
        }
      }
    } else {
      implicit val ctx = ErrorContext(classDef)

      if (classDef.fields.nonEmpty)
        reportError("Hijacked classes may not have fields")

      if (classDef.exportedMembers.nonEmpty || classDef.classExports.nonEmpty)
        reportError("Hijacked classes may not have exports")
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

  def checkFieldDef(fieldDef: FieldDef, classDef: LinkedClass): Unit = {
    val FieldDef(name, tpe, mutable) = fieldDef
    implicit val ctx = ErrorContext(fieldDef)

    if (tpe == NoType)
      reportError(s"FieldDef cannot have type NoType")
  }

  def checkMethodDef(methodDef: MethodDef, classDef: LinkedClass): Unit = {
    val MethodDef(static, Ident(name, _), params, resultType, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    for (ParamDef(name, tpe, _) <- params)
      if (tpe == NoType)
        reportError(s"Parameter $name has type NoType")

    val isConstructor = isConstructorName(name)
    val resultTypeForSig =
      if (isConstructor) NoType
      else resultType

    val advertizedSig = (params.map(_.ptpe), resultTypeForSig)
    val sigFromName = inferMethodType(name, static)
    if (advertizedSig != sigFromName) {
      reportError(
          s"The signature of ${classDef.name.name}.$name, which is "+
          s"$advertizedSig, does not match its name (should be $sigFromName).")
    }

    if (body == EmptyTree) {
      // Abstract
      if (static)
        reportError(s"Static method ${classDef.name.name}.$name cannot be abstract")
      else if (isConstructor)
        reportError(s"Constructor ${classDef.name.name}.$name cannot be abstract")
    } else {
      // Concrete
      val thisType =
        if (static) NoType
        else ClassType(classDef.name.name)
      val bodyEnv = Env.fromSignature(thisType, params, resultType, isConstructor)
      if (resultType == NoType)
        typecheckStat(body, bodyEnv)
      else
        typecheckExpect(body, bodyEnv, resultType)
    }
  }

  def checkExportedMethodDef(methodDef: MethodDef,
      classDef: LinkedClass): Unit = {
    val MethodDef(static, StringLiteral(name), params, resultType, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    if (!classDef.kind.isClass) {
      reportError(s"Exported method def can only appear in a class")
      return
    }

    if (static)
      reportError("Exported method def cannot be static")

    if (name.contains("__") && name != Definitions.ExportedConstructorsName)
      reportError("Exported method def name cannot contain __")

    for (ParamDef(name, tpe, _) <- params) {
      if (tpe == NoType)
        reportError(s"Parameter $name has type NoType")
      else if (tpe != AnyType)
        reportError(s"Parameter $name of exported method def has type $tpe, "+
            "but must be Any")
    }

    if (resultType != AnyType) {
      reportError(s"Result type of exported method def is $resultType, "+
          "but must be Any")
    }

    val thisType = ClassType(classDef.name.name)
    val bodyEnv = Env.fromSignature(thisType, params, resultType)
      .withArgumentsVar(methodDef.pos)
    typecheckExpect(body, bodyEnv, resultType)
  }

  def checkExportedPropertyDef(propDef: PropertyDef,
      classDef: LinkedClass): Unit = {
    val PropertyDef(_, getterBody, setterArg, setterBody) = propDef
    implicit val ctx = ErrorContext(propDef)

    if (!classDef.kind.isClass) {
      reportError(s"Exported property def can only appear in a class")
      return
    }

    val thisType = ClassType(classDef.name.name)

    if (getterBody != EmptyTree) {
      val getterBodyEnv = Env.fromSignature(thisType, Nil, AnyType)
      typecheckExpect(getterBody, getterBodyEnv, AnyType)
    }

    if (setterBody != EmptyTree) {
      if (setterArg.ptpe != AnyType)
        reportError("Setter argument of exported property def has type "+
            s"${setterArg.ptpe}, but must be Any")

      val setterBodyEnv = Env.fromSignature(thisType, List(setterArg), NoType)
      typecheckStat(setterBody, setterBodyEnv)
    }
  }

  def checkConstructorExportDef(ctorDef: ConstructorExportDef,
      classDef: LinkedClass): Unit = {
    val ConstructorExportDef(_, params, body) = ctorDef
    implicit val ctx = ErrorContext(ctorDef)

    if (!classDef.kind.isClass) {
      reportError(s"Exported constructor def can only appear in a class")
      return
    }

    for (ParamDef(name, tpe, _) <- params) {
      if (tpe == NoType)
        reportError(s"Parameter $name has type NoType")
      else if (tpe != AnyType)
        reportError(s"Parameter $name of exported constructor def has type "+
            s"$tpe, but must be Any")
    }

    val thisType = ClassType(classDef.name.name)
    val bodyEnv = Env.fromSignature(thisType, params, NoType)
      .withArgumentsVar(ctorDef.pos)
    typecheckStat(body, bodyEnv)
  }

  def checkModuleExportDef(moduleDef: ModuleExportDef,
      classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(moduleDef)

    if (classDef.kind != ClassKind.ModuleClass)
      reportError(s"Exported module def can only appear in a module class")
  }

  def typecheckStat(tree: Tree, env: Env): Env = {
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
                  c <- tryLookupClass(clazz).right
                  f <- c.lookupField(name)
                  if !f.mutable
                } reportError(s"Assignment to immutable field $name.")
              case _ =>
            }
          case VarRef(Ident(name, _)) if !env.locals(name).mutable =>
            reportError(s"Assignment to immutable variable $name.")
          case _ =>
        }
        val lhsTpe = typecheckExpr(select, env)
        val expectedRhsTpe = select match {
          case _:JSDotSelect | _:JSBracketSelect => AnyType
          case _                                 => lhsTpe
        }
        typecheckExpect(rhs, env, expectedRhsTpe)
        env

      case StoreModule(cls, value) =>
        if (!cls.className.endsWith("$"))
          reportError("StoreModule of non-module class $cls")
        typecheckExpect(value, env, ClassType(cls.className))
        env

      case Block(stats) =>
        (env /: stats) { (prevEnv, stat) =>
          typecheckStat(stat, prevEnv)
        }
        env

      case Labeled(label, NoType, body) =>
        typecheckStat(body, env.withLabeledReturnType(label.name, AnyType))
        env

      case If(cond, thenp, elsep) =>
        typecheckExpect(cond, env, BooleanType)
        typecheckStat(thenp, env)
        typecheckStat(elsep, env)
        env

      case While(cond, body, label) =>
        typecheckExpect(cond, env, BooleanType)
        typecheckStat(body, env)
        env

      case DoWhile(body, cond, label) =>
        typecheckStat(body, env)
        typecheckExpect(cond, env, BooleanType)
        env

      case Try(block, errVar, handler, finalizer) =>
        typecheckStat(block, env)
        if (handler != EmptyTree) {
          val handlerEnv =
            env.withLocal(LocalDef(errVar.name, AnyType, false)(errVar.pos))
          typecheckStat(handler, handlerEnv)
        }
        if (finalizer != EmptyTree) {
          typecheckStat(finalizer, env)
        }
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

  def typecheckExpect(tree: Tree, env: Env, expectedType: Type)(
      implicit ctx: ErrorContext): Unit = {
    val tpe = typecheckExpr(tree, env)
    if (!isSubtype(tpe, expectedType))
      reportError(s"$expectedType expected but $tpe found "+
          s"for tree of type ${tree.getClass.getName}")
  }

  def typecheckExpr(tree: Tree, env: Env): Type = {
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

  def typecheck(tree: Tree, env: Env): Type = {
    implicit val ctx = ErrorContext(tree)

    def checkApplyGeneric(methodName: String, methodFullName: String,
        args: List[Tree], isStatic: Boolean): Unit = {
      val (methodParams, resultType) = inferMethodType(methodName, isStatic)
      if (args.size != methodParams.size)
        reportError(s"Arity mismatch: ${methodParams.size} expected but "+
            s"${args.size} found")
      for ((actual, formal) <- args zip methodParams) {
        typecheckExpect(actual, env, formal)
      }
      if (!isConstructorName(methodName) && tree.tpe != resultType)
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
        typecheckStat(body, env)

      case Try(block, errVar, handler, finalizer) =>
        val tpe = tree.tpe
        typecheckExpect(block, env, tpe)
        if (handler != EmptyTree) {
          val handlerEnv =
            env.withLocal(LocalDef(errVar.name, AnyType, false)(errVar.pos))
          typecheckExpect(handler, handlerEnv, tpe)
        }
        if (finalizer != EmptyTree) {
          typecheckStat(finalizer, env)
        }

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
        checkApplyGeneric(ctor.name, s"$cls.$ctor", args, isStatic = false)

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
              maybeClass.right foreach {
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

      case Apply(receiver, Ident(method, _), args) =>
        val receiverType = typecheckExpr(receiver, env)
        checkApplyGeneric(method, s"$receiverType.$method", args,
            isStatic = false)

      case ApplyStatically(receiver, cls, Ident(method, _), args) =>
        typecheckExpect(receiver, env, cls)
        checkApplyGeneric(method, s"$cls.$method", args, isStatic = false)

      case ApplyStatic(cls, Ident(method, _), args) =>
        val clazz = lookupClass(cls)
        checkApplyGeneric(method, s"$cls.$method", args, isStatic = true)

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
            typecheckExpect(lhs, env, FloatType)
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
            typecheckExpect(lhs, env, DoubleType)
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

      case JSEnvInfo() =>

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
        if (captureParams.size != captureValues.size)
          reportError("Mismatched size for captures: "+
              s"${captureParams.size} params vs ${captureValues.size} values")

        for ((ParamDef(name, ctpe, mutable), value) <- captureParams zip captureValues) {
          if (mutable)
            reportError(s"Capture parameter $name cannot be mutable")
          if (ctpe == NoType)
            reportError(s"Parameter $name has type NoType")
          else
            typecheckExpect(value, env, ctpe)
        }

        for (ParamDef(name, ptpe, mutable) <- params) {
          if (ptpe == NoType)
            reportError(s"Parameter $name has type NoType")
          else if (ptpe != AnyType)
            reportError(s"Closure parameter $name has type $ptpe instead of any")
        }

        val bodyEnv = Env.fromSignature(
            AnyType, captureParams ++ params, AnyType)
        typecheckExpect(body, bodyEnv, AnyType)

      case _ =>
        reportError(s"Invalid expression tree")
    }

    tree.tpe
  }

  def inferMethodType(encodedName: String, isStatic: Boolean)(
      implicit ctx: ErrorContext): (List[Type], Type) = {
    def dropPrivateMarker(params: List[String]): List[String] =
      if (params.nonEmpty && params.head.startsWith("p")) params.tail
      else params

    if (isConstructorName(encodedName)) {
      assert(!isStatic, "Constructor cannot be static")
      val params = dropPrivateMarker(
          encodedName.stripPrefix("init___").split("__").toList)
      if (params == List("")) (Nil, NoType)
      else (params.map(decodeType), NoType)
    } else if (isReflProxyName(encodedName)) {
      assert(!isStatic, "Refl proxy method cannot be static")
      val params = dropPrivateMarker(encodedName.split("__").toList.tail)
      (params.map(decodeType), AnyType)
    } else {
      val paramsAndResult0 =
        encodedName.split("__").toList.tail
      val paramsAndResult =
        dropPrivateMarker(paramsAndResult0)
      (paramsAndResult.init.map(decodeType), decodeType(paramsAndResult.last))
    }
  }

  def decodeType(encodedName: String)(implicit ctx: ErrorContext): Type = {
    if (encodedName.isEmpty) NoType
    else if (encodedName.charAt(0) == 'A') {
      // array type
      val dims = encodedName.indexWhere(_ != 'A')
      val base = encodedName.substring(dims)
      ArrayType(base, dims)
    } else if (encodedName.length == 1) {
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
      if (kind == ClassKind.RawJSType) AnyType
      else ClassType(encodedName)
    }
  }

  def arrayElemType(arrayType: ArrayType)(implicit ctx: ErrorContext): Type = {
    if (arrayType.dimensions == 1) decodeType(arrayType.baseClassName)
    else ArrayType(arrayType.baseClassName, arrayType.dimensions-1)
  }

  def reportError(msg: String)(implicit ctx: ErrorContext): Unit = {
    logger.error(s"$ctx: $msg")
    _errorCount += 1
  }

  def lookupInfo(className: String)(implicit ctx: ErrorContext): Infos.ClassInfo = {
    unit.infos.getOrElse(className, {
      reportError(s"Cannot find info for class $className")
      Infos.ClassInfo(className)
    })
  }

  def tryLookupClass(className: String)(
       implicit ctx: ErrorContext): Either[Infos.ClassInfo, CheckedClass] = {
    classes.get(className).fold[Either[Infos.ClassInfo, CheckedClass]](
        Left(lookupInfo(className)))(Right(_))
  }

  def lookupClass(className: String)(implicit ctx: ErrorContext): CheckedClass = {
    classes.getOrElseUpdate(className, {
      reportError(s"Cannot find class $className")
      new CheckedClass(className, ClassKind.Class,
          Some(ObjectClass), Set(ObjectClass))
    })
  }

  def lookupClass(classType: ClassType)(implicit ctx: ErrorContext): CheckedClass =
    lookupClass(classType.className)

  def isSubclass(lhs: String, rhs: String)(implicit ctx: ErrorContext): Boolean = {
    tryLookupClass(lhs).fold({ info =>
      val parents = info.superClass ++: info.interfaces
      parents.exists(_ == rhs) || parents.exists(isSubclass(_, rhs))
    }, { lhsClass =>
      lhsClass.ancestors.contains(rhs)
    })
  }

  def isSubtype(lhs: Type, rhs: Type)(implicit ctx: ErrorContext): Boolean = {
    Types.isSubtype(lhs, rhs)(isSubclass)
  }

  class Env(
      /** Type of `this`. Can be NoType. */
      val thisTpe: Type,
      /** Local variables in scope (including through closures). */
      val locals: Map[String, LocalDef],
      /** Return types by label. */
      val returnTypes: Map[Option[String], Type],
      /** Whether we're in a constructor of the class */
      val inConstructor: Boolean
  ) {
    def withThis(thisTpe: Type): Env =
      new Env(thisTpe, this.locals, this.returnTypes, this.inConstructor)

    def withLocal(localDef: LocalDef): Env =
      new Env(thisTpe, locals + (localDef.name -> localDef), returnTypes,
          this.inConstructor)

    def withLocals(localDefs: TraversableOnce[LocalDef]): Env =
      new Env(thisTpe, locals ++ localDefs.map(d => d.name -> d), returnTypes,
          this.inConstructor)

    def withReturnType(returnType: Type): Env =
      new Env(this.thisTpe, this.locals,
          returnTypes + (None -> returnType), this.inConstructor)

    def withLabeledReturnType(label: String, returnType: Type): Env =
      new Env(this.thisTpe, this.locals,
          returnTypes + (Some(label) -> returnType), this.inConstructor)

    def withArgumentsVar(pos: Position): Env =
      withLocal(LocalDef("arguments", AnyType, mutable = false)(pos))

    def withInConstructor(inConstructor: Boolean): Env =
      new Env(this.thisTpe, this.locals, this.returnTypes, inConstructor)
  }

  object Env {
    val empty: Env = new Env(NoType, Map.empty, Map.empty, false)

    def fromSignature(thisType: Type, params: List[ParamDef],
        resultType: Type, isConstructor: Boolean = false): Env = {
      val paramLocalDefs =
        for (p @ ParamDef(name, tpe, mutable) <- params) yield
          name.name -> LocalDef(name.name, tpe, mutable)(p.pos)
      new Env(thisType, paramLocalDefs.toMap,
          Map(None -> (if (resultType == NoType) AnyType else resultType)),
          isConstructor)
    }
  }

  class CheckedClass(
      val name: String,
      val kind: ClassKind,
      val superClassName: Option[String],
      val ancestors: Set[String],
      _fields: TraversableOnce[CheckedField] = Nil)(
      implicit ctx: ErrorContext) {

    val fields = _fields.map(f => f.name -> f).toMap

    lazy val superClass = superClassName.map(classes)

    def this(classDef: LinkedClass)(implicit ctx: ErrorContext) = {
      this(classDef.name.name, classDef.kind,
          classDef.superClass.map(_.name),
          classDef.ancestors.toSet,
          classDef.fields.map(CheckedClass.checkedField))
    }

    def isAncestorOfHijackedClass: Boolean =
      AncestorsOfHijackedClasses.contains(name)

    def lookupField(name: String): Option[CheckedField] =
      fields.get(name).orElse(superClass.flatMap(_.lookupField(name)))
  }

  object CheckedClass {
    private def checkedField(fieldDef: FieldDef) = {
      val FieldDef(Ident(name, _), tpe, mutable) = fieldDef
      new CheckedField(name, tpe, mutable)
    }
  }

  class CheckedField(val name: String, val tpe: Type, val mutable: Boolean)
}

object IRChecker {
  private final class ErrorContext private (pos: Position, name: String) {
    override def toString(): String =
      s"${pos.source}(${pos.line+1}:${pos.column+1}:$name)"
  }

  private object ErrorContext {
    implicit def tree2errorContext(tree: Tree): ErrorContext =
      ErrorContext(tree)

    def apply(tree: Tree): ErrorContext =
      new ErrorContext(tree.pos, tree.getClass.getSimpleName)

    def apply(linkedClass: LinkedClass): ErrorContext =
      new ErrorContext(linkedClass.pos, "ClassDef")
  }

  private def isConstructorName(name: String): Boolean =
    name.startsWith("init___")

  private def isReflProxyName(name: String): Boolean =
    name.endsWith("__") && !isConstructorName(name)

  case class LocalDef(name: String, tpe: Type, mutable: Boolean)(val pos: Position)
}
