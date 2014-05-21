/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.language.implicitConversions

import scala.annotation.switch

import scala.collection.mutable

import scala.scalajs.ir._
import Definitions._
import Trees._
import Types._

import scala.scalajs.tools.logging._

/** Checker for the validity of the IR. */
class IRChecker(analyzer: Analyzer, allClassDefs: Seq[ClassDef], logger: Logger) {
  import IRChecker._

  private var _errorCount: Int = 0
  def errorCount: Int = _errorCount

  private val classes: mutable.Map[String, CheckedClass] = {
    val Obj = new CheckedClass(ObjectClass, ClassKind.Class, None, Set.empty)
    val Str = new CheckedClass(StringClass, ClassKind.HijackedClass,
        Some(ObjectClass), AncestorsOfStringClass + ObjectClass)
    val userDefined = allClassDefs.map(new CheckedClass(_))
    mutable.Map.empty[String, CheckedClass] ++=
      (Obj +: Str +: userDefined).map(c => c.name -> c)
  }

  def check(): Boolean = {
    for {
      classDef <- allClassDefs
      if analyzer.classInfos(classDef.name.name).isNeededAtAll
    } {
      classDef.kind match {
        case ClassKind.Class | ClassKind.ModuleClass => checkClass(classDef)
        case ClassKind.TraitImpl                     => checkTraitImpl(classDef)
        case _ =>
      }
    }
    errorCount == 0
  }

  def checkClass(classDef: ClassDef): Unit = {
    if (!analyzer.classInfos(classDef.name.name).isAnySubclassInstantiated)
      return

    for (member <- classDef.defs) {
      implicit val ctx = ErrorContext(member)
      member match {
        // Scala declarations
        case v @ VarDef(_, _, _, _) =>
          checkFieldDef(v, classDef)
        case m @ MethodDef(_: Ident, _, _, _) =>
          checkMethodDef(m, classDef)

        // Exports (ignored, maybe someday we could check them as well)
        case MethodDef(_: StringLiteral, _, _, _) =>
        case PropertyDef(_: StringLiteral, _, _, _) =>
        case ConstructorExportDef(_, _, _) =>
        case ModuleExportDef(_) =>

        // Anything else is illegal
        case _ =>
          reportError(s"Illegal class member of type ${member.getClass.getName}")
      }
    }
  }

  def checkTraitImpl(classDef: ClassDef): Unit = {
    for (member <- classDef.defs) {
      implicit val ctx = ErrorContext(member)
      member match {
        case m @ MethodDef(_, _, _, _) =>
          checkMethodDef(m, classDef)
        case _ =>
          reportError(s"Invalid member for a TraitImpl")
      }
    }
  }

  def checkFieldDef(fieldDef: VarDef, classDef: ClassDef): Unit = {
    val VarDef(name, tpe, mutable, rhs) = fieldDef
    implicit val ctx = ErrorContext(fieldDef)

    if (tpe == NoType)
      reportError(s"VarDef cannot have type NoType")
    else
      typecheckExpect(rhs, Env.empty, tpe)
  }

  def checkMethodDef(methodDef: MethodDef, classDef: ClassDef): Unit = {
    val MethodDef(Ident(name, _), params, resultType, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    if (!analyzer.classInfos(classDef.name.name).methodInfos(name).isReachable)
      return

    for (ParamDef(name, tpe) <- params)
      if (tpe == NoType)
        reportError(s"Parameter $name has type NoType")

    val resultTypeForSig =
      if (isConstructorName(name)) NoType
      else resultType

    val advertizedSig = (params.map(_.ptpe), resultTypeForSig)
    val sigFromName = inferMethodType(name,
        inTraitImpl = classDef.kind == ClassKind.TraitImpl)
    if (advertizedSig != sigFromName) {
      reportError(
          s"The signature of ${classDef.name.name}.$name, which is "+
          s"$advertizedSig, does not match its name (should be $sigFromName).")
    }

    val thisType =
      if (!classDef.kind.isClass) NoType
      else ClassType(classDef.name.name)
    typecheckStat(body, Env.empty.enteringFun(thisType, params, resultType))
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
        /* TODO In theory the following commented out match would verify that
         * we never assign to an immutable field or local variable. But we
         * cannot do that because we *do* emit such assigns:
         * - Assignments to read-only fields are done in constructors
         * - Assignments to local variables is done in tail calls
         * In the future we might want to check that only these legal special
         * cases happen, and nothing else. But it seems non-trivial to do so,
         * so currently we trust scalac not to make us emit illegal assigns.
         */
        /*select match {
          case Select(_, _, false) =>
            reportError("Assignment to immutable field.")
          case VarRef(_, false) =>
            reportError("Assignment to immutable variable.")
          case _ =>
        }*/
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
            env.withLocal(LocalDef(errVar.name, AnyType, true)(errVar.pos))
          typecheckStat(handler, handlerEnv)
        }
        if (finalizer != EmptyTree) {
          typecheckStat(finalizer, env)
        }
        env

      case Switch(selector, cases, default) =>
        typecheckExpr(selector, env)
        for ((value, body) <- cases) {
          typecheckExpr(value, env)
          typecheckStat(body, env)
        }
        if (default != EmptyTree)
          typecheckStat(default, env)
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

  def typecheck(tree: Tree, env: Env): Type = {
    implicit val ctx = ErrorContext(tree)

    def checkApplyGeneric(methodName: String, methodFullName: String,
        args: List[Tree], inTraitImpl: Boolean): Unit = {
      val (methodParams, resultType) = inferMethodType(methodName, inTraitImpl)
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

      case Try(block, errVar, handler, finalizer) =>
        val tpe = tree.tpe
        typecheckExpect(block, env, tpe)
        if (handler != EmptyTree) {
          val handlerEnv =
            env.withLocal(LocalDef(errVar.name, AnyType, true)(errVar.pos))
          typecheckExpect(handler, handlerEnv, tpe)
        }
        if (finalizer != EmptyTree) {
          typecheckStat(finalizer, env)
        }

      case Throw(expr) =>
        typecheckExpr(expr, env)

      case Break(label) =>
        /* Here we could check that it is indeed legal to break to the
         * specified label. However, if we do anything illegal here, it will
         * result in a SyntaxError in JavaScript anyway, so we do not really
         * care.
         */

      case Continue(label) =>
        // Same remark as for Break.

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
        checkApplyGeneric(ctor.name, s"$cls.$ctor", args,
            inTraitImpl = false)

      case LoadModule(cls) =>
        if (!cls.className.endsWith("$"))
          reportError("LoadModule of non-module class $cls")

      case Select(qualifier, Ident(item, _), mutable) =>
        val qualType = typecheckExpr(qualifier, env)
        qualType match {
          case ClassType(cls) =>
            val clazz = lookupClass(cls)
            if (!clazz.kind.isClass) {
              reportError(s"Cannot select $item of non-class $cls")
            } else {
              clazz.lookupField(item).fold[Unit] {
                reportError(s"Class $cls does not have a field $item")
              } { fieldDef =>
                if (fieldDef.tpe != tree.tpe)
                  reportError(s"Select $cls.$item of type "+
                      s"${fieldDef.tpe} typed as ${tree.tpe}")
                if (fieldDef.mutable != mutable)
                  reportError(s"Select $cls.$item with "+
                      s"mutable=${fieldDef.mutable} marked as mutable=$mutable")
              }
            }
          case _ =>
            reportError(s"Cannot select $item of non-class type $qualType")
        }

      case Apply(receiver, Ident(method, _), args) =>
        val receiverType = typecheckExpr(receiver, env)
        if (!isReflProxyName(method)) {
          receiverType match {
            case ClassType(cls) =>
              val clazz = lookupClass(cls)
              if (!clazz.kind.isClass && clazz.kind != ClassKind.Interface) {
                reportError(s"Cannot call Scala method $method of "+
                    s"non-class $cls")
              } else if (clazz.isAncestorOfHijackedClass) {
                reportError(s"Cannot call Scala method $method on "+
                    "ancestor of hijacked class $cls")
              }
            case _ =>
              reportError(s"Cannot call Scala method $method on "+
                  s"non-class type $receiverType")
          }
        }
        checkApplyGeneric(method, s"$receiverType.$method", args,
            inTraitImpl = false)

      case StaticApply(receiver, cls, Ident(method, _), args) =>
        typecheckExpect(receiver, env, cls)
        checkApplyGeneric(method, s"$cls.$method", args, inTraitImpl = false)

      case TraitImplApply(impl, Ident(method, _), args) =>
        val clazz = lookupClass(impl)
        if (clazz.kind != ClassKind.TraitImpl)
          reportError(s"Cannot trait-impl apply method of non-trait-impl $impl")
        checkApplyGeneric(method, s"$impl.$method", args, inTraitImpl = true)

      case UnaryOp(op, lhs, tpe) =>
        op match {
          case "typeof" =>
            if (tpe != StringType)
              reportError(s"typeof typed as $tpe")
            typecheckExpr(lhs, env)
          case "+" | "-" =>
            if (tpe != IntType && tpe != DoubleType)
              reportError(s"Unary $op typed as $tpe")
            typecheckExpect(lhs, env, tpe)
          case "~" =>
            if (tpe != IntType)
              reportError(s"Unary $op typed as $tpe")
            typecheckExpect(lhs, env, IntType)
          case "!" =>
            if (tpe != BooleanType)
              reportError(s"Unary $op typed as $tpe")
            typecheckExpect(lhs, env, BooleanType)
          case "(int)" =>
            if (tpe != IntType)
              reportError(s"Unary $op typed as $tpe")
            typecheckExpect(lhs, env, DoubleType)
          case _ =>
            reportError(s"Invalid unary op $op")
            typecheckExpr(lhs, env)
        }

      case BinaryOp(op, lhs, rhs, tpe) =>
        op match {
          case "===" | "!==" =>
            if (tpe != BooleanType)
              reportError(s"Binary $op typed as $tpe")
            typecheckExpr(lhs, env)
            typecheckExpr(rhs, env)
          case "+" if tpe == StringType =>
            typecheckExpr(lhs, env)
            typecheckExpr(rhs, env)
          case "+" | "-" | "*" | "/" | "%" =>
            if (tpe != IntType && tpe != DoubleType)
              reportError(s"Binary $op typed as $tpe")
            typecheckExpect(lhs, env, tpe)
            typecheckExpect(rhs, env, tpe)
          case "|" | "&" | "^" =>
            if (tpe != IntType && tpe != BooleanType)
              reportError(s"Binary $op typed as $tpe")
            typecheckExpect(lhs, env, tpe)
            typecheckExpect(rhs, env, tpe)
          case "||" | "&&" =>
            if (tpe != BooleanType)
              reportError(s"Binary $op typed as $tpe")
            typecheckExpect(lhs, env, tpe)
            typecheckExpect(rhs, env, tpe)
          case "<<" | ">>>" | ">>" =>
            if (tpe != IntType)
              reportError(s"Binary $op typed as $tpe")
            typecheckExpect(lhs, env, tpe)
            typecheckExpect(rhs, env, tpe)
          case "<" | "<=" | ">" | ">=" =>
            if (tpe != BooleanType)
              reportError(s"Binary $op typed as $tpe")
            typecheckExpect(lhs, env, DoubleType)
            typecheckExpect(rhs, env, DoubleType)
          case "in" =>
            if (tpe != BooleanType)
              reportError(s"Binary $op typed as $tpe")
            typecheckExpect(lhs, env, ClassType(StringClass))
            typecheckExpect(rhs, env, DynType)
          case "instanceof" =>
            if (tpe != BooleanType)
              reportError(s"Binary $op typed as $tpe")
            typecheckExpr(lhs, env)
            typecheckExpect(rhs, env, DynType)
          case _ =>
            reportError(s"Invalid binary op $op")
            typecheckExpr(lhs, env)
            typecheckExpr(rhs, env)
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

      case ClassOf(cls) =>

      case CallHelper(helper, args) =>
        if (!HelperSignature.contains(helper)) {
          reportError(s"Invalid helper $helper")
          args.foreach(typecheckExpr(_, env))
        } else {
          val (params, resultType) = HelperSignature(helper)
          if (args.size != params.size)
            reportError(s"Arity mismatch: ${params.size} expected but ${args.size} found")
          for ((actual, formal) <- args.zip(params))
            typecheckExpect(actual, env, formal)
          if (tree.tpe != resultType)
            reportError(s"Helper $helper of type $resultType typed as ${tree.tpe}")
        }

      // JavaScript expressions

      case JSGlobal() =>

      case JSNew(ctor, args) =>
        typecheckExpect(ctor, env, DynType)
        for (arg <- args)
          typecheckExpr(arg, env)

      case JSDotSelect(qualifier, item) =>
        typecheckExpect(qualifier, env, DynType)

      case JSBracketSelect(qualifier, item) =>
        typecheckExpect(qualifier, env, DynType)
        typecheckExpr(item, env)

      case JSApply(fun, args) =>
        typecheckExpect(fun, env, DynType)
        for (arg <- args)
          typecheckExpr(arg, env)

      case JSDelete(obj, prop) =>
        typecheckExpect(obj, env, DynType)
        typecheckExpr(prop, env)

      case JSUnaryOp(op, lhs) =>
        typecheckExpect(lhs, env, DynType)

      case JSBinaryOp(op, lhs, rhs) =>
        typecheckExpect(lhs, env, DynType)
        typecheckExpect(rhs, env, DynType)

      case JSArrayConstr(items) =>
        for (item <- items)
          typecheckExpr(item, env)

      case JSObjectConstr(fields) =>
        for ((_, value) <- fields)
          typecheckExpr(value, env)

      // Literals

      case _: Literal =>

      // Atomic expressions

      case VarRef(Ident(name, _), mutable) =>
        env.locals.get(name).fold[Unit] {
          reportError(s"Cannot find variable $name in scope")
        } { localDef =>
          if (tree.tpe != localDef.tpe)
            reportError(s"Variable $name of type ${localDef.tpe} "+
                s"typed as ${tree.tpe}")
          if (mutable != localDef.mutable)
            reportError(s"Variable $name with mutable=${localDef.mutable} "+
                s"marked as mutable=$mutable")
        }

      case This() =>
        if (!isSubtype(env.thisTpe, tree.tpe))
          reportError(s"this of type ${env.thisTpe} typed as ${tree.tpe}")

      case Function(thisType, params, resultType, body) =>
        for (ParamDef(name, tpe) <- params)
          if (tpe == NoType)
            reportError(s"Parameter $name has type NoType")
        typecheckStat(body, env.enteringFun(thisType, params, resultType))


      // Type-related

      case Cast(expr, tpe) =>
        typecheckExpr(expr, env)

      case _ =>
        reportError(s"Invalid expression tree")
    }

    tree.tpe
  }

  def inferMethodType(encodedName: String, inTraitImpl: Boolean)(
      implicit ctx: ErrorContext): (List[Type], Type) = {
    def dropPrivateMarker(params: List[String]): List[String] =
      if (params.nonEmpty && params.head.startsWith("p")) params.tail
      else params

    if (isConstructorName(encodedName)) {
      assert(!inTraitImpl, "Trait impl should not have a constructor")
      val params = dropPrivateMarker(
          encodedName.stripPrefix("init___").split("__").toList)
      if (params == List("")) (Nil, NoType)
      else (params.map(decodeType), NoType)
    } else if (isReflProxyName(encodedName)) {
      assert(!inTraitImpl, "Trait impl should not have refl proxy methods")
      val params = dropPrivateMarker(encodedName.split("__").toList.tail)
      (params.map(decodeType), AnyType)
    } else {
      val paramsAndResult0 =
        encodedName.split("__").toList.tail
      val paramsAndResult1 =
        if (inTraitImpl) paramsAndResult0.tail
        else paramsAndResult0
      val paramsAndResult =
        dropPrivateMarker(paramsAndResult1)
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
        case 'J'                   => ClassType(RuntimeLongClass)
        case 'F' | 'D'             => DoubleType
        case 'O'                   => AnyType
        case 'T'                   => ClassType(StringClass) // NOT StringType
      }
    } else if (encodedName == "sr_Nothing$") {
      NothingType
    } else if (encodedName == "sr_Null$") {
      NullType
    } else {
      val clazz = lookupClass(encodedName)
      if (clazz.kind == ClassKind.RawJSType) DynType
      else ClassType(encodedName)
    }
  }

  val HelperSignature: Map[String, (List[Type], Type)] = {
    val StringClassType = ClassType(StringClass)
    val ThrowableType = ClassType(ThrowableClass)
    val CharSeqType = ClassType(CharSequenceClass)
    val RuntimeLongType = ClassType(RuntimeLongClass)
    val NumberType = ClassType(NumberClass)
    Map(
      ("wrapJavaScriptException"  , List(AnyType) -> ThrowableType),
      ("unwrapJavaScriptException", List(AnyType) -> AnyType),

      ("objectToString" , List(AnyType) -> StringClassType),
      ("objectGetClass" , List(AnyType) -> ClassType(ClassClass)),
      ("objectClone"    , List(AnyType) -> AnyType),
      ("objectFinalize" , List(AnyType) -> NoType),
      ("objectNotify"   , List(AnyType) -> NoType),
      ("objectNotifyAll", List(AnyType) -> NoType),
      ("objectEquals"   , List(AnyType, AnyType) -> BooleanType),
      ("objectHashCode" , List(AnyType) -> IntType),

      ("charSequenceLength"     , List(CharSeqType) -> IntType),
      ("charSequenceCharAt"     , List(CharSeqType, IntType) -> IntType),
      ("charSequenceSubSequence", List(CharSeqType, IntType, IntType) -> CharSeqType),

      ("comparableCompareTo", List(ClassType(ComparableClass), AnyType) -> IntType),

      ("booleanBooleanValue", List(ClassType(BoxedBooleanClass)) -> BooleanType),

      ("numberByteValue"  , List(NumberType) -> IntType),
      ("numberShortValue" , List(NumberType) -> IntType),
      ("numberIntValue"   , List(NumberType) -> IntType),
      ("numberLongValue"  , List(NumberType) -> RuntimeLongType),
      ("numberFloatValue" , List(NumberType) -> DoubleType),
      ("numberDoubleValue", List(NumberType) -> DoubleType),

      ("isNaN"     , List(NumberType) -> BooleanType),
      ("isInfinite", List(NumberType) -> BooleanType),

      ("anyEqEq",    List(AnyType, AnyType) -> BooleanType),
      ("anyRefEqEq", List(AnyType, AnyType) -> BooleanType),

      ("bC", List(IntType) -> ClassType(BoxedCharacterClass)),

      ("uZ", List(AnyType) -> BooleanType),
      ("uC", List(AnyType) -> IntType),
      ("uB", List(AnyType) -> IntType),
      ("uS", List(AnyType) -> IntType),
      ("uI", List(AnyType) -> IntType),
      ("uJ", List(AnyType) -> RuntimeLongType),
      ("uF", List(AnyType) -> DoubleType),
      ("uD", List(AnyType) -> DoubleType),

      ("newInstanceWithVarargs", List(DynType, DynType) -> DynType),
      ("applyMethodWithVarargs", List(DynType, StringClassType, DynType) -> DynType),

      ("systemArraycopy", List(AnyType, IntType, AnyType, IntType, IntType) -> NoType),

      ("propertiesOf", List(DynType) -> DynType),

      ("protect", List(DynType) -> DynType)
    )
  }

  def arrayElemType(arrayType: ArrayType)(implicit ctx: ErrorContext): Type = {
    if (arrayType.dimensions == 1) decodeType(arrayType.baseClassName)
    else ArrayType(arrayType.baseClassName, arrayType.dimensions-1)
  }

  def reportError(msg: String)(implicit ctx: ErrorContext): Unit = {
    logger.error(s"$ctx: $msg")
    _errorCount += 1
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
    lookupClass(lhs).isSubclass(lookupClass(rhs))
  }

  def isSubtype(lhs: Type, rhs: Type)(implicit ctx: ErrorContext): Boolean = {
    (lhs != NoType && rhs != NoType) && {
      (lhs == rhs) ||
      ((lhs, rhs) match {
        case (_, AnyType)     => true
        case (NothingType, _) => true

        case (ClassType(RuntimeLongClass), ClassType(cls)) =>
          isSubclass(RuntimeLongClass, cls) || isSubclass(BoxedLongClass, cls)

        case (ClassType(lhsClass), ClassType(rhsClass)) =>
          isSubclass(lhsClass, rhsClass)

        case (NullType, ClassType(_))    => true
        case (NullType, ArrayType(_, _)) => true
        case (NullType, DynType)         => true

        case (UndefType, ClassType(cls)) =>
          isSubclass(BoxedUnitClass, cls)
        case (BooleanType, ClassType(cls)) =>
          isSubclass(BoxedBooleanClass, cls)
        case (IntType, ClassType(cls)) =>
          isSubclass(BoxedIntegerClass, cls) ||
          cls == BoxedByteClass ||
          cls == BoxedShortClass
        case (DoubleType, ClassType(cls)) =>
          isSubclass(BoxedDoubleClass, cls) ||
          cls == BoxedFloatClass
        case (StringType, ClassType(cls)) =>
          isSubclass(StringClass, cls)

        case (IntType, DoubleType) => true

        case (ArrayType(lhsBase, lhsDims), ArrayType(rhsBase, rhsDims)) =>
          if (lhsDims < rhsDims) {
            false // because Array[A] </: Array[Array[A]]
          } else if (lhsDims > rhsDims) {
            rhsBase == ObjectClass // because Array[Array[A]] <: Array[Object]
          } else { // lhsDims == rhsDims
            // lhsBase must be <: rhsBase
            if (isPrimitiveClassName(lhsBase) || isPrimitiveClassName(rhsBase)) {
              lhsBase == rhsBase
            } else {
              isSubclass(lhsBase, rhsBase)
            }
          }

        case _ =>
          false
      })
    }
  }

  class Env(
      /** Type of `this`. Can be NoType. */
      val thisTpe: Type,
      /** Local variables in scope (including through closures). */
      val locals: Map[String, LocalDef],
      /** Return types by label. */
      val returnTypes: Map[Option[String], Type]
  ) {
    def withThis(thisTpe: Type): Env =
      new Env(thisTpe, this.locals, this.returnTypes)

    def withLocal(localDef: LocalDef): Env =
      new Env(thisTpe, locals + (localDef.name -> localDef), returnTypes)

    def withLocals(localDefs: TraversableOnce[LocalDef]): Env =
      new Env(thisTpe, locals ++ localDefs.map(d => d.name -> d), returnTypes)

    def withReturnType(returnType: Type): Env =
      new Env(this.thisTpe, this.locals, returnTypes + (None -> returnType))

    def withLabeledReturnType(label: String, returnType: Type): Env =
      new Env(this.thisTpe, this.locals, returnTypes + (Some(label) -> returnType))

    def enteringFun(thisType: Type, params: List[ParamDef],
        resultType: Type): Env = {
      val paramLocalDefs =
        for (p @ ParamDef(name, tpe) <- params) yield
          name.name -> LocalDef(name.name, tpe, mutable = false)(p.pos)
      new Env(thisType, locals ++ paramLocalDefs,
          Map(None -> (if (resultType == NoType) AnyType else resultType)))
    }
  }

  object Env {
    val empty: Env = new Env(NoType, Map.empty, Map.empty)
  }

  class CheckedClass(
      val name: String,
      val kind: ClassKind,
      val superClassName: Option[String],
      val ancestors: Set[String],
      _fields: TraversableOnce[CheckedField] = Nil) {

    val fields = _fields.map(f => f.name -> f).toMap

    lazy val superClass = superClassName.map(classes)

    def this(classDef: ClassDef) = {
      this(classDef.name.name, classDef.kind,
          classDef.parent.map(_.name),
          classDef.ancestors.map(_.name).toSet,
          CheckedClass.collectFields(classDef))
    }

    def isSubclass(that: CheckedClass): Boolean =
      this == that || ancestors.contains(that.name)

    def isAncestorOfHijackedClass: Boolean =
      AncestorsOfHijackedClasses.contains(name)

    def lookupField(name: String): Option[CheckedField] =
      fields.get(name).orElse(superClass.flatMap(_.lookupField(name)))
  }

  object CheckedClass {
    private def collectFields(classDef: ClassDef) = {
      classDef.defs collect {
        case VarDef(Ident(name, _), tpe, mutable, _) =>
          new CheckedField(name, tpe, mutable)
      }
    }
  }

  class CheckedField(val name: String, val tpe: Type, val mutable: Boolean)
}

object IRChecker {
  private final class ErrorContext(val tree: Tree) extends AnyVal {
    override def toString(): String = {
      val pos = tree.pos
      s"${pos.source}(${pos.line+1}:${pos.column+1}:${tree.getClass.getSimpleName})"
    }

    def pos: Position = tree.pos
  }

  private object ErrorContext {
    implicit def tree2errorContext(tree: Tree): ErrorContext =
      ErrorContext(tree)

    def apply(tree: Tree): ErrorContext =
      new ErrorContext(tree)
  }

  private val isPrimitiveClassName = Set(
    "V", "Z", "C", "B", "S", "I", "J", "F", "D"
  )

  private def isConstructorName(name: String): Boolean =
    name.startsWith("init___")

  private def isReflProxyName(name: String): Boolean =
    name.endsWith("__") && !isConstructorName(name)

  case class LocalDef(name: String, tpe: Type, mutable: Boolean)(val pos: Position)
}
