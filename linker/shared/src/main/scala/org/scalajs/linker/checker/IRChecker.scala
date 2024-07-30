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

import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.standard.LinkedClass
import org.scalajs.linker.standard.LinkTimeProperties
import org.scalajs.linker.checker.ErrorReporter._

/** Checker for the validity of the IR. */
private final class IRChecker(unit: LinkingUnit,
    linkTimeProperties: LinkTimeProperties, reporter: ErrorReporter) {

  import IRChecker._
  import reporter.reportError

  private val classes: mutable.Map[ClassName, CheckedClass] = {
    val tups = for (classDef <- unit.classDefs) yield {
      implicit val ctx = ErrorContext(classDef)
      val c = new CheckedClass(classDef)
      c.name -> c
    }
    mutable.Map(tups: _*)
  }

  def check(): Unit = {
    for (classDef <- unit.classDefs) {
      implicit val ctx = ErrorContext(classDef)

      checkJSSuperClass(classDef)

      classDef.fields.foreach {
        case _: FieldDef            => // no further checks
        case JSFieldDef(_, name, _) => typecheckExpr(name, Env.empty)
      }

      classDef.methods.foreach(checkMethodDef(_, classDef))
      classDef.jsConstructorDef.foreach(checkJSConstructorDef(_, classDef))

      classDef.exportedMembers.foreach {
        case jsMethodDef: JSMethodDef =>
          checkJSMethodDef(jsMethodDef, classDef)

        case jsPropertyDef: JSPropertyDef =>
          checkJSPropertyDef(jsPropertyDef, classDef)
      }
    }

    for (topLevelExport <- unit.topLevelExports) {
      topLevelExport.tree match {
        case TopLevelMethodExportDef(_, methodDef) =>
          implicit val ctx = ErrorContext(methodDef)
          typecheckExpect(methodDef.body, Env.empty, AnyType)

        case _:TopLevelJSClassExportDef | _:TopLevelModuleExportDef |
            _:TopLevelFieldExportDef =>
      }
    }
  }

  private def checkJSSuperClass(classDef: LinkedClass): Unit = {
    implicit val ctx = ErrorContext(classDef)

    if (classDef.kind.isJSClass) {
      classDef.jsSuperClass.fold {
        // .get is OK: the ClassDefChecker checks that a super class is present.
        val superClass = lookupClass(classDef.superClass.get.name)
        if (superClass.jsClassCaptures.isDefined)
          reportError(i"super class ${superClass.name} may not have jsClassCaptures")
        else if (superClass.kind == ClassKind.NativeJSClass && superClass.jsNativeLoadSpec.isEmpty)
          reportError(i"Native super class ${superClass.name} must have a native load spec")
      } { tree =>
        typecheckExpect(tree, Env.empty, AnyType)
      }
    } else {
      assert(classDef.jsSuperClass.isEmpty) // checked by ClassDefChecker
    }
  }

  private def checkMethodDef(methodDef: MethodDef,
      classDef: LinkedClass): Unit = {

    val MethodDef(flags, MethodIdent(name), _, params, resultType, body) =
      methodDef
    implicit val ctx = ErrorContext(methodDef)

    val static = flags.namespace.isStatic

    val advertizedSig = (params.map(_.ptpe), resultType)
    val sigFromName = inferMethodType(name, static)
    if (advertizedSig != sigFromName) {
      reportError(
          i"The signature of ${classDef.name.name}.$name, which is "+
          i"$advertizedSig, does not match its name (should be $sigFromName).")
    }

    body.fold {
      // Abstract
      reportError(
          i"The abstract method ${classDef.name.name}.$name survived the " +
          "Analyzer (this is a bug)")
    } { body =>
      val bodyEnv =
        if (flags.namespace.isConstructor) Env.forConstructorOf(classDef.name.name)
        else Env.empty

      typecheckExpect(body, bodyEnv, resultType)
    }
  }

  private def checkJSConstructorDef(ctorDef: JSConstructorDef,
      clazz: LinkedClass): Unit =  {
    val JSConstructorDef(flags, params, restParam, body) = ctorDef
    implicit val ctx = ErrorContext(ctorDef)

    // JS constructors only get a valid `this` after the super call.

    val bodyEnv = Env.forConstructorOf(clazz.name.name)
    body.beforeSuper.foreach(typecheck(_, bodyEnv))
    body.superCall.args.foreach(typecheckExprOrSpread(_, bodyEnv))
    body.afterSuper.foreach(typecheck(_, bodyEnv))

    val resultType = body.afterSuper.lastOption.fold[Type](NoType)(_.tpe)
    if (resultType == NoType)
      reportError(i"${AnyType} expected but $resultType found for JS constructor body")
  }

  private def checkJSMethodDef(methodDef: JSMethodDef,
      clazz: LinkedClass): Unit =  {
    val JSMethodDef(flags, pName, params, restParam, body) = methodDef
    implicit val ctx = ErrorContext(methodDef)

    val static = flags.namespace.isStatic

    typecheckExpr(pName, Env.empty)

    typecheckExpect(body, Env.empty, AnyType)
  }

  private def checkJSPropertyDef(propDef: JSPropertyDef,
      clazz: LinkedClass): Unit =  {
    val JSPropertyDef(flags, pName, getterBody, setterArgAndBody) = propDef
    implicit val ctx = ErrorContext(propDef)

    typecheckExpr(pName, Env.empty)

    getterBody.foreach(typecheckExpr(_, Env.empty))

    setterArgAndBody.foreach { case (_, body) =>
      typecheck(body, Env.empty)
    }
  }

  private def typecheckExpect(tree: Tree, env: Env, expectedType: Type)(
      implicit ctx: ErrorContext): Unit = {
    typecheck(tree, env)

    if (!isSubtype(tree.tpe, expectedType)) {
      reportError(i"$expectedType expected but ${tree.tpe} found "+
          i"for tree of type ${tree.getClass.getName}")
    }
  }

  private def typecheckExpr(tree: Tree, env: Env)(
      implicit ctx: ErrorContext): Unit = {
    typecheckExpect(tree, env, AnyType)
  }

  private def typecheckExprOrSpread(tree: TreeOrJSSpread, env: Env)(
      implicit ctx: ErrorContext): Unit = {
    tree match {
      case JSSpread(items) =>
        typecheckExpr(items, env)
      case tree: Tree =>
        typecheckExpr(tree, env)
    }
  }

  private def typecheck(tree: Tree, env: Env): Unit = {
    implicit val ctx = ErrorContext(tree)

    def checkApplyGeneric(receiverTypeForError: Any, methodName: MethodName,
        args: List[Tree], tpe: Type, isStatic: Boolean): Unit = {
      val (methodParams, resultType) = inferMethodType(methodName, isStatic)
      for ((actual, formal) <- args zip methodParams) {
        typecheckExpect(actual, env, formal)
      }
      if (tpe != resultType)
        reportError(i"Call to $receiverTypeForError.$methodName of type $resultType typed as ${tree.tpe}")
    }

    tree match {
      // Definitions

      case VarDef(ident, _, vtpe, _, rhs) =>
        typecheckExpect(rhs, env, vtpe)

      // Control flow constructs

      case Skip() =>

      case Block(trees) =>
        trees.foreach(typecheck(_, env))

      case Labeled(label, tpe, body) =>
        typecheckExpect(body, env.withLabeledReturnType(label.name, tpe), tpe)

      case Assign(lhs, rhs) =>
        def checkNonStaticField(receiver: Tree, name: FieldName): Unit = {
          receiver match {
            case This() if env.inConstructorOf == Some(name.className) =>
              // ok
            case _ =>
              if (lookupClass(name.className).lookupField(name).exists(!_.flags.isMutable))
                reportError(i"Assignment to immutable field $name.")
          }
        }

        lhs match {
          case Select(receiver, FieldIdent(name)) =>
            checkNonStaticField(receiver, name)
          case JSPrivateSelect(receiver, FieldIdent(name)) =>
            checkNonStaticField(receiver, name)
          case SelectStatic(FieldIdent(name)) =>
            val c = lookupClass(name.className)
            for {
              f <- c.lookupStaticField(name)
              if !f.flags.isMutable
            } {
              reportError(i"Assignment to immutable static field $name.")
            }

          case _:VarRef | _:ArraySelect | _:RecordSelect | _:JSSelect |
              _:JSSuperSelect | _:JSGlobalRef =>
        }
        typecheckExpr(lhs, env)
        typecheckExpect(rhs, env, lhs.tpe)

      case Return(expr, label) =>
        val returnType = env.returnTypes(label.name)
        if (returnType == NoType)
          typecheckExpr(expr, env)
        else
          typecheckExpect(expr, env, returnType)

      case If(cond, thenp, elsep) =>
        val tpe = tree.tpe
        typecheckExpect(cond, env, BooleanType)
        typecheckExpect(thenp, env, tpe)
        typecheckExpect(elsep, env, tpe)

      case LinkTimeIf(cond, thenp, elsep) =>
        val tpe = tree.tpe
        if (linkTimeProperties.evaluateLinkTimeTree(cond))
          typecheckExpect(thenp, env, tpe)
        else
          typecheckExpect(elsep, env, tpe)

      case While(cond, body) =>
        typecheckExpect(cond, env, BooleanType)
        typecheck(body, env)

      case ForIn(obj, keyVar, _, body) =>
        typecheckExpr(obj, env)
        typecheck(body, env)

      case TryCatch(block, errVar, _, handler) =>
        val tpe = tree.tpe
        typecheckExpect(block, env, tpe)
        typecheckExpect(handler, env, tpe)

      case TryFinally(block, finalizer) =>
        val tpe = tree.tpe
        typecheckExpect(block, env, tpe)
        typecheck(finalizer, env)

      case Throw(expr) =>
        typecheckExpr(expr, env)

      case Match(selector, cases, default) =>
        // Typecheck the selector as an int or a java.lang.String
        typecheck(selector, env)
        if (!isSubtype(selector.tpe, IntType) && !isSubtype(selector.tpe, BoxedStringType)) {
          reportError(
              i"int or java.lang.String expected but ${selector.tpe} found" +
              i"for tree of type ${selector.getClass.getName}")
        }

        // The alternatives are MatchableLiterals, no point typechecking them
        val tpe = tree.tpe
        for ((_, body) <- cases)
          typecheckExpect(body, env, tpe)
        typecheckExpect(default, env, tpe)

      case Debugger() =>

      // Scala expressions

      case New(className, ctor, args) =>
        val clazz = lookupClass(className)
        if (clazz.kind != ClassKind.Class)
          reportError(i"new $className which is not a class")
        checkApplyGeneric(className, ctor.name, args, NoType, isStatic = false)

      case LoadModule(className) =>
        val clazz = lookupClass(className)
        if (clazz.kind != ClassKind.ModuleClass)
          reportError("LoadModule of non-module class $className")

      case StoreModule() =>
        // Nothing to check; everything is checked in ClassDefChecker
        ()

      case Select(qualifier, FieldIdent(item)) =>
        val className = item.className
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

      case SelectStatic(FieldIdent(item)) =>
        val className = item.className
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
        typecheckExpr(receiver, env)
        val fullCheck = receiver.tpe match {
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
          checkApplyGeneric(receiver.tpe, method, args, tree.tpe, isStatic = false)
        } else {
          for (arg <- args)
            typecheckExpr(arg, env)
        }

      case ApplyStatically(_, receiver, className, MethodIdent(method), args) =>
        typecheckExpect(receiver, env, ClassType(className))
        checkApplyGeneric(className, method, args, tree.tpe, isStatic = false)

      case ApplyStatic(_, className, MethodIdent(method), args) =>
        checkApplyGeneric(className, method, args, tree.tpe, isStatic = true)

      case ApplyDynamicImport(_, className, MethodIdent(method), args) =>
        checkApplyGeneric(className, method, args, AnyType, isStatic = true)

        val resultType = method.resultTypeRef
        if (resultType != ClassRef(ObjectClass)) {
          reportError(
              i"illegal dynamic import call to $className.$method " +
              i"with non-object result type: $resultType")
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
          case LongToInt | LongToDouble | LongToFloat =>
            LongType
          case FloatToDouble =>
            FloatType
          case DoubleToInt | DoubleToFloat | DoubleToLong =>
            DoubleType
          case String_length =>
            StringType
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
          case String_charAt =>
            StringType
        }
        val expectedRhsType = (op: @switch) match {
          case Long_<< | Long_>>> | Long_>> | String_charAt => IntType
          case _                                            => expectedLhsType
        }
        typecheckExpect(lhs, env, expectedLhsType)
        typecheckExpect(rhs, env, expectedRhsType)

      case NewArray(typeRef, lengths) =>
        for (length <- lengths)
          typecheckExpect(length, env, IntType)

      case ArrayValue(typeRef, elems) =>
        val elemType = arrayElemType(typeRef)
        for (elem <- elems)
          typecheckExpect(elem, env, elemType)

      case ArrayLength(array) =>
        typecheckExpr(array, env)
        if (!array.tpe.isInstanceOf[ArrayType])
          reportError(i"Array type expected but ${array.tpe} found")

      case ArraySelect(array, index) =>
        typecheckExpect(index, env, IntType)
        typecheckExpr(array, env)
        array.tpe match {
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

      case WrapAsThrowable(expr) =>
        typecheckExpr(expr, env)

      case UnwrapFromThrowable(expr) =>
        typecheckExpect(expr, env, ClassType(ThrowableClass))

      // JavaScript expressions

      case JSNew(ctor, args) =>
        typecheckExpr(ctor, env)
        for (arg <- args)
          typecheckExprOrSpread(arg, env)

      case JSPrivateSelect(qualifier, field) =>
        typecheckExpr(qualifier, env)
        val className = field.name.className
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

      case JSNewTarget() =>

      case JSImportMeta() =>

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

      case JSDelete(qualifier, item) =>
        typecheckExpr(qualifier, env)
        typecheckExpr(item, env)

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

      case _: Literal =>

      // Atomic expressions

      case _: VarRef =>

      case This() =>

      case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
        assert(captureParams.size == captureValues.size) // checked by ClassDefChecker

        // Check compliance of captureValues wrt. captureParams in the current env
        for ((ParamDef(_, _, ctpe, _), value) <- captureParams zip captureValues) {
          typecheckExpect(value, env, ctpe)
        }

        // Then check the closure params and body in its own env
        typecheckExpect(body, Env.empty, AnyType)

      case CreateJSClass(className, captureValues) =>
        val clazz = lookupClass(className)
        clazz.jsClassCaptures.fold {
          reportError(i"Invalid CreateJSClass of top-level class $className")
        } { captureParams =>
          if (captureParams.size != captureValues.size) {
            reportError("Mismatched size for class captures: " +
                i"${captureParams.size} params vs ${captureValues.size} values")
          }

          for ((ParamDef(_, _, ctpe, _), value) <- captureParams.zip(captureValues))
            typecheckExpect(value, env, ctpe)
        }

      case _:RecordSelect | _:RecordValue | _:Transient | _:JSSuperConstructorCall =>
        reportError("invalid tree")
    }
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

      case _ =>
        // Non ClassTypes are checked by the ClassDef checker.
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
      /** Return types by label. */
      val returnTypes: Map[LabelName, Type],
      /** Whether we're in a constructor of the class */
      val inConstructorOf: Option[ClassName]
  ) {
    import Env._

    def withLabeledReturnType(label: LabelName, returnType: Type): Env =
      new Env(returnTypes + (label -> returnType), this.inConstructorOf)
  }

  private object Env {
    val empty: Env = new Env(Map.empty, None)

    def forConstructorOf(className: ClassName): Env =
      new Env(Map.empty, inConstructorOf = Some(className))
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
      val jsNativeMembers: Set[MethodName]) {

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
  private val BoxedStringType = ClassType(BoxedStringClass)

  /** Checks that the IR in a [[frontend.LinkingUnit LinkingUnit]] is correct.
   *
   *  @return Count of IR checking errors (0 in case of success)
   */
  def check(unit: LinkingUnit,
      linkTimeProperties: LinkTimeProperties, logger: Logger): Int = {
    val reporter = new LoggerErrorReporter(logger)
    new IRChecker(unit, linkTimeProperties, reporter).check()
    reporter.errorCount
  }
}
