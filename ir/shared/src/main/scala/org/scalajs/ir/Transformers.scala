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

package org.scalajs.ir

import Trees._
import Types._
import Version.Unversioned

object Transformers {

  abstract class Transformer {
    final def transformStats(trees: List[Tree]): List[Tree] =
      trees.map(transformStat(_))

    final def transformStat(tree: Tree): Tree =
      transform(tree, isStat = true)

    final def transformExpr(tree: Tree): Tree =
      transform(tree, isStat = false)

    def transformExprOrJSSpread(tree: TreeOrJSSpread): TreeOrJSSpread = {
      implicit val pos = tree.pos

      tree match {
        case JSSpread(items) => JSSpread(transformExpr(items))
        case tree: Tree      => transformExpr(tree)
      }
    }

    def transform(tree: Tree, isStat: Boolean): Tree = {
      implicit val pos = tree.pos

      tree match {
        // Definitions

        case VarDef(ident, originalName, vtpe, mutable, rhs) =>
          VarDef(ident, originalName, vtpe, mutable, transformExpr(rhs))

        // Control flow constructs

        case Block(stats) =>
          Block(stats.init.map(transformStat) :+ transform(stats.last, isStat))

        case Labeled(label, tpe, body) =>
          Labeled(label, tpe, transform(body, isStat))

        case Assign(lhs, rhs) =>
          Assign(transformExpr(lhs).asInstanceOf[AssignLhs], transformExpr(rhs))

        case Return(expr, label) =>
          Return(transformExpr(expr), label)

        case If(cond, thenp, elsep) =>
          If(transformExpr(cond), transform(thenp, isStat),
              transform(elsep, isStat))(tree.tpe)

        case LinkTimeIf(cond, thenp, elsep) =>
          LinkTimeIf(cond, transform(thenp, isStat),
              transform(elsep, isStat))(tree.tpe)

        case While(cond, body) =>
          While(transformExpr(cond), transformStat(body))

        case ForIn(obj, keyVar, keyVarOriginalName, body) =>
          ForIn(transformExpr(obj), keyVar, keyVarOriginalName,
              transformStat(body))

        case TryCatch(block, errVar, errVarOriginalName, handler) =>
          TryCatch(transform(block, isStat), errVar, errVarOriginalName,
              transform(handler, isStat))(tree.tpe)

        case TryFinally(block, finalizer) =>
          TryFinally(transform(block, isStat), transformStat(finalizer))

        case Throw(expr) =>
          Throw(transformExpr(expr))

        case Match(selector, cases, default) =>
          Match(transformExpr(selector),
              cases map (c => (c._1, transform(c._2, isStat))),
              transform(default, isStat))(tree.tpe)

        // Scala expressions

        case New(className, ctor, args) =>
          New(className, ctor, args map transformExpr)

        case Select(qualifier, field) =>
          Select(transformExpr(qualifier), field)(tree.tpe)

        case Apply(flags, receiver, method, args) =>
          Apply(flags, transformExpr(receiver), method,
              args map transformExpr)(tree.tpe)

        case ApplyStatically(flags, receiver, className, method, args) =>
          ApplyStatically(flags, transformExpr(receiver), className, method,
              args map transformExpr)(tree.tpe)

        case ApplyStatic(flags, className, method, args) =>
          ApplyStatic(flags, className, method, args map transformExpr)(tree.tpe)

        case ApplyDynamicImport(flags, className, method, args) =>
          ApplyDynamicImport(flags, className, method, args.map(transformExpr))

        case UnaryOp(op, lhs) =>
          UnaryOp(op, transformExpr(lhs))

        case BinaryOp(op, lhs, rhs) =>
          BinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        case NewArray(tpe, lengths) =>
          NewArray(tpe, lengths map transformExpr)

        case ArrayValue(tpe, elems) =>
          ArrayValue(tpe, elems map transformExpr)

        case ArrayLength(array) =>
          ArrayLength(transformExpr(array))

        case ArraySelect(array, index) =>
          ArraySelect(transformExpr(array), transformExpr(index))(tree.tpe)

        case RecordValue(tpe, elems) =>
          RecordValue(tpe, elems map transformExpr)

        case RecordSelect(record, field) =>
          RecordSelect(transformExpr(record), field)(tree.tpe)

        case IsInstanceOf(expr, testType) =>
          IsInstanceOf(transformExpr(expr), testType)

        case AsInstanceOf(expr, tpe) =>
          AsInstanceOf(transformExpr(expr), tpe)

        case GetClass(expr) =>
          GetClass(transformExpr(expr))

        case Clone(expr) =>
          Clone(transformExpr(expr))

        case IdentityHashCode(expr) =>
          IdentityHashCode(transformExpr(expr))

        case WrapAsThrowable(expr) =>
          WrapAsThrowable(transformExpr(expr))

        case UnwrapFromThrowable(expr) =>
          UnwrapFromThrowable(transformExpr(expr))

        // JavaScript expressions

        case JSNew(ctor, args) =>
          JSNew(transformExpr(ctor), args.map(transformExprOrJSSpread))

        case JSPrivateSelect(qualifier, field) =>
          JSPrivateSelect(transformExpr(qualifier), field)

        case JSSelect(qualifier, item) =>
          JSSelect(transformExpr(qualifier), transformExpr(item))

        case JSFunctionApply(fun, args) =>
          JSFunctionApply(transformExpr(fun), args.map(transformExprOrJSSpread))

        case JSMethodApply(receiver, method, args) =>
          JSMethodApply(transformExpr(receiver), transformExpr(method),
              args.map(transformExprOrJSSpread))

        case JSSuperSelect(superClass, qualifier, item) =>
          JSSuperSelect(superClass, transformExpr(qualifier),
              transformExpr(item))

        case JSSuperMethodCall(superClass, receiver, method, args) =>
          JSSuperMethodCall(superClass, transformExpr(receiver),
              transformExpr(method), args.map(transformExprOrJSSpread))

        case JSSuperConstructorCall(args) =>
          JSSuperConstructorCall(args.map(transformExprOrJSSpread))

        case JSImportCall(arg) =>
          JSImportCall(transformExpr(arg))

        case JSDelete(qualifier, item) =>
          JSDelete(transformExpr(qualifier), transformExpr(item))

        case JSUnaryOp(op, lhs) =>
          JSUnaryOp(op, transformExpr(lhs))

        case JSBinaryOp(op, lhs, rhs) =>
          JSBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        case JSArrayConstr(items) =>
          JSArrayConstr(items.map(transformExprOrJSSpread))

        case JSObjectConstr(fields) =>
          JSObjectConstr(fields.map { field =>
            (transformExpr(field._1), transformExpr(field._2))
          })

        case JSTypeOfGlobalRef(globalRef) =>
          JSTypeOfGlobalRef(transformExpr(globalRef).asInstanceOf[JSGlobalRef])

        // Atomic expressions

        case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
          Closure(arrow, captureParams, params, restParam, transformExpr(body),
              captureValues.map(transformExpr))

        case CreateJSClass(className, captureValues) =>
          CreateJSClass(className, captureValues.map(transformExpr))

        // Transients
        case Transient(value) =>
          value.transform(this, isStat)

        // Trees that need not be transformed

        case _:Skip | _:Debugger | _:LoadModule | _:StoreModule |
            _:SelectStatic | _:SelectJSNativeMember | _:LoadJSConstructor |
            _:LoadJSModule | _:JSNewTarget | _:JSImportMeta | _:JSLinkingInfo |
            _:Literal | _:VarRef | _:This | _:JSGlobalRef  =>
          tree
      }
    }
  }

  abstract class ClassTransformer extends Transformer {
    def transformClassDef(tree: ClassDef): ClassDef = {
      import tree._
      ClassDef(name, originalName, kind, jsClassCaptures, superClass,
          interfaces, jsSuperClass.map(transformExpr), jsNativeLoadSpec,
          fields.map(transformAnyFieldDef(_)),
          methods.map(transformMethodDef), jsConstructor.map(transformJSConstructorDef),
          jsMethodProps.map(transformJSMethodPropDef), jsNativeMembers,
          topLevelExportDefs.map(transformTopLevelExportDef))(
          tree.optimizerHints)(tree.pos)
    }

    def transformAnyFieldDef(fieldDef: AnyFieldDef): AnyFieldDef =
      fieldDef

    def transformMethodDef(methodDef: MethodDef): MethodDef = {
      val MethodDef(flags, name, originalName, args, resultType, body) = methodDef
      val newBody = body.map(transform(_, isStat = resultType == NoType))
      MethodDef(flags, name, originalName, args, resultType, newBody)(
          methodDef.optimizerHints, Unversioned)(methodDef.pos)
    }

    def transformJSConstructorDef(jsConstructor: JSConstructorDef): JSConstructorDef = {
      val JSConstructorDef(flags, args, restParam, body) = jsConstructor
      JSConstructorDef(flags, args, restParam, transformJSConstructorBody(body))(
          jsConstructor.optimizerHints, Unversioned)(jsConstructor.pos)
    }

    def transformJSMethodPropDef(jsMethodPropDef: JSMethodPropDef): JSMethodPropDef = {
      jsMethodPropDef match {
        case jsMethodDef: JSMethodDef =>
          transformJSMethodDef(jsMethodDef)

        case JSPropertyDef(flags, name, getterBody, setterArgAndBody) =>
          JSPropertyDef(
              flags,
              transformExpr(name),
              getterBody.map(transformStat),
              setterArgAndBody map { case (arg, body) =>
                (arg, transformStat(body))
              })(Unversioned)(jsMethodPropDef.pos)
      }
    }

    def transformJSMethodDef(jsMethodDef: JSMethodDef): JSMethodDef = {
      val JSMethodDef(flags, name, args, restParam, body) = jsMethodDef
      JSMethodDef(flags, transformExpr(name), args, restParam, transformExpr(body))(
          jsMethodDef.optimizerHints, Unversioned)(jsMethodDef.pos)
    }

    def transformJSConstructorBody(body: JSConstructorBody): JSConstructorBody = {
      implicit val pos = body.pos

      val newBeforeSuper = body.beforeSuper.map(transformStat(_))
      val newSuperCall = transformStat(body.superCall).asInstanceOf[JSSuperConstructorCall]
      val newAfterSuper = body.afterSuper match {
        case stats :+ expr => stats.map(transformStat(_)) :+ transformExpr(expr)
        case empty         => empty // cannot use Nil here because the compiler does not know that it is exhaustive
      }

      JSConstructorBody(newBeforeSuper, newSuperCall, newAfterSuper)
    }

    def transformTopLevelExportDef(
        exportDef: TopLevelExportDef): TopLevelExportDef = {

      implicit val pos = exportDef.pos

      exportDef match {
        case _:TopLevelJSClassExportDef | _:TopLevelModuleExportDef |
            _:TopLevelFieldExportDef =>
          exportDef

        case TopLevelMethodExportDef(moduleID, methodDef) =>
          TopLevelMethodExportDef(moduleID, transformJSMethodDef(methodDef))
      }
    }
  }

}
