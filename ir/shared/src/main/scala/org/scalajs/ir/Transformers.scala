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
    final def transformTreeOrJSSpread(tree: TreeOrJSSpread): TreeOrJSSpread = {
      implicit val pos = tree.pos

      tree match {
        case JSSpread(items) => JSSpread(transform(items))
        case tree: Tree      => transform(tree)
      }
    }

    final def transformTrees(trees: List[Tree]): List[Tree] =
      trees.map(transform(_))

    final def transformTreeOpt(treeOpt: Option[Tree]): Option[Tree] =
      treeOpt.map(transform(_))

    def transform(tree: Tree): Tree = {
      implicit val pos = tree.pos

      tree match {
        // Definitions

        case VarDef(ident, originalName, vtpe, mutable, rhs) =>
          VarDef(ident, originalName, vtpe, mutable, transform(rhs))

        // Control flow constructs

        case Block(stats) =>
          Block(transformTrees(stats))

        case Labeled(label, tpe, body) =>
          Labeled(label, tpe, transform(body))

        case Assign(lhs, rhs) =>
          Assign(transform(lhs).asInstanceOf[AssignLhs], transform(rhs))

        case Return(expr, label) =>
          Return(transform(expr), label)

        case If(cond, thenp, elsep) =>
          If(transform(cond), transform(thenp), transform(elsep))(tree.tpe)

        case LinkTimeIf(cond, thenp, elsep) =>
          LinkTimeIf(transform(cond), transform(thenp), transform(elsep))(tree.tpe)

        case While(cond, body) =>
          While(transform(cond), transform(body))

        case ForIn(obj, keyVar, keyVarOriginalName, body) =>
          ForIn(transform(obj), keyVar, keyVarOriginalName, transform(body))

        case TryCatch(block, errVar, errVarOriginalName, handler) =>
          TryCatch(transform(block), errVar, errVarOriginalName,
              transform(handler))(tree.tpe)

        case TryFinally(block, finalizer) =>
          TryFinally(transform(block), transform(finalizer))

        case Match(selector, cases, default) =>
          Match(transform(selector), cases.map(c => (c._1, transform(c._2))),
              transform(default))(tree.tpe)

        case JSAwait(arg) =>
          JSAwait(transform(arg))

        // Scala expressions

        case New(className, ctor, args) =>
          New(className, ctor, transformTrees(args))

        case Select(qualifier, field) =>
          Select(transform(qualifier), field)(tree.tpe)

        case Apply(flags, receiver, method, args) =>
          Apply(flags, transform(receiver), method,
              transformTrees(args))(tree.tpe)

        case ApplyStatically(flags, receiver, className, method, args) =>
          ApplyStatically(flags, transform(receiver), className, method,
              transformTrees(args))(tree.tpe)

        case ApplyStatic(flags, className, method, args) =>
          ApplyStatic(flags, className, method, transformTrees(args))(tree.tpe)

        case ApplyDynamicImport(flags, className, method, args) =>
          ApplyDynamicImport(flags, className, method, transformTrees(args))

        case ApplyTypedClosure(flags, fun, args) =>
          ApplyTypedClosure(flags, transform(fun), transformTrees(args))

        case NewLambda(descriptor, fun) =>
          NewLambda(descriptor, transform(fun))(tree.tpe)

        case UnaryOp(op, lhs) =>
          UnaryOp(op, transform(lhs))

        case BinaryOp(op, lhs, rhs) =>
          BinaryOp(op, transform(lhs), transform(rhs))

        case NewArray(tpe, length) =>
          NewArray(tpe, transform(length))

        case ArrayValue(tpe, elems) =>
          ArrayValue(tpe, transformTrees(elems))

        case ArraySelect(array, index) =>
          ArraySelect(transform(array), transform(index))(tree.tpe)

        case RecordValue(tpe, elems) =>
          RecordValue(tpe, transformTrees(elems))

        case RecordSelect(record, field) =>
          RecordSelect(transform(record), field)(tree.tpe)

        case IsInstanceOf(expr, testType) =>
          IsInstanceOf(transform(expr), testType)

        case AsInstanceOf(expr, tpe) =>
          AsInstanceOf(transform(expr), tpe)

        // JavaScript expressions

        case JSNew(ctor, args) =>
          JSNew(transform(ctor), args.map(transformTreeOrJSSpread))

        case JSPrivateSelect(qualifier, field) =>
          JSPrivateSelect(transform(qualifier), field)

        case JSSelect(qualifier, item) =>
          JSSelect(transform(qualifier), transform(item))

        case JSFunctionApply(fun, args) =>
          JSFunctionApply(transform(fun), args.map(transformTreeOrJSSpread))

        case JSMethodApply(receiver, method, args) =>
          JSMethodApply(transform(receiver), transform(method),
              args.map(transformTreeOrJSSpread))

        case JSSuperSelect(superClass, qualifier, item) =>
          JSSuperSelect(superClass, transform(qualifier), transform(item))

        case JSSuperMethodCall(superClass, receiver, method, args) =>
          JSSuperMethodCall(superClass, transform(receiver),
              transform(method), args.map(transformTreeOrJSSpread))

        case JSSuperConstructorCall(args) =>
          JSSuperConstructorCall(args.map(transformTreeOrJSSpread))

        case JSImportCall(arg) =>
          JSImportCall(transform(arg))

        case JSDelete(qualifier, item) =>
          JSDelete(transform(qualifier), transform(item))

        case JSUnaryOp(op, lhs) =>
          JSUnaryOp(op, transform(lhs))

        case JSBinaryOp(op, lhs, rhs) =>
          JSBinaryOp(op, transform(lhs), transform(rhs))

        case JSArrayConstr(items) =>
          JSArrayConstr(items.map(transformTreeOrJSSpread))

        case JSObjectConstr(fields) =>
          JSObjectConstr(fields.map { field =>
            (transform(field._1), transform(field._2))
          })

        case JSTypeOfGlobalRef(globalRef) =>
          JSTypeOfGlobalRef(transform(globalRef).asInstanceOf[JSGlobalRef])

        // Atomic expressions

        case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
          Closure(flags, captureParams, params, restParam, resultType,
              transform(body), transformTrees(captureValues))

        case CreateJSClass(className, captureValues) =>
          CreateJSClass(className, transformTrees(captureValues))

        // Transients
        case Transient(value) =>
          value.transform(this)

        // Trees that need not be transformed

        case _:Skip | _:Debugger | _:LoadModule | _:StoreModule |
            _:SelectStatic | _:SelectJSNativeMember | _:LoadJSConstructor |
            _:LoadJSModule | _:JSNewTarget | _:JSImportMeta |
            _:Literal | _:VarRef | _:JSGlobalRef | _:LinkTimeProperty =>
          tree
      }
    }
  }

  abstract class ClassTransformer extends Transformer {
    def transformClassDef(tree: ClassDef): ClassDef = {
      import tree._
      ClassDef(name, originalName, kind, jsClassCaptures, superClass,
          interfaces, transformTreeOpt(jsSuperClass), jsNativeLoadSpec,
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
      val newBody = transformTreeOpt(body)
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

        case jsPropertyDef: JSPropertyDef =>
          transformJSPropertyDef(jsPropertyDef)
      }
    }

    def transformJSMethodDef(jsMethodDef: JSMethodDef): JSMethodDef = {
      val JSMethodDef(flags, name, args, restParam, body) = jsMethodDef
      JSMethodDef(flags, transform(name), args, restParam, transform(body))(
          jsMethodDef.optimizerHints, Unversioned)(jsMethodDef.pos)
    }

    def transformJSPropertyDef(jsPropertyDef: JSPropertyDef): JSPropertyDef = {
      val JSPropertyDef(flags, name, getterBody, setterArgAndBody) = jsPropertyDef
      JSPropertyDef(
        flags,
        transform(name),
        transformTreeOpt(getterBody),
        setterArgAndBody.map { case (arg, body) =>
          (arg, transform(body))
        }
      )(Unversioned)(jsPropertyDef.pos)
    }

    def transformJSConstructorBody(body: JSConstructorBody): JSConstructorBody = {
      implicit val pos = body.pos

      val newBeforeSuper = transformTrees(body.beforeSuper)
      val newSuperCall = transform(body.superCall).asInstanceOf[JSSuperConstructorCall]
      val newAfterSuper = transformTrees(body.afterSuper)

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

  /** Transformer that only transforms in the local scope.
   *
   *  In practice, this means stopping at `Closure` boundaries: their
   *  `captureValues` are transformed, but not their other members.
   */
  abstract class LocalScopeTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
        Closure(flags, captureParams, params, restParam, resultType, body,
            transformTrees(captureValues))(tree.pos)
      case _ =>
        super.transform(tree)
    }
  }

}
