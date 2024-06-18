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

object Traversers {

  class Traverser {
    def traverseTreeOrJSSpread(tree: TreeOrJSSpread): Unit = tree match {
      case JSSpread(items) => traverse(items)
      case tree: Tree      => traverse(tree)
    }

    def traverse(tree: Tree): Unit = tree match {
      // Definitions

      case VarDef(_, _, _, _, rhs) =>
        traverse(rhs)

      // Control flow constructs

      case Block(stats) =>
        stats foreach traverse

      case Labeled(label, tpe, body) =>
        traverse(body)

      case Assign(lhs, rhs) =>
        traverse(lhs)
        traverse(rhs)

      case Return(expr, label) =>
        traverse(expr)

      case If(cond, thenp, elsep) =>
        traverse(cond)
        traverse(thenp)
        traverse(elsep)

      case LinkTimeIf(_, thenp, elsep) =>
        traverse(thenp)
        traverse(elsep)

      case While(cond, body) =>
        traverse(cond)
        traverse(body)

      case ForIn(obj, _, _, body) =>
        traverse(obj)
        traverse(body)

      case TryCatch(block, _, _, handler) =>
        traverse(block)
        traverse(handler)

      case TryFinally(block, finalizer) =>
        traverse(block)
        traverse(finalizer)

      case Throw(expr) =>
        traverse(expr)

      case Match(selector, cases, default) =>
        traverse(selector)
        cases foreach (c => (c._1 map traverse, traverse(c._2)))
        traverse(default)

      // Scala expressions

      case New(_, _, args) =>
        args foreach traverse

      case Select(qualifier, _) =>
        traverse(qualifier)

      case Apply(_, receiver, _, args) =>
        traverse(receiver)
        args foreach traverse

      case ApplyStatically(_, receiver, _, _, args) =>
        traverse(receiver)
        args foreach traverse

      case ApplyStatic(_, _, _, args) =>
        args foreach traverse

      case ApplyDynamicImport(_, _, _, args) =>
        args.foreach(traverse)

      case UnaryOp(op, lhs) =>
        traverse(lhs)

      case BinaryOp(op, lhs, rhs) =>
        traverse(lhs)
        traverse(rhs)

      case NewArray(tpe, lengths) =>
        lengths foreach traverse

      case ArrayValue(tpe, elems) =>
        elems foreach traverse

      case ArrayLength(array) =>
        traverse(array)

      case ArraySelect(array, index) =>
        traverse(array)
        traverse(index)

      case RecordValue(tpe, elems) =>
        elems foreach traverse

      case RecordSelect(record, field) =>
        traverse(record)

      case IsInstanceOf(expr, _) =>
        traverse(expr)

      case AsInstanceOf(expr, _) =>
        traverse(expr)

      case GetClass(expr) =>
        traverse(expr)

      case Clone(expr) =>
        traverse(expr)

      case IdentityHashCode(expr) =>
        traverse(expr)

      case WrapAsThrowable(expr) =>
        traverse(expr)

      case UnwrapFromThrowable(expr) =>
        traverse(expr)

      // JavaScript expressions

      case JSNew(ctor, args) =>
        traverse(ctor)
        args.foreach(traverseTreeOrJSSpread)

      case JSPrivateSelect(qualifier, _) =>
        traverse(qualifier)

      case JSSelect(qualifier, item) =>
        traverse(qualifier)
        traverse(item)

      case JSFunctionApply(fun, args) =>
        traverse(fun)
        args.foreach(traverseTreeOrJSSpread)

      case JSMethodApply(receiver, method, args) =>
        traverse(receiver)
        traverse(method)
        args.foreach(traverseTreeOrJSSpread)

      case JSSuperSelect(superClass, qualifier, item) =>
        traverse(superClass)
        traverse(qualifier)
        traverse(item)

      case JSSuperMethodCall(superClass, receiver, method, args) =>
        traverse(superClass)
        traverse(receiver)
        traverse(method)
        args.foreach(traverseTreeOrJSSpread)

      case JSSuperConstructorCall(args) =>
        args.foreach(traverseTreeOrJSSpread)

      case JSImportCall(arg) =>
        traverse(arg)

      case JSDelete(qualifier, item) =>
        traverse(qualifier)
        traverse(item)

      case JSUnaryOp(op, lhs) =>
        traverse(lhs)

      case JSBinaryOp(op, lhs, rhs) =>
        traverse(lhs)
        traverse(rhs)

      case JSArrayConstr(items) =>
        items.foreach(traverseTreeOrJSSpread)

      case JSObjectConstr(fields) =>
        for ((key, value) <- fields) {
          traverse(key)
          traverse(value)
        }

      case JSTypeOfGlobalRef(globalRef) =>
        traverse(globalRef)

      // Atomic expressions

      case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
        traverse(body)
        captureValues.foreach(traverse)

      case CreateJSClass(_, captureValues) =>
        captureValues.foreach(traverse)

      // Transients

      case Transient(value) =>
        value.traverse(this)

      // Trees that need not be traversed

      case _:Skip | _:Debugger | _:LoadModule | _:StoreModule |
          _:SelectStatic | _:SelectJSNativeMember | _:LoadJSConstructor |
          _:LoadJSModule | _:JSNewTarget | _:JSImportMeta | _:JSLinkingInfo |
          _:Literal | _:VarRef | _:This | _:JSGlobalRef =>
    }

    def traverseClassDef(tree: ClassDef): Unit = {
      tree.jsSuperClass.foreach(traverse)
      tree.fields.foreach(traverseAnyFieldDef)
      tree.methods.foreach(traverseMethodDef)
      tree.jsConstructor.foreach(traverseJSConstructorDef)
      tree.jsMethodProps.foreach(traverseJSMethodPropDef)
      tree.topLevelExportDefs.foreach(traverseTopLevelExportDef)
    }

    def traverseAnyFieldDef(fieldDef: AnyFieldDef): Unit = ()

    def traverseMethodDef(methodDef: MethodDef): Unit =
      methodDef.body.foreach(traverse)

    def traverseJSConstructorDef(jsConstructor: JSConstructorDef): Unit =
      jsConstructor.body.allStats.foreach(traverse)

    def traverseJSMethodPropDef(jsMethodPropDef: JSMethodPropDef): Unit = {
      jsMethodPropDef match {
        case JSMethodDef(_, name, _, _, body) =>
          traverse(name)
          traverse(body)

        case JSPropertyDef(_, name, getterBody, setterArgAndBody) =>
          traverse(name)
          getterBody.foreach(traverse)
          setterArgAndBody.foreach(argAndBody => traverse(argAndBody._2))
      }
    }

    def traverseTopLevelExportDef(exportDef: TopLevelExportDef): Unit = {
      exportDef match {
        case _:TopLevelJSClassExportDef | _:TopLevelModuleExportDef |
            _:TopLevelFieldExportDef =>

        case TopLevelMethodExportDef(_, methodDef) =>
          traverseJSMethodPropDef(methodDef)
      }
    }
  }

}
