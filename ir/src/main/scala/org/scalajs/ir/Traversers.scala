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

      case VarDef(ident, vtpe, mutable, rhs) =>
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

      case While(cond, body) =>
        traverse(cond)
        traverse(body)

      case DoWhile(body, cond) =>
        traverse(body)
        traverse(cond)

      case ForIn(obj, keyVar, body) =>
        traverse(obj)
        traverse(body)

      case TryCatch(block, errVar, handler) =>
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

      case New(cls, ctor, args) =>
        args foreach traverse

      case StoreModule(cls, value) =>
        traverse(value)

      case Select(qualifier, item) =>
        traverse(qualifier)

      case Apply(_, receiver, method, args) =>
        traverse(receiver)
        args foreach traverse

      case ApplyStatically(_, receiver, cls, method, args) =>
        traverse(receiver)
        args foreach traverse

      case ApplyStatic(_, cls, method, args) =>
        args foreach traverse

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

      case IsInstanceOf(expr, cls) =>
        traverse(expr)

      case AsInstanceOf(expr, cls) =>
        traverse(expr)

      case Unbox(expr, charCode) =>
        traverse(expr)

      case GetClass(expr) =>
        traverse(expr)

      // JavaScript expressions

      case JSNew(ctor, args) =>
        traverse(ctor)
        args.foreach(traverseTreeOrJSSpread)

      case JSDotSelect(qualifier, item) =>
        traverse(qualifier)

      case JSBracketSelect(qualifier, item) =>
        traverse(qualifier)
        traverse(item)

      case JSFunctionApply(fun, args) =>
        traverse(fun)
        args.foreach(traverseTreeOrJSSpread)

      case JSDotMethodApply(receiver, method, args) =>
        traverse(receiver)
        args.foreach(traverseTreeOrJSSpread)

      case JSBracketMethodApply(receiver, method, args) =>
        traverse(receiver)
        traverse(method)
        args.foreach(traverseTreeOrJSSpread)

      case JSSuperBracketSelect(superClass, qualifier, item) =>
        traverse(superClass)
        traverse(qualifier)
        traverse(item)

      case JSSuperBracketCall(superClass, receiver, method, args) =>
        traverse(superClass)
        traverse(receiver)
        traverse(method)
        args.foreach(traverseTreeOrJSSpread)

      case JSSuperConstructorCall(args) =>
        args.foreach(traverseTreeOrJSSpread)

      case JSImportCall(arg) =>
        traverse(arg)

      case JSDelete(prop) =>
        traverse(prop)

      case JSUnaryOp(op, lhs) =>
        traverse(lhs)

      case JSBinaryOp(op, lhs, rhs) =>
        traverse(lhs)
        traverse(rhs)

      case JSArrayConstr(items) =>
        items.foreach(traverseTreeOrJSSpread)

      case JSObjectConstr(fields) =>
        for ((key, value) <- fields) {
          key match {
            case ComputedName(tree, _) =>
              traverse(tree)
            case _ =>
          }
          traverse(value)
        }

      // Atomic expressions

      case Closure(arrow, captureParams, params, body, captureValues) =>
        traverse(body)
        captureValues.foreach(traverse)

      case CreateJSClass(_, captureValues) =>
        captureValues.foreach(traverse)

      // Trees that need not be traversed

      case _:Skip | _:Debugger | _:LoadModule | _:SelectStatic |
          _:LoadJSConstructor | _:LoadJSModule | _:JSLinkingInfo | _:Literal |
          _:VarRef | _:This | _:JSGlobalRef | _:Transient =>
    }

    def traverseClassDef(tree: ClassDef): Unit = {
      tree.jsSuperClass.foreach(traverse)
      tree.memberDefs.foreach(traverseMemberDef)
      tree.topLevelExportDefs.foreach(traverseTopLevelExportDef)
    }

    def traverseMemberDef(memberDef: MemberDef): Unit = {
      memberDef match {
        case FieldDef(_, _, _) =>

        case MethodDef(_, _, _, _, body) =>
          body.foreach(traverse)

        case PropertyDef(_, _, getterBody, setterArgAndBody) =>
          getterBody.foreach(traverse)
          setterArgAndBody.foreach(argAndBody => traverse(argAndBody._2))
      }
    }

    def traverseTopLevelExportDef(exportDef: TopLevelExportDef): Unit = {
      exportDef match {
        case _:TopLevelJSClassExportDef | _:TopLevelModuleExportDef |
            _:TopLevelFieldExportDef =>

        case TopLevelMethodExportDef(methodDef) =>
          traverseMemberDef(methodDef)
      }
    }
  }

}
