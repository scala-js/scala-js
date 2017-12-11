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

object Transformers {

  abstract class Transformer {
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

        case VarDef(ident, vtpe, mutable, rhs) =>
          VarDef(ident, vtpe, mutable, transformExpr(rhs))

        // Control flow constructs

        case Block(stats) =>
          Block(stats.init.map(transformStat) :+ transform(stats.last, isStat))

        case Labeled(label, tpe, body) =>
          Labeled(label, tpe, transform(body, isStat))

        case Assign(lhs, rhs) =>
          Assign(transformExpr(lhs), transformExpr(rhs))

        case Return(expr, label) =>
          Return(transformExpr(expr), label)

        case If(cond, thenp, elsep) =>
          If(transformExpr(cond), transform(thenp, isStat),
              transform(elsep, isStat))(tree.tpe)

        case While(cond, body) =>
          While(transformExpr(cond), transformStat(body))

        case DoWhile(body, cond) =>
          DoWhile(transformStat(body), transformExpr(cond))

        case ForIn(obj, keyVar, body) =>
          ForIn(transformExpr(obj), keyVar, transformStat(body))

        case TryCatch(block, errVar, handler) =>
          TryCatch(transform(block, isStat), errVar,
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

        case New(cls, ctor, args) =>
          New(cls, ctor, args map transformExpr)

        case StoreModule(cls, value) =>
          StoreModule(cls, transformExpr(value))

        case Select(qualifier, item) =>
          Select(transformExpr(qualifier), item)(tree.tpe)

        case Apply(receiver, method, args) =>
          Apply(transformExpr(receiver), method,
              args map transformExpr)(tree.tpe)

        case ApplyStatically(receiver, cls, method, args) =>
          ApplyStatically(transformExpr(receiver), cls, method,
              args map transformExpr)(tree.tpe)

        case ApplyStatic(cls, method, args) =>
          ApplyStatic(cls, method, args map transformExpr)(tree.tpe)

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

        case IsInstanceOf(expr, cls) =>
          IsInstanceOf(transformExpr(expr), cls)

        case AsInstanceOf(expr, cls) =>
          AsInstanceOf(transformExpr(expr), cls)

        case Unbox(expr, charCode) =>
          Unbox(transformExpr(expr), charCode)

        case GetClass(expr) =>
          GetClass(transformExpr(expr))

        // JavaScript expressions

        case JSNew(ctor, args) =>
          JSNew(transformExpr(ctor), args.map(transformExprOrJSSpread))

        case JSDotSelect(qualifier, item) =>
          JSDotSelect(transformExpr(qualifier), item)

        case JSBracketSelect(qualifier, item) =>
          JSBracketSelect(transformExpr(qualifier), transformExpr(item))

        case JSFunctionApply(fun, args) =>
          JSFunctionApply(transformExpr(fun), args.map(transformExprOrJSSpread))

        case JSDotMethodApply(receiver, method, args) =>
          JSDotMethodApply(transformExpr(receiver), method,
              args.map(transformExprOrJSSpread))

        case JSBracketMethodApply(receiver, method, args) =>
          JSBracketMethodApply(transformExpr(receiver), transformExpr(method),
              args.map(transformExprOrJSSpread))

        case JSSuperBracketSelect(superClass, qualifier, item) =>
          JSSuperBracketSelect(superClass, transformExpr(qualifier),
              transformExpr(item))

        case JSSuperBracketCall(superClass, receiver, method, args) =>
          JSSuperBracketCall(superClass, transformExpr(receiver),
              transformExpr(method), args.map(transformExprOrJSSpread))

        case JSSuperConstructorCall(args) =>
          JSSuperConstructorCall(args.map(transformExprOrJSSpread))

        case JSDelete(prop) =>
          JSDelete(transformExpr(prop))

        case JSUnaryOp(op, lhs) =>
          JSUnaryOp(op, transformExpr(lhs))

        case JSBinaryOp(op, lhs, rhs) =>
          JSBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        case JSArrayConstr(items) =>
          JSArrayConstr(items.map(transformExprOrJSSpread))

        case JSObjectConstr(fields) =>
          JSObjectConstr(fields map {
            case (name, value) =>
              val newName = name match {
                case ComputedName(tree, logicalName) =>
                  ComputedName(transformExpr(tree), logicalName)
                case _ =>
                  name
              }
              (newName, transformExpr(value))
          })

        // Atomic expressions

        case Closure(arrow, captureParams, params, body, captureValues) =>
          Closure(arrow, captureParams, params, transformExpr(body),
              captureValues.map(transformExpr))

        case CreateJSClass(cls, captureValues) =>
          CreateJSClass(cls, captureValues.map(transformExpr))

        // Trees that need not be transformed

        case _:Skip | _:Debugger | _:LoadModule | _:SelectStatic |
            _:LoadJSConstructor | _:LoadJSModule  | _:JSLinkingInfo |
            _:Literal | _:VarRef | _:This | _:JSGlobalRef | _:Transient  =>
          tree
      }
    }
  }

  abstract class ClassTransformer extends Transformer {
    def transformClassDef(tree: ClassDef): ClassDef = {
      import tree._
      ClassDef(name, kind, jsClassCaptures, superClass, interfaces,
          jsSuperClass.map(transformExpr), jsNativeLoadSpec,
          memberDefs.map(transformMemberDef),
          topLevelExportDefs.map(transformTopLevelExportDef))(
          tree.optimizerHints)(tree.pos)
    }

    def transformMemberDef(memberDef: MemberDef): MemberDef = {
      implicit val pos = memberDef.pos

      memberDef match {
        case FieldDef(_, _, _) =>
          memberDef

        case memberDef: MethodDef =>
          val MethodDef(flags, name, args, resultType, body) = memberDef
          MethodDef(flags, name, args, resultType, body.map(transformStat))(
              memberDef.optimizerHints, None)

        case PropertyDef(flags, name, getterBody, setterArgAndBody) =>
          PropertyDef(
              flags,
              name,
              getterBody.map(transformStat),
              setterArgAndBody map { case (arg, body) =>
                (arg, transformStat(body))
              })
      }
    }

    def transformTopLevelExportDef(
        exportDef: TopLevelExportDef): TopLevelExportDef = {

      implicit val pos = exportDef.pos

      exportDef match {
        case _:TopLevelJSClassExportDef | _:TopLevelModuleExportDef |
            _:TopLevelFieldExportDef =>
          exportDef

        case TopLevelMethodExportDef(methodDef) =>
          TopLevelMethodExportDef(
              transformMemberDef(methodDef).asInstanceOf[MethodDef])
      }
    }
  }

}
