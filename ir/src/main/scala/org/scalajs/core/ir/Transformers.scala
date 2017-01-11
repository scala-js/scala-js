/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import Trees._

object Transformers {

  abstract class Transformer {
    final def transformStat(tree: Tree): Tree =
      transform(tree, isStat = true)

    final def transformExpr(tree: Tree): Tree =
      transform(tree, isStat = false)

    def transform(tree: Tree, isStat: Boolean): Tree = {
      implicit val pos = tree.pos

      tree match {
        // Definitions

        case VarDef(ident, vtpe, mutable, rhs) =>
          VarDef(ident, vtpe, mutable, transformExpr(rhs))

        // Control flow constructs

        case Block(stats :+ expr) =>
          Block(stats.map(transformStat) :+ transform(expr, isStat))

        case Labeled(label, tpe, body) =>
          Labeled(label, tpe, transform(body, isStat))

        case Assign(lhs, rhs) =>
          Assign(transformExpr(lhs), transformExpr(rhs))

        case Return(expr, label) =>
          Return(transformExpr(expr), label)

        case If(cond, thenp, elsep) =>
          If(transformExpr(cond), transform(thenp, isStat),
              transform(elsep, isStat))(tree.tpe)

        case While(cond, body, label) =>
          While(transformExpr(cond), transformStat(body), label)

        case DoWhile(body, cond, label) =>
          DoWhile(transformStat(body), transformExpr(cond), label)

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

        case CallHelper(helper, args) =>
          CallHelper(helper, args map transformExpr)(tree.tpe)

        // JavaScript expressions

        case JSNew(ctor, args) =>
          JSNew(transformExpr(ctor), args map transformExpr)

        case JSDotSelect(qualifier, item) =>
          JSDotSelect(transformExpr(qualifier), item)

        case JSBracketSelect(qualifier, item) =>
          JSBracketSelect(transformExpr(qualifier), transformExpr(item))

        case JSFunctionApply(fun, args) =>
          JSFunctionApply(transformExpr(fun), args map transformExpr)

        case JSDotMethodApply(receiver, method, args) =>
          JSDotMethodApply(transformExpr(receiver), method,
              args map transformExpr)

        case JSBracketMethodApply(receiver, method, args) =>
          JSBracketMethodApply(transformExpr(receiver), transformExpr(method),
              args map transformExpr)

        case JSSuperBracketSelect(cls, qualifier, item) =>
          JSSuperBracketSelect(cls, transformExpr(qualifier),
              transformExpr(item))

        case JSSuperBracketCall(cls, receiver, method, args) =>
          JSSuperBracketCall(cls, transformExpr(receiver),
              transformExpr(method), args map transformExpr)

        case JSSuperConstructorCall(args) =>
          JSSuperConstructorCall(args map transformExpr)

        case JSSpread(items) =>
          JSSpread(transformExpr(items))

        case JSDelete(prop) =>
          JSDelete(transformExpr(prop))

        case JSUnaryOp(op, lhs) =>
          JSUnaryOp(op, transformExpr(lhs))

        case JSBinaryOp(op, lhs, rhs) =>
          JSBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        case JSArrayConstr(items) =>
          JSArrayConstr(items map transformExpr)

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

        case Closure(captureParams, params, body, captureValues) =>
          Closure(captureParams, params, transformExpr(body),
              captureValues.map(transformExpr))

        // Trees that need not be transformed

        case _:Skip | _:Continue | _:Debugger | _:LoadModule | _:SelectStatic |
            _:LoadJSConstructor | _:LoadJSModule  | _:JSLinkingInfo |
            _:Literal | _:UndefinedParam | _:VarRef | _:This  =>
          tree

        case _ =>
          sys.error(s"Invalid tree in transform() of class ${tree.getClass}")
      }
    }
  }

  abstract class ClassTransformer extends Transformer {
    def transformClassDef(tree: ClassDef): ClassDef = {
      val ClassDef(name, kind, superClass, parents, jsName, defs) = tree
      ClassDef(name, kind, superClass, parents, jsName, defs.map(transformDef))(
          tree.optimizerHints)(tree.pos)
    }

    def transformDef(tree: Tree): Tree = {
      implicit val pos = tree.pos

      tree match {
        case FieldDef(_, _, _, _) =>
          tree

        case tree: MethodDef =>
          val MethodDef(static, name, args, resultType, body) = tree
          MethodDef(static, name, args, resultType, body.map(transformStat))(
              tree.optimizerHints, None)

        case PropertyDef(static, name, getterBody, setterArgAndBody) =>
          PropertyDef(
              static,
              name,
              getterBody.map(transformStat),
              setterArgAndBody map { case (arg, body) =>
                (arg, transformStat(body))
              })

        case ConstructorExportDef(fullName, args, body) =>
          ConstructorExportDef(fullName, args, transformStat(body))

        case _:JSClassExportDef | _:ModuleExportDef |
            _:TopLevelModuleExportDef | _:TopLevelFieldExportDef =>
          tree

        case TopLevelMethodExportDef(methodDef) =>
          TopLevelMethodExportDef(
              transformDef(methodDef).asInstanceOf[MethodDef])

        case _ =>
          sys.error(s"Invalid tree in transformDef() of class ${tree.getClass}")
      }
    }
  }

}
