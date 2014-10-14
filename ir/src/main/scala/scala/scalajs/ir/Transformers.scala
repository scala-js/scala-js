/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import Trees._

object Transformers {

  class Transformer {
    def transformStat(tree: Tree): Tree = {
      implicit val pos = tree.pos

      tree match {
        // Definitions

        case VarDef(ident, vtpe, mutable, rhs) =>
          VarDef(ident, vtpe, mutable, transformExpr(rhs))

        // Control flow constructs

        case Block(stats) =>
          Block(stats map transformStat)

        case Labeled(label, tpe, body) =>
          Labeled(label, tpe, transformStat(body))

        case Assign(lhs, rhs) =>
          Assign(transformExpr(lhs), transformExpr(rhs))

        case Return(expr, label) =>
          Return(transformExpr(expr), label)

        case If(cond, thenp, elsep) =>
          If(transformExpr(cond), transformStat(thenp), transformStat(elsep))(tree.tpe)

        case While(cond, body, label) =>
          While(transformExpr(cond), transformStat(body), label)

        case DoWhile(body, cond, label) =>
          DoWhile(transformStat(body), transformExpr(cond), label)

        case Try(block, errVar, handler, finalizer) =>
          Try(transformStat(block), errVar, transformStat(handler), transformStat(finalizer))(tree.tpe)

        case Throw(expr) =>
          Throw(transformExpr(expr))

        case Match(selector, cases, default) =>
          Match(transformExpr(selector),
              cases map (c => (c._1, transformStat(c._2))),
              transformStat(default))(tree.tpe)

        // Scala expressions

        case New(cls, ctor, args) =>
          New(cls, ctor, args map transformExpr)

        case StoreModule(cls, value) =>
          StoreModule(cls, transformExpr(value))

        case Select(qualifier, item, mutable) =>
          Select(transformExpr(qualifier), item, mutable)(tree.tpe)

        case Apply(receiver, method, args) =>
          Apply(transformExpr(receiver), method,
              args map transformExpr)(tree.tpe)

        case StaticApply(receiver, cls, method, args) =>
          StaticApply(transformExpr(receiver), cls, method,
              args map transformExpr)(tree.tpe)

        case TraitImplApply(impl, method, args) =>
          TraitImplApply(impl, method, args map transformExpr)(tree.tpe)

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
            case (name, value) => (name, transformExpr(value))
          })

        // Atomic expressions

        case Closure(thisType, args, resultType, body, captures) =>
          Closure(thisType, args, resultType,
              if (resultType == Types.NoType) transformStat(body)
              else transformExpr(body),
              captures map transformExpr)

        // Type-related

        case Cast(expr, tpe) =>
          Cast(transformExpr(expr), tpe)

        // Classes

        case ClassDef(name, kind, parent, ancestors, defs) =>
          ClassDef(name, kind, parent, ancestors, defs map transformDef)

        // Trees that need not be transformed

        case _:Skip | _:Continue | _:LoadModule | _:ClassOf |
            _:JSGlobal | _:Literal | _:VarRef | _:This | EmptyTree =>
          tree

        case _ =>
          sys.error(s"Invalid tree in transformStat() of class ${tree.getClass}")
      }
    }

    def transformExpr(tree: Tree): Tree = {
      implicit val pos = tree.pos

      tree match {
        // Control flow constructs

        case Block(stats :+ expr) =>
          Block((stats map transformStat) :+ transformExpr(expr))

        case Labeled(label, tpe, body) =>
          Labeled(label, tpe, transformExpr(body))

        case Return(expr, label) =>
          Return(transformExpr(expr), label)

        case If(cond, thenp, elsep) =>
          If(transformExpr(cond), transformExpr(thenp), transformExpr(elsep))(tree.tpe)

        case While(cond, body, label) =>
          While(transformExpr(cond), transformStat(body), label)

        case Try(block, errVar, handler, finalizer) =>
          Try(transformExpr(block), errVar, transformExpr(handler), transformStat(finalizer))(tree.tpe)

        case Throw(expr) =>
          Throw(transformExpr(expr))

        case Match(selector, cases, default) =>
          Match(transformExpr(selector),
              cases map (c => (c._1, transformExpr(c._2))),
              transformExpr(default))(tree.tpe)

        // Scala expressions

        case New(cls, constr, args) =>
          New(cls, constr, args map transformExpr)

        case Select(qualifier, item, mutable) =>
          Select(transformExpr(qualifier), item, mutable)(tree.tpe)

        case Apply(receiver, method, args) =>
          Apply(transformExpr(receiver), method,
              args map transformExpr)(tree.tpe)

        case StaticApply(receiver, cls, method, args) =>
          StaticApply(transformExpr(receiver), cls, method,
              args map transformExpr)(tree.tpe)

        case TraitImplApply(impl, method, args) =>
          TraitImplApply(impl, method, args map transformExpr)(tree.tpe)

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

        case CallHelper(helper, args) =>
          CallHelper(helper, args map transformExpr)(tree.tpe)

        // JavaScript expressions

        case JSNew(constr, args) =>
          JSNew(transformExpr(constr), args map transformExpr)

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

        case JSUnaryOp(op, lhs) =>
          JSUnaryOp(op, transformExpr(lhs))

        case JSBinaryOp(op, lhs, rhs) =>
          JSBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        case JSArrayConstr(items) =>
          JSArrayConstr(items map transformExpr)

        case JSObjectConstr(fields) =>
          JSObjectConstr(fields map {
            case (name, value) => (name, transformExpr(value))
          })

        // Atomic expressions

        case Closure(thisType, args, resultType, body, captures) =>
          Closure(thisType, args, resultType,
              if (resultType == Types.NoType) transformStat(body)
              else transformExpr(body),
              captures map transformExpr)

        // Type-related

        case Cast(expr, tpe) =>
          Cast(transformExpr(expr), tpe)

        // Trees that need not be transformed

        case _:Continue | _:LoadModule | _:ClassOf | _:JSGlobal |
            _:Literal | _:VarRef | _:This | EmptyTree =>
          tree

        case _ =>
          sys.error(s"Invalid tree in transformExpr() of class ${tree.getClass}")
      }
    }

    def transformDef(tree: Tree): Tree = {
      implicit val pos = tree.pos

      tree match {
        case VarDef(name, vtpe, mutable, rhs) =>
          VarDef(name, vtpe, mutable, transformExpr(rhs))

        case MethodDef(name, args, resultType, body) =>
          MethodDef(name, args, resultType, transformStat(body))(None)

        case PropertyDef(name, getterBody, setterArg, setterBody) =>
          PropertyDef(
              name,
              transformStat(getterBody),
              setterArg,
              transformStat(setterBody))

        case ConstructorExportDef(fullName, args, body) =>
          ConstructorExportDef(fullName, args, transformStat(body))

        case ModuleExportDef(_) =>
          tree

        case _ =>
          sys.error(s"Invalid tree in transformDef() of class ${tree.getClass}")
      }
    }
  }

}
