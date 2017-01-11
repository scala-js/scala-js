/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import Trees._

object Traversers {

  class Traverser {
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

      case While(cond, body, label) =>
        traverse(cond)
        traverse(body)

      case DoWhile(body, cond, label) =>
        traverse(body)
        traverse(cond)

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

      case Apply(receiver, method, args) =>
        traverse(receiver)
        args foreach traverse

      case ApplyStatically(receiver, cls, method, args) =>
        traverse(receiver)
        args foreach traverse

      case ApplyStatic(cls, method, args) =>
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

      case CallHelper(helper, args) =>
        args foreach traverse

      // JavaScript expressions

      case JSNew(ctor, args) =>
        traverse(ctor)
        args foreach traverse

      case JSDotSelect(qualifier, item) =>
        traverse(qualifier)

      case JSBracketSelect(qualifier, item) =>
        traverse(qualifier)
        traverse(item)

      case JSFunctionApply(fun, args) =>
        traverse(fun)
        args foreach traverse

      case JSDotMethodApply(receiver, method, args) =>
        traverse(receiver)
        args foreach traverse

      case JSBracketMethodApply(receiver, method, args) =>
        traverse(receiver)
        traverse(method)
        args foreach traverse

      case JSSuperBracketSelect(cls, qualifier, item) =>
        traverse(qualifier)
        traverse(item)

      case JSSuperBracketCall(cls, receiver, method, args) =>
        traverse(receiver)
        traverse(method)
        args foreach traverse

      case JSSuperConstructorCall(args) =>
        args foreach traverse

      case JSSpread(items) =>
        traverse(items)

      case JSDelete(prop) =>
        traverse(prop)

      case JSUnaryOp(op, lhs) =>
        traverse(lhs)

      case JSBinaryOp(op, lhs, rhs) =>
        traverse(lhs)
        traverse(rhs)

      case JSArrayConstr(items) =>
        items foreach traverse

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

      case Closure(captureParams, params, body, captureValues) =>
        traverse(body)
        captureValues.foreach(traverse)

      // Classes

      case ClassDef(name, kind, superClass, parents, jsName, defs) =>
        defs foreach traverse

      case MethodDef(static, name, args, resultType, body) =>
        body.foreach(traverse)

      case PropertyDef(static, name, getterBody, setterArgAndBody) =>
        getterBody.foreach(traverse)
        setterArgAndBody foreach { case (_, body) =>
          traverse(body)
        }

      case ConstructorExportDef(fullName, args, body) =>
        traverse(body)

      case TopLevelMethodExportDef(methodDef) =>
        traverse(methodDef)

      // Trees that need not be traversed

      case _:Skip | _:Continue | _:Debugger | _:LoadModule | _:SelectStatic |
          _:LoadJSConstructor | _:LoadJSModule | _:JSLinkingInfo | _:Literal |
          _:UndefinedParam | _:VarRef | _:This | _:FieldDef |
          _:JSClassExportDef | _:ModuleExportDef | _:TopLevelModuleExportDef |
          _:TopLevelFieldExportDef =>

      case _ =>
        sys.error(s"Invalid tree in traverse() of class ${tree.getClass}")
    }
  }

}
