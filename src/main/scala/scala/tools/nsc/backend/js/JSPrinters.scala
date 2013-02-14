/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

import java.io.PrintWriter

trait JSPrinters { self: scalajs.JSGlobal =>
  /** Basically copied from scala.reflect.internal.Printers */
  trait JSIndentationManager {
    val out: PrintWriter

    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    protected def doPrintPositions = settings.Xprintpos.value

    def indent() = indentMargin += indentStep
    def undent() = indentMargin -= indentStep

    def printPosition(tree: js.Tree) = if (doPrintPositions) print(tree.pos.show)

    def println() {
      out.println()
      while (indentMargin > indentString.length())
        indentString += indentString
      if (indentMargin > 0)
        out.write(indentString, 0, indentMargin)
    }

    def printSeq[a](ls: List[a])(printelem: a => Unit)(printsep: => Unit) {
      ls match {
        case List() =>
        case List(x) => printelem(x)
        case x :: rest => printelem(x); printsep; printSeq(rest)(printelem)(printsep)
      }
    }

    def printColumn(ts: List[js.Tree], start: String, sep: String, end: String) {
      print(start); indent; println()
      printSeq(ts){print(_)}{print(sep); println()}; undent; println(); print(end)
    }

    def printRow(ts: List[js.Tree], start: String, sep: String, end: String) {
      print(start); printSeq(ts){print(_)}{print(sep)}; print(end)
    }

    def printRow(ts: List[js.Tree], sep: String) { printRow(ts, "", sep, "") }

    def print(args: Any*): Unit
  }

  class JSTreePrinter(val out: PrintWriter) extends JSIndentationManager {
    def printBlock(tree: js.Tree) {
      tree match {
        case js.Block(_, _) =>
          printTree(tree)
        case _ =>
          printColumn(List(tree), "{", ";", "}")
      }
    }

    def printTree(tree: js.Tree) {
      tree match {
        case js.EmptyTree =>
          print("<empty>")

        // Identifiers

        case js.Ident(name) =>
          print(name)

        // Definitions

        case js.VarDef(ident, js.EmptyTree) =>
          print("var ", ident)

        case js.VarDef(ident, rhs) =>
          print("var ", ident, " = ", rhs)

        case js.FunDef(name, args, body) =>
          print("function ", name)
          printRow(args, "(", ", ", ") ")
          printBlock(body)

        // Statement-only language constructs

        case js.Skip() =>
          print("<skip>")

        case js.Block(stats, expr) =>
          printColumn(stats :+ expr, "{", ";", "}")

        case js.Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case js.Return(expr) =>
          print("return ", expr)

        case js.If(cond, thenp, elsep) =>
          print("if (", cond, ") ")
          printBlock(thenp)
          print(" else ")
          printBlock(elsep)

        case js.While(cond, body) =>
          print("while (", cond, ") ")
          printBlock(body)

        case js.Try(block, errVar, handler, finalizer) =>
          print("try ")
          printBlock(block)
          if (handler != EmptyTree) {
            print(" catch (", errVar, ") ")
            printBlock(handler)
          }
          if (finalizer != EmptyTree) {
            print(" finally ")
            printBlock(finalizer)
          }

        case js.Throw(expr) =>
          print("throw ", expr)

        case js.Break() =>
          print("break")

        case js.Continue() =>
          print("continue")

        // Expressions

        case js.DotSelect(qualifier, item) =>
          print(qualifier, ".", item)

        case js.BracketSelect(qualifier, item) =>
          print(qualifier, "[", item, "]")

        case js.Apply(fun, args) =>
          print(fun)
          printRow(args, "(", ", ", ")")

        case js.Function(args, body) =>
          printRow(args, "function(", ", ", ") ")
          printBlock(body)

        case js.UnaryOp(op, lhs) =>
          print(op, lhs)

        case js.BinaryOp(op, lhs, rhs) =>
          print(lhs, " ", op, " ", rhs)

        case js.New(fun, args) =>
          print("new ", fun)
          printRow(args, "(", ", ", ")")

        case js.This() =>
          print("this")

        // Literals

        case js.Undefined() =>
          print("undefined")

        case js.Null() =>
          print("null")

        case js.IntLiteral(value) =>
          print(value)

        case js.DoubleLiteral(value) =>
          print(value)

        case js.StringLiteral(value) =>
          // TODO quote
          print("\"", value, "\"")

        // Compounds

        case js.ArrayConstr(items) =>
          printRow(items, "[", ", ", "]")

        case js.ObjectConstr(fields) =>
          print("{"); indent; println()
          printSeq(fields) {
            case (name, value) => print(name, ": ", value)
          } {
            print(","); println()
          }
          undent; println(); print("}")

        // Classes - from ECMAScript 6, can be desugared into other concepts

        case js.ClassDef(name, parent, defs) =>
          print("class ", name)
          if (parent != js.EmptyTree) {
            print(" extends ", parent)
          }
          print(" ")
          printColumn(defs, "{", "", "}")
          println()

        case js.MethodDef(name, args, body) =>
          print(name)
          printRow(args, "(", ", ", ") ")
          printBlock(body)

        case js.GetterDef(name, body) =>
          // TODO
          print("<getter>")

        case js.SetterDef(name, arg, body) =>
          // TODO
          print("<setter>")

        case js.Super() =>
          print("super")
      }
    }

    def print(args: Any*): Unit = args foreach {
      case tree: js.Tree =>
        printPosition(tree)
        printTree(tree)
      case arg =>
        out.print(if (arg == null) "null" else arg.toString)
    }
  }
}
