/* JSM - JavaScript manipulation library
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.jsm
package ast

import java.io.PrintWriter

import Trees._

object Printers {
  /** Basically copied from scala.reflect.internal.Printers */
  trait IndentationManager {
    val out: PrintWriter

    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    protected def doPrintPositions = false // settings.Xprintpos.value

    def indent() = indentMargin += indentStep
    def undent() = indentMargin -= indentStep

    def printPosition(tree: Tree) = if (doPrintPositions) print(tree.pos)

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

    def printColumn(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); indent; println()
      printSeq(ts){print(_)}{print(sep); println()}; undent; println(); print(end)
    }

    def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); printSeq(ts){print(_)}{print(sep)}; print(end)
    }

    def printRow(ts: List[Tree], sep: String) { printRow(ts, "", sep, "") }

    def print(args: Any*): Unit
  }

  class TreePrinter(val out: PrintWriter) extends IndentationManager {
    def printBlock(tree: Tree) {
      tree match {
        case Block(_, _) =>
          printTree(tree)
        case _ =>
          printColumn(List(tree), "{", ";", "}")
      }
    }

    def printTree(tree: Tree) {
      tree match {
        case EmptyTree =>
          print("<empty>")

        // Identifiers

        case Ident(name) =>
          print(name)

        // Definitions

        case VarDef(ident, EmptyTree) =>
          print("var ", ident)

        case VarDef(ident, rhs) =>
          print("var ", ident, " = ", rhs)

        case FunDef(name, args, body) =>
          print("function ", name)
          printRow(args, "(", ", ", ") ")
          printBlock(body)

        // Statement-only language constructs

        case Skip() =>
          print("<skip>")

        case Block(stats, expr) =>
          printColumn(stats :+ expr, "{", ";", "}")

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case Return(expr) =>
          print("return ", expr)

        case If(cond, thenp, elsep) =>
          print("if (", cond, ") ")
          printBlock(thenp)
          print(" else ")
          printBlock(elsep)

        case While(cond, body) =>
          print("while (", cond, ") ")
          printBlock(body)

        case Try(block, errVar, handler, finalizer) =>
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

        case Throw(expr) =>
          print("throw ", expr)

        case Break() =>
          print("break")

        case Continue() =>
          print("continue")

        // Expressions

        case DotSelect(qualifier, item) =>
          print(qualifier, ".", item)

        case BracketSelect(qualifier, item) =>
          print(qualifier, "[", item, "]")

        case Apply(fun, args) =>
          print(fun)
          printRow(args, "(", ", ", ")")

        case Function(args, body) =>
          printRow(args, "function(", ", ", ") ")
          printBlock(body)

        case UnaryOp(op, lhs) =>
          print(op, lhs)

        case BinaryOp(op, lhs, rhs) =>
          print(lhs, " ", op, " ", rhs)

        case New(fun, args) =>
          print("new ", fun)
          printRow(args, "(", ", ", ")")

        case This() =>
          print("this")

        // Literals

        case Undefined() =>
          print("undefined")

        case Null() =>
          print("null")

        case IntLiteral(value) =>
          print(value)

        case DoubleLiteral(value) =>
          print(value)

        case StringLiteral(value) =>
          // TODO quote
          print("\"", value, "\"")

        // Compounds

        case ArrayConstr(items) =>
          printRow(items, "[", ", ", "]")

        case ObjectConstr(fields) =>
          print("{"); indent; println()
          printSeq(fields) {
            case (name, value) => print(name, ": ", value)
          } {
            print(","); println()
          }
          undent; println(); print("}")

        // Classes - from ECMAScript 6, can be desugared into other concepts

        case ClassDef(name, parents, defs) =>
          print("class ", name)
          if (!parents.isEmpty) {
            print(" extends ")
            printSeq(parents){printTree}{print(", ")}
          }
          print(" ")
          printColumn(defs, "{", "", "}")
          println()

        case MethodDef(name, args, body) =>
          print(name)
          printRow(args, "(", ", ", ") ")
          printBlock(body)

        case GetterDef(name, body) =>
          // TODO
          print("<getter>")

        case SetterDef(name, arg, body) =>
          // TODO
          print("<setter>")

        case Super() =>
          print("super")
      }
    }

    def print(args: Any*): Unit = args foreach {
      case tree: Tree =>
        printPosition(tree)
        printTree(tree)
      case arg =>
        out.print(if (arg == null) "null" else arg.toString)
    }
  }
}
