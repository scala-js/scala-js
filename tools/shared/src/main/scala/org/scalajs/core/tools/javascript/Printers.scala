/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.javascript

import scala.annotation.switch

import scala.util.control.Breaks

import java.io.Writer
import java.net.URI

import org.scalajs.core.ir
import ir.Position
import ir.Position.NoPosition
import ir.Printers.IndentationManager
import ir.Utils.escapeJS

import Trees._

import org.scalajs.core.tools.sourcemap.SourceMapWriter

object Printers {

  class JSTreePrinter(protected val out: Writer) extends IndentationManager {
    def printTopLevelTree(tree: Tree) {
      tree match {
        case Skip() =>
          // do not print anything
        case Block(stats) =>
          for (stat <- stats)
            printTopLevelTree(stat)
        case _ =>
          printStat(tree)
          if (shouldPrintSepAfterTree(tree))
            print(";")
          println()
      }
    }

    protected def shouldPrintSepAfterTree(tree: Tree): Boolean =
      !tree.isInstanceOf[DocComment]

    protected def printBlock(tree: Tree): Unit = {
      val trees = tree match {
        case Block(trees) => trees
        case _            => List(tree)
      }
      print("{"); indent(); println()
      printSeq(trees) { x =>
        printStat(x)
      } { x =>
        if (shouldPrintSepAfterTree(x))
          print(";")
        println()
      }
      undent(); println(); print("}")
    }

    protected def printSig(args: List[ParamDef]): Unit = {
      printRow(args, "(", ", ", ")")
      print(" ")
    }

    protected def printArgs(args: List[Tree]): Unit = {
      printRow(args, "(", ", ", ")")
    }

    def printStat(tree: Tree): Unit =
      printTree(tree, isStat = true)

    def printTree(tree: Tree, isStat: Boolean): Unit = {
      tree match {
        case EmptyTree =>
          print("<empty>")

        // Comments

        case DocComment(text) =>
          val lines = text.split("\n").toList
          if (lines.tail.isEmpty) {
            print("/** ", lines.head, " */")
          } else {
            print("/** ", lines.head); println()
            for (line <- lines.tail) {
              print(" *  ", line); println()
            }
            print(" */")
          }

        // Definitions

        case VarDef(ident, rhs) =>
          print("var ", ident)
          if (rhs != EmptyTree)
            print(" = ", rhs)

        case Let(ident, mutable, rhs) =>
          print(if (mutable) "let " else "const ", ident)
          if (rhs != EmptyTree)
            print(" = ", rhs)

        case ParamDef(ident, rest) =>
          if (rest)
            print("...")
          print(ident)

        // Control flow constructs

        case Skip() =>
          print("/*<skip>*/")

        case tree @ Block(trees) =>
          if (isStat)
            printBlock(tree)
          else
            printRow(trees, "(", ", ", ")")

        case Labeled(label, body) =>
          print(label, ": ")
          printBlock(body)

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case Return(expr) =>
          print("return ", expr)

        case If(cond, thenp, elsep) =>
          if (isStat) {
            print("if (", cond, ") ")
            printBlock(thenp)
            elsep match {
              case Skip() => ()
              case If(_, _, _) =>
                print(" else ")
                printTree(elsep, isStat)
              case _ =>
                print(" else ")
                printBlock(elsep)
            }
          } else {
            print("(", cond, " ? ", thenp, " : ", elsep, ")")
          }

        case While(cond, body, label) =>
          if (label.isDefined)
            print(label.get, ": ")
          print("while (", cond, ") ")
          printBlock(body)

        case DoWhile(body, cond, label) =>
          if (label.isDefined)
            print(label.get, ": ")
          print("do ")
          printBlock(body)
          print(" while (", cond, ")")

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

        case Break(label) =>
          if (label.isEmpty) print("break")
          else print("break ", label.get)

        case Continue(label) =>
          if (label.isEmpty) print("continue")
          else print("continue ", label.get)

        case Switch(selector, cases, default) =>
          print("switch (", selector, ") ")
          print("{"); indent
          for ((value, body) <- cases) {
            println()
            print("case ", value, ":"); indent; println()
            printStat(body)
            print(";")
            undent
          }
          if (default != EmptyTree) {
            println()
            print("default:"); indent; println()
            printStat(default)
            print(";")
            undent
          }
          undent; println(); print("}")

        case Debugger() =>
          print("debugger")

        // Expressions

        case New(ctor, args) =>
          def containsOnlySelectsFromAtom(tree: Tree): Boolean = tree match {
            case DotSelect(qual, _)     => containsOnlySelectsFromAtom(qual)
            case BracketSelect(qual, _) => containsOnlySelectsFromAtom(qual)
            case VarRef(_)              => true
            case This()                 => true
            case _                      => false // in particular, Apply
          }
          if (containsOnlySelectsFromAtom(ctor))
            print("new ", ctor)
          else
            print("new (", ctor, ")")
          printArgs(args)

        case DotSelect(qualifier, item) =>
          print(qualifier, ".", item)

        case BracketSelect(qualifier, item) =>
          print(qualifier, "[", item, "]")

        case Apply(fun, args) =>
          print(fun)
          printArgs(args)

        case Delete(prop) =>
          print("delete ", prop)

        case UnaryOp(op, lhs) =>
          import ir.Trees.JSUnaryOp._
          print("(", (op: @switch) match {
            case + => "+"
            case - => "-"
            case ~ => "~"
            case ! => "!"

            case `typeof` => "typeof "
          }, lhs, ")")

        case BinaryOp(op, lhs, rhs) =>
          import ir.Trees.JSBinaryOp._
          print("(", lhs, " ", (op: @switch) match {
            case === => "==="
            case !== => "!=="

            case + => "+"
            case - => "-"
            case * => "*"
            case / => "/"
            case % => "%"

            case |   => "|"
            case &   => "&"
            case ^   => "^"
            case <<  => "<<"
            case >>  => ">>"
            case >>> => ">>>"

            case <  => "<"
            case <= => "<="
            case >  => ">"
            case >= => ">="

            case && => "&&"
            case || => "||"

            case `in`         => "in"
            case `instanceof` => "instanceof"
          }, " ", rhs, ")")

        case ArrayConstr(items) =>
          printRow(items, "[", ", ", "]")

        case ObjectConstr(Nil) =>
          if (isStat)
            print("({})") // force expression position for the object literal
          else
            print("{}")

        case ObjectConstr(fields) =>
          if (isStat)
            print("(") // force expression position for the object literal
          print("{"); indent; println()
          printSeq(fields) {
            case (name, value) => print(name, ": ", value)
          } { _ =>
            print(",")
            println()
          }
          undent; println(); print("}")
          if (isStat)
            print(")")

        // Literals

        case Undefined() =>
          print("(void 0)")

        case Null() =>
          print("null")

        case BooleanLiteral(value) =>
          print(if (value) "true" else "false")

        case IntLiteral(value) =>
          if (value >= 0)
            print(value)
          else
            print("(", value, ")")

        case DoubleLiteral(value) =>
          if (value == 0 && 1 / value < 0)
            print("(-0)")
          else if (value >= 0)
            print(value)
          else
            print("(", value, ")")

        case StringLiteral(value) =>
          print("\"", escapeJS(value), "\"")

        // Atomic expressions

        case VarRef(ident) =>
          print(ident)

        case This() =>
          print("this")

        case Function(args, body) =>
          print("(function")
          printSig(args)
          printBlock(body)
          print(")")

        // ECMAScript 6 classes

        case ClassDef(optClassName, optParentClass, members) =>
          print("class")
          for (className <- optClassName)
            print(" ", className)
          for (parentClass <- optParentClass)
            print(" extends ", parentClass)
          print(" {"); indent
          for (member <- members) {
            println()
            print(member, ";")
          }
          undent; println(); print("}")

        case MethodDef(static, name, params, body) =>
          if (static)
            print("static ")
          print(name)
          printSig(params)
          printBlock(body)

        case GetterDef(static, name, body) =>
          if (static)
            print("static ")
          print("get ", name)
          printSig(Nil)
          printBlock(body)

        case SetterDef(static, name, param, body) =>
          if (static)
            print("static ")
          print("set ", name)
          printSig(List(param))
          printBlock(body)

        case Super() =>
          print("super")

        case _ =>
          print(s"<error, elem of class ${tree.getClass()}>")
      }
    }

    protected def printIdent(ident: Ident): Unit =
      printString(escapeJS(ident.name))

    def printOne(arg: Any): Unit = arg match {
      case tree: Tree =>
        printTree(tree, isStat = false)
      case ident: Ident =>
        printIdent(ident)
      case arg =>
        printString(if (arg == null) "null" else arg.toString)
    }

    protected def printString(s: String): Unit = {
      out.write(s)
    }

    // Make it public
    override def println(): Unit = super.println()

    def complete(): Unit = ()
  }

  class JSTreePrinterWithSourceMap(_out: Writer,
      sourceMap: SourceMapWriter) extends JSTreePrinter(_out) {

    private var column = 0

    override def printTree(tree: Tree, isStat: Boolean): Unit = {
      val pos = tree.pos
      if (pos.isDefined)
        sourceMap.startNode(column, pos)

      super.printTree(tree, isStat)

      if (pos.isDefined)
        sourceMap.endNode(column)
    }

    override protected def printIdent(ident: Ident): Unit = {
      if (ident.pos.isDefined)
        sourceMap.startNode(column, ident.pos, ident.originalName)
      super.printIdent(ident)
      if (ident.pos.isDefined)
        sourceMap.endNode(column)
    }

    override def println(): Unit = {
      super.println()
      sourceMap.nextLine()
      column = this.indentMargin
    }

    override protected def printString(s: String): Unit = {
      // assume no EOL char in s, and assume s only has ASCII characters
      super.printString(s)
      column += s.length()
    }

    override def complete(): Unit = {
      sourceMap.complete()
      super.complete()
    }
  }

  /** Prints a tree to find original locations based on line numbers.
   *  @param untilLine last 0-based line the positions should be recorded for
   */
  class ReverseSourceMapPrinter(untilLine: Int)
      extends JSTreePrinter(ReverseSourceMapPrinter.NullWriter) {

    private val positions = Array.fill(untilLine+1)(NoPosition)
    private var curLine = 0

    private val doneBreak = new Breaks

    def apply(x: Int): Position = positions(x)

    def reverseSourceMap(tree: Tree): Unit = doneBreak.breakable {
      printTopLevelTree(tree)
    }

    override def printTree(tree: Tree, isStat: Boolean): Unit = {
      if (positions(curLine).isEmpty)
        positions(curLine) = tree.pos

      super.printTree(tree, isStat)
    }

    override protected def printIdent(ident: Ident): Unit = {
      if (positions(curLine).isEmpty)
        positions(curLine) = ident.pos

      super.printIdent(ident)
    }

    override def println(): Unit = {
      super.println()
      curLine += 1
      if (curLine > untilLine)
        doneBreak.break()
    }

    override protected def printString(s: String): Unit = {
      // assume no EOL char in s, and assume s only has ASCII characters
      // therefore, we fully ignore the string
    }
  }

  object ReverseSourceMapPrinter {
    private object NullWriter extends Writer {
      def close(): Unit = ()
      def flush(): Unit = ()
      def write(buf: Array[Char], off: Int, len: Int): Unit = ()
    }
  }

}
