/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.javascript

import scala.annotation.switch

// Unimport default print and println to avoid invoking them by mistake
import scala.Predef.{print => _, println => _, _}

import scala.util.control.Breaks

import java.io.Writer

import org.scalajs.core.ir
import ir.Position
import ir.Position.NoPosition
import ir.Printers.IndentationManager
import ir.Utils.printEscapeJS

import Trees._

/* Printers code was hand-optimized with raw performance in mind.
 * It was hand optimized using fastOptJS on examples/reversi and testsSuite/test
 * as performance benchmarks. JProfiler was used to measure the performance of
 * hotspots in this object.
 */
object Printers {

  class JSTreePrinter(protected val out: Writer) extends IndentationManager {

    def printTopLevelTree(tree: Tree) {
      tree match {
        case Skip() =>
          // do not print anything
        case tree: Block =>
          var rest = tree.stats
          while (rest.nonEmpty) {
            printTopLevelTree(rest.head)
            rest = rest.tail
          }
        case _ =>
          printStat(tree)
          if (shouldPrintSepAfterTree(tree))
            print(';')
          println()
      }
    }

    protected def shouldPrintSepAfterTree(tree: Tree): Boolean = tree match {
      case _:DocComment | _:FunctionDef | _:ClassDef => false
      case _                                         => true
    }

    protected def printRow(ts: List[Tree], start: Char, end: Char): Unit = {
      print(start)
      var rest = ts
      while (rest.nonEmpty) {
        print(rest.head)
        rest = rest.tail
        if (rest.nonEmpty)
          print(", ")
      }
      print(end)
    }

    protected def printBlock(tree: Tree): Unit = {
      print('{'); indent(); println()
      tree match {
        case tree: Block =>
          var rest = tree.stats
          while (rest.nonEmpty) {
            val x = rest.head
            rest = rest.tail
            printStat(x)
            if (rest.nonEmpty) {
              if (shouldPrintSepAfterTree(x))
                print(';')
              println()
            }
          }

        case _ =>
          printStat(tree)
      }
      undent(); println(); print('}')
    }

    protected def printSig(args: List[ParamDef]): Unit = {
      printRow(args, '(', ')')
      print(' ')
    }

    protected def printArgs(args: List[Tree]): Unit =
      printRow(args, '(', ')')

    def printStat(tree: Tree): Unit =
      printTree(tree, isStat = true)

    protected def print(tree: Tree): Unit =
      printTree(tree, isStat = false)

    def printTree(tree: Tree, isStat: Boolean): Unit = {
      tree match {
        // Comments

        case DocComment(text) =>
          val lines = text.split("\n").toList
          if (lines.tail.isEmpty) {
            print("/** ")
            print(lines.head)
            print(" */")
          } else {
            print("/** ")
            print(lines.head)
            println()
            var rest = lines.tail
            while (rest.nonEmpty) {
              print(" *  ")
              print(rest.head)
              println()
              rest = rest.tail
            }
            print(" */")
          }

        // Definitions

        case VarDef(ident, optRhs) =>
          print("var ")
          print(ident)
          optRhs foreach { rhs =>
            print(" = ")
            print(rhs)
          }

        case Let(ident, mutable, optRhs) =>
          print(if (mutable) "let " else "const ")
          print(ident)
          optRhs foreach { rhs =>
            print(" = ")
            print(rhs)
          }

        case ParamDef(ident, rest) =>
          if (rest)
            print("...")
          print(ident)

        // Control flow constructs

        case Skip() =>
          print("/*<skip>*/")

        case tree: Block =>
          if (isStat)
            printBlock(tree)
          else
            printRow(tree.stats, '(', ')')

        case Labeled(label, body) =>
          print(label)
          print(": ")
          printBlock(body)

        case Assign(lhs, rhs) =>
          print(lhs)
          print(" = ")
          print(rhs)

        case Return(expr) =>
          print("return ")
          print(expr)

        case If(cond, thenp, elsep) =>
          if (isStat) {
            print("if (")
            print(cond)
            print(") ")
            printBlock(thenp)
            elsep match {
              case Skip() => ()
              case _: If =>
                print(" else ")
                printTree(elsep, isStat)
              case _ =>
                print(" else ")
                printBlock(elsep)
            }
          } else {
            print('(')
            print(cond)
            print(" ? ")
            print(thenp)
            print(" : ")
            print(elsep)
            print(')')
          }

        case While(cond, body, label) =>
          if (label.isDefined) {
            print(label.get)
            print(": ")
          }
          print("while (")
          print(cond)
          print(") ")
          printBlock(body)

        case DoWhile(body, cond, label) =>
          if (label.isDefined) {
            print(label.get)
            print(": ")
          }
          print("do ")
          printBlock(body)
          print(" while (")
          print(cond)
          print(')')

        case TryFinally(TryCatch(block, errVar, handler), finalizer) =>
          print("try ")
          printBlock(block)
          print(" catch (")
          print(errVar)
          print(") ")
          printBlock(handler)
          print(" finally ")
          printBlock(finalizer)

        case TryCatch(block, errVar, handler) =>
          print("try ")
          printBlock(block)
          print(" catch (")
          print(errVar)
          print(") ")
          printBlock(handler)

        case TryFinally(block, finalizer) =>
          print("try ")
          printBlock(block)
          print(" finally ")
          printBlock(finalizer)

        case Throw(expr) =>
          print("throw ")
          print(expr)

        case Break(label) =>
          if (label.isEmpty) print("break")
          else {
            print("break ")
            print(label.get)
          }

        case Continue(label) =>
          if (label.isEmpty) print("continue")
          else {
            print("continue ")
            print(label.get)
          }

        case Switch(selector, cases, default) =>
          print("switch (")
          print(selector)
          print(") ")
          print('{')
          indent()
          var rest = cases
          while (rest.nonEmpty) {
            val next = rest.head
            rest = rest.tail
            println()
            print("case ")
            print(next._1)
            print(':')
            if (!next._2.isInstanceOf[Skip]) {
              print(' ')
              printBlock(next._2)
            }
          }

          default match {
            case Skip() =>
            case _ =>
              println()
              print("default: ")
              printBlock(default)
          }

          undent()
          println()
          print('}')

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
          if (containsOnlySelectsFromAtom(ctor)) {
            print("new ")
            print(ctor)
          } else {
            print("new (")
            print(ctor)
            print(')')
          }
          printArgs(args)

        case DotSelect(qualifier, item) =>
          qualifier match {
            case _:IntLiteral | _:DoubleLiteral =>
              print("(")
              print(qualifier)
              print(")")
            case _ =>
              print(qualifier)
          }
          print(".")
          print(item)

        case BracketSelect(qualifier, item) =>
          print(qualifier)
          print('[')
          print(item)
          print(']')

        case Apply(fun, args) =>
          print(fun)
          printArgs(args)

        case Spread(items) =>
          print("...")
          print(items)

        case Delete(prop) =>
          print("delete ")
          print(prop)

        case UnaryOp(op, lhs) =>
          import ir.Trees.JSUnaryOp._
          print('(')
          if (op == `typeof`) {
            print("typeof ")
          } else {
            (op: @switch) match {
              case + => print('+')
              case - => print('-')
              case ~ => print('~')
              case ! => print('!')
              case `typeof` => print("typeof ")
            }
          }
          print(lhs)
          print(')')

        case BinaryOp(op, lhs, rhs) =>
          import ir.Trees.JSBinaryOp._
          print('(')
          print(lhs)
          print(' ')
          print((op: @switch) match {
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
          })
          print(' ')
          print(rhs)
          print(')')

        case ArrayConstr(items) =>
          printRow(items, '[', ']')

        case ObjectConstr(Nil) =>
          if (isStat)
            print("({})") // force expression position for the object literal
          else
            print("{}")

        case ObjectConstr(fields) =>
          if (isStat)
            print('(') // force expression position for the object literal
          print('{')
          indent()
          println()
          var rest = fields
          while (rest.nonEmpty) {
            val x = rest.head
            rest = rest.tail
            print(x._1)
            print(": ")
            print(x._2)
            if (rest.nonEmpty) {
              print(',')
              println()
            }
          }
          undent()
          println()
          print('}')
          if (isStat)
            print(')')

        // Literals

        case Undefined() =>
          print("(void 0)")

        case Null() =>
          print("null")

        case BooleanLiteral(value) =>
          print(if (value) "true" else "false")

        case IntLiteral(value) =>
          if (value >= 0) {
            print(value.toString)
          } else {
            print('(')
            print(value.toString)
            print(')')
          }

        case DoubleLiteral(value) =>
          if (value == 0 && 1 / value < 0) {
            print("(-0)")
          } else if (value >= 0) {
            print(value.toString)
          } else {
            print('(')
            print(value.toString)
            print(')')
          }

        case StringLiteral(value) =>
          print('\"')
          printEscapeJS(value, out)
          print('\"')

        // Atomic expressions

        case VarRef(ident) =>
          print(ident)

        case This() =>
          print("this")

        case Function(args, body) =>
          print("(function")
          printSig(args)
          printBlock(body)
          print(')')

        // Named function definition

        case FunctionDef(name, args, body) =>
          if (!isStat)
            print('(')
          print("function ")
          print(name)
          printSig(args)
          printBlock(body)
          if (!isStat)
            print(')')

        // ECMAScript 6 classes

        case ClassDef(optClassName, optParentClass, members) =>
          print("class")
          if (optClassName.isDefined) {
            print(' ')
            print(optClassName.get)
          }
          if (optParentClass.isDefined) {
            print(" extends ")
            print(optParentClass.get)
          }
          print(" {"); indent()
          var rest = members
          while (rest.nonEmpty) {
            println()
            print(rest.head)
            print(';')
            rest = rest.tail
          }
          undent(); println(); print('}')

        case MethodDef(static, name, params, body) =>
          if (static)
            print("static ")
          print(name)
          printSig(params)
          printBlock(body)

        case GetterDef(static, name, body) =>
          if (static)
            print("static ")
          print("get ")
          print(name)
          printSig(Nil)
          printBlock(body)

        case SetterDef(static, name, param, body) =>
          if (static)
            print("static ")
          print("set ")
          print(name)
          print('(')
          print(param)
          print(") ")
          printBlock(body)

        case Super() =>
          print("super")

        case _ =>
          print(s"<error, elem of class ${tree.getClass}>")
      }
    }

    protected def print(ident: Ident): Unit =
      printEscapeJS(ident.name, out)

    private final def print(propName: PropertyName): Unit = propName match {
      case lit: StringLiteral => print(lit: Tree)
      case ident: Ident       => print(ident)

      case ComputedName(tree) =>
        print("[")
        print(tree)
        print("]")
    }

    protected def print(s: String): Unit =
      out.write(s)

    protected def print(c: Int): Unit =
      out.write(c)

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

    override protected def print(ident: Ident): Unit = {
      if (ident.pos.isDefined)
        sourceMap.startNode(column, ident.pos, ident.originalName)
      super.print(ident)
      if (ident.pos.isDefined)
        sourceMap.endNode(column)
    }

    override def println(): Unit = {
      super.println()
      sourceMap.nextLine()
      column = this.getIndentMargin()
    }

    override protected def print(s: String): Unit = {
      // assume no EOL char in s, and assume s only has ASCII characters
      super.print(s)
      column += s.length()
    }

    override protected def print(c: Int): Unit = {
      // assume c is not EOL, and assume c is an ASCII character
      super.print(c)
      column += 1
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

    override protected def print(ident: Ident): Unit = {
      if (positions(curLine).isEmpty)
        positions(curLine) = ident.pos

      super.print(ident)
    }

    override def println(): Unit = {
      super.println()
      curLine += 1
      if (curLine > untilLine)
        doneBreak.break()
    }

    override protected def print(s: String): Unit = {
      // assume no EOL char in s, and assume s only has ASCII characters
      // therefore, we fully ignore the string
    }

    override protected def print(c: Int): Unit = {
      // assume c is not EOL, and assume c is an ASCII characters
      // therefore, we fully ignore the char
    }
  }

  private object ReverseSourceMapPrinter {
    private object NullWriter extends Writer {
      def close(): Unit = ()
      def flush(): Unit = ()
      def write(buf: Array[Char], off: Int, len: Int): Unit = ()
    }
  }

}
