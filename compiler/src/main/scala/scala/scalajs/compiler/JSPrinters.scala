/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

import java.io.PrintWriter
import scala.collection.mutable.{ ListBuffer, HashMap, Stack, StringBuilder }

import scala.reflect.internal.util.SourceFile

/** Printers and source map emitters for JavaScript ASTs
 *
 *  @author Sébastien Doeraene
 */
trait JSPrinters { self: JSGlobalAddons =>
  import global._

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

    def printSeq[a](ls: List[a])(printelem: a => Unit)(printsep: Boolean => Unit) {
      ls match {
        case List() =>
        case List(x) => printelem(x)
        case x :: rest =>
          printelem(x)
          printsep(!x.isInstanceOf[js.DocComment])
          printSeq(rest)(printelem)(printsep)
      }
    }

    def printColumn(ts: List[js.Tree], start: String, sep: String, end: String) {
      print(start); indent; println()
      printSeq(ts){print(_)}{ needsSep =>
        if (needsSep)
          print(sep)
        println()
      }
      undent; println(); print(end)
    }

    def printRow(ts: List[js.Tree], start: String, sep: String, end: String) {
      print(start)
      printSeq(ts){print(_)}{ needsSep =>
        if (needsSep)
          print(sep)
      }
      print(end)
    }

    def printRow(ts: List[js.Tree], sep: String) { printRow(ts, "", sep, "") }

    def print(args: Any*): Unit
  }

  class JSTreePrinter(val out: PrintWriter) extends JSIndentationManager {
    def printBlock(tree: js.Tree) {
      tree match {
        case js.Block(_) =>
          printTree(tree)
        case _ =>
          printColumn(List(tree), "{", ";", "}")
      }
    }

    def printTopLevelTree(tree: js.Tree) {
      tree match {
        case js.Block(stats) =>
          for (stat <- stats)
            printTopLevelTree(stat)
        case _ =>
          printTree(tree)
          if (!tree.isInstanceOf[js.DocComment])
            print(";")
          println()
      }
    }

    def printTree(tree: js.Tree) {
      tree match {
        case js.EmptyTree =>
          print("<empty>")

        // Comments

        case js.DocComment(text) =>
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

        // Identifiers

        case js.Ident(name, _) =>
          print(escapeJS(name))

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
          print("/*<skip>*/")

        case js.Block(stats) =>
          printColumn(stats, "{", ";", "}")

        case js.Labeled(label, body) =>
          print(label, ": ")
          printBlock(body)

        case js.Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case js.Return(expr, label) =>
          if (label.isEmpty) print("return ", expr)
          else print("return(", label.get, ") ", expr)

        case js.If(cond, thenp, elsep) =>
          print("if (", cond, ") ")
          printBlock(thenp)
          elsep match {
            case js.Skip() => ()
            case _ =>
              print(" else ")
              printBlock(elsep)
          }

        case js.While(cond, body, label) =>
          if (label.isDefined)
            print(label.get, ": ")
          print("while (", cond, ") ")
          printBlock(body)

        case js.DoWhile(body, cond, label) =>
          if (label.isDefined)
            print(label.get, ": ")
          print("do ")
          printBlock(body)
          print(" while (", cond, ")")

        case js.Try(block, errVar, handler, finalizer) =>
          print("try ")
          printBlock(block)
          if (handler != js.EmptyTree) {
            print(" catch (", errVar, ") ")
            printBlock(handler)
          }
          if (finalizer != js.EmptyTree) {
            print(" finally ")
            printBlock(finalizer)
          }

        case js.Throw(expr) =>
          print("throw ", expr)

        case js.Break(label) =>
          if (label.isEmpty) print("break")
          else print("break ", label.get)

        case js.Continue(label) =>
          if (label.isEmpty) print("continue")
          else print("continue ", label.get)

        case js.Switch(selector, cases, default) =>
          print("switch (", selector, ") ")
          print("{"); indent
          for ((value, body) <- cases) {
            println()
            print("case ", value, ":"); indent; println()
            print(body, ";")
            undent
          }
          if (default != js.EmptyTree) {
            println()
            print("default:"); indent; println()
            print(default, ";")
            undent
          }
          undent; println(); print("}")

        case js.Match(selector, cases, default) =>
          print("match (", selector, ") ")
          print("{"); indent
          for ((value, body) <- cases) {
            println()
            print("case ", value, ":"); indent; println()
            print(body, ";")
            undent
          }
          if (default != js.EmptyTree) {
            println()
            print("default:"); indent; println()
            print(default, ";")
            undent
          }
          undent; println(); print("}")

        case js.Debugger() =>
          print("debugger")

        // Expressions

        case js.DotSelect(qualifier, item) =>
          print(qualifier, ".", item)

        case js.BracketSelect(qualifier, item) =>
          print(qualifier, "[", item, "]")

        case js.Apply(fun, args) =>
          print(fun)
          printRow(args, "(", ", ", ")")

        case js.Function(args, body) =>
          print("(")
          printRow(args, "function(", ", ", ") ")
          printBlock(body)
          print(")")

        case js.UnaryOp("typeof", lhs) =>
          print("typeof(", lhs, ")")

        case js.UnaryOp(op, lhs) =>
          print("(", op, lhs, ")")

        case js.BinaryOp(op, lhs, rhs) =>
          print("(", lhs, " ", op, " ", rhs, ")")

        case js.New(fun, args) =>
          def containsOnlySelectsFromAtom(fun: js.Tree): Boolean = fun match {
            case js.DotSelect(qual, _) => containsOnlySelectsFromAtom(qual)
            case js.BracketSelect(qual, _) => containsOnlySelectsFromAtom(qual)
            case js.Ident(_, _) => true
            case js.This() => true
            case _ => false // in particular, js.Apply
          }
          if (containsOnlySelectsFromAtom(fun))
            print("new ", fun)
          else
            print("new (", fun, ")")
          printRow(args, "(", ", ", ")")

        case js.This() =>
          print("this")

        case js.BracketDelete(obj, prop) =>
          print("delete ", obj, "[", prop, "]")

        // Literals

        case js.Undefined() =>
          print("undefined")

        case js.UndefinedParam() =>
          print("<undefined param>")

        case js.Null() =>
          print("null")

        case js.BooleanLiteral(value) =>
          print(if (value) "true" else "false")

        case js.IntLiteral(value) =>
          print(value)

        case js.DoubleLiteral(value) =>
          print(value)

        case js.StringLiteral(value, _) =>
          print("\"", escapeJS(value), "\"")

        // Compounds

        case js.ArrayConstr(items) =>
          printRow(items, "[", ", ", "]")

        case js.ObjectConstr(Nil) =>
          print("{}")

        case js.ObjectConstr(fields) =>
          print("{"); indent; println()
          printSeq(fields) {
            case (name, value) => print(name, ": ", value)
          } { needsSep =>
            if (needsSep)
              print(",")
            println()
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

        case js.PropertyDef(name, _, _, _) =>
          // TODO
          print(s"<property: $name>")

        case js.Super() =>
          print("super")

        case _ =>
          abort("Do not know how to print tree of class "+tree.getClass)
      }
    }

    def print(args: Any*): Unit = args foreach {
      case tree: js.Tree =>
        printPosition(tree)
        printTree(tree)
      case arg =>
        printString(if (arg == null) "null" else arg.toString)
    }

    protected def printString(s: String): Unit = {
      out.print(s)
    }

    def close(): Unit = ()
  }

  private def escapeJS(str: String): String = {
    /* Note that Java and JavaScript happen to use the same encoding for
     * Unicode, namely UTF-16, which means that 1 char from Java always equals
     * 1 char in JavaScript. */
    val builder = new StringBuilder
    str foreach {
      case '\\' => builder.append("\\\\")
      case '"' => builder.append("\\\"")
      case '\u0007' => builder.append("\\a")
      case '\u0008' => builder.append("\\b")
      case '\u0009' => builder.append("\\t")
      case '\u000A' => builder.append("\\n")
      case '\u000B' => builder.append("\\v")
      case '\u000C' => builder.append("\\f")
      case '\u000D' => builder.append("\\r")
      case c =>
        if (c >= 32 && c <= 126) builder.append(c.toChar) // ASCII printable characters
        else builder.append(f"\\u$c%04x")
    }
    builder.result()
  }

  private val Base64Map =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "abcdefghijklmnopqrstuvwxyz" +
      "0123456789+/"

  class SourceMapWriter(val out: PrintWriter, val generatedFile: String) {
    private val sources = new ListBuffer[String]
    private val _srcToIndex = new HashMap[SourceFile, Int]

    private val names = new ListBuffer[String]
    private val _nameToIndex = new HashMap[String, Int]

    private val nodePosStack = new Stack[(Position, Option[String])]
    nodePosStack.push((NoPosition, None))

    private var lineCountInGenerated = 0
    private var lastColumnInGenerated = 0
    private var firstSegmentOfLine = true
    private var lastSource: SourceFile = null
    private var lastSourceIndex = 0
    private var lastLine: Int = 0
    private var lastColumn: Int = 0
    private var lastNameIndex: Int = 0

    private var pendingColumnInGenerated: Int = -1
    private var pendingPos: Position = NoPosition
    private var pendingName: Option[String] = None

    writeHeader()

    private def sourceToIndex(source: SourceFile) =
      _srcToIndex.getOrElseUpdate(source,
          (sources += sourceToURI(source)).size-1)

    /** Relatively hacky way to get a Web-friendly URI to the source file */
    private def sourceToURI(source: SourceFile): String = {
      source.file.file match {
        case null => source.path
        case file =>
          val uri = file.toURI.toASCIIString
          if (uri.startsWith("file:/") && uri.charAt(6) != '/')
            "file:///" + uri.substring(6)
          else
            uri
      }
    }

    private def nameToIndex(name: String) =
      _nameToIndex.getOrElseUpdate(name, (names += name).size-1)

    private def writeHeader(): Unit = {
      out.println("{")
      out.println("\"version\": 3,")
      out.println("\"file\": " + jsonString(generatedFile) + ",")
      out.print("\"mappings\": \"")
    }

    def nextLine(): Unit = {
      writePendingSegment()
      out.print(';')
      lineCountInGenerated += 1
      lastColumnInGenerated = 0
      firstSegmentOfLine = true
      pendingColumnInGenerated = -1
      pendingPos = nodePosStack.top._1
      pendingName = nodePosStack.top._2
    }

    def startNode(column: Int, originalPos: Position,
        originalName: Option[String] = None): Unit = {
      nodePosStack.push((originalPos, originalName))
      startSegment(column, originalPos, originalName)
    }

    def endNode(column: Int): Unit = {
      nodePosStack.pop()
      startSegment(column, nodePosStack.top._1, nodePosStack.top._2)
    }

    private def startSegment(startColumn: Int, originalPos: Position,
        originalName: Option[String]): Unit = {
      // There is no point in outputting a segment with the same information
      if ((originalPos == pendingPos) && (originalName == pendingName))
        return

      // Write pending segment if it covers a non-empty range
      if (startColumn != pendingColumnInGenerated)
        writePendingSegment()

      // New pending
      pendingColumnInGenerated = startColumn
      pendingPos = originalPos
      pendingName =
        if (startColumn != pendingColumnInGenerated) originalName
        else pendingName orElse originalName
    }

    private def writePendingSegment() {
      if (pendingColumnInGenerated < 0)
        return

      // Segments of a line are separated by ','
      if (firstSegmentOfLine) firstSegmentOfLine = false
      else out.print(',')

      // Generated column field
      writeBase64VLQ(pendingColumnInGenerated-lastColumnInGenerated)
      lastColumnInGenerated = pendingColumnInGenerated

      // If the position is NoPosition, stop here
      if (!pendingPos.isDefined)
        return

      // Extract relevant properties of pendingPos
      val source = pendingPos.source
      val line = pendingPos.line-1
      val column = pendingPos.column-1

      // Source index field
      if (source eq lastSource) { // highly likely
        writeBase64VLQ0()
      } else {
        val sourceIndex = sourceToIndex(source)
        writeBase64VLQ(sourceIndex-lastSourceIndex)
        lastSource = source
        lastSourceIndex = sourceIndex
      }

      // Line field
      writeBase64VLQ(line - lastLine)
      lastLine = line

      // Column field
      writeBase64VLQ(column - lastColumn)
      lastColumn = column

      // Name field
      if (pendingName.isDefined) {
        val nameIndex = nameToIndex(pendingName.get)
        writeBase64VLQ(nameIndex-lastNameIndex)
        lastNameIndex = nameIndex
      }
    }

    def close(): Unit = {
      writePendingSegment()

      out.println("\",")
      out.println(
          sources.map(jsonString(_)).mkString("\"sources\": [", ", ", "],"))
      out.println(
          names.map(jsonString(_)).mkString("\"names\": [", ", ", "],"))
      out.println("\"lineCount\": "+lineCountInGenerated)
      out.println("}")
    }

    /** Write the Base 64 VLQ of an integer to the mappings
     *  Inspired by the implementation in Closure Compiler:
     *  http://code.google.com/p/closure-compiler/source/browse/src/com/google/debugging/sourcemap/Base64VLQ.java
     */
    private def writeBase64VLQ(value0: Int): Unit = {
      // Sign is encoded in the least significant bit
      var value =
        if (value0 < 0) ((-value0) << 1) + 1
        else value0 << 1

      // Some constants
      // Each base-64 digit covers 6 bits, but 1 is used for the continuation
      val VLQBaseShift = 5
      val VLQBase = 1 << VLQBaseShift
      val VLQBaseMask = VLQBase - 1
      val VLQContinuationBit = VLQBase

      // Write as many base-64 digits as necessary to encode value
      do {
        var digit = value & VLQBaseMask
        value = value >>> VLQBaseShift
        if (value != 0)
          digit |= VLQContinuationBit
        out.print(Base64Map.charAt(digit))
      } while (value != 0)
    }

    private def writeBase64VLQ0(): Unit =
      out.print('A')

    private def jsonString(s: String) =
      "\"" + escapeJS(s) + "\""
  }

  class JSTreePrinterWithSourceMap(_out: PrintWriter,
      val sourceMapOut: PrintWriter,
      generatedFile: String) extends JSTreePrinter(_out) {

    private val sourceMap = new SourceMapWriter(sourceMapOut, generatedFile)
    private var column = 0

    override def printTree(tree: js.Tree): Unit = {
      val pos = tree.pos
      if (pos.isDefined) {
        val originalName = tree match {
          case js.PropertyName(_, origName) => origName
          case _ => None
        }
        sourceMap.startNode(column, pos, originalName)
      }

      super.printTree(tree)

      if (pos.isDefined)
        sourceMap.endNode(column)
    }

    override def println(): Unit = {
      super.println()
      sourceMap.nextLine()
      column = this.indentMargin
    }

    override def printString(s: String): Unit = {
      // assume no EOL char in s, and assume s only has ASCII characters
      super.printString(s)
      column += s.length()
    }

    override def close(): Unit = {
      sourceMap.close()
      super.close()
    }
  }
}
