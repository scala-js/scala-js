/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import scala.annotation.switch

import java.io.Writer
import java.net.URI

import Position._
import Trees._
import Types._
import Infos._

object Printers {

  /** Basically copied from scala.reflect.internal.Printers */
  trait IndentationManager {
    val out: Writer

    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    def indent() = indentMargin += indentStep
    def undent() = indentMargin -= indentStep

    def println() {
      out.write('\n')
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
          printsep(!x.isInstanceOf[DocComment])
          printSeq(rest)(printelem)(printsep)
      }
    }

    def printColumn(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); indent; println()
      printSeq(ts){print(_)}{ needsSep =>
        if (needsSep)
          print(sep)
        println()
      }
      undent; println(); print(end)
    }

    def printRow(ts: List[Any], start: String, sep: String, end: String) {
      print(start)
      printSeq(ts){print(_)}{ needsSep =>
        if (needsSep)
          print(sep)
      }
      print(end)
    }

    def printRow(ts: List[Any], sep: String) { printRow(ts, "", sep, "") }

    def print(args: Any*): Unit
  }

  class IRTreePrinter(val out: Writer,
      val jsMode: Boolean) extends IndentationManager {

    @deprecated("Use the constructor with jsMode", "0.5.3")
    def this(out: Writer) = this(out, jsMode = true)

    def printBlock(tree: Tree, isStat: Boolean): Unit = {
      def printStatBlock(trees: List[Tree]): Unit = {
        print("{"); indent; println()
        printSeq(trees){printStat(_)}{ needsSep =>
          if (needsSep)
            print(";")
          println()
        }
        undent; println(); print("}")
      }
      val trees = tree match {
        case Block(trees) => trees
        case _            => List(tree)
      }
      if (isStat || !jsMode)
        printStatBlock(trees)
      else
        printRow(trees, "(", ", ", ")")
    }

    def printTopLevelTree(tree: Tree) {
      tree match {
        case Skip() =>
          // do not print anything
        case Block(stats) =>
          for (stat <- stats)
            printTopLevelTree(stat)
        case _ =>
          printStat(tree)
          if (!tree.isInstanceOf[DocComment])
            print(";")
          println()
      }
    }

    def printSig(args: List[ParamDef], resultType: Type): Unit = {
      printRow(args, "(", ", ", ")")
      if (!jsMode && resultType != NoType)
        print(": ", resultType, " = ")
      else
        print(" ")
    }

    def printArgs(args: List[Tree]): Unit = {
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

        case VarDef(ident, vtpe, mutable, rhs) =>
          if (jsMode || mutable)
            print("var ")
          else
            print("val ")
          print(ident)
          printOptType(vtpe)
          if (rhs != EmptyTree)
            print(" = ", rhs)

        case ParamDef(ident, ptpe, mutable) =>
          if (!jsMode && mutable)
            print("var ")
          print(ident)
          printOptType(ptpe)

        // Control flow constructs

        case Skip() =>
          print("/*<skip>*/")

        case tree: Block =>
          printBlock(tree, isStat)

        case Labeled(label, tpe, body) =>
          print(label)
          if (!jsMode && tpe != NoType)
            print("[", tpe, "]")
          print(": ")
          printBlock(body, isStat = true)

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case Return(expr, label) =>
          if (label.isEmpty) print("return ", expr)
          else print("return(", label.get, ") ", expr)

        case If(cond, thenp, elsep) =>
          if (isStat || !jsMode) {
            print("if (", cond, ") ")
            printBlock(thenp, isStat)
            elsep match {
              case Skip() => ()
              case If(_, _, _) =>
                print(" else ")
                printTree(elsep, isStat)
              case _ =>
                print(" else ")
                printBlock(elsep, isStat)
            }
          } else {
            print("(", cond, " ? ", thenp, " : ", elsep, ")")
          }

        case While(cond, body, label) =>
          if (label.isDefined)
            print(label.get, ": ")
          print("while (", cond, ") ")
          printBlock(body, isStat = true)

        case DoWhile(body, cond, label) =>
          if (label.isDefined)
            print(label.get, ": ")
          print("do ")
          printBlock(body, isStat = true)
          print(" while (", cond, ")")

        case Try(block, errVar, handler, finalizer) =>
          print("try ")
          printBlock(block, isStat = true)
          if (handler != EmptyTree) {
            print(" catch (", errVar, ") ")
            printBlock(handler, isStat = true)
          }
          if (finalizer != EmptyTree) {
            print(" finally ")
            printBlock(finalizer, isStat = true)
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

        case Match(selector, cases, default) =>
          print("match (", selector, ") ")
          print("{"); indent
          for ((value, body) <- cases) {
            println()
            print("case ", value, ":"); indent; println()
            printTree(body, isStat)
            print(";")
            undent
          }
          if (default != EmptyTree) {
            println()
            print("default:"); indent; println()
            printTree(default, isStat)
            print(";")
            undent
          }
          undent; println(); print("}")

        case Debugger() =>
          print("debugger")

        // Scala expressions

        case New(cls, ctor, args) =>
          print("new ", cls, "().", ctor)
          printArgs(args)

        case LoadModule(cls) =>
          print("mod:", cls)

        case StoreModule(cls, value) =>
          print("mod:", cls, "<-", value)

        case Select(qualifier, item, _) =>
          print(qualifier, ".", item)

        case Apply(receiver, method, args) =>
          print(receiver, ".", method)
          printArgs(args)

        case StaticApply(receiver, cls, method, args) =>
          print(receiver, ".", cls, "::", method)
          printArgs(args)

        case TraitImplApply(impl, method, args) =>
          print(impl, "::", method)
          printArgs(args)

        case UnaryOp(op, lhs) =>
          import UnaryOp._
          print("(", (op: @switch) match {
            case `typeof`         => "typeof"
            case Int_+ | Double_+ => "+"
            case Int_- | Double_- => "-"
            case Int_~            => "~"
            case Boolean_!        => "!"
            case DoubleToInt      => "(int)"
          }, lhs, ")")

        case BinaryOp(op, lhs, rhs) =>
          import BinaryOp._
          print("(", lhs, " ", (op: @switch) match {
            case === => "==="
            case !== => "!=="

            case <  => "<"
            case <= => "<="
            case >  => ">"
            case >= => ">="

            case String_+ => "+[string]"

            case `in`         => "in"
            case `instanceof` => "instanceof"

            case Int_+ => "+[int]"
            case Int_- => "-[int]"
            case Int_* => "*[int]"
            case Int_/ => "/[int]"
            case Int_% => "%[int]"

            case Int_|   => "|"
            case Int_&   => "&"
            case Int_^   => "^"
            case Int_<<  => "<<"
            case Int_>>> => ">>>"
            case Int_>>  => ">>"

            case Double_+ => "+"
            case Double_- => "-"
            case Double_* => "*"
            case Double_/ => "/"
            case Double_% => "%"

            case Boolean_|  => "|[bool]"
            case Boolean_&  => "&[bool]"
            case Boolean_^  => "^[bool]"
            case Boolean_|| => "||"
            case Boolean_&& => "&&"
          }, " ", rhs, ")")

        case NewArray(tpe, lengths) =>
          print("new ", tpe.baseClassName)
          for (length <- lengths)
            print("[", length, "]")
          for (dim <- lengths.size until tpe.dimensions)
            print("[]")

        case ArrayValue(tpe, elems) =>
          print(tpe)
          printArgs(elems)

        case ArrayLength(array) =>
          print(array, ".length")

        case ArraySelect(array, index) =>
          print(array, "[", index, "]")

        case RecordValue(tpe, elems) =>
          print("(")
          var first = true
          for ((field, value) <- tpe.fields zip elems) {
            if (first) first = false
            else print(", ")
            print(field.name, " = ", value)
          }
          print(")")

        case IsInstanceOf(expr, cls) =>
          print(expr, ".isInstanceOf[", cls, "]")

        case AsInstanceOf(expr, cls) =>
          print(expr, ".asInstanceOf[", cls, "]")

        case ClassOf(cls) =>
          print("classOf[", cls, "]")

        case CallHelper(helper, args) =>
          print(helper)
          printArgs(args)

        // JavaScript expressions

        case JSGlobal() =>
          print("<global>")

        case JSNew(ctor, args) =>
          def containsOnlySelectsFromAtom(tree: Tree): Boolean = tree match {
            case JSDotSelect(qual, _) => containsOnlySelectsFromAtom(qual)
            case JSBracketSelect(qual, _) => containsOnlySelectsFromAtom(qual)
            case VarRef(_, _) => true
            case This() => true
            case _ => false // in particular, Apply
          }
          if (containsOnlySelectsFromAtom(ctor))
            print("new ", ctor)
          else
            print("new (", ctor, ")")
          printArgs(args)

        case JSDotSelect(qualifier, item) =>
          print(qualifier, ".", item)

        case JSBracketSelect(qualifier, item) =>
          print(qualifier, "[", item, "]")

        case JSFunctionApply(fun, args) =>
          fun match {
            case _:JSDotSelect | _:JSBracketSelect | _:Select =>
              print("protect(", fun, ")")
            case _ =>
              print(fun)
          }
          printArgs(args)

        case JSDotMethodApply(receiver, method, args) =>
          print(receiver, ".", method)
          printArgs(args)

        case JSBracketMethodApply(receiver, method, args) =>
          print(receiver, "[", method, "]")
          printArgs(args)

        case JSApply(fun, args) =>
          print(fun)
          printArgs(args)

        case JSDelete(obj, prop) =>
          print("delete ", obj, "[", prop, "]")

        case JSUnaryOp("typeof", lhs) =>
          print("typeof(", lhs, ")")

        case JSUnaryOp(op, lhs) =>
          print("(", op, lhs, ")")

        case JSBinaryOp(op, lhs, rhs) =>
          print("(", lhs, " ", op, " ", rhs, ")")

        case JSArrayConstr(items) =>
          printRow(items, "[", ", ", "]")

        case JSObjectConstr(Nil) =>
          print("{}")

        case JSObjectConstr(fields) =>
          print("{"); indent; println()
          printSeq(fields) {
            case (name, value) => print(name, ": ", value)
          } { needsSep =>
            if (needsSep)
              print(",")
            println()
          }
          undent; println(); print("}")

        // Literals

        case Undefined() =>
          print("(void 0)")

        case UndefinedParam() =>
          print("<undefined param>")

        case Null() =>
          print("null")

        case BooleanLiteral(value) =>
          print(if (value) "true" else "false")

        case IntLiteral(value) =>
          print(value)

        case DoubleLiteral(value) =>
          print(value)

        case StringLiteral(value, _) =>
          print("\"", escapeJS(value), "\"")

        // Atomic expressions

        case VarRef(ident, _) =>
          print(ident)

        case This() =>
          print("this")

        case Closure(thisType, args, resultType, body, captures) =>
          print("(lambda")
          printRow(captures, "<", ", ", ">")
          if (!jsMode && thisType != NoType)
            print("[this: ", thisType, "]")
          printSig(args, resultType)
          printBlock(body, isStat = resultType == NoType)
          print(")")

        case Function(thisType, args, resultType, body) =>
          print("(function")
          if (!jsMode && thisType != NoType)
            print("[this: ", thisType, "]")
          printSig(args, resultType)
          printBlock(body, isStat = true)
          print(")")

        // Type-related

        case Cast(expr, tpe) =>
          print(expr, ".cast[", tpe, "]")

        // Classes

        case ClassDef(name, kind, parent, ancestors, defs) =>
          kind match {
            case ClassKind.Class         => print("class ")
            case ClassKind.ModuleClass   => print("module class ")
            case ClassKind.Interface     => print("interface ")
            case ClassKind.RawJSType     => print("jstype ")
            case ClassKind.HijackedClass => print("hijacked class ")
            case ClassKind.TraitImpl     => print("trait impl ")
          }
          print(name)
          parent.foreach(print(" extends ", _))
          if (ancestors.nonEmpty)
            printRow(ancestors, " ancestors ", ", ", "")
          print(" ")
          printColumn(defs, "{", "", "}")
          println()

        case MethodDef(name, args, resultType, body) =>
          print(name)
          printSig(args, resultType)
          printBlock(body, isStat = resultType == NoType)

        case PropertyDef(name, _, _, _) =>
          // TODO
          print(s"<property: $name>")

        case ConstructorExportDef(fullName, args, body) =>
          print("export \"", escapeJS(fullName), "\"")
          printSig(args, NoType) // NoType as trick not to display a type
          printBlock(body, isStat = false)

        case ModuleExportDef(fullName) =>
          print("export \"", escapeJS(fullName), "\"")

        case _ =>
          print(s"<error, elem of class ${tree.getClass()}>")
      }
    }

    def printOptType(tpe: Type): Unit =
      if (!jsMode) print(": ", tpe)

    def printType(tpe: Type): Unit = tpe match {
      case AnyType              => print("any")
      case NothingType          => print("nothing")
      case UndefType            => print("void")
      case BooleanType          => print("boolean")
      case IntType              => print("int")
      case DoubleType           => print("number")
      case StringType           => print("string")
      case NullType             => print("null")
      case ClassType(className) => print(className)
      case DynType              => print("dyn")
      case NoType               => print("<notype>")

      case ArrayType(base, dims) =>
        print(base)
        for (i <- 1 to dims)
          print("[]")

      case RecordType(fields) =>
        print("(")
        var first = false
        for (RecordType.Field(name, _, tpe, mutable) <- fields) {
          if (first) first = false
          else print(", ")
          if (mutable)
            print("var ")
          print(name, ": ", tpe)
        }
        print(")")
    }

    def printIdent(ident: Ident): Unit =
      printString(escapeJS(ident.name))

    def print(args: Any*): Unit = args foreach {
      case tree: Tree =>
        printTree(tree, isStat = false)
      case tpe: Type =>
        printType(tpe)
      case ident: Ident =>
        printIdent(ident)
      case arg =>
        printString(if (arg == null) "null" else arg.toString)
    }

    protected def printString(s: String): Unit = {
      out.write(s)
    }

    def complete(): Unit = ()
  }

  def escapeJS(str: String): String = {
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

  class IRTreePrinterWithSourceMap(_out: Writer, jsMode: Boolean,
      sourceMap: SourceMapWriter) extends IRTreePrinter(_out, jsMode) {

    @deprecated("Use the constructor with jsMode", "0.5.3")
    def this(out: Writer, sourceMap: SourceMapWriter) =
      this(out, jsMode = true, sourceMap)

    private var column = 0

    override def printTree(tree: Tree, isStat: Boolean): Unit = {
      val pos = tree.pos
      if (pos.isDefined) {
        val originalName = tree match {
          case StringLiteral(_, origName) => origName
          case _ => None
        }
        sourceMap.startNode(column, pos, originalName)
      }

      super.printTree(tree, isStat)

      if (pos.isDefined)
        sourceMap.endNode(column)
    }

    override def printIdent(ident: Ident): Unit = {
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

    override def printString(s: String): Unit = {
      // assume no EOL char in s, and assume s only has ASCII characters
      super.printString(s)
      column += s.length()
    }

    override def complete(): Unit = {
      sourceMap.complete()
      super.complete()
    }
  }

  class InfoPrinter(val out: Writer) extends IndentationManager {
    def printClassInfo(classInfo: ClassInfo): Unit = {
      import classInfo._
      println("name: ", escapeJS(name))
      println("encodedName: ", escapeJS(encodedName))
      println("isExported: ", isExported)
      println("ancestorCount: ", ancestorCount)
      println("kind: ", kind)
      println("superClass: ", superClass)

      if (ancestors.nonEmpty) {
        println("ancestors: ",
            ancestors.map(escapeJS).mkString("[", ", ", "]"))
      }

      if (optimizerHints != OptimizerHints.empty)
        println("optimizerHints: ", optimizerHints)

      print("methods:")
      indent(); println()
      methods.foreach(printMethodInfo)
      undent(); println()
    }

    def printMethodInfo(methodInfo: MethodInfo): Unit = {
      import methodInfo._
      print(escapeJS(encodedName), ":")
      indent(); println()

      if (isAbstract)
        println("isAbstract: ", isAbstract)
      if (isExported)
        println("isExported: ", isExported)
      if (calledMethods.nonEmpty) {
        print("calledMethods:")
        indent(); println()
        printSeq(calledMethods.toList) { case (caller, callees) =>
          print(escapeJS(caller), ": ")
          print(callees.map(escapeJS).mkString("[", ", ", "]"))
        } { _ => println() }
        undent(); println()
      }
      if (calledMethodsStatic.nonEmpty) {
        print("calledMethodsStatic:")
        indent(); println()
        printSeq(calledMethodsStatic.toList) { case (caller, callees) =>
          print(escapeJS(caller), ": ")
          print(callees.map(escapeJS).mkString("[", ", ", "]"))
        } { _ => println() }
        undent(); println()
      }
      if (instantiatedClasses.nonEmpty) {
        println("instantiatedClasses: ",
            instantiatedClasses.map(escapeJS).mkString("[", ", ", "]"))
      }
      if (accessedModules.nonEmpty) {
        println("accessedModules: ",
            accessedModules.map(escapeJS).mkString("[", ", ", "]"))
      }
      if (accessedClassData.nonEmpty) {
        println("accessedClassData: ",
            accessedClassData.map(escapeJS).mkString("[", ", ", "]"))
      }
      if (optimizerHints != OptimizerHints.empty)
        println("optimizerHints: ", optimizerHints)

      undent(); println()
    }

    private def println(arg1: Any, args: Any*): Unit = {
      print((arg1 +: args): _*)
      println()
    }

    def print(args: Any*): Unit = args foreach {
      case classInfo: ClassInfo   => printClassInfo(classInfo)
      case methodInfo: MethodInfo => printMethodInfo(methodInfo)
      case arg                    => out.write(arg.toString())
    }

    def complete(): Unit = ()
  }

}
