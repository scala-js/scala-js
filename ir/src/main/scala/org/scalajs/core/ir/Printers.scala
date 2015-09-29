/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import scala.annotation.{switch, tailrec}

import java.io.Writer

import Position._
import Trees._
import Types._
import Infos._
import Utils.escapeJS

object Printers {

  /** Basically copied from scala.reflect.internal.Printers */
  abstract class IndentationManager {
    protected val out: Writer

    private var indentMargin = 0
    private val indentStep = 2
    private var indentString = "                                        " // 40

    protected def indent(): Unit = indentMargin += indentStep
    protected def undent(): Unit = indentMargin -= indentStep

    protected def getIndentMargin(): Int = indentMargin

    protected def println(): Unit = {
      out.write('\n')
      while (indentMargin > indentString.length())
        indentString += indentString
      if (indentMargin > 0)
        out.write(indentString, 0, indentMargin)
    }
  }

  class IRTreePrinter(protected val out: Writer) extends IndentationManager {
    def printTopLevelTree(tree: Tree): Unit = {
      tree match {
        case Skip() =>
          // do not print anything
        case Block(stats) =>
          for (stat <- stats)
            printTopLevelTree(stat)
        case _ =>
          printTree(tree)
          println()
      }
    }

    protected final def printColumn(ts: List[Tree], start: String, sep: String,
        end: String): Unit = {
      printString(start); indent(); println()
      var rest = ts
      while (rest.nonEmpty) {
        printTree(rest.head)
        rest = rest.tail
        if (rest.nonEmpty) {
          printString(sep)
          println()
        }
      }
      undent(); println(); printString(end)
    }

    protected final def printRow(ts: List[Any], start: String, sep: String,
        end: String): Unit = {
      printString(start)
      var rest = ts
      while (rest.nonEmpty) {
        printOne(rest.head)
        rest = rest.tail
        if (rest.nonEmpty)
          printString(sep)
      }
      printString(end)
    }

    protected def printBlock(tree: Tree): Unit = {
      tree match {
        case Block(trees) =>
          printColumn(trees, "{", ";", "}")

        case _ =>
          print('{'); indent(); println()
          print(tree)
          undent(); println(); print('}')
      }
    }

    protected def printSig(args: List[ParamDef], resultType: Type): Unit = {
      printRow(args, "(", ", ", ")")
      if (resultType != NoType) {
        printString(": ")
        printOne(resultType)
        printString(" = ")
      } else {
        printChar(' ')
      }
    }

    protected def printArgs(args: List[Tree]): Unit = {
      printRow(args, "(", ", ", ")")
    }

    def printTree(tree: Tree): Unit = {
      tree match {
        case EmptyTree =>
          print("<empty>")

        // Definitions

        case VarDef(ident, vtpe, mutable, rhs) =>
          if (mutable)
            print("var ")
          else
            print("val ")
          print(ident, ": ", vtpe)
          if (rhs != EmptyTree)
            print(" = ", rhs)

        case ParamDef(ident, ptpe, mutable, rest) =>
          if (mutable)
            print("var ")
          if (rest)
            print("...")
          print(ident, ": ", ptpe)

        // Control flow constructs

        case Skip() =>
          print("/*<skip>*/")

        case tree: Block =>
          printBlock(tree)

        case Labeled(label, tpe, body) =>
          print(label)
          if (tpe != NoType)
            print("[", tpe, "]")
          print(": ")
          printBlock(body)

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case Return(expr, label) =>
          if (label.isEmpty) print("return ", expr)
          else print("return(", label.get, ") ", expr)

        case If(cond, BooleanLiteral(true), elsep) =>
          print(cond, " || ", elsep)
        case If(cond, thenp, BooleanLiteral(false)) =>
          print(cond, " && ", thenp)

        case If(cond, thenp, elsep) =>
          print("if (", cond, ") ")
          printBlock(thenp)
          elsep match {
            case Skip() => ()
            case If(_, _, _) =>
              print(" else ")
              printTree(elsep)
            case _ =>
              print(" else ")
              printBlock(elsep)
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

        case Continue(label) =>
          if (label.isEmpty) print("continue")
          else print("continue ", label.get)

        case Match(selector, cases, default) =>
          print("match (", selector, ") ")
          print("{"); indent
          for ((values, body) <- cases) {
            println()
            printRow(values, "case ", " | ", ":"); indent; println()
            printTree(body)
            print(";")
            undent
          }
          if (default != EmptyTree) {
            println()
            print("default:"); indent; println()
            printTree(default)
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

        case Select(qualifier, item) =>
          print(qualifier, ".", item)

        case Apply(receiver, method, args) =>
          print(receiver, ".", method)
          printArgs(args)

        case ApplyStatically(receiver, cls, method, args) =>
          print(receiver, ".", cls, "::", method)
          printArgs(args)

        case ApplyStatic(cls, method, args) =>
          print(cls, "::", method)
          printArgs(args)

        case UnaryOp(op, lhs) =>
          import UnaryOp._
          print("(", (op: @switch) match {
            case Boolean_!                 => "!"
            case IntToLong | DoubleToLong  => "(long)"
            case DoubleToInt | LongToInt   => "(int)"
            case DoubleToFloat             => "(float)"
            case LongToDouble              => "(double)"
          }, lhs, ")")

        case BinaryOp(BinaryOp.Int_-, IntLiteral(0), rhs) =>
          print("(-", rhs, ")")
        case BinaryOp(BinaryOp.Int_^, IntLiteral(-1), rhs) =>
          print("(~", rhs, ")")
        case BinaryOp(BinaryOp.Long_-, LongLiteral(0L), rhs) =>
          print("(-", rhs, ")")
        case BinaryOp(BinaryOp.Long_^, LongLiteral(-1L), rhs) =>
          print("(~", rhs, ")")
        case BinaryOp(BinaryOp.Float_-, FloatLiteral(0.0f), rhs) =>
          print("(-", rhs, ")")
        case BinaryOp(BinaryOp.Double_-,
            IntLiteral(0) | FloatLiteral(0.0f) | DoubleLiteral(0.0), rhs) =>
          print("(-", rhs, ")")

        case BinaryOp(op, lhs, rhs) =>
          import BinaryOp._
          print("(", lhs, " ", (op: @switch) match {
            case === => "==="
            case !== => "!=="

            case String_+ => "+[string]"

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

            case Float_+ => "+[float]"
            case Float_- => "-[float]"
            case Float_* => "*[float]"
            case Float_/ => "/[float]"
            case Float_% => "%[float]"

            case Double_+ => "+"
            case Double_- => "-"
            case Double_* => "*"
            case Double_/ => "/"
            case Double_% => "%"

            case Num_== => "=="
            case Num_!= => "!="
            case Num_<  => "<"
            case Num_<= => "<="
            case Num_>  => ">"
            case Num_>= => ">="

            case Long_+ => "+[long]"
            case Long_- => "-[long]"
            case Long_* => "*[long]"
            case Long_/ => "/[long]"
            case Long_% => "%[long]"

            case Long_|   => "|[long]"
            case Long_&   => "&[long]"
            case Long_^   => "^[long]"
            case Long_<<  => "<<[long]"
            case Long_>>> => ">>>[long]"
            case Long_>>  => ">>[long]"

            case Long_== => "==[long]"
            case Long_!= => "!=[long]"
            case Long_<  => "<[long]"
            case Long_<= => "<=[long]"
            case Long_>  => ">[long]"
            case Long_>= => ">=[long]"

            case Boolean_== => "==[bool]"
            case Boolean_!= => "!=[bool]"
            case Boolean_|  => "|[bool]"
            case Boolean_&  => "&[bool]"
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

        case Unbox(expr, charCode) =>
          print(expr, ".asInstanceOf[", charCode, "]")

        case GetClass(expr) =>
          print(expr, ".getClass()")

        case CallHelper(helper, args) =>
          print(helper)
          printArgs(args)

        // JavaScript expressions

        case JSNew(ctor, args) =>
          def containsOnlySelectsFromAtom(tree: Tree): Boolean = tree match {
            case JSDotSelect(qual, _)     => containsOnlySelectsFromAtom(qual)
            case JSBracketSelect(qual, _) => containsOnlySelectsFromAtom(qual)
            case VarRef(_)                => true
            case This()                   => true
            case _                        => false // in particular, Apply
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

        case JSSuperBracketSelect(cls, qualifier, item) =>
          print(qualifier, ".", cls, "::super[", item, "]")

        case JSSuperBracketCall(cls, receiver, method, args) =>
          print(receiver, ".", cls, "::super[", method, "]")
          printArgs(args)

        case JSSuperConstructorCall(args) =>
          print("super")
          printArgs(args)

        case LoadJSConstructor(cls) =>
          print("constructorOf[", cls, "]")

        case LoadJSModule(cls) =>
          print("mod:", cls)

        case JSSpread(items) =>
          print("...", items)

        case JSDelete(prop) =>
          print("delete ", prop)

        case JSUnaryOp(op, lhs) =>
          import JSUnaryOp._
          print("(", (op: @switch) match {
            case + => "+"
            case - => "-"
            case ~ => "~"
            case ! => "!"

            case `typeof` => "typeof "
          }, lhs, ")")

        case JSBinaryOp(op, lhs, rhs) =>
          import JSBinaryOp._
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

        case JSArrayConstr(items) =>
          printRow(items, "[", ", ", "]")

        case JSObjectConstr(Nil) =>
          print("{}")

        case JSObjectConstr(fields) =>
          print("{"); indent; println()
          var rest = fields
          while (rest.nonEmpty) {
            printOne(rest.head._1)
            printOne(": ")
            printOne(rest.head._2)
            rest = rest.tail
            if (rest.nonEmpty) {
              printString(",")
              println()
            }
          }
          undent; println(); print("}")

        case JSEnvInfo() =>
          print("<envinfo>")

        case JSLinkingInfo() =>
          print("<linkinginfo>")

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

        case FloatLiteral(value) =>
          if (value == 0.0f && 1.0f / value < 0.0f)
            print("(-0)")
          else if (value >= 0.0f)
            print(value)
          else
            print("(", value, ")")

        case DoubleLiteral(value) =>
          if (value == 0.0 && 1.0 / value < 0.0)
            print("(-0)")
          else if (value >= 0.0)
            print(value)
          else
            print("(", value, ")")

        case StringLiteral(value) =>
          print("\"", escapeJS(value), "\"")

        case ClassOf(cls) =>
          print("classOf[", cls, "]")

        // Specials

        case UndefinedParam() =>
          print("<undefined param>")

        // Atomic expressions

        case VarRef(ident) =>
          print(ident)

        case This() =>
          print("this")

        case Closure(captureParams, params, body, captureValues) =>
          print("(lambda")
          printRow(captureValues, "<", ", ", ">")
          printRow(captureParams ++ params, "(", ", ", ") = ")
          printBlock(body)
          print(")")

        // Classes

        case tree: ClassDef =>
          val ClassDef(name, kind, superClass, interfaces, jsName, defs) = tree
          if (tree.optimizerHints != OptimizerHints.empty)
            print("@hints(", tree.optimizerHints.bits, ") ")
          kind match {
            case ClassKind.Class         => print("class ")
            case ClassKind.ModuleClass   => print("module class ")
            case ClassKind.Interface     => print("interface ")
            case ClassKind.RawJSType     => print("jstype ")
            case ClassKind.HijackedClass => print("hijacked class ")
            case ClassKind.JSClass       => print("js class ")
            case ClassKind.JSModuleClass => print("js module class ")
          }
          print(name)
          superClass.foreach(print(" extends ", _))
          if (interfaces.nonEmpty)
            printRow(interfaces, " implements ", ", ", "")
          jsName.foreach(print(" jsname ", _))
          print(" ")
          printColumn(defs, "{", "", "}")
          println()

        case FieldDef(name, vtpe, mutable) =>
          if (mutable)
            print("var ")
          else
            print("val ")
          print(name, ": ", vtpe)

        case tree: MethodDef =>
          val MethodDef(static, name, args, resultType, body) = tree
          if (tree.optimizerHints != OptimizerHints.empty)
            print("@hints(", tree.optimizerHints.bits, ") ")
          if (static)
            print("static ")
          print("def ", name)
          printSig(args, resultType)
          if (body == EmptyTree)
            print("<abstract>")
          else
            printBlock(body)

        case PropertyDef(name, _, _, _) =>
          // TODO
          print("<property: ")
          print(name)
          print('>')

        case ConstructorExportDef(fullName, args, body) =>
          print("export \"", escapeJS(fullName), "\"")
          printSig(args, NoType) // NoType as trick not to display a type
          printBlock(body)

        case JSClassExportDef(fullName) =>
          print("export class \"", escapeJS(fullName), "\"")

        case ModuleExportDef(fullName) =>
          print("export module \"", escapeJS(fullName), "\"")

        case _ =>
          print(s"<error, elem of class ${tree.getClass()}>")
      }
    }

    def printType(tpe: Type): Unit = tpe match {
      case AnyType              => print("any")
      case NothingType          => print("nothing")
      case UndefType            => print("void")
      case BooleanType          => print("boolean")
      case IntType              => print("int")
      case LongType             => print("long")
      case FloatType            => print("float")
      case DoubleType           => print("number")
      case StringType           => print("string")
      case NullType             => print("null")
      case ClassType(className) => print(className)
      case NoType               => print("<notype>")

      case ArrayType(base, dims) =>
        print(base)
        for (i <- 1 to dims)
          print("[]")

      case RecordType(fields) =>
        print('(')
        var first = false
        for (RecordType.Field(name, _, tpe, mutable) <- fields) {
          if (first) first = false
          else print(", ")
          if (mutable)
            print("var ")
          print(name, ": ", tpe)
        }
        print(')')
    }

    protected def printIdent(ident: Ident): Unit =
      printString(escapeJS(ident.name))

    protected def printOne(arg: Any): Unit = arg match {
      case tree: Tree =>
        printTree(tree)
      case tpe: Type =>
        printType(tpe)
      case ident: Ident =>
        printIdent(ident)
      case arg =>
        printString(if (arg == null) "null" else arg.toString)
    }

    protected def printString(s: String): Unit =
      out.write(s)

    protected def printChar(c: Int): Unit =
      out.write(c)

    // Make it public
    override def println(): Unit = super.println()

    def complete(): Unit = ()
  }

  class InfoPrinter(protected val out: Writer) extends IndentationManager {
    def printClassInfoHeader(classInfo: ClassInfo): Unit = {
      import classInfo._
      println("encodedName: ", escapeJS(encodedName))
      println("isExported: ", isExported)
      println("kind: ", kind)
      println("superClass: ", superClass)

      if (interfaces.nonEmpty) {
        println("interfaces: ",
            interfaces.map(escapeJS).mkString("[", ", ", "]"))
      }
    }

    def printClassInfo(classInfo: ClassInfo): Unit = {
      import classInfo._

      printClassInfoHeader(classInfo)

      print("methods:")
      indent(); println()
      methods.foreach(printMethodInfo)
      undent(); println()
    }

    def printMethodInfo(methodInfo: MethodInfo): Unit = {
      import methodInfo._
      print(escapeJS(encodedName))
      print(":")
      indent(); println()

      if (isStatic)
        println("isStatic: ", isStatic)
      if (isAbstract)
        println("isAbstract: ", isAbstract)
      if (isExported)
        println("isExported: ", isExported)
      if (methodsCalled.nonEmpty) {
        print("methodsCalled:")
        indent(); println()
        val iter = methodsCalled.iterator
        while (iter.hasNext) {
          val (cls, callers) = iter.next()
          print(escapeJS(cls))
          print(": [")
          printRow(callers, ": [", ", ", "}")
          if (iter.hasNext)
            println()
        }
        undent(); println()
      }
      if (methodsCalledStatically.nonEmpty) {
        print("methodsCalledStatically:")
        indent(); println()
        val iter = methodsCalledStatically.iterator
        while (iter.hasNext) {
          val (cls, callers) = iter.next
          print(escapeJS(cls))
          printRow(callers, ": [", ", ", "}")
          if (iter.hasNext)
            println()
        }
        undent(); println()
      }
      if (staticMethodsCalled.nonEmpty) {
        print("staticMethodsCalled:")
        indent(); println()
        val iter = methodsCalledStatically.iterator
        while (iter.hasNext) {
          val (cls, callers) = iter.next()
          print(escapeJS(cls))
          printRow(callers, ": [", ", ", "}")
          if (iter.hasNext)
            println()
        }
        undent(); println()
      }
      if (instantiatedClasses.nonEmpty)
        printRow(instantiatedClasses, "instantiatedClasses: [", ", ", "]")
      if (accessedModules.nonEmpty)
        printRow(accessedModules, "accessedModules: [", ", ", "]")
      if (usedInstanceTests.nonEmpty)
        printRow(usedInstanceTests, "usedInstanceTests: [", ", ", "]")
      if (accessedClassData.nonEmpty)
        printRow(accessedClassData, "accessedClassData: [", ", ", "]")

      undent(); println()
    }

    protected def printRow(ts: List[String], start: String, sep: String,
        end: String): Unit = {
      print(start)
      var rest = ts
      while (rest.nonEmpty) {
        print(rest.head)
        rest = rest.tail
        if (rest.nonEmpty)
          print(sep)
      }
      print(end)
    }

    private final def println(arg1: Any, args: Any*): Unit = {
      printOne(arg1)
      var i = 0
      val len = args.length
      while (i < len) {
        printOne(args(i))
        i += 1
      }
      println()
    }

    protected def printOne(arg: Any): Unit = arg match {
      case classInfo: ClassInfo   => printClassInfo(classInfo)
      case methodInfo: MethodInfo => printMethodInfo(methodInfo)
      case arg                    => print(arg.toString())
    }

    protected def print(s: String): Unit =
      out.write(s)

    protected def print(c: Int): Unit =
      out.write(c)

    def complete(): Unit = ()
  }

}
