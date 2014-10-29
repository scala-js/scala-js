/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import scala.annotation.{switch, tailrec}

import java.io.Writer

import Position._
import Trees._
import Types._
import Infos._
import Utils.escapeJS

object Printers {

  /** Basically copied from scala.reflect.internal.Printers */
  trait IndentationManager {
    protected val out: Writer

    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    protected def indent(): Unit = indentMargin += indentStep
    protected def undent(): Unit = indentMargin -= indentStep

    protected def println(): Unit = {
      out.write('\n')
      while (indentMargin > indentString.length())
        indentString += indentString
      if (indentMargin > 0)
        out.write(indentString, 0, indentMargin)
    }

    @tailrec
    protected final def printSeq[A](ls: List[A])(printelem: A => Unit)(
        printsep: A => Unit): Unit = {
      ls match {
        case Nil =>
        case x :: Nil =>
          printelem(x)
        case x :: rest =>
          printelem(x)
          printsep(x)
          printSeq(rest)(printelem)(printsep)
      }
    }

    protected def printColumn(ts: List[Any], start: String, sep: String,
        end: String): Unit = {
      print(start); indent(); println()
      printSeq(ts) { x =>
        print(x)
      } { _ =>
        print(sep)
        println()
      }
      undent(); println(); print(end)
    }

    protected def printRow(ts: List[Any], start: String, sep: String,
        end: String): Unit = {
      print(start)
      printSeq(ts) { x =>
        print(x)
      } { _ =>
        print(sep)
      }
      print(end)
    }

    protected def printRow(ts: List[Any], sep: String): Unit =
      printRow(ts, "", sep, "")

    protected def print(args: Any*): Unit =
      args.foreach(printOne)

    protected def printOne(arg: Any): Unit
  }

  class IRTreePrinter(protected val out: Writer) extends IndentationManager {
    def printTopLevelTree(tree: Tree) {
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

    protected def printBlock(tree: Tree): Unit = {
      val trees = tree match {
        case Block(trees) => trees
        case _            => List(tree)
      }
      printColumn(trees, "{", ";", "}")
    }

    protected def printSig(args: List[ParamDef], resultType: Type): Unit = {
      printRow(args, "(", ", ", ")")
      if (resultType != NoType)
        print(": ", resultType, " = ")
      else
        print(" ")
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

        case ParamDef(ident, ptpe, mutable) =>
          if (mutable)
            print("var ")
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
            case `typeof`                  => "typeof"
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
            case VarRef(_, _)             => true
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

        case JSDelete(prop) =>
          print("delete ", prop)

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
          } { _ =>
            print(",")
            println()
          }
          undent; println(); print("}")

        case JSEnvInfo() =>
          print("<envinfo>")

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

        // Atomic expressions

        case VarRef(ident, _) =>
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
          printBlock(body)

        case PropertyDef(name, _, _, _) =>
          // TODO
          print(s"<property: $name>")

        case ConstructorExportDef(fullName, args, body) =>
          print("export \"", escapeJS(fullName), "\"")
          printSig(args, NoType) // NoType as trick not to display a type
          printBlock(body)

        case ModuleExportDef(fullName) =>
          print("export \"", escapeJS(fullName), "\"")

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

    protected def printString(s: String): Unit = {
      out.write(s)
    }

    // Make it public
    override def println(): Unit = super.println()

    def complete(): Unit = ()
  }

  class InfoPrinter(protected val out: Writer) extends IndentationManager {
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

    protected def printOne(arg: Any): Unit = arg match {
      case classInfo: ClassInfo   => printClassInfo(classInfo)
      case methodInfo: MethodInfo => printMethodInfo(methodInfo)
      case arg                    => out.write(arg.toString())
    }

    def complete(): Unit = ()
  }

}
