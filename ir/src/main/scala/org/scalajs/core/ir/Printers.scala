/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import scala.annotation.switch

// Unimport default print and println to avoid invoking them by mistake
import scala.Predef.{print => _, println => _, _}

import java.io.Writer

import Position._
import Trees._
import Types._
import Infos._
import Utils.printEscapeJS

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
          print(tree)
          println()
      }
    }

    protected final def printColumn(ts: List[Tree], start: String, sep: String,
        end: String): Unit = {
      print(start); indent()
      var rest = ts
      while (rest.nonEmpty) {
        println()
        print(rest.head)
        rest = rest.tail
        if (rest.nonEmpty)
          print(sep)
      }
      undent(); println(); print(end)
    }

    protected final def printRow(ts: List[Tree], start: String, sep: String,
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
        print(": ")
        print(resultType)
        print(" = ")
      } else {
        print(' ')
      }
    }

    protected def printArgs(args: List[Tree]): Unit = {
      printRow(args, "(", ", ", ")")
    }

    def print(tree: Tree): Unit = {
      tree match {
        case EmptyTree =>
          print("<empty>")

        // Definitions

        case VarDef(ident, vtpe, mutable, rhs) =>
          if (mutable)
            print("var ")
          else
            print("val ")
          print(ident)
          print(": ")
          print(vtpe)
          print(" = ")
          print(rhs)

        case ParamDef(ident, ptpe, mutable, rest) =>
          if (mutable)
            print("var ")
          if (rest)
            print("...")
          print(ident)
          print(": ")
          print(ptpe)

        // Control flow constructs

        case Skip() =>
          print("/*<skip>*/")

        case tree: Block =>
          printBlock(tree)

        case Labeled(label, tpe, body) =>
          print(label)
          if (tpe != NoType) {
            print('[')
            print(tpe)
            print(']')
          }
          print(": ")
          printBlock(body)

        case Assign(lhs, rhs) =>
          print(lhs)
          print(" = ")
          print(rhs)

        case Return(expr, label) =>
          if (label.isEmpty) {
            print("return ")
            print(expr)
          } else {
            print("return(")
            print(label.get)
            print(") ")
            print(expr)
          }

        case If(cond, BooleanLiteral(true), elsep) =>
          print(cond)
          print(" || ")
          print(elsep)

        case If(cond, thenp, BooleanLiteral(false)) =>
          print(cond)
          print(" && ")
          print(thenp)

        case If(cond, thenp, elsep) =>
          print("if (")
          print(cond)
          print(") ")

          printBlock(thenp)
          elsep match {
            case Skip() => ()
            case If(_, _, _) =>
              print(" else ")
              print(elsep)
            case _ =>
              print(" else ")
              printBlock(elsep)
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

        case Try(block, errVar, handler, finalizer) =>
          print("try ")
          printBlock(block)
          if (handler != EmptyTree) {
            print(" catch (")
            print(errVar)
            print(") ")
            printBlock(handler)
          }
          if (finalizer != EmptyTree) {
            print(" finally ")
            printBlock(finalizer)
          }

        case Throw(expr) =>
          print("throw ")
          print(expr)

        case Continue(label) =>
          print("continue")
          if (label.isDefined) {
            print(' ')
            print(label.get)
          }

        case Match(selector, cases, default) =>
          print("match (")
          print(selector)
          print(") {"); indent
          for ((values, body) <- cases) {
            println()
            printRow(values, "case ", " | ", ":"); indent; println()
            print(body)
            print(";")
            undent
          }
          println()
          print("default:"); indent; println()
          print(default)
          print(";")
          undent
          undent; println(); print('}')

        case Debugger() =>
          print("debugger")

        // Scala expressions

        case New(cls, ctor, args) =>
          print("new ")
          print(cls)
          print("().")
          print(ctor)
          printArgs(args)

        case LoadModule(cls) =>
          print("mod:")
          print(cls)

        case StoreModule(cls, value) =>
          print("mod:")
          print(cls)
          print("<-")
          print(value)

        case Select(qualifier, item) =>
          print(qualifier)
          print('.')
          print(item)

        case Apply(receiver, method, args) =>
          print(receiver)
          print(".")
          print(method)
          printArgs(args)

        case ApplyStatically(receiver, cls, method, args) =>
          print(receiver)
          print(".")
          print(cls)
          print("::")
          print(method)
          printArgs(args)

        case ApplyStatic(cls, method, args) =>
          print(cls)
          print("::")
          print(method)
          printArgs(args)

        case UnaryOp(op, lhs) =>
          import UnaryOp._
          print('(')
          print((op: @switch) match {
            case Boolean_!                 => "!"
            case IntToLong | DoubleToLong  => "(long)"
            case DoubleToInt | LongToInt   => "(int)"
            case DoubleToFloat             => "(float)"
            case LongToDouble              => "(double)"
          })
          print(lhs)
          print(')')

        case BinaryOp(BinaryOp.Int_-, IntLiteral(0), rhs) =>
          print("(-")
          print(rhs)
          print(')')

        case BinaryOp(BinaryOp.Int_^, IntLiteral(-1), rhs) =>
          print("(~")
          print(rhs)
          print(')')

        case BinaryOp(BinaryOp.Long_-, LongLiteral(0L), rhs) =>
          print("(-")
          print(rhs)
          print(')')

        case BinaryOp(BinaryOp.Long_^, LongLiteral(-1L), rhs) =>
          print("(~")
          print(rhs)
          print(')')

        case BinaryOp(BinaryOp.Float_-, FloatLiteral(0.0f), rhs) =>
          print("(-")
          print(rhs)
          print(')')

        case BinaryOp(BinaryOp.Double_-,
            IntLiteral(0) | FloatLiteral(0.0f) | DoubleLiteral(0.0), rhs) =>
          print("(-")
          print(rhs)
          print(')')

        case BinaryOp(op, lhs, rhs) =>
          import BinaryOp._
          print('(')
          print(lhs)
          print(' ')
          print((op: @switch) match {
            case === => "==="
            case !== => "!=="

            case String_+ => "+[string]"

            case Int_+ => "+[int]"
            case Int_- => "-[int]"
            case Int_* => "*[int]"
            case Int_/ => "/[int]"
            case Int_% => "%[int]"

            case Int_|   => "|[int]"
            case Int_&   => "&[int]"
            case Int_^   => "^[int]"
            case Int_<<  => "<<[int]"
            case Int_>>> => ">>>[int]"
            case Int_>>  => ">>[int]"

            case Float_+ => "+[float]"
            case Float_- => "-[float]"
            case Float_* => "*[float]"
            case Float_/ => "/[float]"
            case Float_% => "%[float]"

            case Double_+ => "+[double]"
            case Double_- => "-[double]"
            case Double_* => "*[double]"
            case Double_/ => "/[double]"
            case Double_% => "%[double]"

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
          })
          print(' ')
          print(rhs)
          print(')')

        case NewArray(tpe, lengths) =>
          print("new ")
          print(tpe.baseClassName)
          for (length <- lengths) {
            print('[')
            print(length)
            print(']')
          }
          for (dim <- lengths.size until tpe.dimensions)
            print("[]")

        case ArrayValue(tpe, elems) =>
          print(tpe)
          printArgs(elems)

        case ArrayLength(array) =>
          print(array)
          print(".length")

        case ArraySelect(array, index) =>
          print(array)
          print('[')
          print(index)
          print(']')

        case RecordValue(tpe, elems) =>
          print('(')
          var first = true
          for ((field, value) <- tpe.fields zip elems) {
            if (first) first = false
            else print(", ")
            print(field.name)
            print(" = ")
            print(value)
          }
          print(')')

        case IsInstanceOf(expr, cls) =>
          print(expr)
          print(".isInstanceOf[")
          printRefType(cls)
          print(']')

        case AsInstanceOf(expr, cls) =>
          print(expr)
          print(".asInstanceOf[")
          printRefType(cls)
          print(']')

        case Unbox(expr, charCode) =>
          print(expr)
          print(".asInstanceOf[")
          print(charCode)
          print(']')

        case GetClass(expr) =>
          print(expr)
          print(".getClass()")

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
          if (containsOnlySelectsFromAtom(ctor)) {
            print("new ")
            print(ctor)
          } else {
            print("new (")
            print(ctor)
            print(')')
          }
          printArgs(args)

        case JSDotSelect(qualifier, item) =>
          print(qualifier)
          print(".")
          print(item)

        case JSBracketSelect(qualifier, item) =>
          print(qualifier)
          print('[')
          print(item)
          print(']')

        case JSFunctionApply(fun, args) =>
          fun match {
            case _:JSDotSelect | _:JSBracketSelect | _:Select =>
              print("(0, ")
              print(fun)
              print(')')

            case _ =>
              print(fun)
          }
          printArgs(args)

        case JSDotMethodApply(receiver, method, args) =>
          print(receiver)
          print(".")
          print(method)
          printArgs(args)

        case JSBracketMethodApply(receiver, method, args) =>
          print(receiver)
          print('[')
          print(method)
          print(']')
          printArgs(args)

        case JSSuperBracketSelect(cls, qualifier, item) =>
          print(qualifier)
          print('.')
          print(cls)
          print("::super[")
          print(item)
          print(']')

        case JSSuperBracketCall(cls, receiver, method, args) =>
          print(receiver)
          print('.')
          print(cls)
          print("::super[")
          print(method)
          print(']')
          printArgs(args)

        case JSSuperConstructorCall(args) =>
          print("super")
          printArgs(args)

        case LoadJSConstructor(cls) =>
          print("constructorOf[")
          print(cls)
          print(']')

        case LoadJSModule(cls) =>
          print("mod:")
          print(cls)

        case JSSpread(items) =>
          print("...")
          print(items)

        case JSDelete(prop) =>
          print("delete ")
          print(prop)

        case JSUnaryOp(op, lhs) =>
          import JSUnaryOp._
          print('(')
          print((op: @switch) match {
            case + => "+"
            case - => "-"
            case ~ => "~"
            case ! => "!"

            case `typeof` => "typeof "
          })
          print(lhs)
          print(")")

        case JSBinaryOp(op, lhs, rhs) =>
          import JSBinaryOp._
          print('(')
          print(lhs)
          print(" ")
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
          print(" ")
          print(rhs)
          print(')')

        case JSArrayConstr(items) =>
          printRow(items, "[", ", ", "]")

        case JSObjectConstr(Nil) =>
          print("{}")

        case JSObjectConstr(fields) =>
          print('{'); indent; println()
          var rest = fields
          while (rest.nonEmpty) {
            print(rest.head._1)
            print(": ")
            print(rest.head._2)
            rest = rest.tail
            if (rest.nonEmpty) {
              print(",")
              println()
            }
          }
          undent; println(); print('}')

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
          if (value >= 0) {
            print(value.toString)
          } else {
            print('(')
            print(value.toString)
            print(')')
          }

        case LongLiteral(value) =>
          if (value < 0L)
            print('(')
          print(value.toString)
          print('L')
          if (value < 0L)
            print(')')

        case FloatLiteral(value) =>
          if (value == 0.0f && 1.0f / value < 0.0f) {
            print("(-0f)")
          } else {
            if (value < 0.0f)
              print('(')
            print(value.toString)
            print('f')
            if (value < 0.0f)
              print(')')
          }

        case DoubleLiteral(value) =>
          if (value == 0.0 && 1.0 / value < 0.0) {
            print("(-0d)")
          } else {
            if (value < 0.0)
              print('(')
            print(value.toString)
            print('d')
            if (value < 0.0)
              print(')')
          }

        case StringLiteral(value) =>
          print('\"')
          printEscapeJS(value, out)
          print('\"')

        case ClassOf(cls) =>
          print("classOf[")
          printRefType(cls)
          print(']')

        // Specials

        case UndefinedParam() =>
          print("<undefined param>")

        // Atomic expressions

        case VarRef(ident) =>
          print(ident)

        case This() =>
          print("this")

        case Closure(captureParams, params, body, captureValues) =>
          print("(lambda<")
          var first = true
          for ((param, value) <- captureParams.zip(captureValues)) {
            if (first)
              first = false
            else
              print(", ")
            print(param)
            print(" = ")
            print(value)
          }
          printRow(params, ">(", ", ", ") = ")
          printBlock(body)
          print(')')

        // Classes

        case tree: ClassDef =>
          val ClassDef(name, kind, superClass, interfaces, jsName, defs) = tree
          print(tree.optimizerHints)
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
          superClass.foreach { cls =>
            print(" extends ")
            print(cls)
          }
          if (interfaces.nonEmpty) {
            print(" implements ")
            var rest = interfaces
            while (rest.nonEmpty) {
              print(rest.head)
              rest = rest.tail
              if (rest.nonEmpty)
                print(", ")
            }
          }
          jsName.foreach { name =>
            print(" jsname ")
            print(name)
          }
          print(" ")
          printColumn(defs, "{", "", "}")

        case FieldDef(name, vtpe, mutable) =>
          if (mutable)
            print("var ")
          else
            print("val ")
          print(name)
          print(": ")
          print(vtpe)

        case tree: MethodDef =>
          val MethodDef(static, name, args, resultType, body) = tree
          print(tree.optimizerHints)
          if (static)
            print("static ")
          print("def ")
          print(name)
          printSig(args, resultType)
          if (body == EmptyTree)
            print("<abstract>")
          else
            printBlock(body)

        case PropertyDef(name, getterBody, setterArg, setterBody) =>
          if (getterBody != EmptyTree) {
            print("get ")
            print(name)
            printSig(Nil, AnyType)
            printBlock(getterBody)

            if (setterBody != EmptyTree)
              println()
          }

          if (setterBody != EmptyTree) {
            print("set ")
            print(name)
            printSig(setterArg :: Nil, NoType)
            printBlock(setterBody)
          }

        case ConstructorExportDef(fullName, args, body) =>
          print("export \"")
          printEscapeJS(fullName, out)
          print('\"')
          printSig(args, NoType) // NoType as trick not to display a type
          printBlock(body)

        case JSClassExportDef(fullName) =>
          print("export class \"")
          printEscapeJS(fullName, out)
          print('\"')

        case ModuleExportDef(fullName) =>
          print("export module \"")
          printEscapeJS(fullName, out)
          print('\"')

        case _ =>
          print(s"<error, elem of class ${tree.getClass()}>")
      }
    }

    def printRefType(tpe: ReferenceType): Unit =
      print(tpe.asInstanceOf[Type])

    def print(tpe: Type): Unit = tpe match {
      case AnyType              => print("any")
      case NothingType          => print("nothing")
      case UndefType            => print("void")
      case BooleanType          => print("boolean")
      case IntType              => print("int")
      case LongType             => print("long")
      case FloatType            => print("float")
      case DoubleType           => print("double")
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
        var first = true
        for (RecordType.Field(name, _, tpe, mutable) <- fields) {
          if (first)
            first = false
          else
            print(", ")
          if (mutable)
            print("var ")
          print(name)
          print(": ")
          print(tpe)
        }
        print(')')
    }

    protected def print(ident: Ident): Unit =
      printEscapeJS(ident.name, out)

    private final def print(propName: PropertyName): Unit = propName match {
      case lit: StringLiteral => print(lit: Tree)
      case ident: Ident       => print(ident)
    }

    protected def print(s: String): Unit =
      out.write(s)

    protected def print(c: Int): Unit =
      out.write(c)

    protected def print(optimizerHints: OptimizerHints)(
        implicit dummy: DummyImplicit): Unit = {
      if (optimizerHints != OptimizerHints.empty) {
        print("@hints(")
        print(optimizerHints.bits.toString)
        print(") ")
      }
    }

    // Make it public
    override def println(): Unit = super.println()

    def complete(): Unit = ()
  }

  class InfoPrinter(protected val out: Writer) extends IndentationManager {
    def printClassInfoHeader(classInfo: ClassInfo): Unit = {
      import classInfo._
      print("encodedName: ")
      printEscapeJS(encodedName, out)
      println()
      print("isExported: ")
      print(isExported.toString)
      println()
      print("kind: ")
      print(kind.toString)
      println()
      print("superClass: ")
      print(if (superClass == null) "null" else superClass.toString)
      println()

      if (interfaces.nonEmpty) {
        print("interfaces: [")
        var rest = interfaces
        while (rest.nonEmpty) {
          printEscapeJS(rest.head, out)
          rest = rest.tail
          if (rest.nonEmpty)
            print(", ")
        }
        print(']')
        println()
      }
    }

    def print(classInfo: ClassInfo): Unit = {
      import classInfo._

      printClassInfoHeader(classInfo)

      print("methods:")
      indent(); println()
      methods.foreach((mi: MethodInfo) => print(mi))
      undent(); println()
    }

    def print(methodInfo: MethodInfo): Unit = {
      import methodInfo._
      printEscapeJS(encodedName, out)
      print(":")
      indent(); println()

      if (isStatic) {
        print("isStatic: ")
        print(isStatic.toString)
        println()
      }
      if (isAbstract) {
        print("isAbstract: ")
        print(isAbstract.toString)
        println()
      }
      if (isExported) {
        print("isExported: ")
        print(isExported.toString)
        println()
      }
      if (methodsCalled.nonEmpty) {
        print("methodsCalled:")
        indent(); println()
        val iter = methodsCalled.iterator
        while (iter.hasNext) {
          val (cls, callers) = iter.next()
          printEscapeJS(cls, out)
          printRow(callers, ": [", ", ", "]")
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
          printEscapeJS(cls, out)
          printRow(callers, ": [", ", ", "]")
          if (iter.hasNext)
            println()
        }
        undent(); println()
      }
      if (staticMethodsCalled.nonEmpty) {
        print("staticMethodsCalled:")
        indent(); println()
        val iter = staticMethodsCalled.iterator
        while (iter.hasNext) {
          val (cls, callers) = iter.next()
          printEscapeJS(cls, out)
          printRow(callers, ": [", ", ", "]")
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

    protected def print(s: String): Unit =
      out.write(s)

    protected def print(c: Int): Unit =
      out.write(c)

    def complete(): Unit = ()
  }

  abstract class RawIndentationManager extends IndentationManager {
    private var indentSize = 0
    private val indentString = "  "

    override def indent(): Unit = indentSize += 1
    override def undent(): Unit = indentSize -= 1

    protected def print(s: String): Unit = {
      out.write(indentString * indentSize)
      out.write(s)
    }

    protected def println(s: String): Unit =
      print(s + "\n")

    override protected def println(): Unit =
      out.write("\n")
  }

  class IRASTPrinter(protected val out: Writer) extends RawIndentationManager {
    /** Pretty-prints the IR's AST nodes.
      */
    def printTopLevelTree(tree: Tree): Unit =
      println(tree)

    private final def println(tree: Tree): Unit = {
      print(tree)
      out.write("\n")
    }

    private final def printList(ts: List[Tree]): Unit = {
      if (ts.isEmpty) {
        print("List ()")
      } else {
        println("List (")
        indent()
        var rest = ts
        while (rest.nonEmpty) {
          print(rest.head)
          rest = rest.tail
          if (rest.nonEmpty)
            println()
        }
        undent()
        paren()
      }
    }

    private final def printlnList(ts: List[Tree]): Unit = {
      printList(ts)
      println()
    }

    private final def printIdentList(is: List[Ident]): Unit = {
      if (is.isEmpty) {
        print("List ()")
      } else {
        println("List (")
        indent()
        var rest = is
        while (rest.nonEmpty) {
          print(rest.head)
          rest = rest.tail
          if (rest.nonEmpty)
            println()
        }
        undent()
        paren()
      }
    }

    private final def printlnIdentList(is: List[Ident]): Unit = {
      printIdentList(is)
      println()
    }

    private final def print(ident: Ident): Unit =
      print(s"Ident(${ident.name}, ${ident.originalName})")

    private final def println(ident: Ident): Unit = {
      print(ident)
      out.write("\n")
    }

    private def print(tpe: Type): Unit =
      print(tpe.toString)


    private final def println(tp: Type): Unit = {
      print(tp)
      out.write("\n")
    }

    private final def paren(): Unit = {
      out.write(")")
    }

    private final def print(tree: Tree): Unit = {
      tree match {
        case EmptyTree =>
          print("EmptyTree")

        // Definitions

        case VarDef(ident, vtpe, mutable, rhs) =>
          println("VarDef (")
          indent()
          println(ident)
          println(vtpe)
          println("mutable = " + mutable.toString)
          print(rhs); paren()
          undent()


        case ParamDef(ident, ptpe, mutable, rest) =>
          println("ParamDef (")
          indent()
          println(ident)
          println(ptpe)
          println("mutable = " + mutable.toString)
          print("rest = " + rest.toString); paren()
          undent()

        // Control flow constructs

        case Skip() =>
          print("Skip")

        case Block(trees) =>
          printList(trees)

        case Labeled(label, tpe, body) =>
          println("Labeled (")
          indent()
          println(tpe)
          print(body); paren()
          undent()

        case Assign(lhs, rhs) =>
          println("Assign (")
          indent()
          println(lhs)
          print(rhs); paren()
          undent()

        case Return(expr, label) =>
          println("Return (")
          indent()
          println(expr)
          label match {
            case Some(l) => print(l)
            case None    => print("None")
          }
          paren()
          undent()

        case If(cond, thenp, elsep) =>
          println("If (")
          indent()
          println(cond)
          println(thenp)
          print(elsep); paren()
          undent()

        case While(cond, body, label) =>
          println("While (")
          indent()
          println(cond)
          print(body); paren()
          undent()

        case DoWhile(body, cond, label) =>
          println("DoWhile (")
          indent()
          println(body)
          println(cond)
          label match {
            case Some(l) => print(l)
            case None    => print("None")
          }
          paren()
          undent()

        case Try(block, errVar, handler, finalizer) =>
          println("Try (")
          indent()
          println(block)
          println(errVar)
          println(handler)
          print(finalizer); paren()
          undent()

        case Throw(expr) =>
          println("Throw (")
          indent()
          print(expr); paren()
          undent()

        case Continue(label) =>
          println("Continue (")
          indent()
          label match {
            case Some(l) => print(l)
            case None    => print("None")
          }
          paren()
          undent()

        case Match(selector, cases, default) =>
          println("Match (")
          indent()
          println(selector)

          cases foreach { pair =>
            print(pair._1.toString()); println(":")
            indent(); print(pair._2); undent()
          }

          print(default); paren()
          undent()


        case Debugger() =>
          print("Debugger")

        // Scala expressions

        case New(cls, ctor, args) =>
          println("New (")
          indent()
          println(cls)
          println(ctor)
          printList(args); paren()
          undent()

        case LoadModule(cls) =>
          print("LoadModule (")
          print(cls); paren()

        case StoreModule(cls, value) =>
          println("StoreModule (")
          indent()
          println(cls)
          print(value); paren()
          undent()

        case Select(qualifier, item) =>
          println("Select (")
          indent()
          println(qualifier)
          print(item); paren()
          undent()

        case Apply(receiver, method, args) =>
          println("Apply (")
          indent()
          println(receiver)
          println(method)
          printList(args); paren()
          undent()

        case ApplyStatically(receiver, cls, method, args) =>
          println("ApplyStatically (")
          indent()
          println(receiver)
          println(cls)
          println(method)
          printList(args); paren()
          undent()

        case ApplyStatic(cls, method, args) =>
          print("ApplyStatic (")
          indent()
          println(cls)
          println(method)
          printList(args); paren()
          undent()

        case UnaryOp(op, lhs) =>
          println("UnaryOp (")
          indent()
          println(op.toString)
          print(lhs); paren()
          undent()

        case BinaryOp(op, lhs, rhs) =>
          println("BinaryOp (")
          indent()
          println(op.toString)
          println(lhs)
          print(rhs); paren()
          undent()

        case NewArray(tpe, lengths) =>
          println("NewArray (")
          indent()
          println(tpe)
          printList(lengths); paren()
          undent()

        case ArrayValue(tpe, elems) =>
          println("ArrayValue (")
          indent()
          println(tpe)
          printList(elems); paren()
          undent()

        case ArrayLength(array) =>
          println("ArrayLength (")
          indent()
          print(array); paren()
          undent()

        case ArraySelect(array, index) =>
          println("ArraySelect (")
          indent()
          println(array)
          print(index); paren()
          undent()

        case RecordValue(tpe, elems) =>
          println("RecordValue (")
          indent()
          println(tpe)
          printList(elems); paren()
          undent()

        case IsInstanceOf(expr, cls) =>
          println("IsInstanceOf (")
          indent()
          println(expr)
          printRefType(cls); paren()
          undent()

        case AsInstanceOf(expr, cls) =>
          println("AsInstanceOf (")
          indent()
          println(expr)
          printRefType(cls); paren()
          undent()

        case Unbox(expr, charCode) =>
          println("Unbox (")
          indent()
          println(expr)
          print(charCode); paren()
          undent()

        case GetClass(expr) =>
          println("GetClass (")
          indent()
          print(expr); paren()
          undent()

        case CallHelper(helper, args) =>
          println("CallHelper (")
          indent()
          println(helper)
          printList(args); paren()
          undent()

        // JavaScript expressions

        case JSNew(ctor, args) =>
          println("JSNew (")
          indent()
          println(ctor)
          printList(args); paren()
          undent()

        case JSDotSelect(qualifier, item) =>
          println("JSDotSelect (")
          indent()
          println(qualifier)
          print(item); paren()
          undent()

        case JSBracketSelect(qualifier, item) =>
          println("JSBracketSelect (")
          indent()
          println(qualifier)
          print(item); paren()
          undent()

        case JSFunctionApply(fun, args) =>
          println("JSFunctionApply (")
          indent()
          println(fun)
          printList(args); paren()
          undent()

        case JSDotMethodApply(receiver, method, args) =>
          println("JSDotMethodApply (")
          indent()
          println(receiver)
          println(method)
          printList(args); paren()
          undent()

        case JSBracketMethodApply(receiver, method, args) =>
          println("JSBracketMethodApply (")
          indent()
          println(receiver)
          println(method)
          printList(args); paren()
          undent()

        case JSSuperBracketSelect(cls, qualifier, item) =>
          println("JSSuperBracketSelect (")
          indent()
          println(qualifier)
          println(cls)
          print(item); paren()
          undent()

        case JSSuperBracketCall(cls, receiver, method, args) =>
          println("JSSuperBracketCall (")
          indent()
          println(receiver)
          println(cls)
          println(method)
          printList(args); paren()
          undent()

        case JSSuperConstructorCall(args) =>
          println("JSSuperConstructorCall (")
          indent()
          printList(args); paren()
          undent()

        case LoadJSConstructor(cls) =>
          println("LoadJSConstructor (")
          indent()
          print(cls); paren()
          undent()

        case LoadJSModule(cls) =>
          println("LoadJSModule (")
          indent()
          print(cls); paren()
          undent()

        case JSSpread(items) =>
          println("JSSpreas (")
          indent()
          print(items); paren()
          undent()

        case JSDelete(prop) =>
          println("JSDelete (")
          indent()
          print(prop); paren()
          undent()

        case JSUnaryOp(op, lhs) =>
          println("JSUnaryOp (")
          indent()
          print(lhs); paren()
          undent()

        case JSBinaryOp(op, lhs, rhs) =>
          println("JSBinaryOp (")
          indent()
          println(lhs)
          print(rhs); paren()
          undent()

        case JSArrayConstr(items) =>
          println("JSArrayConstr (")
          indent()
          printList(items); paren()
          undent()

        case JSObjectConstr(fields) =>
          println("JSObjectConstr (")
          indent()
          fields foreach { pair =>
            print(pair._1); print(": "); println(pair._2)
          }
          paren()
          undent()

        case JSLinkingInfo() =>
          print("JSLinkingInfo")

        case lit: Literal =>
          print(lit.toString)

        case UndefinedParam() =>
          print("UndefinedParam")

        // Atomic expressions

        case VarRef(ident) =>
          println("VarRef (")
          indent()
          print(ident); paren()
          undent()

        case This() =>
          print("This")

        case Closure(captureParams, params, body, captureValues) =>
          println("Closure (")
          indent()
          printlnList(captureParams)
          printlnList(params)
          println(body)
          printList(captureValues); paren()
          undent()

        // Classes

        case ClassDef(name, kind, superClass, interfaces, jsName, defs) =>
          println("ClassDef (")
          indent()
          println(name)
          println(kind.toString)

          superClass match {
            case Some(sc) => println(sc)
            case None     => println("None")
          }

          printlnIdentList(interfaces)
          println(jsName.toString)
          printList(defs); paren()
          undent()

        case FieldDef(name, vtpe, mutable) =>
          println("FieldDef (")
          indent()
          println(name.name)
          println(vtpe)
          print("mutable = " + mutable.toString + ")")
          undent()

        case MethodDef(static, name, args, resultType, body) =>
          println("MethodDef (")
          indent()
          println("static = " + static.toString)
          println(name.toString)
          printlnList(args)
          println(resultType)
          print(body); paren()
          undent()

        case PropertyDef(name, getterBody, setterArg, setterBody) =>
          println("PropertyDef (")
          indent()
          println(name.toString)
          println(getterBody)
          println(setterArg)
          print(setterBody); paren()
          undent()

        case ConstructorExportDef(fullName, args, body) =>
          println("ConstructorExportDef (")
          indent()
          println(fullName)
          printlnList(args)
          print(body)
          undent()

        case JSClassExportDef(fullName) =>
          println("JSClassExportDef (")
          indent()
          print(fullName + ")")
          undent()

        case ModuleExportDef(fullName) =>
          println("ModuleExportDef (")
          indent()
          print(fullName + ")")
          undent()

        case _ =>
          print(s"<error, elem of class ${tree.getClass()}>")
      }
    }

    private final def printRefType(tpe: ReferenceType): Unit =
      print(tpe.asInstanceOf[Type])

    private final def print(propName: PropertyName): Unit = propName match {
      case lit: StringLiteral => print(lit: Tree)
      case ident: Ident       => print(ident)
    }

    private def print(c: Int): Unit =
      print(c.toString)

    override def println(): Unit = super.println()

    def complete(): Unit = ()
  }

}
