/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.ir

import scala.annotation.switch

// Unimport default print and println to avoid invoking them by mistake
import scala.Predef.{print => _, println => _, _}

import java.io.Writer

import Position._
import Trees._
import Types._
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
    protected final def printColumn(ts: List[IRNode], start: String,
        sep: String, end: String): Unit = {
      print(start); indent()
      var rest = ts
      while (rest.nonEmpty) {
        println()
        printAnyNode(rest.head)
        rest = rest.tail
        if (rest.nonEmpty)
          print(sep)
      }
      undent(); println(); print(end)
    }

    protected final def printRow(ts: List[IRNode], start: String, sep: String,
        end: String): Unit = {
      print(start)
      var rest = ts
      while (rest.nonEmpty) {
        printAnyNode(rest.head)
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

    protected def printArgs(args: List[TreeOrJSSpread]): Unit = {
      printRow(args, "(", ", ", ")")
    }

    def printAnyNode(node: IRNode): Unit = {
      node match {
        case node: Ident             => print(node)
        case node: ComputedName      => print(node)
        case node: ParamDef          => print(node)
        case node: Tree              => print(node)
        case node: JSSpread          => print(node)
        case node: ClassDef          => print(node)
        case node: MemberDef         => print(node)
        case node: TopLevelExportDef => print(node)
      }
    }

    def print(paramDef: ParamDef): Unit = {
      val ParamDef(ident, ptpe, mutable, rest) = paramDef

      if (mutable)
        print("var ")
      if (rest)
        print("...")
      print(ident)
      print(": ")
      print(ptpe)
    }

    def print(tree: Tree): Unit = {
      tree match {
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
          print("return@")
          print(label)
          print(" ")
          print(expr)

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

        case While(cond, body) =>
          print("while (")
          print(cond)
          print(") ")
          printBlock(body)

        case DoWhile(body, cond) =>
          print("do ")
          printBlock(body)
          print(" while (")
          print(cond)
          print(')')

        case ForIn(obj, keyVar, body) =>
          print("for (val ")
          print(keyVar)
          print(" in ")
          print(obj)
          print(") ")
          printBlock(body)

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

        case SelectStatic(cls, item) =>
          print(cls)
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
            case Boolean_! =>
              "!"
            case IntToChar =>
              "(char)"
            case IntToByte =>
              "(byte)"
            case IntToShort =>
              "(short)"
            case CharToInt | ByteToInt | ShortToInt | LongToInt | DoubleToInt =>
              "(int)"
            case IntToLong | DoubleToLong =>
              "(long)"
            case DoubleToFloat =>
              "(float)"
            case IntToDouble | LongToDouble | FloatToDouble =>
              "(double)"
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

            case Boolean_== => "==[bool]"
            case Boolean_!= => "!=[bool]"
            case Boolean_|  => "|[bool]"
            case Boolean_&  => "&[bool]"

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

            case Int_== => "==[int]"
            case Int_!= => "!=[int]"
            case Int_<  => "<[int]"
            case Int_<= => "<=[int]"
            case Int_>  => ">[int]"
            case Int_>= => ">=[int]"

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

            case Double_== => "==[double]"
            case Double_!= => "!=[double]"
            case Double_<  => "<[double]"
            case Double_<= => "<=[double]"
            case Double_>  => ">[double]"
            case Double_>= => ">=[double]"
          })
          print(' ')
          print(rhs)
          print(')')

        case NewArray(tpe, lengths) =>
          print("new ")
          print(tpe.arrayTypeRef.baseClassName)
          for (length <- lengths) {
            print('[')
            print(length)
            print(']')
          }
          for (dim <- lengths.size until tpe.arrayTypeRef.dimensions)
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

        case IsInstanceOf(expr, typeRef) =>
          print(expr)
          print(".isInstanceOf[")
          print(typeRef)
          print(']')

        case AsInstanceOf(expr, typeRef) =>
          print(expr)
          print(".asInstanceOf[")
          print(typeRef)
          print(']')

        case Unbox(expr, charCode) =>
          print(expr)
          print(".asInstanceOf[")
          print(charCode)
          print(']')

        case GetClass(expr) =>
          print(expr)
          print(".getClass()")

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

        case JSSuperBracketSelect(superClass, qualifier, item) =>
          print("super(")
          print(superClass)
          print(")::")
          print(qualifier)
          print('[')
          print(item)
          print(']')

        case JSSuperBracketCall(superClass, receiver, method, args) =>
          print("super(")
          print(superClass)
          print(")::")
          print(receiver)
          print('[')
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

        case JSGlobalRef(ident) =>
          print("global:")
          print(ident)

        case JSLinkingInfo() =>
          print("<linkinginfo>")

        // Literals

        case Undefined() =>
          print("(void 0)")

        case Null() =>
          print("null")

        case BooleanLiteral(value) =>
          print(if (value) "true" else "false")

        case CharLiteral(value) =>
          print('\'')
          printEscapeJS(value.toString(), out)
          print('\'')

        case ByteLiteral(value) =>
          if (value >= 0) {
            print(value.toString)
            print("_b")
          } else {
            print('(')
            print(value.toString)
            print("_b)")
          }

        case ShortLiteral(value) =>
          if (value >= 0) {
            print(value.toString)
            print("_s")
          } else {
            print('(')
            print(value.toString)
            print("_s)")
          }

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

        case ClassOf(typeRef) =>
          print("classOf[")
          print(typeRef)
          print(']')

        // Atomic expressions

        case VarRef(ident) =>
          print(ident)

        case This() =>
          print("this")

        case Closure(arrow, captureParams, params, body, captureValues) =>
          if (arrow)
            print("(arrow-lambda<")
          else
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

        case CreateJSClass(cls, captureValues) =>
          print("createjsclass[")
          print(cls)
          printRow(captureValues, "](", ", ", ")")

        // Transient

        case Transient(value) =>
          value.printIR(this)
      }
    }

    def print(spread: JSSpread): Unit = {
      print("...")
      print(spread.items)
    }

    def print(classDef: ClassDef): Unit = {
      import classDef._
      for (jsClassCaptures <- classDef.jsClassCaptures) {
        if (jsClassCaptures.isEmpty)
          print("captures: none")
        else
          printRow(jsClassCaptures, "captures: ", ", ", "")
        println()
      }
      print(classDef.optimizerHints)
      kind match {
        case ClassKind.Class               => print("class ")
        case ClassKind.ModuleClass         => print("module class ")
        case ClassKind.Interface           => print("interface ")
        case ClassKind.AbstractJSType      => print("abstract js type ")
        case ClassKind.HijackedClass       => print("hijacked class ")
        case ClassKind.JSClass             => print("js class ")
        case ClassKind.JSModuleClass       => print("js module class ")
        case ClassKind.NativeJSClass       => print("native js class ")
        case ClassKind.NativeJSModuleClass => print("native js module class ")
      }
      print(name)
      superClass.foreach { cls =>
        print(" extends ")
        print(cls)
        jsSuperClass.foreach { tree =>
          print(" (via ")
          print(tree)
          print(")")
        }
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
      jsNativeLoadSpec.foreach { spec =>
        print(" loadfrom ")
        print(spec)
      }
      print(" ")
      printColumn(memberDefs ::: topLevelExportDefs, "{", "", "}")
    }

    def print(memberDef: MemberDef): Unit = {
      memberDef match {
        case FieldDef(static, name, vtpe, mutable) =>
          if (static)
            print("static ")
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
          body.fold {
            print("<abstract>")
          } { body =>
            printBlock(body)
          }

        case PropertyDef(static, name, getterBody, setterArgAndBody) =>
          getterBody foreach { body =>
            if (static)
              print("static ")
            print("get ")
            print(name)
            printSig(Nil, AnyType)
            printBlock(body)
          }

          if (getterBody.isDefined && setterArgAndBody.isDefined) {
            println()
          }

          setterArgAndBody foreach { case (arg, body) =>
            if (static)
              print("static ")
            print("set ")
            print(name)
            printSig(arg :: Nil, NoType)
            printBlock(body)
          }
      }
    }

    def print(topLevelExportDef: TopLevelExportDef): Unit = {
      topLevelExportDef match {
        case TopLevelConstructorExportDef(fullName, args, body) =>
          print("export top constructor \"")
          printEscapeJS(fullName, out)
          print('\"')
          printSig(args, NoType) // NoType as trick not to display a type
          printBlock(body)

        case TopLevelJSClassExportDef(fullName) =>
          print("export top class \"")
          printEscapeJS(fullName, out)
          print('\"')

        case TopLevelModuleExportDef(fullName) =>
          print("export top module \"")
          printEscapeJS(fullName, out)
          print('\"')

        case TopLevelMethodExportDef(methodDef) =>
          print("export top ")
          print(methodDef)

        case TopLevelFieldExportDef(fullName, field) =>
          print("export top static field ")
          print(field)
          print(" as \"")
          printEscapeJS(fullName, out)
          print('\"')
      }
    }

    def print(tpe: TypeRef): Unit = tpe match {
      case ClassRef(className) =>
        print(className)
      case ArrayTypeRef(base, dims) =>
        print(base)
        for (i <- 1 to dims)
          print("[]")
    }

    def print(tpe: Type): Unit = tpe match {
      case AnyType              => print("any")
      case NothingType          => print("nothing")
      case UndefType            => print("void")
      case BooleanType          => print("boolean")
      case CharType             => print("char")
      case ByteType             => print("byte")
      case ShortType            => print("short")
      case IntType              => print("int")
      case LongType             => print("long")
      case FloatType            => print("float")
      case DoubleType           => print("double")
      case StringType           => print("string")
      case NullType             => print("null")
      case ClassType(className) => print(className)
      case NoType               => print("<notype>")

      case ArrayType(arrayTypeRef) =>
        print(arrayTypeRef)

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

    def print(ident: Ident): Unit =
      printEscapeJS(ident.name, out)

    def print(propName: PropertyName): Unit = propName match {
      case lit: StringLiteral => print(lit: Tree)
      case ident: Ident       => print(ident)

      case ComputedName(tree, index) =>
        print("[")
        print(tree)
        print("](")
        print(index)
        print(")")
    }

    def print(spec: JSNativeLoadSpec): Unit = {
      def printPath(path: List[String]): Unit = {
        for (propName <- path) {
          print("[\"")
          printEscapeJS(propName, out)
          print("\"]")
        }
      }

      spec match {
        case JSNativeLoadSpec.Global(globalRef, path) =>
          print("global:")
          print(globalRef)
          printPath(path)

        case JSNativeLoadSpec.Import(module, path) =>
          print("import(")
          print(module)
          print(')')
          printPath(path)

        case JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, globalSpec) =>
          print(importSpec)
          print(" fallback ")
          print(globalSpec)
      }
    }

    def print(s: String): Unit =
      out.write(s)

    def print(c: Int): Unit =
      out.write(c)

    def print(optimizerHints: OptimizerHints)(
        implicit dummy: DummyImplicit): Unit = {
      if (optimizerHints != OptimizerHints.empty) {
        print("@hints(")
        print(OptimizerHints.toBits(optimizerHints).toString)
        print(") ")
      }
    }

    // Make it public
    override def println(): Unit = super.println()

    def complete(): Unit = ()
  }

}
