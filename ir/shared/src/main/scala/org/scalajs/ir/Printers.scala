/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.ir

import scala.annotation.switch

// Unimport default print and println to avoid invoking them by mistake
import scala.Predef.{print => _, println => _, _}

import java.io.Writer

import Names._
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
      val trees = tree match {
        case Block(trees) => trees
        case _            => tree :: Nil
      }
      printBlock(trees)
    }

    protected def printBlock(trees: List[Tree]): Unit =
      printColumn(trees, "{", ";", "}")

    protected def printSig(args: List[ParamDef], restParam: Option[ParamDef],
        resultType: Type): Unit = {
      print("(")
      var rem = args
      while (rem.nonEmpty) {
        printAnyNode(rem.head)
        rem = rem.tail
        if (rem.nonEmpty || restParam.nonEmpty)
          print(", ")
      }

      restParam.foreach { p =>
        print("...")
        printAnyNode(p)
      }

      print(")")

      if (resultType != NoType) {
        print(": ")
        print(resultType)
        print(" = ")
      } else {
        print(' ')
      }
    }

    def printArgs(args: List[TreeOrJSSpread]): Unit = {
      printRow(args, "(", ", ", ")")
    }

    def printAnyNode(node: IRNode): Unit = {
      node match {
        case node: LocalIdent        => print(node)
        case node: LabelIdent        => print(node)
        case node: SimpleFieldIdent  => print(node)
        case node: FieldIdent        => print(node)
        case node: MethodIdent       => print(node)
        case node: ClassIdent        => print(node)
        case node: ParamDef          => print(node)
        case node: Tree              => print(node)
        case node: JSSpread          => print(node)
        case node: ClassDef          => print(node)
        case node: MemberDef         => print(node)
        case node: JSConstructorBody => printBlock(node.allStats)
        case node: TopLevelExportDef => print(node)
        case node: LinkTimeTree      => print(node)
      }
    }

    def print(paramDef: ParamDef): Unit = {
      val ParamDef(ident, originalName, ptpe, mutable) = paramDef
      if (mutable)
        print("var ")
      print(ident)
      print(originalName)
      print(": ")
      print(ptpe)
    }

    def print(tree: Tree): Unit = {
      tree match {
        // Definitions

        case VarDef(ident, originalName, vtpe, mutable, rhs) =>
          if (mutable)
            print("var ")
          else
            print("val ")
          print(ident)
          print(originalName)
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

        case LinkTimeIf(cond, thenp, elsep) =>
          print("linkTimeIf (")
          print(cond)
          print(") ")

          printBlock(thenp)
          print(" else ")
          printBlock(elsep)

        case While(cond, body) =>
          print("while (")
          print(cond)
          print(") ")
          printBlock(body)

        case ForIn(obj, keyVar, keyVarOriginalName, body) =>
          print("for (val ")
          print(keyVar)
          print(keyVarOriginalName)
          print(" in ")
          print(obj)
          print(") ")
          printBlock(body)

        case TryFinally(TryCatch(block, errVar, errVarOriginalName, handler), finalizer) =>
          print("try ")
          printBlock(block)
          print(" catch (")
          print(errVar)
          print(errVarOriginalName)
          print(") ")
          printBlock(handler)
          print(" finally ")
          printBlock(finalizer)

        case TryCatch(block, errVar, errVarOriginalName, handler) =>
          print("try ")
          printBlock(block)
          print(" catch (")
          print(errVar)
          print(errVarOriginalName)
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
          print(") {"); indent()
          for ((values, body) <- cases) {
            println()
            printRow(values, "case ", " | ", ":"); indent(); println()
            print(body)
            print(";")
            undent()
          }
          println()
          print("default:"); indent(); println()
          print(default)
          print(";")
          undent()
          undent(); println(); print('}')

        case Debugger() =>
          print("debugger")

        // Scala expressions

        case New(className, ctor, args) =>
          print("new ")
          print(className)
          print("().")
          print(ctor)
          printArgs(args)

        case LoadModule(className) =>
          print("mod:")
          print(className)

        case StoreModule() =>
          print("<storeModule>")

        case Select(qualifier, field) =>
          print(qualifier)
          print('.')
          print(field)

        case SelectStatic(field) =>
          print(field)

        case SelectJSNativeMember(className, member) =>
          print(className)
          print("::")
          print(member)

        case Apply(flags, receiver, method, args) =>
          print(receiver)
          print(".")
          print(method)
          printArgs(args)

        case ApplyStatically(flags, receiver, className, method, args) =>
          print(receiver)
          print(".")
          print(className)
          print("::")
          print(flags)
          print(method)
          printArgs(args)

        case ApplyStatic(flags, className, method, args) =>
          print(className)
          print("::")
          print(flags)
          print(method)
          printArgs(args)

        case ApplyDynamicImport(flags, className, method, args) =>
          print("dynamicImport ")
          print(className)
          print("::")
          print(flags)
          print(method)
          printArgs(args)

        case UnaryOp(UnaryOp.String_length, lhs) =>
          print(lhs)
          print(".length")

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
            case DoubleToFloat | LongToFloat =>
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

        case BinaryOp(BinaryOp.String_charAt, lhs, rhs) =>
          print(lhs)
          print('[')
          print(rhs)
          print(']')

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

        case NewArray(typeRef, lengths) =>
          print("new ")
          print(typeRef.base)
          for (length <- lengths) {
            print('[')
            print(length)
            print(']')
          }
          for (dim <- lengths.size until typeRef.dimensions)
            print("[]")

        case ArrayValue(typeRef, elems) =>
          print(typeRef)
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

        case RecordSelect(record, field) =>
          print(record)
          print('.')
          print(field)

        case IsInstanceOf(expr, testType) =>
          print(expr)
          print(".isInstanceOf[")
          print(testType)
          print(']')

        case AsInstanceOf(expr, tpe) =>
          print(expr)
          print(".asInstanceOf[")
          print(tpe)
          print(']')

        case GetClass(expr) =>
          print(expr)
          print(".getClass()")

        case Clone(expr) =>
          print("<clone>(")
          print(expr)
          print(')')

        case IdentityHashCode(expr) =>
          print("<identityHashCode>(")
          print(expr)
          print(')')

        case WrapAsThrowable(expr) =>
          print("<wrapAsThrowable>(")
          print(expr)
          print(")")

        case UnwrapFromThrowable(expr) =>
          print("<unwrapFromThrowable>(")
          print(expr)
          print(")")

        // JavaScript expressions

        case JSNew(ctor, args) =>
          def containsOnlySelectsFromAtom(tree: Tree): Boolean = tree match {
            case JSPrivateSelect(qual, _) => containsOnlySelectsFromAtom(qual)
            case JSSelect(qual, _)        => containsOnlySelectsFromAtom(qual)
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

        case JSPrivateSelect(qualifier, field) =>
          print(qualifier)
          print('.')
          print(field)

        case JSSelect(qualifier, item) =>
          print(qualifier)
          print('[')
          print(item)
          print(']')

        case JSFunctionApply(fun, args) =>
          fun match {
            case _:JSPrivateSelect | _:JSSelect | _:Select =>
              print("(0, ")
              print(fun)
              print(')')

            case _ =>
              print(fun)
          }
          printArgs(args)

        case JSMethodApply(receiver, method, args) =>
          print(receiver)
          print('[')
          print(method)
          print(']')
          printArgs(args)

        case JSSuperSelect(superClass, qualifier, item) =>
          print("super(")
          print(superClass)
          print(")::")
          print(qualifier)
          print('[')
          print(item)
          print(']')

        case JSSuperMethodCall(superClass, receiver, method, args) =>
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

        case JSImportCall(arg) =>
          print("import(")
          print(arg)
          print(')')

        case JSNewTarget() =>
          print("new.target")

        case JSImportMeta() =>
          print("import.meta")

        case LoadJSConstructor(className) =>
          print("constructorOf[")
          print(className)
          print(']')

        case LoadJSModule(className) =>
          print("mod:")
          print(className)

        case JSDelete(qualifier, item) =>
          print("delete ")
          print(qualifier)
          print('[')
          print(item)
          print(']')

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

            case ** => "**"
          })
          print(" ")
          print(rhs)
          print(')')

        case JSArrayConstr(items) =>
          printRow(items, "[", ", ", "]")

        case JSObjectConstr(Nil) =>
          print("{}")

        case JSObjectConstr(fields) =>
          print('{'); indent(); println()
          var rest = fields
          while (rest.nonEmpty) {
            val elem = rest.head
            elem._1 match {
              case key: StringLiteral =>
                print(key: Tree)
              case key =>
                print('[')
                print(key)
                print(']')
            }
            print(": ")
            print(rest.head._2)
            rest = rest.tail
            if (rest.nonEmpty) {
              print(",")
              println()
            }
          }
          undent(); println(); print('}')

        case JSGlobalRef(ident) =>
          print("global:")
          print(ident)

        case JSTypeOfGlobalRef(globalRef) =>
          print("(typeof ")
          print(globalRef)
          print(")")

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

        case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
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
          print(">")
          printSig(params, restParam, AnyType)
          printBlock(body)
          print(')')

        case CreateJSClass(className, captureValues) =>
          print("createjsclass[")
          print(className)
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
      print(originalName)
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
      printColumn(fields ::: methods ::: jsConstructor.toList :::
          jsMethodProps ::: jsNativeMembers ::: topLevelExportDefs, "{", "", "}")
    }

    def print(memberDef: MemberDef): Unit = {
      memberDef match {
        case FieldDef(flags, name, originalName, vtpe) =>
          print(flags.namespace.prefixString)
          if (flags.isMutable)
            print("var ")
          else
            print("val ")
          print(name)
          print(originalName)
          print(": ")
          print(vtpe)

        case JSFieldDef(flags, name, vtpe) =>
          print(flags.namespace.prefixString)
          if (flags.isMutable)
            print("var ")
          else
            print("val ")
          printJSMemberName(name)
          print(": ")
          print(vtpe)

        case tree: MethodDef =>
          val MethodDef(flags, name, originalName, args, resultType, body) = tree
          print(tree.optimizerHints)
          print(flags.namespace.prefixString)
          print("def ")
          print(name)
          print(originalName)
          printSig(args, restParam = None, resultType)
          body.fold {
            print("<abstract>")
          } { body =>
            printBlock(body)
          }

        case tree: JSConstructorDef =>
          val JSConstructorDef(flags, args, restParam, body) = tree
          print(tree.optimizerHints)
          print(flags.namespace.prefixString)
          print("def constructor")
          printSig(args, restParam, AnyType)
          printBlock(body.allStats)

        case tree: JSMethodDef =>
          val JSMethodDef(flags, name, args, restParam, body) = tree
          print(tree.optimizerHints)
          print(flags.namespace.prefixString)
          print("def ")
          printJSMemberName(name)
          printSig(args, restParam, AnyType)
          printBlock(body)

        case JSPropertyDef(flags, name, getterBody, setterArgAndBody) =>
          getterBody foreach { body =>
            print(flags.namespace.prefixString)
            print("get ")
            printJSMemberName(name)
            printSig(Nil, None, AnyType)
            printBlock(body)
          }

          if (getterBody.isDefined && setterArgAndBody.isDefined) {
            println()
          }

          setterArgAndBody foreach { case (arg, body) =>
            print(flags.namespace.prefixString)
            print("set ")
            printJSMemberName(name)
            printSig(arg :: Nil, None, NoType)
            printBlock(body)
          }

        case JSNativeMemberDef(flags, name, jsNativeLoadSpec) =>
          print(flags.namespace.prefixString)
          print("native ")
          print(name)
          print(" loadfrom ")
          print(jsNativeLoadSpec)
      }
    }

    def print(topLevelExportDef: TopLevelExportDef): Unit = {
      print("export top[moduleID=\"")
      printEscapeJS(topLevelExportDef.moduleID, out)
      print("\"] ")

      topLevelExportDef match {
        case TopLevelJSClassExportDef(_, exportName) =>
          print("class \"")
          printEscapeJS(exportName, out)
          print("\"")

        case TopLevelModuleExportDef(_, exportName) =>
          print("module \"")
          printEscapeJS(exportName, out)
          print("\"")

        case TopLevelMethodExportDef(_, methodDef) =>
          print(methodDef)

        case TopLevelFieldExportDef(_, exportName, field) =>
          print("static field ")
          print(field)
          print(" as \"")
          printEscapeJS(exportName, out)
          print("\"")
      }
    }

    def print(typeRef: TypeRef): Unit = typeRef match {
      case PrimRef(tpe) =>
        print(tpe)
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

    def print(ident: LocalIdent): Unit =
      print(ident.name)

    def print(ident: LabelIdent): Unit =
      print(ident.name)

    def print(ident: SimpleFieldIdent): Unit =
      print(ident.name)

    def print(ident: FieldIdent): Unit =
      print(ident.name)

    def print(ident: MethodIdent): Unit =
      print(ident.name)

    def print(ident: ClassIdent): Unit =
      print(ident.name)

    def print(name: Name): Unit =
      printEscapeJS(name.nameString, out)

    def print(name: FieldName): Unit =
      printEscapeJS(name.nameString, out)

    def print(name: MethodName): Unit =
      printEscapeJS(name.nameString, out)

    def print(originalName: OriginalName): Unit = {
      if (originalName.isDefined) {
        print('{')
        print(originalName.get.toString())
        print('}')
      }
    }

    def printJSMemberName(name: Tree): Unit = name match {
      case name: StringLiteral =>
        print(name)
      case _ =>
        print("[")
        print(name)
        print("]")
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

    def print(cond: LinkTimeTree): Unit = {
      import LinkTimeOp._
      cond match {
        case LinkTimeTree.BinaryOp(op, lhs, rhs) =>
          print(lhs)
          print(" ")
          print(op match {
            case Boolean_== => "=="
            case Boolean_!= => "!="
            case Boolean_|| => "||"
            case Boolean_&& => "&&"

            case Int_== => "=="
            case Int_!= => "!="
            case Int_<  => "<"
            case Int_<= => "<="
            case Int_>  => ">"
            case Int_>= => ">="
          })
          print(" ")
          print(rhs)
        case LinkTimeTree.BooleanConst(v) =>
          if (v) print("true") else print("false")
        case LinkTimeTree.IntConst(v) =>
          print(v.toString)
        case LinkTimeTree.Property(name, _) =>
          print("prop[")
          print(name)
          print("]")
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

    def print(flags: ApplyFlags)(
        implicit dummy1: DummyImplicit, dummy2: DummyImplicit): Unit = {
      if (flags.isPrivate)
        print("private::")
    }

    // Make it public
    override def println(): Unit = super.println()

    def complete(): Unit = ()
  }

}
