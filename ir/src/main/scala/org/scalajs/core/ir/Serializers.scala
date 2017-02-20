/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import scala.annotation.switch

import java.io._
import java.net.URI

import scala.collection.mutable

import Definitions.isConstructorName
import Position._
import Trees._
import Types._
import Tags._

import Utils.JumpBackByteArrayOutputStream

object Serializers {
  def serialize(stream: OutputStream, tree: Tree): Unit = {
    new Serializer().serialize(stream, tree)
  }

  def deserialize(stream: InputStream, version: String): Tree = {
    new Deserializer(stream, version).deserialize()
  }

  // true for easier debugging (not for "production", it adds 8 bytes per node)
  private final val UseDebugMagic = false
  private final val DebugMagic = 0x3fa8ef84
  private final val PosDebugMagic = 0x65f0ec32

  private object PositionFormat {
    /* Positions are serialized incrementally as diffs wrt the last position.
     *
     * Formats are (the first byte is decomposed in bits):
     *
     *   1st byte | next bytes |  description
     *  -----------------------------------------
     *   ccccccc0 |            | Column diff (7-bit signed)
     *   llllll01 | CC         | Line diff (6-bit signed), column (8-bit unsigned)
     *   ____0011 | LL LL CC   | Line diff (16-bit signed), column (8-bit unsigned)
     *   ____0111 | 12 bytes   | File index, line, column (all 32-bit signed)
     *   11111111 |            | NoPosition (is not compared/stored in last position)
     *
     * Underscores are irrelevant and must be set to 0.
     */

    final val Format1Mask = 0x01
    final val Format1MaskValue = 0x00
    final val Format1Shift = 1

    final val Format2Mask = 0x03
    final val Format2MaskValue = 0x01
    final val Format2Shift = 2

    final val Format3Mask = 0x0f
    final val Format3MaskValue = 0x03

    final val FormatFullMask = 0x0f
    final val FormatFullMaskValue = 0x7

    final val FormatNoPositionValue = -1
  }

  private final class Serializer {
    private[this] val bufferUnderlying = new JumpBackByteArrayOutputStream
    private[this] val buffer = new DataOutputStream(bufferUnderlying)

    private[this] val files = mutable.ListBuffer.empty[URI]
    private[this] val fileIndexMap = mutable.Map.empty[URI, Int]
    private def fileToIndex(file: URI): Int =
      fileIndexMap.getOrElseUpdate(file, (files += file).size - 1)

    private[this] val strings = mutable.ListBuffer.empty[String]
    private[this] val stringIndexMap = mutable.Map.empty[String, Int]
    private def stringToIndex(str: String): Int =
      stringIndexMap.getOrElseUpdate(str, (strings += str).size - 1)

    private[this] var lastPosition: Position = Position.NoPosition

    def serialize(stream: OutputStream, tree: Tree): Unit = {
      // Write tree to buffer and record files and strings
      writeTree(tree)

      val s = new DataOutputStream(stream)

      // Emit the files
      s.writeInt(files.size)
      files.foreach(f => s.writeUTF(f.toString))

      // Emit the strings
      s.writeInt(strings.size)
      strings.foreach(s.writeUTF)

      // Paste the buffer
      bufferUnderlying.writeTo(s)

      s.flush()
    }

    def writeTree(tree: Tree): Unit = {
      import buffer._
      writePosition(tree.pos)
      tree match {
        case VarDef(ident, vtpe, mutable, rhs) =>
          writeByte(TagVarDef)
          writeIdent(ident); writeType(vtpe); writeBoolean(mutable); writeTree(rhs)

        case ParamDef(ident, ptpe, mutable, rest) =>
          writeByte(TagParamDef)
          writeIdent(ident); writeType(ptpe); writeBoolean(mutable); writeBoolean(rest)

        case Skip() =>
          writeByte(TagSkip)

        case Block(stats) =>
          writeByte(TagBlock)
          writeTrees(stats)

        case Labeled(label, tpe, body) =>
          writeByte(TagLabeled)
          writeIdent(label); writeType(tpe); writeTree(body)

        case Assign(lhs, rhs) =>
          writeByte(TagAssign)
          writeTree(lhs); writeTree(rhs)

        case Return(expr, label) =>
          writeByte(TagReturn)
          writeTree(expr); writeOptIdent(label)

        case If(cond, thenp, elsep) =>
          writeByte(TagIf)
          writeTree(cond); writeTree(thenp); writeTree(elsep)
          writeType(tree.tpe)

        case While(cond, body, label) =>
          writeByte(TagWhile)
          writeTree(cond); writeTree(body); writeOptIdent(label)

        case DoWhile(body, cond, label) =>
          writeByte(TagDoWhile)
          writeTree(body); writeTree(cond); writeOptIdent(label)

        case TryCatch(block, errVar, handler) =>
          writeByte(TagTryCatch)
          writeTree(block); writeIdent(errVar); writeTree(handler)
          writeType(tree.tpe)

        case TryFinally(block, finalizer) =>
          writeByte(TagTryFinally)
          writeTree(block); writeTree(finalizer)

        case Throw(expr) =>
          writeByte(TagThrow)
          writeTree(expr)

        case Continue(label) =>
          writeByte(TagContinue)
          writeOptIdent(label)

        case Match(selector, cases, default) =>
          writeByte(TagMatch)
          writeTree(selector)
          writeInt(cases.size)
          cases foreach { caze =>
            writeTrees(caze._1); writeTree(caze._2)
          }
          writeTree(default)
          writeType(tree.tpe)

        case Debugger() =>
          writeByte(TagDebugger)

        case New(cls, ctor, args) =>
          writeByte(TagNew)
          writeClassType(cls); writeIdent(ctor); writeTrees(args)

        case LoadModule(cls) =>
          writeByte(TagLoadModule)
          writeClassType(cls)

        case StoreModule(cls, value) =>
          writeByte(TagStoreModule)
          writeClassType(cls); writeTree(value)

        case Select(qualifier, item) =>
          writeByte(TagSelect)
          writeTree(qualifier); writeIdent(item)
          writeType(tree.tpe)

        case SelectStatic(cls, item) =>
          writeByte(TagSelectStatic)
          writeClassType(cls); writeIdent(item)
          writeType(tree.tpe)

        case Apply(receiver, method, args) =>
          writeByte(TagApply)
          writeTree(receiver); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyStatically(receiver, cls, method, args) =>
          writeByte(TagApplyStatically)
          writeTree(receiver); writeClassType(cls); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyStatic(cls, method, args) =>
          writeByte(TagApplyStatic)
          writeClassType(cls); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case UnaryOp(op, lhs) =>
          writeByte(TagUnaryOp)
          writeByte(op); writeTree(lhs)

        case BinaryOp(op, lhs, rhs) =>
          writeByte(TagBinaryOp)
          writeByte(op); writeTree(lhs); writeTree(rhs)

        case NewArray(tpe, lengths) =>
          writeByte(TagNewArray)
          writeArrayType(tpe); writeTrees(lengths)

        case ArrayValue(tpe, elems) =>
          writeByte(TagArrayValue)
          writeArrayType(tpe); writeTrees(elems)

        case ArrayLength(array) =>
          writeByte(TagArrayLength)
          writeTree(array)

        case ArraySelect(array, index) =>
          writeByte(TagArraySelect)
          writeTree(array); writeTree(index)
          writeType(tree.tpe)

        case RecordValue(tpe, elems) =>
          writeByte(TagRecordValue)
          writeType(tpe); writeTrees(elems)

        case IsInstanceOf(expr, cls) =>
          writeByte(TagIsInstanceOf)
          writeTree(expr); writeReferenceType(cls)

        case AsInstanceOf(expr, cls) =>
          writeByte(TagAsInstanceOf)
          writeTree(expr); writeReferenceType(cls)

        case Unbox(expr, charCode) =>
          writeByte(TagUnbox)
          writeTree(expr); writeByte(charCode.toByte)

        case GetClass(expr) =>
          writeByte(TagGetClass)
          writeTree(expr)

        case CallHelper(helper, args) =>
          writeByte(TagCallHelper)
          writeString(helper); writeTrees(args)
          writeType(tree.tpe)

        case JSNew(ctor, args) =>
          writeByte(TagJSNew)
          writeTree(ctor); writeTrees(args)

        case JSDotSelect(qualifier, item) =>
          writeByte(TagJSDotSelect)
          writeTree(qualifier); writeIdent(item)

        case JSBracketSelect(qualifier, item) =>
          writeByte(TagJSBracketSelect)
          writeTree(qualifier); writeTree(item)

        case JSFunctionApply(fun, args) =>
          writeByte(TagJSFunctionApply)
          writeTree(fun); writeTrees(args)

        case JSDotMethodApply(receiver, method, args) =>
          writeByte(TagJSDotMethodApply)
          writeTree(receiver); writeIdent(method); writeTrees(args)

        case JSBracketMethodApply(receiver, method, args) =>
          writeByte(TagJSBracketMethodApply)
          writeTree(receiver); writeTree(method); writeTrees(args)

        case JSSuperBracketSelect(cls, qualifier, item) =>
          writeByte(TagJSSuperBracketSelect)
          writeClassType(cls); writeTree(qualifier); writeTree(item)

        case JSSuperBracketCall(cls, receiver, method, args) =>
          writeByte(TagJSSuperBracketCall)
          writeClassType(cls); writeTree(receiver); writeTree(method); writeTrees(args)

        case JSSuperConstructorCall(args) =>
          writeByte(TagJSSuperConstructorCall)
          writeTrees(args)

        case LoadJSConstructor(cls) =>
          writeByte(TagLoadJSConstructor)
          writeClassType(cls)

        case LoadJSModule(cls) =>
          writeByte(TagLoadJSModule)
          writeClassType(cls)

        case JSSpread(items) =>
          writeByte(TagJSSpread)
          writeTree(items)

        case JSDelete(prop) =>
          writeByte(TagJSDelete)
          writeTree(prop)

        case JSUnaryOp(op, lhs) =>
          writeByte(TagJSUnaryOp)
          writeInt(op); writeTree(lhs)

        case JSBinaryOp(op, lhs, rhs) =>
          writeByte(TagJSBinaryOp)
          writeInt(op); writeTree(lhs); writeTree(rhs)

        case JSArrayConstr(items) =>
          writeByte(TagJSArrayConstr)
          writeTrees(items)

        case JSObjectConstr(fields) =>
          writeByte(TagJSObjectConstr)
          writeInt(fields.size)
          fields foreach { field =>
            writePropertyName(field._1); writeTree(field._2)
          }

        case JSLinkingInfo() =>
          writeByte(TagJSLinkingInfo)

        case Undefined() =>
          writeByte(TagUndefined)

        case Null() =>
          writeByte(TagNull)

        case BooleanLiteral(value) =>
          writeByte(TagBooleanLiteral)
          writeBoolean(value)

        case IntLiteral(value) =>
          writeByte(TagIntLiteral)
          writeInt(value)

        case LongLiteral(value) =>
          writeByte(TagLongLiteral)
          writeLong(value)

        case FloatLiteral(value) =>
          writeByte(TagFloatLiteral)
          writeFloat(value)

        case DoubleLiteral(value) =>
          writeByte(TagDoubleLiteral)
          writeDouble(value)

        case StringLiteral(value) =>
          writeByte(TagStringLiteral)
          writeString(value)

        case ClassOf(cls) =>
          writeByte(TagClassOf)
          writeReferenceType(cls)

        case UndefinedParam() =>
          writeByte(TagUndefinedParam)
          writeType(tree.tpe)

        case VarRef(ident) =>
          writeByte(TagVarRef)
          writeIdent(ident)
          writeType(tree.tpe)

        case This() =>
          writeByte(TagThis)
          writeType(tree.tpe)

        case Closure(captureParams, params, body, captureValues) =>
          writeByte(TagClosure)
          writeTrees(captureParams)
          writeTrees(params)
          writeTree(body)
          writeTrees(captureValues)

        case tree: ClassDef =>
          val ClassDef(name, kind, superClass, parents, jsNativeLoadSpec,
              defs) = tree
          writeByte(TagClassDef)
          writeIdent(name)
          writeByte(ClassKind.toByte(kind))
          writeOptIdent(superClass)
          writeIdents(parents)
          writeJSNativeLoadSpec(jsNativeLoadSpec)
          writeTrees(defs)
          writeInt(tree.optimizerHints.bits)

        case FieldDef(static, name, ftpe, mutable) =>
          writeByte(TagFieldDef)
          writeBoolean(static)
          writePropertyName(name)
          writeType(ftpe)
          writeBoolean(mutable)

        case methodDef: MethodDef =>
          val MethodDef(static, name, args, resultType, body) = methodDef

          writeByte(TagMethodDef)
          writeOptHash(methodDef.hash)

          // Prepare for back-jump and write dummy length
          bufferUnderlying.markJump()
          writeInt(-1)

          // Write out method def
          writeBoolean(static); writePropertyName(name)
          writeTrees(args); writeType(resultType); writeOptTree(body)
          writeInt(methodDef.optimizerHints.bits)

          // Jump back and write true length
          val length = bufferUnderlying.jumpBack()
          writeInt(length)
          bufferUnderlying.continue()

        case PropertyDef(static, name, getter, setterArgAndBody) =>
          writeByte(TagPropertyDef)
          writeBoolean(static)
          writePropertyName(name)
          writeOptTree(getter)
          writeBoolean(setterArgAndBody.isDefined)
          setterArgAndBody foreach { case (arg, body) =>
            writeTree(arg); writeTree(body)
          }

        case ConstructorExportDef(fullName, args, body) =>
          writeByte(TagConstructorExportDef)
          writeString(fullName); writeTrees(args); writeTree(body)

        case JSClassExportDef(fullName) =>
          writeByte(TagJSClassExportDef)
          writeString(fullName)

        case ModuleExportDef(fullName) =>
          writeByte(TagModuleExportDef)
          writeString(fullName)

        case TopLevelModuleExportDef(fullName) =>
          writeByte(TagTopLevelModuleExportDef)
          writeString(fullName)

        case TopLevelMethodExportDef(methodDef) =>
          writeByte(TagTopLevelMethodExportDef)
          writeTree(methodDef)

        case TopLevelFieldExportDef(fullName, field) =>
          writeByte(TagTopLevelFieldExportDef)
          writeString(fullName); writeIdent(field)
      }
      if (UseDebugMagic)
        writeInt(DebugMagic)
    }

    def writeTrees(trees: List[Tree]): Unit = {
      buffer.writeInt(trees.size)
      trees.foreach(writeTree)
    }

    def writeOptTree(optTree: Option[Tree]): Unit = {
      optTree.fold {
        writePosition(Position.NoPosition)
        buffer.writeByte(TagEmptyTree)
      } { tree =>
        writeTree(tree)
      }
    }

    def writeIdent(ident: Ident): Unit = {
      writePosition(ident.pos)
      writeString(ident.name); writeString(ident.originalName.getOrElse(""))
    }

    def writeIdents(idents: List[Ident]): Unit = {
      buffer.writeInt(idents.size)
      idents.foreach(writeIdent)
    }

    def writeOptIdent(optIdent: Option[Ident]): Unit = {
      buffer.writeBoolean(optIdent.isDefined)
      optIdent.foreach(writeIdent)
    }

    def writeType(tpe: Type): Unit = {
      tpe match {
        case AnyType     => buffer.write(TagAnyType)
        case NothingType => buffer.write(TagNothingType)
        case UndefType   => buffer.write(TagUndefType)
        case BooleanType => buffer.write(TagBooleanType)
        case IntType     => buffer.write(TagIntType)
        case LongType    => buffer.write(TagLongType)
        case FloatType   => buffer.write(TagFloatType)
        case DoubleType  => buffer.write(TagDoubleType)
        case StringType  => buffer.write(TagStringType)
        case NullType    => buffer.write(TagNullType)
        case NoType      => buffer.write(TagNoType)

        case tpe: ClassType =>
          buffer.write(TagClassType)
          writeClassType(tpe)

        case tpe: ArrayType =>
          buffer.write(TagArrayType)
          writeArrayType(tpe)

        case RecordType(fields) =>
          buffer.write(TagRecordType)
          buffer.writeInt(fields.size)
          for (RecordType.Field(name, originalName, tpe, mutable) <- fields) {
            writeString(name)
            writeString(originalName.getOrElse(""))
            writeType(tpe)
            buffer.writeBoolean(mutable)
          }
      }
    }

    def writeClassType(tpe: ClassType): Unit =
      writeString(tpe.className)

    def writeArrayType(tpe: ArrayType): Unit = {
      writeString(tpe.baseClassName)
      buffer.writeInt(tpe.dimensions)
    }

    def writeReferenceType(tpe: ReferenceType): Unit =
      writeType(tpe.asInstanceOf[Type])

    def writePropertyName(name: PropertyName): Unit = name match {
      case name: Ident =>
        buffer.writeByte(TagPropertyNameIdent)
        writeIdent(name)

      case name: StringLiteral =>
        buffer.writeByte(TagPropertyNameStringLiteral)
        writeTree(name)

      case ComputedName(tree, index) =>
        buffer.writeByte(TagPropertyNameComputedName)
        writeTree(tree)
        writeString(index)
    }

    def writePosition(pos: Position): Unit = {
      import buffer._
      import PositionFormat._

      def writeFull(): Unit = {
        writeByte(FormatFullMaskValue)
        writeInt(fileToIndex(pos.source))
        writeInt(pos.line)
        writeInt(pos.column)
      }

      if (pos == Position.NoPosition) {
        writeByte(FormatNoPositionValue)
      } else if (lastPosition == Position.NoPosition ||
          pos.source != lastPosition.source) {
        writeFull()
        lastPosition = pos
      } else {
        val line = pos.line
        val column = pos.column
        val lineDiff = line - lastPosition.line
        val columnDiff = column - lastPosition.column
        val columnIsByte = column >= 0 && column < 256

        if (lineDiff == 0 && columnDiff >= -64 && columnDiff < 64) {
          writeByte((columnDiff << Format1Shift) | Format1MaskValue)
        } else if (lineDiff >= -32 && lineDiff < 32 && columnIsByte) {
          writeByte((lineDiff << Format2Shift) | Format2MaskValue)
          writeByte(column)
        } else if (lineDiff >= Short.MinValue && lineDiff <= Short.MaxValue && columnIsByte) {
          writeByte(Format3MaskValue)
          writeShort(lineDiff)
          writeByte(column)
        } else {
          writeFull()
        }

        lastPosition = pos
      }

      if (UseDebugMagic)
        writeInt(PosDebugMagic)
    }

    def writeJSNativeLoadSpec(jsNativeLoadSpec: Option[JSNativeLoadSpec]): Unit = {
      import buffer._

      jsNativeLoadSpec.fold {
        writeByte(TagJSNativeLoadSpecNone)
      } { spec =>
        spec match {
          case JSNativeLoadSpec.Global(path) =>
            writeByte(TagJSNativeLoadSpecGlobal)
            writeStrings(path)

          case JSNativeLoadSpec.Import(module, path) =>
            writeByte(TagJSNativeLoadSpecImport)
            writeString(module)
            writeStrings(path)
        }
      }
    }

    def writeOptHash(optHash: Option[TreeHash]): Unit = {
      buffer.writeBoolean(optHash.isDefined)
      for (hash <- optHash) {
        buffer.write(hash.treeHash)
        buffer.write(hash.posHash)
      }
    }

    def writeString(s: String): Unit =
      buffer.writeInt(stringToIndex(s))

    def writeStrings(strings: List[String]): Unit = {
      buffer.writeInt(strings.size)
      strings.foreach(writeString)
    }
  }

  private final class Deserializer(stream: InputStream, sourceVersion: String) {
    private[this] val useHacks060 = sourceVersion == "0.6.0"
    private[this] val useHacks065 =
      Set("0.6.0", "0.6.3", "0.6.4", "0.6.5").contains(sourceVersion)
    private[this] val useHacks066 =
      useHacks065 || sourceVersion == "0.6.6"
    private[this] val useHacks068 =
      useHacks066 || sourceVersion == "0.6.8"
    private[this] val useHacks0614 =
      useHacks068 || Set("0.6.13", "0.6.14").contains(sourceVersion)

    private[this] val input = new DataInputStream(stream)

    private[this] val files =
      Array.fill(input.readInt())(new URI(input.readUTF()))

    private[this] val strings =
      Array.fill(input.readInt())(input.readUTF())

    private[this] var lastPosition: Position = Position.NoPosition

    private[this] var foundArguments: Boolean = false

    def deserialize(): Tree = {
      readTree()
    }

    def readTree(): Tree = {
      val pos = readPosition()
      readTreeFromTag(input.readByte())(pos)
    }

    def readOptTree(): Option[Tree] = {
      // TODO switch tag and position when we can break binary compat.
      val pos = readPosition()
      val tag = input.readByte()
      if (tag == TagEmptyTree) None
      else Some(readTreeFromTag(tag)(pos))
    }

    private def readTreeFromTag(tag: Byte)(implicit pos: Position): Tree = {
      import input._
      val result = (tag: @switch) match {
        case TagEmptyTree =>
          sys.error("Found invalid TagEmptyTree")

        case TagVarDef   => VarDef(readIdent(), readType(), readBoolean(), readTree())
        case TagParamDef =>
          ParamDef(readIdent(), readType(), readBoolean(),
              rest = if (useHacks060) false else readBoolean())

        case TagSkip     => Skip()
        case TagBlock    => Block(readTrees())
        case TagLabeled  => Labeled(readIdent(), readType(), readTree())
        case TagAssign   => Assign(readTree(), readTree())
        case TagReturn   => Return(readTree(), readOptIdent())
        case TagIf       => If(readTree(), readTree(), readTree())(readType())
        case TagWhile    => While(readTree(), readTree(), readOptIdent())
        case TagDoWhile  => DoWhile(readTree(), readTree(), readOptIdent())

        case TagTry =>
          if (!useHacks068) {
            sys.error("Invalid tag TagTry")
          }

          val block = readTree()
          val errVar = readIdent()
          val handler = readOptTree()
          val finalizer = readOptTree()
          val tpe = readType()

          val maybeCatch = handler.fold(block)(
              handler => TryCatch(block, errVar, handler)(tpe))
          finalizer.fold(maybeCatch)(
              finalizer => TryFinally(maybeCatch, finalizer))

        case TagTryCatch =>
          TryCatch(readTree(), readIdent(), readTree())(readType())

        case TagTryFinally =>
          TryFinally(readTree(), readTree())

        case TagThrow    => Throw(readTree())
        case TagContinue => Continue(readOptIdent())
        case TagMatch    =>
          Match(readTree(), List.fill(readInt()) {
            (readTrees().map(_.asInstanceOf[Literal]), readTree())
          }, readTree())(readType())
        case TagDebugger => Debugger()

        case TagNew             => New(readClassType(), readIdent(), readTrees())
        case TagLoadModule      => LoadModule(readClassType())
        case TagStoreModule     => StoreModule(readClassType(), readTree())
        case TagSelect          => Select(readTree(), readIdent())(readType())
        case TagSelectStatic    => SelectStatic(readClassType(), readIdent())(readType())
        case TagApply           => Apply(readTree(), readIdent(), readTrees())(readType())
        case TagApplyStatically =>
          val result1 =
            ApplyStatically(readTree(), readClassType(), readIdent(), readTrees())(readType())
          if (useHacks065 && result1.tpe != NoType && isConstructorName(result1.method.name))
            result1.copy()(NoType)
          else
            result1
        case TagApplyStatic     => ApplyStatic(readClassType(), readIdent(), readTrees())(readType())
        case TagUnaryOp         => UnaryOp(readByte(), readTree())
        case TagBinaryOp        => BinaryOp(readByte(), readTree(), readTree())
        case TagNewArray        => NewArray(readArrayType(), readTrees())
        case TagArrayValue      => ArrayValue(readArrayType(), readTrees())
        case TagArrayLength     => ArrayLength(readTree())
        case TagArraySelect     => ArraySelect(readTree(), readTree())(readType())
        case TagRecordValue     => RecordValue(readType().asInstanceOf[RecordType], readTrees())
        case TagIsInstanceOf    => IsInstanceOf(readTree(), readReferenceType())
        case TagAsInstanceOf    => AsInstanceOf(readTree(), readReferenceType())
        case TagUnbox           => Unbox(readTree(), readByte().toChar)
        case TagGetClass        => GetClass(readTree())
        case TagCallHelper      => CallHelper(readString(), readTrees())(readType())

        case TagJSNew                => JSNew(readTree(), readTrees())
        case TagJSDotSelect          => JSDotSelect(readTree(), readIdent())
        case TagJSBracketSelect      => JSBracketSelect(readTree(), readTree())
        case TagJSFunctionApply      => JSFunctionApply(readTree(), readTrees())
        case TagJSDotMethodApply     => JSDotMethodApply(readTree(), readIdent(), readTrees())
        case TagJSBracketMethodApply => JSBracketMethodApply(readTree(), readTree(), readTrees())
        case TagJSSuperBracketSelect => JSSuperBracketSelect(readClassType(), readTree(), readTree())
        case TagJSSuperBracketCall   =>
          JSSuperBracketCall(readClassType(), readTree(), readTree(), readTrees())
        case TagJSSuperConstructorCall => JSSuperConstructorCall(readTrees())
        case TagLoadJSConstructor    => LoadJSConstructor(readClassType())
        case TagLoadJSModule         => LoadJSModule(readClassType())
        case TagJSSpread             => JSSpread(readTree())
        case TagJSDelete             => JSDelete(readTree())
        case TagJSUnaryOp            => JSUnaryOp(readInt(), readTree())
        case TagJSBinaryOp           => JSBinaryOp(readInt(), readTree(), readTree())
        case TagJSArrayConstr        => JSArrayConstr(readTrees())
        case TagJSObjectConstr       =>
          JSObjectConstr(List.fill(readInt())((readPropertyName(), readTree())))
        case TagJSLinkingInfo        => JSLinkingInfo()

        case TagJSEnvInfo =>
          if (useHacks066)
            JSBracketSelect(JSLinkingInfo(), StringLiteral("envInfo"))
          else
            throw new MatchError(tag)

        case TagUndefined      => Undefined()
        case TagNull           => Null()
        case TagBooleanLiteral => BooleanLiteral(readBoolean())
        case TagIntLiteral     => IntLiteral(readInt())
        case TagLongLiteral    => LongLiteral(readLong())
        case TagFloatLiteral   => FloatLiteral(readFloat())
        case TagDoubleLiteral  => DoubleLiteral(readDouble())
        case TagStringLiteral  => StringLiteral(readString())
        case TagClassOf        => ClassOf(readReferenceType())

        case TagUndefinedParam => UndefinedParam()(readType())

        case TagVarRef =>
          val result = VarRef(readIdent())(readType())
          if (useHacks060 && result.ident.name == "arguments")
            foundArguments = true
          result

        case TagThis    => This()(readType())
        case TagClosure =>
          Closure(readParamDefs(), readParamDefs(), readTree(), readTrees())

        case TagClassDef =>
          val name = readIdent()
          val kind0 = ClassKind.fromByte(readByte())
          val superClass = readOptIdent()
          val parents = readIdents()
          val jsNativeLoadSpec = readJSNativeLoadSpec()
          val defs0 = readTrees()
          val defs = if (useHacks065) {
            defs0.filter {
              case MethodDef(_, Ident(name, _), _, _, _) =>
                !Definitions.isReflProxyName(name)
              case _ =>
                true
            }
          } else {
            defs0
          }
          val optimizerHints = new OptimizerHints(readInt())

          val kind = {
            if (useHacks068 && kind0 == ClassKind.AbstractJSType &&
                jsNativeLoadSpec.isDefined) {
              ClassKind.NativeJSClass
            } else {
              kind0
            }
          }

          ClassDef(name, kind, superClass, parents, jsNativeLoadSpec, defs)(
              optimizerHints)

        case TagFieldDef =>
          if (useHacks0614)
            FieldDef(static = false, readIdent(), readType(), readBoolean())
          else
            FieldDef(readBoolean(), readPropertyName(), readType(), readBoolean())

        case TagStringLitFieldDef if useHacks0614 =>
          FieldDef(static = false, readTree().asInstanceOf[StringLiteral],
              readType(), readBoolean())

        case TagMethodDef =>
          val optHash = readOptHash()
          // read and discard the length
          val len = readInt()
          assert(len >= 0)
          val result1 = MethodDef(readBoolean(), readPropertyName(),
              readParamDefs(), readType(), readOptTree())(
              new OptimizerHints(readInt()), optHash)
          val result2 = if (foundArguments) {
            foundArguments = false
            new RewriteArgumentsTransformer().transformMethodDef(result1)
          } else {
            result1
          }
          if (useHacks065 && result2.resultType != NoType &&
              isConstructorName(result2.name.encodedName)) {
            result2.copy(resultType = NoType, body = result2.body)(
                result2.optimizerHints, result2.hash)(
                result2.pos)
          } else {
            result2
          }

        case TagPropertyDef =>
          val static =
            if (useHacks0614) false
            else readBoolean()
          val name = readPropertyName()
          val getterBody = readOptTree()
          val setterArgAndBody = if (useHacks068) {
            val setterArg = readTree().asInstanceOf[ParamDef]
            readOptTree().map(setterBody => (setterArg, setterBody))
          } else {
            if (readBoolean())
              Some((readTree().asInstanceOf[ParamDef], readTree()))
            else
              None
          }
          PropertyDef(static, name, getterBody, setterArgAndBody)

        case TagConstructorExportDef =>
          val result = ConstructorExportDef(readString(), readParamDefs(), readTree())
          if (foundArguments) {
            foundArguments = false
            new RewriteArgumentsTransformer().transformConstructorExportDef(result)
          } else {
            result
          }

        case TagJSClassExportDef        => JSClassExportDef(readString())
        case TagModuleExportDef         => ModuleExportDef(readString())
        case TagTopLevelModuleExportDef => TopLevelModuleExportDef(readString())
        case TagTopLevelMethodExportDef => TopLevelMethodExportDef(readTree().asInstanceOf[MethodDef])
        case TagTopLevelFieldExportDef  => TopLevelFieldExportDef(readString(), readIdent())
      }
      if (UseDebugMagic) {
        val magic = readInt()
        assert(magic == DebugMagic,
            s"Bad magic after reading a ${result.getClass}!")
      }
      result
    }

    def readTrees(): List[Tree] =
      List.fill(input.readInt())(readTree())

    def readParamDefs(): List[ParamDef] =
      readTrees().map(_.asInstanceOf[ParamDef])

    def readIdent(): Ident = {
      implicit val pos = readPosition()
      val name = readString()
      val originalName = readString()
      Ident(name, if (originalName.isEmpty) None else Some(originalName))
    }

    def readIdents(): List[Ident] =
      List.fill(input.readInt())(readIdent())

    def readOptIdent(): Option[Ident] = {
      if (input.readBoolean()) Some(readIdent())
      else None
    }

    def readType(): Type = {
      val tag = input.readByte()
      (tag: @switch) match {
        case TagAnyType     => AnyType
        case TagNothingType => NothingType
        case TagUndefType   => UndefType
        case TagBooleanType => BooleanType
        case TagIntType     => IntType
        case TagLongType    => LongType
        case TagFloatType   => FloatType
        case TagDoubleType  => DoubleType
        case TagStringType  => StringType
        case TagNullType    => NullType
        case TagNoType      => NoType

        case TagClassType => readClassType()
        case TagArrayType => readArrayType()

        case TagRecordType =>
          RecordType(List.fill(input.readInt()) {
            val name = readString()
            val originalName = readString()
            val tpe = readType()
            val mutable = input.readBoolean()
            RecordType.Field(name,
                if (originalName.isEmpty) None else Some(originalName),
                tpe, mutable)
          })
      }
    }

    def readClassType(): ClassType =
      ClassType(readString())

    def readArrayType(): ArrayType =
      ArrayType(readString(), input.readInt())

    def readReferenceType(): ReferenceType =
      readType().asInstanceOf[ReferenceType]

    def readPropertyName(): PropertyName = {
      if (useHacks0614) {
        if (input.readBoolean()) readIdent()
        else readTree().asInstanceOf[StringLiteral]
      } else {
        input.readByte() match {
          case TagPropertyNameIdent =>
            readIdent()

          case TagPropertyNameStringLiteral =>
            readTree().asInstanceOf[StringLiteral]

          case TagPropertyNameComputedName =>
            ComputedName(readTree(), readString())
        }
      }
    }

    def readPosition(): Position = {
      import input._
      import PositionFormat._

      val first = readByte()

      val result = if (first == FormatNoPositionValue) {
        Position.NoPosition
      } else {
        val result = if ((first & FormatFullMask) == FormatFullMaskValue) {
          val file = files(readInt())
          val line = readInt()
          val column = readInt()
          Position(file, line, column)
        } else {
          assert(lastPosition != NoPosition,
              "Position format error: first position must be full")
          if ((first & Format1Mask) == Format1MaskValue) {
            val columnDiff = first >> Format1Shift
            Position(lastPosition.source, lastPosition.line,
                lastPosition.column + columnDiff)
          } else if ((first & Format2Mask) == Format2MaskValue) {
            val lineDiff = first >> Format2Shift
            val column = readByte() & 0xff // unsigned
            Position(lastPosition.source,
                lastPosition.line + lineDiff, column)
          } else {
            assert((first & Format3Mask) == Format3MaskValue,
                s"Position format error: first byte $first does not match any format")
            val lineDiff = readShort()
            val column = readByte() & 0xff // unsigned
            Position(lastPosition.source,
                lastPosition.line + lineDiff, column)
          }
        }
        lastPosition = result
        result
      }

      if (UseDebugMagic) {
        val magic = readInt()
        assert(magic == PosDebugMagic,
            s"Bad magic after reading position with first byte $first")
      }

      result
    }

    def readJSNativeLoadSpec(): Option[JSNativeLoadSpec] = {
      if (useHacks068) {
        Some(readString()).filter(_ != "").map { jsFullName =>
          JSNativeLoadSpec.Global(jsFullName.split("\\.").toList)
        }
      } else {
        (input.readByte(): @switch) match {
          case TagJSNativeLoadSpecNone =>
            None
          case TagJSNativeLoadSpecGlobal =>
            Some(JSNativeLoadSpec.Global(readStrings()))
          case TagJSNativeLoadSpecImport =>
            Some(JSNativeLoadSpec.Import(readString(), readStrings()))
        }
      }
    }

    def readOptHash(): Option[TreeHash] = {
      if (input.readBoolean()) {
        val treeHash = new Array[Byte](20)
        val posHash = new Array[Byte](20)
        input.readFully(treeHash)
        input.readFully(posHash)
        Some(new TreeHash(treeHash, posHash))
      } else None
    }

    def readString(): String = {
      strings(input.readInt())
    }

    def readStrings(): List[String] =
      List.fill(input.readInt())(readString())
  }

  private class RewriteArgumentsTransformer extends Transformers.Transformer {
    import RewriteArgumentsTransformer._

    private[this] var paramToIndex: Map[String, Int] = _

    def transformMethodDef(tree: MethodDef): MethodDef = {
      /* Ideally, we would re-hash the new MethodDef here, but we cannot do
       * that because it prevents the JS version of the tools to link.
       * Since the hashes of exported methods are not used by our pipeline
       * anyway, we simply put None.
       */
      val MethodDef(static, name, args, resultType, body) = tree
      setupParamToIndex(args)
      MethodDef(static, name, List(argumentsParamDef(tree.pos)),
          resultType, body.map(transform(_, isStat = resultType == NoType)))(
          tree.optimizerHints, None)(tree.pos)
    }

    def transformConstructorExportDef(
        tree: ConstructorExportDef): ConstructorExportDef = {
      val ConstructorExportDef(name, args, body) = tree
      setupParamToIndex(args)
      ConstructorExportDef(name, List(argumentsParamDef(tree.pos)),
          transformStat(body))(tree.pos)
    }

    private def setupParamToIndex(params: List[ParamDef]): Unit =
      paramToIndex = params.map(_.name.name).zipWithIndex.toMap

    private def argumentsParamDef(implicit pos: Position): ParamDef =
      ParamDef(Ident(ArgumentsName), AnyType, mutable = false, rest = true)

    private def argumentsRef(implicit pos: Position): VarRef =
      VarRef(Ident(ArgumentsName))(AnyType)

    override def transform(tree: Tree, isStat: Boolean): Tree = tree match {
      case VarRef(Ident(name, origName)) =>
        implicit val pos = tree.pos
        paramToIndex.get(name).fold {
          if (name == "arguments") argumentsRef
          else tree
        } { paramIndex =>
          JSBracketSelect(argumentsRef, IntLiteral(paramIndex))
        }

      case _ =>
        super.transform(tree, isStat)
    }
  }

  private object RewriteArgumentsTransformer {
    private final val ArgumentsName = "$arguments"
  }
}
