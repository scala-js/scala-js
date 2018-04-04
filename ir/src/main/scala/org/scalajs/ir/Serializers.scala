/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.ir

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
  /** Scala.js IR File Magic Number
   *
   *    CA FE : first part of magic number of Java class files
   *    4A 53 : "JS" in ASCII
   *
   */
  final val IRMagicNumber = 0xCAFE4A53

  def serialize(stream: OutputStream, classDef: ClassDef): Unit = {
    new Serializer().serialize(stream, classDef)
  }

  def deserializeEntryPointsInfo(stream: InputStream): EntryPointsInfo = {
    new Deserializer(stream).deserializeEntryPointsInfo()
  }

  def deserialize(stream: InputStream): ClassDef = {
    new Deserializer(stream).deserialize()
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

    def serialize(stream: OutputStream, classDef: ClassDef): Unit = {
      // Write tree to buffer and record files and strings
      writeClassDef(classDef)

      val s = new DataOutputStream(stream)

      // Write the Scala.js IR magic number
      s.writeInt(IRMagicNumber)

      // Write the Scala.js Version
      s.writeUTF(ScalaJSVersions.binaryEmitted)

      // Write the entry points info
      val entryPointsInfo = EntryPointsInfo.forClassDef(classDef)
      s.writeUTF(entryPointsInfo.encodedName)
      s.writeBoolean(entryPointsInfo.hasEntryPoint)

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
          writeTree(expr); writeIdent(label)

        case If(cond, thenp, elsep) =>
          writeByte(TagIf)
          writeTree(cond); writeTree(thenp); writeTree(elsep)
          writeType(tree.tpe)

        case While(cond, body) =>
          writeByte(TagWhile)
          writeTree(cond); writeTree(body)

        case DoWhile(body, cond) =>
          writeByte(TagDoWhile)
          writeTree(body); writeTree(cond)

        case ForIn(obj, keyVar, body) =>
          writeByte(TagForIn)
          writeTree(obj); writeIdent(keyVar); writeTree(body)

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

        case IsInstanceOf(expr, typeRef) =>
          writeByte(TagIsInstanceOf)
          writeTree(expr); writeTypeRef(typeRef)

        case AsInstanceOf(expr, typeRef) =>
          writeByte(TagAsInstanceOf)
          writeTree(expr); writeTypeRef(typeRef)

        case Unbox(expr, charCode) =>
          writeByte(TagUnbox)
          writeTree(expr); writeByte(charCode.toByte)

        case GetClass(expr) =>
          writeByte(TagGetClass)
          writeTree(expr)

        case JSNew(ctor, args) =>
          writeByte(TagJSNew)
          writeTree(ctor); writeTreeOrJSSpreads(args)

        case JSDotSelect(qualifier, item) =>
          writeByte(TagJSDotSelect)
          writeTree(qualifier); writeIdent(item)

        case JSBracketSelect(qualifier, item) =>
          writeByte(TagJSBracketSelect)
          writeTree(qualifier); writeTree(item)

        case JSFunctionApply(fun, args) =>
          writeByte(TagJSFunctionApply)
          writeTree(fun); writeTreeOrJSSpreads(args)

        case JSDotMethodApply(receiver, method, args) =>
          writeByte(TagJSDotMethodApply)
          writeTree(receiver); writeIdent(method); writeTreeOrJSSpreads(args)

        case JSBracketMethodApply(receiver, method, args) =>
          writeByte(TagJSBracketMethodApply)
          writeTree(receiver); writeTree(method); writeTreeOrJSSpreads(args)

        case JSSuperBracketSelect(superClass, qualifier, item) =>
          writeByte(TagJSSuperBracketSelect)
          writeTree(superClass); writeTree(qualifier); writeTree(item)

        case JSSuperBracketCall(superClass, receiver, method, args) =>
          writeByte(TagJSSuperBracketCall)
          writeTree(superClass); writeTree(receiver); writeTree(method); writeTreeOrJSSpreads(args)

        case JSSuperConstructorCall(args) =>
          writeByte(TagJSSuperConstructorCall)
          writeTreeOrJSSpreads(args)

        case LoadJSConstructor(cls) =>
          writeByte(TagLoadJSConstructor)
          writeClassType(cls)

        case LoadJSModule(cls) =>
          writeByte(TagLoadJSModule)
          writeClassType(cls)

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
          writeTreeOrJSSpreads(items)

        case JSObjectConstr(fields) =>
          writeByte(TagJSObjectConstr)
          writeInt(fields.size)
          fields foreach { field =>
            writePropertyName(field._1); writeTree(field._2)
          }

        case JSGlobalRef(ident) =>
          writeByte(TagJSGlobalRef)
          writeIdent(ident)

        case JSLinkingInfo() =>
          writeByte(TagJSLinkingInfo)

        case Undefined() =>
          writeByte(TagUndefined)

        case Null() =>
          writeByte(TagNull)

        case BooleanLiteral(value) =>
          writeByte(TagBooleanLiteral)
          writeBoolean(value)

        case CharLiteral(value) =>
          writeByte(TagCharLiteral)
          writeChar(value)

        case ByteLiteral(value) =>
          writeByte(TagByteLiteral)
          writeByte(value)

        case ShortLiteral(value) =>
          writeByte(TagShortLiteral)
          writeShort(value)

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

        case ClassOf(typeRef) =>
          writeByte(TagClassOf)
          writeTypeRef(typeRef)

        case VarRef(ident) =>
          writeByte(TagVarRef)
          writeIdent(ident)
          writeType(tree.tpe)

        case This() =>
          writeByte(TagThis)
          writeType(tree.tpe)

        case Closure(arrow, captureParams, params, body, captureValues) =>
          writeByte(TagClosure)
          writeBoolean(arrow)
          writeParamDefs(captureParams)
          writeParamDefs(params)
          writeTree(body)
          writeTrees(captureValues)

        case CreateJSClass(cls, captureValues) =>
          writeByte(TagCreateJSClass)
          writeClassRef(cls)
          writeTrees(captureValues)

        case Transient(value) =>
          throw new InvalidIRException(tree,
              "Cannot serialize a transient IR node (its value is of class " +
              s"${value.getClass})")
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

    def writeTreeOrJSSpreads(trees: List[TreeOrJSSpread]): Unit = {
      buffer.writeInt(trees.size)
      trees.foreach(writeTreeOrJSSpread)
    }

    def writeTreeOrJSSpread(tree: TreeOrJSSpread): Unit = {
      tree match {
        case JSSpread(items) =>
          writePosition(tree.pos)
          buffer.writeByte(TagJSSpread)
          writeTree(items)
        case tree: Tree =>
          writeTree(tree)
      }
    }

    def writeClassDef(classDef: ClassDef): Unit = {
      import buffer._
      import classDef._

      writePosition(classDef.pos)
      writeIdent(name)
      writeByte(ClassKind.toByte(kind))
      writeBoolean(jsClassCaptures.isDefined)
      jsClassCaptures.foreach(writeParamDefs(_))
      writeOptIdent(superClass)
      writeIdents(interfaces)
      writeOptTree(jsSuperClass)
      writeJSNativeLoadSpec(jsNativeLoadSpec)
      writeMemberDefs(memberDefs)
      writeTopLevelExportDefs(topLevelExportDefs)
      writeInt(OptimizerHints.toBits(optimizerHints))
    }

    def writeMemberDef(memberDef: MemberDef): Unit = {
      import buffer._
      writePosition(memberDef.pos)
      memberDef match {
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
          writeParamDefs(args); writeType(resultType); writeOptTree(body)
          writeInt(OptimizerHints.toBits(methodDef.optimizerHints))

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
            writeParamDef(arg); writeTree(body)
          }
      }
    }

    def writeMemberDefs(memberDefs: List[MemberDef]): Unit = {
      buffer.writeInt(memberDefs.size)
      memberDefs.foreach(writeMemberDef)
    }

    def writeTopLevelExportDef(topLevelExportDef: TopLevelExportDef): Unit = {
      import buffer._
      writePosition(topLevelExportDef.pos)
      topLevelExportDef match {
        case TopLevelConstructorExportDef(fullName, args, body) =>
          writeByte(TagTopLevelConstructorExportDef)
          writeString(fullName); writeParamDefs(args); writeTree(body)

        case TopLevelJSClassExportDef(fullName) =>
          writeByte(TagTopLevelJSClassExportDef)
          writeString(fullName)

        case TopLevelModuleExportDef(fullName) =>
          writeByte(TagTopLevelModuleExportDef)
          writeString(fullName)

        case TopLevelMethodExportDef(methodDef) =>
          writeByte(TagTopLevelMethodExportDef)
          writeMemberDef(methodDef)

        case TopLevelFieldExportDef(fullName, field) =>
          writeByte(TagTopLevelFieldExportDef)
          writeString(fullName); writeIdent(field)
      }
    }

    def writeTopLevelExportDefs(
        topLevelExportDefs: List[TopLevelExportDef]): Unit = {
      buffer.writeInt(topLevelExportDefs.size)
      topLevelExportDefs.foreach(writeTopLevelExportDef)
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

    def writeParamDef(paramDef: ParamDef): Unit = {
      writePosition(paramDef.pos)
      writeIdent(paramDef.name)
      writeType(paramDef.ptpe)
      buffer.writeBoolean(paramDef.mutable)
      buffer.writeBoolean(paramDef.rest)
    }

    def writeParamDefs(paramDefs: List[ParamDef]): Unit = {
      buffer.writeInt(paramDefs.size)
      paramDefs.foreach(writeParamDef)
    }

    def writeType(tpe: Type): Unit = {
      tpe match {
        case AnyType     => buffer.write(TagAnyType)
        case NothingType => buffer.write(TagNothingType)
        case UndefType   => buffer.write(TagUndefType)
        case BooleanType => buffer.write(TagBooleanType)
        case CharType    => buffer.write(TagCharType)
        case ByteType    => buffer.write(TagByteType)
        case ShortType   => buffer.write(TagShortType)
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
      writeString(tpe.arrayTypeRef.baseClassName)
      buffer.writeInt(tpe.arrayTypeRef.dimensions)
    }

    def writeTypeRef(tpe: TypeRef): Unit = tpe match {
      case ClassRef(className) =>
        buffer.writeByte(TagClassRef)
        writeString(className)
      case ArrayTypeRef(baseClassName, dimensions) =>
        buffer.writeByte(TagArrayTypeRef)
        writeString(baseClassName)
        buffer.writeInt(dimensions)
    }

    def writeClassRef(cls: ClassRef): Unit =
      writeString(cls.className)

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

      def writeGlobalSpec(spec: JSNativeLoadSpec.Global): Unit = {
        writeString(spec.globalRef)
        writeStrings(spec.path)
      }

      def writeImportSpec(spec: JSNativeLoadSpec.Import): Unit = {
        writeString(spec.module)
        writeStrings(spec.path)
      }

      jsNativeLoadSpec.fold {
        writeByte(TagJSNativeLoadSpecNone)
      } { spec =>
        spec match {
          case spec: JSNativeLoadSpec.Global =>
            writeByte(TagJSNativeLoadSpecGlobal)
            writeGlobalSpec(spec)

          case spec: JSNativeLoadSpec.Import =>
            writeByte(TagJSNativeLoadSpecImport)
            writeImportSpec(spec)

          case JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, globalSpec) =>
            writeByte(TagJSNativeLoadSpecImportWithGlobalFallback)
            writeImportSpec(importSpec)
            writeGlobalSpec(globalSpec)
        }
      }
    }

    def writeOptHash(optHash: Option[TreeHash]): Unit = {
      buffer.writeBoolean(optHash.isDefined)
      for (hash <- optHash)
        buffer.write(hash.hash)
    }

    def writeString(s: String): Unit =
      buffer.writeInt(stringToIndex(s))

    def writeStrings(strings: List[String]): Unit = {
      buffer.writeInt(strings.size)
      strings.foreach(writeString)
    }
  }

  private final class Deserializer(stream: InputStream) {
    private[this] val input = new DataInputStream(stream)

    private[this] var sourceVersion: String = _
    private[this] var files: Array[URI] = _
    private[this] var strings: Array[String] = _

    private[this] var lastPosition: Position = Position.NoPosition

    def deserializeEntryPointsInfo(): EntryPointsInfo = {
      sourceVersion = readHeader()
      readEntryPointsInfo()
    }

    def deserialize(): ClassDef = {
      sourceVersion = readHeader()
      readEntryPointsInfo() // discarded
      files = Array.fill(input.readInt())(new URI(input.readUTF()))
      strings = Array.fill(input.readInt())(input.readUTF())
      readClassDef()
    }

    /** Reads the Scala.js IR header and verifies the version compatibility.
     *
     *  @return the binary version that was read
     */
    private def readHeader(): String = {
      // Check magic number
      if (input.readInt() != IRMagicNumber)
        throw new IOException("Not a Scala.js IR file")

      // Check that we support this version of the IR
      val version = input.readUTF()
      val supported = ScalaJSVersions.binarySupported
      if (!supported.contains(version)) {
        throw new IRVersionNotSupportedException(version, supported,
            s"This version ($version) of Scala.js IR is not supported. " +
            s"Supported versions are: ${supported.mkString(", ")}")
      }

      version
    }

    private def readEntryPointsInfo(): EntryPointsInfo = {
      val encodedName = input.readUTF()
      val hasEntryPoint = input.readBoolean()
      new EntryPointsInfo(encodedName, hasEntryPoint)
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

    def readTreeOrJSSpread(): TreeOrJSSpread = {
      val pos = readPosition()
      val tag = input.readByte()
      if (tag == TagJSSpread)
        JSSpread(readTree())(pos)
      else
        readTreeFromTag(tag)(pos)
    }

    def readTreeOrJSSpreads(): List[TreeOrJSSpread] =
      List.fill(input.readInt())(readTreeOrJSSpread)

    private def readTreeFromTag(tag: Byte)(implicit pos: Position): Tree = {
      import input._
      val result = (tag: @switch) match {
        case TagEmptyTree =>
          throw new IOException("Found invalid TagEmptyTree")

        case TagVarDef  => VarDef(readIdent(), readType(), readBoolean(), readTree())
        case TagSkip    => Skip()
        case TagBlock   => Block(readTrees())
        case TagLabeled => Labeled(readIdent(), readType(), readTree())
        case TagAssign  => Assign(readTree(), readTree())
        case TagReturn  => Return(readTree(), readIdent())
        case TagIf      => If(readTree(), readTree(), readTree())(readType())
        case TagWhile   => While(readTree(), readTree())
        case TagDoWhile => DoWhile(readTree(), readTree())
        case TagForIn   => ForIn(readTree(), readIdent(), readTree())

        case TagTryCatch =>
          TryCatch(readTree(), readIdent(), readTree())(readType())

        case TagTryFinally =>
          TryFinally(readTree(), readTree())

        case TagThrow    => Throw(readTree())
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
        case TagApplyStatically => ApplyStatically(readTree(), readClassType(), readIdent(), readTrees())(readType())
        case TagApplyStatic     => ApplyStatic(readClassType(), readIdent(), readTrees())(readType())
        case TagUnaryOp         => UnaryOp(readByte(), readTree())
        case TagBinaryOp        => BinaryOp(readByte(), readTree(), readTree())
        case TagNewArray        => NewArray(readArrayType(), readTrees())
        case TagArrayValue      => ArrayValue(readArrayType(), readTrees())
        case TagArrayLength     => ArrayLength(readTree())
        case TagArraySelect     => ArraySelect(readTree(), readTree())(readType())
        case TagRecordValue     => RecordValue(readType().asInstanceOf[RecordType], readTrees())
        case TagIsInstanceOf    => IsInstanceOf(readTree(), readTypeRef())
        case TagAsInstanceOf    => AsInstanceOf(readTree(), readTypeRef())
        case TagUnbox           => Unbox(readTree(), readByte().toChar)
        case TagGetClass        => GetClass(readTree())

        case TagJSNew                => JSNew(readTree(), readTreeOrJSSpreads())
        case TagJSDotSelect          => JSDotSelect(readTree(), readIdent())
        case TagJSBracketSelect      => JSBracketSelect(readTree(), readTree())
        case TagJSFunctionApply      => JSFunctionApply(readTree(), readTreeOrJSSpreads())
        case TagJSDotMethodApply     => JSDotMethodApply(readTree(), readIdent(), readTreeOrJSSpreads())
        case TagJSBracketMethodApply => JSBracketMethodApply(readTree(), readTree(), readTreeOrJSSpreads())
        case TagJSSuperBracketSelect => JSSuperBracketSelect(readTree(), readTree(), readTree())
        case TagJSSuperBracketCall   =>
          JSSuperBracketCall(readTree(), readTree(), readTree(), readTreeOrJSSpreads())
        case TagJSSuperConstructorCall => JSSuperConstructorCall(readTreeOrJSSpreads())
        case TagLoadJSConstructor    => LoadJSConstructor(readClassType())
        case TagLoadJSModule         => LoadJSModule(readClassType())
        case TagJSDelete             => JSDelete(readTree())
        case TagJSUnaryOp            => JSUnaryOp(readInt(), readTree())
        case TagJSBinaryOp           => JSBinaryOp(readInt(), readTree(), readTree())
        case TagJSArrayConstr        => JSArrayConstr(readTreeOrJSSpreads())
        case TagJSObjectConstr       =>
          JSObjectConstr(List.fill(readInt())((readPropertyName(), readTree())))
        case TagJSGlobalRef          => JSGlobalRef(readIdent())
        case TagJSLinkingInfo        => JSLinkingInfo()

        case TagUndefined      => Undefined()
        case TagNull           => Null()
        case TagBooleanLiteral => BooleanLiteral(readBoolean())
        case TagCharLiteral    => CharLiteral(readChar())
        case TagByteLiteral    => ByteLiteral(readByte())
        case TagShortLiteral   => ShortLiteral(readShort())
        case TagIntLiteral     => IntLiteral(readInt())
        case TagLongLiteral    => LongLiteral(readLong())
        case TagFloatLiteral   => FloatLiteral(readFloat())
        case TagDoubleLiteral  => DoubleLiteral(readDouble())
        case TagStringLiteral  => StringLiteral(readString())
        case TagClassOf        => ClassOf(readTypeRef())

        case TagVarRef =>
          VarRef(readIdent())(readType())
        case TagThis =>
          This()(readType())
        case TagClosure =>
          Closure(readBoolean(), readParamDefs(), readParamDefs(), readTree(),
              readTrees())
        case TagCreateJSClass =>
          CreateJSClass(readClassRef(), readTrees())
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

    def readClassDef(): ClassDef = {
      import input._

      implicit val pos = readPosition()
      val name = readIdent()
      val kind = ClassKind.fromByte(readByte())
      val hasJSClassCaptures = readBoolean()
      val jsClassCaptures =
        if (!hasJSClassCaptures) None
        else Some(readParamDefs())
      val superClass = readOptIdent()
      val parents = readIdents()
      val jsSuperClass = readOptTree()
      val jsNativeLoadSpec = readJSNativeLoadSpec()
      val memberDefs = readMemberDefs()
      val topLevelExportDefs = readTopLevelExportDefs()
      val optimizerHints = OptimizerHints.fromBits(readInt())
      ClassDef(name, kind, jsClassCaptures, superClass, parents, jsSuperClass,
          jsNativeLoadSpec, memberDefs, topLevelExportDefs)(
          optimizerHints)
    }

    def readMemberDef(): MemberDef = {
      import input._

      implicit val pos = readPosition()
      val tag = input.readByte()

      (tag: @switch) match {
        case TagFieldDef =>
          FieldDef(readBoolean(), readPropertyName(), readType(), readBoolean())

        case TagMethodDef =>
          val optHash = readOptHash()
          // read and discard the length
          val len = readInt()
          assert(len >= 0)
          MethodDef(readBoolean(), readPropertyName(),
              readParamDefs(), readType(), readOptTree())(
              OptimizerHints.fromBits(readInt()), optHash)

        case TagPropertyDef =>
          val static = readBoolean()
          val name = readPropertyName()
          val getterBody = readOptTree()
          val setterArgAndBody = {
            if (readBoolean())
              Some((readParamDef(), readTree()))
            else
              None
          }
          PropertyDef(static, name, getterBody, setterArgAndBody)
      }
    }

    def readMemberDefs(): List[MemberDef] =
      List.fill(input.readInt())(readMemberDef())

    def readTopLevelExportDef(): TopLevelExportDef = {
      implicit val pos = readPosition()
      val tag = input.readByte()

      (tag: @switch) match {
        case TagTopLevelConstructorExportDef =>
          TopLevelConstructorExportDef(readString(), readParamDefs(),
              readTree())

        case TagTopLevelJSClassExportDef => TopLevelJSClassExportDef(readString())
        case TagTopLevelModuleExportDef  => TopLevelModuleExportDef(readString())
        case TagTopLevelMethodExportDef  => TopLevelMethodExportDef(readMemberDef().asInstanceOf[MethodDef])
        case TagTopLevelFieldExportDef   => TopLevelFieldExportDef(readString(), readIdent())
      }
    }

    def readTopLevelExportDefs(): List[TopLevelExportDef] =
      List.fill(input.readInt())(readTopLevelExportDef())

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

    def readParamDef(): ParamDef = {
      import input._

      implicit val pos = readPosition()
      ParamDef(readIdent(), readType(), readBoolean(), readBoolean())
    }

    def readParamDefs(): List[ParamDef] =
      List.fill(input.readInt())(readParamDef())

    def readType(): Type = {
      val tag = input.readByte()
      (tag: @switch) match {
        case TagAnyType     => AnyType
        case TagNothingType => NothingType
        case TagUndefType   => UndefType
        case TagBooleanType => BooleanType
        case TagCharType    => CharType
        case TagByteType    => ByteType
        case TagShortType   => ShortType
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
      ArrayType(ArrayTypeRef(readString(), input.readInt()))

    def readTypeRef(): TypeRef = {
      input.readByte() match {
        case TagClassRef =>
          ClassRef(readString())
        case TagArrayTypeRef =>
          ArrayTypeRef(readString(), input.readInt())
      }
    }

    def readClassRef(): ClassRef =
      ClassRef(readString())

    def readPropertyName(): PropertyName = {
      input.readByte() match {
        case TagPropertyNameIdent =>
          readIdent()
        case TagPropertyNameStringLiteral =>
          readTree().asInstanceOf[StringLiteral]
        case TagPropertyNameComputedName =>
          ComputedName(readTree(), readString())
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
      def readGlobalSpec(): JSNativeLoadSpec.Global =
        JSNativeLoadSpec.Global(readString(), readStrings())

      def readImportSpec(): JSNativeLoadSpec.Import =
        JSNativeLoadSpec.Import(readString(), readStrings())

      (input.readByte(): @switch) match {
        case TagJSNativeLoadSpecNone =>
          None
        case TagJSNativeLoadSpecGlobal =>
          Some(readGlobalSpec())
        case TagJSNativeLoadSpecImport =>
          Some(readImportSpec())
        case TagJSNativeLoadSpecImportWithGlobalFallback =>
          Some(JSNativeLoadSpec.ImportWithGlobalFallback(
              readImportSpec(), readGlobalSpec()))
      }
    }

    def readOptHash(): Option[TreeHash] = {
      if (input.readBoolean()) {
        val hash = new Array[Byte](20)
        input.readFully(hash)
        Some(new TreeHash(hash))
      } else None
    }

    def readString(): String = {
      strings(input.readInt())
    }

    def readStrings(): List[String] =
      List.fill(input.readInt())(readString())
  }
}
