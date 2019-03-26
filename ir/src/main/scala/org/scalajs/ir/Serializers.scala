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

import java.io._
import java.nio._
import java.net.URI

import scala.collection.mutable
import scala.concurrent._

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

  /** Deserializes entry points from the given buffer.
   *
   *  Sets the buffer's order to BIG_ENDAIN.
   *
   *  @throws java.nio.BufferUnderflowException if not enough data is available
   *      in the buffer. In this case the buffer's position is unspecified and
   *      needs to be reset by the caller.
   */
  def deserializeEntryPointsInfo(buf: ByteBuffer): EntryPointsInfo =
    new Deserializer(buf).deserializeEntryPointsInfo()

  /** Deserializes a class def from the given buffer.
   *
   *  Sets the buffer's order to BIG_ENDAIN.
   *
   *  @throws java.nio.BufferUnderflowException if not enough data is available
   *      in the buffer. In this case the buffer's position is unspecified and
   *      needs to be reset by the caller.
   */
  def deserialize(buf: ByteBuffer): ClassDef =
    new Deserializer(buf).deserialize()

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
          writeClassRef(cls); writeIdent(ctor); writeTrees(args)

        case LoadModule(cls) =>
          writeByte(TagLoadModule)
          writeClassRef(cls)

        case StoreModule(cls, value) =>
          writeByte(TagStoreModule)
          writeClassRef(cls); writeTree(value)

        case Select(qualifier, item) =>
          writeByte(TagSelect)
          writeTree(qualifier); writeIdent(item)
          writeType(tree.tpe)

        case SelectStatic(cls, item) =>
          writeByte(TagSelectStatic)
          writeClassRef(cls); writeIdent(item)
          writeType(tree.tpe)

        case Apply(flags, receiver, method, args) =>
          writeByte(TagApply)
          writeApplyFlags(flags); writeTree(receiver); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyStatically(flags, receiver, cls, method, args) =>
          writeByte(TagApplyStatically)
          writeApplyFlags(flags); writeTree(receiver); writeClassRef(cls); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyStatic(flags, cls, method, args) =>
          writeByte(TagApplyStatic)
          writeApplyFlags(flags); writeClassRef(cls); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case UnaryOp(op, lhs) =>
          writeByte(TagUnaryOp)
          writeByte(op); writeTree(lhs)

        case BinaryOp(op, lhs, rhs) =>
          writeByte(TagBinaryOp)
          writeByte(op); writeTree(lhs); writeTree(rhs)

        case NewArray(tpe, lengths) =>
          writeByte(TagNewArray)
          writeArrayTypeRef(tpe); writeTrees(lengths)

        case ArrayValue(tpe, elems) =>
          writeByte(TagArrayValue)
          writeArrayTypeRef(tpe); writeTrees(elems)

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
          writeClassRef(cls)

        case LoadJSModule(cls) =>
          writeByte(TagLoadJSModule)
          writeClassRef(cls)

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
        case FieldDef(flags, name, ftpe) =>
          writeByte(TagFieldDef)
          writeInt(MemberFlags.toBits(flags))
          writePropertyName(name)
          writeType(ftpe)

        case methodDef: MethodDef =>
          val MethodDef(flags, name, args, resultType, body) = methodDef

          writeByte(TagMethodDef)
          writeOptHash(methodDef.hash)

          // Prepare for back-jump and write dummy length
          bufferUnderlying.markJump()
          writeInt(-1)

          // Write out method def
          writeInt(MemberFlags.toBits(flags)); writePropertyName(name)
          writeParamDefs(args); writeType(resultType); writeOptTree(body)
          writeInt(OptimizerHints.toBits(methodDef.optimizerHints))

          // Jump back and write true length
          val length = bufferUnderlying.jumpBack()
          writeInt(length)
          bufferUnderlying.continue()

        case PropertyDef(flags, name, getter, setterArgAndBody) =>
          writeByte(TagPropertyDef)
          writeInt(MemberFlags.toBits(flags))
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
        case TopLevelJSClassExportDef(exportName) =>
          writeByte(TagTopLevelJSClassExportDef)
          writeString(exportName)

        case TopLevelModuleExportDef(exportName) =>
          writeByte(TagTopLevelModuleExportDef)
          writeString(exportName)

        case TopLevelMethodExportDef(methodDef) =>
          writeByte(TagTopLevelMethodExportDef)
          writeMemberDef(methodDef)

        case TopLevelFieldExportDef(exportName, field) =>
          writeByte(TagTopLevelFieldExportDef)
          writeString(exportName); writeIdent(field)
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

        case ClassType(className) =>
          buffer.write(TagClassType)
          writeString(className)

        case ArrayType(arrayTypeRef) =>
          buffer.write(TagArrayType)
          writeArrayTypeRef(arrayTypeRef)

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

    def writeTypeRef(typeRef: TypeRef): Unit = typeRef match {
      case typeRef: ClassRef =>
        buffer.writeByte(TagClassRef)
        writeClassRef(typeRef)
      case typeRef: ArrayTypeRef =>
        buffer.writeByte(TagArrayTypeRef)
        writeArrayTypeRef(typeRef)
    }

    def writeClassRef(cls: ClassRef): Unit =
      writeString(cls.className)

    def writeArrayTypeRef(typeRef: ArrayTypeRef): Unit = {
      writeString(typeRef.baseClassName)
      buffer.writeInt(typeRef.dimensions)
    }

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

    def writeApplyFlags(flags: ApplyFlags): Unit =
      buffer.writeInt(ApplyFlags.toBits(flags))

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

  private final class Deserializer(buf: ByteBuffer) {
    private[this] var sourceVersion: String = _
    private[this] var files: Array[URI] = _
    private[this] var strings: Array[String] = _

    private[this] var lastPosition: Position = Position.NoPosition

    buf.order(ByteOrder.BIG_ENDIAN)

    def deserializeEntryPointsInfo(): EntryPointsInfo = {
      sourceVersion = getHeader()
      getEntryPointsInfo()
    }

    def deserialize(): ClassDef = {
      sourceVersion = getHeader()
      getEntryPointsInfo() // discarded
      files = Array.fill(buf.getInt())(new URI(getUTF()))
      strings = Array.fill(buf.getInt())(getUTF())
      getClassDef()
    }

    /** Reads the Scala.js IR header and verifies the version compatibility.
     *
     *  @return the binary version that was read
     */
    private def getHeader(): String = {
      // Check magic number
      if (buf.getInt() != IRMagicNumber)
        throw new IOException("Not a Scala.js IR file")

      // Check that we support this version of the IR
      val version = getUTF()
      val supported = ScalaJSVersions.binarySupported
      if (!supported.contains(version)) {
        throw new IRVersionNotSupportedException(version, supported,
            s"This version ($version) of Scala.js IR is not supported. " +
            s"Supported versions are: ${supported.mkString(", ")}")
      }

      version
    }

    private def getUTF(): String = {
      // DataInput.readUTF for buffers.

      val length = buf.getShort() & 0xffff // unsigned
      var res = ""
      var i = 0

      def badFormat(msg: String) = throw new UTFDataFormatException(msg)

      while (i < length) {
        val a = buf.get()

        i += 1

        val char = {
          if ((a & 0x80) == 0x00) { // 0xxxxxxx
            a.toChar
          } else if ((a & 0xE0) == 0xC0 && i < length) { // 110xxxxx
            val b = buf.get()
            i += 1

            if ((b & 0xC0) != 0x80) // 10xxxxxx
              badFormat(f"Expected 2 bytes, found: $b%#02x (init: $a%#02x)")

            (((a & 0x1F) << 6) | (b & 0x3F)).toChar
          } else if ((a & 0xF0) == 0xE0 && i < length - 1) { // 1110xxxx
            val b = buf.get()
            val c = buf.get()
            i += 2

            if ((b & 0xC0) != 0x80)   // 10xxxxxx
              badFormat(f"Expected 3 bytes, found: $b%#02x (init: $a%#02x)")

            if ((c & 0xC0) != 0x80)   // 10xxxxxx
              badFormat(
                  f"Expected 3 bytes, found: $b%#02x, $c%#02x (init: $a%#02x)")

            (((a & 0x0F) << 12) | ((b & 0x3F) << 6) | (c & 0x3F)).toChar
          } else {
            val rem = length - i
            badFormat(f"Unexpected start of char: $a%#02x ($rem%d bytes to go)")
          }
        }

        res += char
      }

      res
    }

    private def getEntryPointsInfo(): EntryPointsInfo = {
      val encodedName = getUTF()
      val hasEntryPoint = getBoolean()
      new EntryPointsInfo(encodedName, hasEntryPoint)
    }

    def getTree(): Tree = {
      val pos = getPosition()
      getTreeFromTag(buf.get())(pos)
    }

    def getOptTree(): Option[Tree] = {
      // TODO switch tag and position when we can break binary compat.
      val pos = getPosition()
      val tag = buf.get()
      if (tag == TagEmptyTree) None
      else Some(getTreeFromTag(tag)(pos))
    }

    def getTreeOrJSSpread(): TreeOrJSSpread = {
      val pos = getPosition()
      val tag = buf.get()
      if (tag == TagJSSpread)
        JSSpread(getTree())(pos)
      else
        getTreeFromTag(tag)(pos)
    }

    def getTreeOrJSSpreads(): List[TreeOrJSSpread] =
      List.fill(buf.getInt())(getTreeOrJSSpread())

    private def getTreeFromTag(tag: Byte)(implicit pos: Position): Tree = {
      import buf._

      val result = (tag: @switch) match {
        case TagEmptyTree =>
          throw new IOException("Found invalid TagEmptyTree")

        case TagVarDef  => VarDef(getIdent(), getType(), getBoolean(), getTree())
        case TagSkip    => Skip()
        case TagBlock   => Block(getTrees())
        case TagLabeled => Labeled(getIdent(), getType(), getTree())
        case TagAssign  => Assign(getTree(), getTree())
        case TagReturn  => Return(getTree(), getIdent())
        case TagIf      => If(getTree(), getTree(), getTree())(getType())
        case TagWhile   => While(getTree(), getTree())
        case TagDoWhile => DoWhile(getTree(), getTree())
        case TagForIn   => ForIn(getTree(), getIdent(), getTree())

        case TagTryCatch =>
          TryCatch(getTree(), getIdent(), getTree())(getType())

        case TagTryFinally =>
          TryFinally(getTree(), getTree())

        case TagThrow    => Throw(getTree())
        case TagMatch    =>
          Match(getTree(), List.fill(getInt()) {
            (getTrees().map(_.asInstanceOf[IntLiteral]), getTree())
          }, getTree())(getType())
        case TagDebugger => Debugger()

        case TagNew          => New(getClassRef(), getIdent(), getTrees())
        case TagLoadModule   => LoadModule(getClassRef())
        case TagStoreModule  => StoreModule(getClassRef(), getTree())
        case TagSelect       => Select(getTree(), getIdent())(getType())
        case TagSelectStatic => SelectStatic(getClassRef(), getIdent())(getType())

        case TagApply =>
          Apply(getApplyFlags(), getTree(), getIdent(), getTrees())(
              getType())
        case TagApplyStatically =>
          ApplyStatically(getApplyFlags(), getTree(), getClassRef(),
              getIdent(), getTrees())(getType())
        case TagApplyStatic =>
          ApplyStatic(getApplyFlags(), getClassRef(), getIdent(),
              getTrees())(getType())

        case TagUnaryOp      => UnaryOp(get(), getTree())
        case TagBinaryOp     => BinaryOp(get(), getTree(), getTree())
        case TagNewArray     => NewArray(getArrayTypeRef(), getTrees())
        case TagArrayValue   => ArrayValue(getArrayTypeRef(), getTrees())
        case TagArrayLength  => ArrayLength(getTree())
        case TagArraySelect  => ArraySelect(getTree(), getTree())(getType())
        case TagRecordValue  => RecordValue(getType().asInstanceOf[RecordType], getTrees())
        case TagIsInstanceOf => IsInstanceOf(getTree(), getTypeRef())
        case TagAsInstanceOf => AsInstanceOf(getTree(), getTypeRef())
        case TagUnbox        => Unbox(getTree(), get().toChar)
        case TagGetClass     => GetClass(getTree())

        case TagJSNew                => JSNew(getTree(), getTreeOrJSSpreads())
        case TagJSDotSelect          => JSDotSelect(getTree(), getIdent())
        case TagJSBracketSelect      => JSBracketSelect(getTree(), getTree())
        case TagJSFunctionApply      => JSFunctionApply(getTree(), getTreeOrJSSpreads())
        case TagJSDotMethodApply     => JSDotMethodApply(getTree(), getIdent(), getTreeOrJSSpreads())
        case TagJSBracketMethodApply => JSBracketMethodApply(getTree(), getTree(), getTreeOrJSSpreads())
        case TagJSSuperBracketSelect => JSSuperBracketSelect(getTree(), getTree(), getTree())
        case TagJSSuperBracketCall   =>
          JSSuperBracketCall(getTree(), getTree(), getTree(), getTreeOrJSSpreads())
        case TagJSSuperConstructorCall => JSSuperConstructorCall(getTreeOrJSSpreads())
        case TagLoadJSConstructor    => LoadJSConstructor(getClassRef())
        case TagLoadJSModule         => LoadJSModule(getClassRef())
        case TagJSDelete             => JSDelete(getTree())
        case TagJSUnaryOp            => JSUnaryOp(getInt(), getTree())
        case TagJSBinaryOp           => JSBinaryOp(getInt(), getTree(), getTree())
        case TagJSArrayConstr        => JSArrayConstr(getTreeOrJSSpreads())
        case TagJSObjectConstr       =>
          JSObjectConstr(List.fill(getInt())((getPropertyName(), getTree())))
        case TagJSGlobalRef          => JSGlobalRef(getIdent())
        case TagJSLinkingInfo        => JSLinkingInfo()

        case TagUndefined      => Undefined()
        case TagNull           => Null()
        case TagBooleanLiteral => BooleanLiteral(getBoolean())
        case TagCharLiteral    => CharLiteral(getChar())
        case TagByteLiteral    => ByteLiteral(get())
        case TagShortLiteral   => ShortLiteral(getShort())
        case TagIntLiteral     => IntLiteral(getInt())
        case TagLongLiteral    => LongLiteral(getLong())
        case TagFloatLiteral   => FloatLiteral(getFloat())
        case TagDoubleLiteral  => DoubleLiteral(getDouble())
        case TagStringLiteral  => StringLiteral(getString())
        case TagClassOf        => ClassOf(getTypeRef())

        case TagVarRef =>
          VarRef(getIdent())(getType())
        case TagThis =>
          This()(getType())
        case TagClosure =>
          Closure(getBoolean(), getParamDefs(), getParamDefs(), getTree(),
              getTrees())
        case TagCreateJSClass =>
          CreateJSClass(getClassRef(), getTrees())
      }
      if (UseDebugMagic) {
        val magic = getInt()
        assert(magic == DebugMagic,
            s"Bad magic after reading a ${result.getClass}!")
      }
      result
    }

    def getTrees(): List[Tree] =
      List.fill(buf.getInt())(getTree())

    def getClassDef(): ClassDef = {
      import buf._

      implicit val pos = getPosition()
      val name = getIdent()
      val kind = ClassKind.fromByte(get())
      val hasJSClassCaptures = getBoolean()
      val jsClassCaptures =
        if (!hasJSClassCaptures) None
        else Some(getParamDefs())
      val superClass = getOptIdent()
      val parents = getIdents()
      val jsSuperClass = getOptTree()
      val jsNativeLoadSpec = getJSNativeLoadSpec()
      val memberDefs = getMemberDefs()
      val topLevelExportDefs = getTopLevelExportDefs()
      val optimizerHints = OptimizerHints.fromBits(getInt())
      ClassDef(name, kind, jsClassCaptures, superClass, parents, jsSuperClass,
          jsNativeLoadSpec, memberDefs, topLevelExportDefs)(
          optimizerHints)
    }

    def getMemberDef(): MemberDef = {
      import buf._

      implicit val pos = getPosition()
      val tag = buf.get()

      (tag: @switch) match {
        case TagFieldDef =>
          FieldDef(MemberFlags.fromBits(getInt()), getPropertyName(), getType())

        case TagMethodDef =>
          val optHash = getOptHash()
          // read and discard the length
          val len = getInt()
          assert(len >= 0)
          MethodDef(MemberFlags.fromBits(getInt()), getPropertyName(),
              getParamDefs(), getType(), getOptTree())(
              OptimizerHints.fromBits(getInt()), optHash)

        case TagPropertyDef =>
          val flags = MemberFlags.fromBits(getInt())
          val name = getPropertyName()
          val getterBody = getOptTree()
          val setterArgAndBody = {
            if (getBoolean())
              Some((getParamDef(), getTree()))
            else
              None
          }
          PropertyDef(flags, name, getterBody, setterArgAndBody)
      }
    }

    def getMemberDefs(): List[MemberDef] =
      List.fill(buf.getInt())(getMemberDef())

    def getTopLevelExportDef(): TopLevelExportDef = {
      implicit val pos = getPosition()
      val tag = buf.get()

      (tag: @switch) match {
        case TagTopLevelJSClassExportDef => TopLevelJSClassExportDef(getString())
        case TagTopLevelModuleExportDef  => TopLevelModuleExportDef(getString())
        case TagTopLevelMethodExportDef  => TopLevelMethodExportDef(getMemberDef().asInstanceOf[MethodDef])
        case TagTopLevelFieldExportDef   => TopLevelFieldExportDef(getString(), getIdent())
      }
    }

    def getTopLevelExportDefs(): List[TopLevelExportDef] =
      List.fill(buf.getInt())(getTopLevelExportDef())

    def getIdent(): Ident = {
      implicit val pos = getPosition()
      val name = getString()
      val originalName = getString()
      Ident(name, if (originalName.isEmpty) None else Some(originalName))
    }

    def getIdents(): List[Ident] =
      List.fill(buf.getInt())(getIdent())

    def getOptIdent(): Option[Ident] = {
      if (getBoolean()) Some(getIdent())
      else None
    }

    def getParamDef(): ParamDef = {
      implicit val pos = getPosition()
      ParamDef(getIdent(), getType(), getBoolean(), getBoolean())
    }

    def getParamDefs(): List[ParamDef] =
      List.fill(buf.getInt())(getParamDef())

    def getType(): Type = {
      val tag = buf.get()
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

        case TagClassType => ClassType(getString())
        case TagArrayType => ArrayType(getArrayTypeRef())

        case TagRecordType =>
          RecordType(List.fill(buf.getInt()) {
            val name = getString()
            val originalName = getString()
            val tpe = getType()
            val mutable = getBoolean()
            RecordType.Field(name,
                if (originalName.isEmpty) None else Some(originalName),
                tpe, mutable)
          })
      }
    }

    def getTypeRef(): TypeRef = {
      buf.get() match {
        case TagClassRef =>
          getClassRef()
        case TagArrayTypeRef =>
          getArrayTypeRef()
      }
    }

    def getClassRef(): ClassRef =
      ClassRef(getString())

    def getArrayTypeRef(): ArrayTypeRef =
      ArrayTypeRef(getString(), buf.getInt())

    def getPropertyName(): PropertyName = {
      buf.get() match {
        case TagPropertyNameIdent =>
          getIdent()
        case TagPropertyNameStringLiteral =>
          getTree().asInstanceOf[StringLiteral]
        case TagPropertyNameComputedName =>
          ComputedName(getTree(), getString())
      }
    }

    def getApplyFlags(): ApplyFlags =
      ApplyFlags.fromBits(buf.getInt())

    def getPosition(): Position = {
      import PositionFormat._
      import buf._

      val first = get()

      val result = if (first == FormatNoPositionValue) {
        Position.NoPosition
      } else {
        val result = if ((first & FormatFullMask) == FormatFullMaskValue) {
          val file = files(getInt())
          val line = getInt()
          val column = getInt()
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
            val column = get() & 0xff // unsigned
            Position(lastPosition.source,
                lastPosition.line + lineDiff, column)
          } else {
            assert((first & Format3Mask) == Format3MaskValue,
                s"Position format error: first byte $first does not match any format")
            val lineDiff = getShort()
            val column = get() & 0xff // unsigned
            Position(lastPosition.source,
                lastPosition.line + lineDiff, column)
          }
        }
        lastPosition = result
        result
      }

      if (UseDebugMagic) {
        val magic = getInt()
        assert(magic == PosDebugMagic,
            s"Bad magic after reading position with first byte $first")
      }

      result
    }

    def getJSNativeLoadSpec(): Option[JSNativeLoadSpec] = {
      def getGlobalSpec(): JSNativeLoadSpec.Global =
        JSNativeLoadSpec.Global(getString(), getStrings())

      def getImportSpec(): JSNativeLoadSpec.Import =
        JSNativeLoadSpec.Import(getString(), getStrings())

      (buf.get(): @switch) match {
        case TagJSNativeLoadSpecNone =>
          None
        case TagJSNativeLoadSpecGlobal =>
          Some(getGlobalSpec())
        case TagJSNativeLoadSpecImport =>
          Some(getImportSpec())
        case TagJSNativeLoadSpecImportWithGlobalFallback =>
          Some(JSNativeLoadSpec.ImportWithGlobalFallback(
              getImportSpec(), getGlobalSpec()))
      }
    }

    def getOptHash(): Option[TreeHash] = {
      if (getBoolean()) {
        val hash = new Array[Byte](20)
        buf.get(hash)
        Some(new TreeHash(hash))
      } else None
    }

    def getBoolean(): Boolean = buf.get() != 0

    def getString(): String = {
      strings(buf.getInt())
    }

    def getStrings(): List[String] =
      List.fill(buf.getInt())(getString())
  }
}
