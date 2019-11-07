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

import Names._
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
   *  @throws java.nio.BufferUnderflowException if not enough data is available
   *      in the buffer. In this case the buffer's position is unspecified and
   *      needs to be reset by the caller.
   */
  def deserializeEntryPointsInfo(buf: ByteBuffer): EntryPointsInfo =
    withBigEndian(buf)(new Deserializer(_).deserializeEntryPointsInfo())

  /** Deserializes a class def from the given buffer.
   *
   *  @throws java.nio.BufferUnderflowException if not enough data is available
   *      in the buffer. In this case the buffer's position is unspecified and
   *      needs to be reset by the caller.
   */
  def deserialize(buf: ByteBuffer): ClassDef =
    withBigEndian(buf)(new Deserializer(_).deserialize())

  @inline
  private def withBigEndian[T](buf: ByteBuffer)(body: ByteBuffer => T): T = {
    val o = buf.order()
    buf.order(ByteOrder.BIG_ENDIAN)
    try body(buf)
    finally buf.order(o)
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

  private final class EncodedNameKey(val encoded: UTF8String) {
    override def equals(that: Any): Boolean = that match {
      case that: EncodedNameKey =>
        UTF8String.equals(this.encoded, that.encoded)
      case _ =>
        false
    }

    override def hashCode(): Int =
      UTF8String.hashCode(encoded)
  }

  private final class Serializer {
    private[this] val bufferUnderlying = new JumpBackByteArrayOutputStream
    private[this] val buffer = new DataOutputStream(bufferUnderlying)

    private[this] val files = mutable.ListBuffer.empty[URI]
    private[this] val fileIndexMap = mutable.Map.empty[URI, Int]
    private def fileToIndex(file: URI): Int =
      fileIndexMap.getOrElseUpdate(file, (files += file).size - 1)

    private[this] val encodedNames = mutable.ListBuffer.empty[UTF8String]
    private[this] val encodedNameIndexMap = mutable.Map.empty[EncodedNameKey, Int]
    private def encodedNameToIndex(encoded: UTF8String): Int = {
      val byteString = new EncodedNameKey(encoded)
      encodedNameIndexMap.getOrElseUpdate(byteString,
          (encodedNames += encoded).size - 1)
    }

    private[this] val methodNames = mutable.ListBuffer.empty[MethodName]
    private[this] val methodNameIndexMap = mutable.Map.empty[MethodName, Int]
    private def methodNameToIndex(methodName: MethodName): Int = {
      methodNameIndexMap.getOrElseUpdate(methodName, {
        // need to reserve the internal simple names

        def reserveTypeRef(typeRef: TypeRef): Unit = typeRef match {
          case _: PrimRef =>
            // nothing to do
          case ClassRef(className) =>
            encodedNameToIndex(className.encoded)
          case ArrayTypeRef(base, _) =>
            reserveTypeRef(base)
        }

        encodedNameToIndex(methodName.simpleName.encoded)
        methodName.paramTypeRefs.foreach(reserveTypeRef(_))
        reserveTypeRef(methodName.resultTypeRef)
        (methodNames += methodName).size - 1
      })
    }

    private[this] val strings = mutable.ListBuffer.empty[String]
    private[this] val stringIndexMap = mutable.Map.empty[String, Int]
    private def stringToIndex(str: String): Int =
      stringIndexMap.getOrElseUpdate(str, (strings += str).size - 1)

    private[this] var lastPosition: Position = Position.NoPosition

    def serialize(stream: OutputStream, classDef: ClassDef): Unit = {
      // Write tree to buffer and record files, names and strings
      writeClassDef(classDef)

      val s = new DataOutputStream(stream)

      // Write the Scala.js IR magic number
      s.writeInt(IRMagicNumber)

      // Write the Scala.js Version
      s.writeUTF(ScalaJSVersions.binaryEmitted)

      // Write the entry points info
      val entryPointsInfo = EntryPointsInfo.forClassDef(classDef)
      val entryPointEncodedName = entryPointsInfo.className.encoded.bytes
      s.writeInt(entryPointEncodedName.length)
      s.write(entryPointEncodedName)
      s.writeBoolean(entryPointsInfo.hasEntryPoint)

      // Emit the files
      s.writeInt(files.size)
      files.foreach(f => s.writeUTF(f.toString))

      // Emit the names
      s.writeInt(encodedNames.size)
      encodedNames.foreach { encodedName =>
        s.writeInt(encodedName.length)
        s.write(encodedName.bytes)
      }

      def writeTypeRef(typeRef: TypeRef): Unit = typeRef match {
        case PrimRef(tpe) =>
          tpe match {
            case NoType      => s.writeByte(TagVoidRef)
            case BooleanType => s.writeByte(TagBooleanRef)
            case CharType    => s.writeByte(TagCharRef)
            case ByteType    => s.writeByte(TagByteRef)
            case ShortType   => s.writeByte(TagShortRef)
            case IntType     => s.writeByte(TagIntRef)
            case LongType    => s.writeByte(TagLongRef)
            case FloatType   => s.writeByte(TagFloatRef)
            case DoubleType  => s.writeByte(TagDoubleRef)
            case NullType    => s.writeByte(TagNullRef)
            case NothingType => s.writeByte(TagNothingRef)
          }
        case ClassRef(className) =>
          s.writeByte(TagClassRef)
          s.writeInt(encodedNameIndexMap(new EncodedNameKey(className.encoded)))
        case ArrayTypeRef(base, dimensions) =>
          s.writeByte(TagArrayTypeRef)
          writeTypeRef(base)
          s.writeInt(dimensions)
      }

      // Emit the method names
      s.writeInt(methodNames.size)
      methodNames.foreach { methodName =>
        s.writeInt(encodedNameIndexMap(
            new EncodedNameKey(methodName.simpleName.encoded)))
        s.writeInt(methodName.paramTypeRefs.size)
        methodName.paramTypeRefs.foreach(writeTypeRef(_))
        writeTypeRef(methodName.resultTypeRef)
        s.writeBoolean(methodName.isReflectiveProxy)
        writeName(methodName.simpleName)
      }

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
          writeLocalIdent(ident); writeType(vtpe); writeBoolean(mutable); writeTree(rhs)

        case Skip() =>
          writeByte(TagSkip)

        case Block(stats) =>
          writeByte(TagBlock)
          writeTrees(stats)

        case Labeled(label, tpe, body) =>
          writeByte(TagLabeled)
          writeLabelIdent(label); writeType(tpe); writeTree(body)

        case Assign(lhs, rhs) =>
          writeByte(TagAssign)
          writeTree(lhs); writeTree(rhs)

        case Return(expr, label) =>
          writeByte(TagReturn)
          writeTree(expr); writeLabelIdent(label)

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
          writeTree(obj); writeLocalIdent(keyVar); writeTree(body)

        case TryCatch(block, errVar, handler) =>
          writeByte(TagTryCatch)
          writeTree(block); writeLocalIdent(errVar); writeTree(handler)
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

        case New(className, ctor, args) =>
          writeByte(TagNew)
          writeName(className); writeMethodIdent(ctor); writeTrees(args)

        case LoadModule(className) =>
          writeByte(TagLoadModule)
          writeName(className)

        case StoreModule(className, value) =>
          writeByte(TagStoreModule)
          writeName(className); writeTree(value)

        case Select(qualifier, className, field) =>
          writeByte(TagSelect)
          writeTree(qualifier); writeName(className); writeFieldIdent(field)
          writeType(tree.tpe)

        case SelectStatic(className, field) =>
          writeByte(TagSelectStatic)
          writeName(className); writeFieldIdent(field)
          writeType(tree.tpe)

        case Apply(flags, receiver, method, args) =>
          writeByte(TagApply)
          writeApplyFlags(flags); writeTree(receiver); writeMethodIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyStatically(flags, receiver, className, method, args) =>
          writeByte(TagApplyStatically)
          writeApplyFlags(flags); writeTree(receiver); writeName(className); writeMethodIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyStatic(flags, className, method, args) =>
          writeByte(TagApplyStatic)
          writeApplyFlags(flags); writeName(className); writeMethodIdent(method); writeTrees(args)
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

        case RecordSelect(record, field) =>
          writeByte(TagRecordSelect)
          writeTree(record); writeFieldIdent(field)
          writeType(tree.tpe)

        case IsInstanceOf(expr, testType) =>
          writeByte(TagIsInstanceOf)
          writeTree(expr); writeType(testType)

        case AsInstanceOf(expr, tpe) =>
          writeByte(TagAsInstanceOf)
          writeTree(expr); writeType(tpe)

        case GetClass(expr) =>
          writeByte(TagGetClass)
          writeTree(expr)

        case JSNew(ctor, args) =>
          writeByte(TagJSNew)
          writeTree(ctor); writeTreeOrJSSpreads(args)

        case JSPrivateSelect(qualifier, className, field) =>
          writeByte(TagJSPrivateSelect)
          writeTree(qualifier); writeName(className); writeFieldIdent(field)

        case JSSelect(qualifier, item) =>
          writeByte(TagJSSelect)
          writeTree(qualifier); writeTree(item)

        case JSFunctionApply(fun, args) =>
          writeByte(TagJSFunctionApply)
          writeTree(fun); writeTreeOrJSSpreads(args)

        case JSMethodApply(receiver, method, args) =>
          writeByte(TagJSMethodApply)
          writeTree(receiver); writeTree(method); writeTreeOrJSSpreads(args)

        case JSSuperSelect(superClass, qualifier, item) =>
          writeByte(TagJSSuperSelect)
          writeTree(superClass); writeTree(qualifier); writeTree(item)

        case JSSuperMethodCall(superClass, receiver, method, args) =>
          writeByte(TagJSSuperMethodCall)
          writeTree(superClass); writeTree(receiver); writeTree(method); writeTreeOrJSSpreads(args)

        case JSSuperConstructorCall(args) =>
          writeByte(TagJSSuperConstructorCall)
          writeTreeOrJSSpreads(args)

        case JSImportCall(arg) =>
          writeByte(TagJSImportCall)
          writeTree(arg)

        case LoadJSConstructor(className) =>
          writeByte(TagLoadJSConstructor)
          writeName(className)

        case LoadJSModule(className) =>
          writeByte(TagLoadJSModule)
          writeName(className)

        case JSDelete(qualifier, item) =>
          writeByte(TagJSDelete)
          writeTree(qualifier)
          writeTree(item)

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
          fields.foreach { field =>
            writeTree(field._1); writeTree(field._2)
          }

        case JSGlobalRef(name) =>
          writeByte(TagJSGlobalRef)
          writeString(name)

        case JSTypeOfGlobalRef(globalRef) =>
          writeByte(TagJSTypeOfGlobalRef)
          writeTree(globalRef)

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
          writeLocalIdent(ident)
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

        case CreateJSClass(className, captureValues) =>
          writeByte(TagCreateJSClass)
          writeName(className)
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
      writeClassIdent(name)
      writeByte(ClassKind.toByte(kind))
      writeBoolean(jsClassCaptures.isDefined)
      jsClassCaptures.foreach(writeParamDefs(_))
      writeOptClassIdent(superClass)
      writeClassIdents(interfaces)
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
          writeFieldIdent(name)
          writeType(ftpe)

        case JSFieldDef(flags, name, ftpe) =>
          writeByte(TagJSFieldDef)
          writeInt(MemberFlags.toBits(flags))
          writeTree(name)
          writeType(ftpe)

        case methodDef: MethodDef =>
          val MethodDef(flags, name, args, resultType, body) = methodDef

          writeByte(TagMethodDef)
          writeOptHash(methodDef.hash)

          // Prepare for back-jump and write dummy length
          bufferUnderlying.markJump()
          writeInt(-1)

          // Write out method def
          writeInt(MemberFlags.toBits(flags)); writeMethodIdent(name)
          writeParamDefs(args); writeType(resultType); writeOptTree(body)
          writeInt(OptimizerHints.toBits(methodDef.optimizerHints))

          // Jump back and write true length
          val length = bufferUnderlying.jumpBack()
          writeInt(length)
          bufferUnderlying.continue()

        case methodDef: JSMethodDef =>
          val JSMethodDef(flags, name, args, body) = methodDef

          writeByte(TagJSMethodDef)
          writeOptHash(methodDef.hash)

          // Prepare for back-jump and write dummy length
          bufferUnderlying.markJump()
          writeInt(-1)

          // Write out method def
          writeInt(MemberFlags.toBits(flags)); writeTree(name)
          writeParamDefs(args); writeTree(body)
          writeInt(OptimizerHints.toBits(methodDef.optimizerHints))

          // Jump back and write true length
          val length = bufferUnderlying.jumpBack()
          writeInt(length)
          bufferUnderlying.continue()

        case JSPropertyDef(flags, name, getter, setterArgAndBody) =>
          writeByte(TagJSPropertyDef)
          writeInt(MemberFlags.toBits(flags))
          writeTree(name)
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
          writeString(exportName); writeFieldIdent(field)
      }
    }

    def writeTopLevelExportDefs(
        topLevelExportDefs: List[TopLevelExportDef]): Unit = {
      buffer.writeInt(topLevelExportDefs.size)
      topLevelExportDefs.foreach(writeTopLevelExportDef)
    }

    def writeLocalIdent(ident: LocalIdent): Unit = {
      writePosition(ident.pos)
      writeName(ident.name); writeString(ident.originalName.getOrElse(""))
    }

    def writeLabelIdent(ident: LabelIdent): Unit = {
      writePosition(ident.pos)
      writeName(ident.name)
    }

    def writeFieldIdent(ident: FieldIdent): Unit = {
      writePosition(ident.pos)
      writeName(ident.name); writeString(ident.originalName.getOrElse(""))
    }

    def writeMethodIdent(ident: MethodIdent): Unit = {
      writePosition(ident.pos)
      writeMethodName(ident.name); writeString(ident.originalName.getOrElse(""))
    }

    def writeClassIdent(ident: ClassIdent): Unit = {
      writePosition(ident.pos)
      writeName(ident.name); writeString(ident.originalName.getOrElse(""))
    }

    def writeClassIdents(idents: List[ClassIdent]): Unit = {
      buffer.writeInt(idents.size)
      idents.foreach(writeClassIdent)
    }

    def writeOptClassIdent(optIdent: Option[ClassIdent]): Unit = {
      buffer.writeBoolean(optIdent.isDefined)
      optIdent.foreach(writeClassIdent)
    }

    def writeName(name: Name): Unit =
      buffer.writeInt(encodedNameToIndex(name.encoded))

    def writeMethodName(name: MethodName): Unit =
      buffer.writeInt(methodNameToIndex(name))

    def writeParamDef(paramDef: ParamDef): Unit = {
      writePosition(paramDef.pos)
      writeLocalIdent(paramDef.name)
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
          writeName(className)

        case ArrayType(arrayTypeRef) =>
          buffer.write(TagArrayType)
          writeArrayTypeRef(arrayTypeRef)

        case RecordType(fields) =>
          buffer.write(TagRecordType)
          buffer.writeInt(fields.size)
          for (RecordType.Field(name, originalName, tpe, mutable) <- fields) {
            writeName(name)
            writeString(originalName.getOrElse(""))
            writeType(tpe)
            buffer.writeBoolean(mutable)
          }
      }
    }

    def writeTypeRef(typeRef: TypeRef): Unit = typeRef match {
      case PrimRef(tpe) =>
        tpe match {
          case NoType      => buffer.writeByte(TagVoidRef)
          case BooleanType => buffer.writeByte(TagBooleanRef)
          case CharType    => buffer.writeByte(TagCharRef)
          case ByteType    => buffer.writeByte(TagByteRef)
          case ShortType   => buffer.writeByte(TagShortRef)
          case IntType     => buffer.writeByte(TagIntRef)
          case LongType    => buffer.writeByte(TagLongRef)
          case FloatType   => buffer.writeByte(TagFloatRef)
          case DoubleType  => buffer.writeByte(TagDoubleRef)
          case NullType    => buffer.writeByte(TagNullRef)
          case NothingType => buffer.writeByte(TagNothingRef)
        }
      case ClassRef(className) =>
        buffer.writeByte(TagClassRef)
        writeName(className)
      case typeRef: ArrayTypeRef =>
        buffer.writeByte(TagArrayTypeRef)
        writeArrayTypeRef(typeRef)
    }

    def writeArrayTypeRef(typeRef: ArrayTypeRef): Unit = {
      writeTypeRef(typeRef.base)
      buffer.writeInt(typeRef.dimensions)
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
    require(buf.order() == ByteOrder.BIG_ENDIAN)

    private[this] var sourceVersion: String = _
    private[this] var files: Array[URI] = _
    private[this] var encodedNames: Array[UTF8String] = _
    private[this] var localNames: Array[LocalName] = _
    private[this] var labelNames: Array[LabelName] = _
    private[this] var fieldNames: Array[FieldName] = _
    private[this] var simpleMethodNames: Array[SimpleMethodName] = _
    private[this] var classNames: Array[ClassName] = _
    private[this] var methodNames: Array[MethodName] = _
    private[this] var strings: Array[String] = _

    private[this] var lastPosition: Position = Position.NoPosition

    def deserializeEntryPointsInfo(): EntryPointsInfo = {
      sourceVersion = readHeader()
      readEntryPointsInfo()
    }

    def deserialize(): ClassDef = {
      sourceVersion = readHeader()
      readEntryPointsInfo() // discarded
      files = Array.fill(readInt())(new URI(readUTF()))
      encodedNames = Array.fill(readInt()) {
        val len = readInt()
        val encodedName = new Array[Byte](len)
        buf.get(encodedName)
        UTF8String.createAcquiringByteArray(encodedName)
      }
      localNames = new Array(encodedNames.length)
      labelNames = new Array(encodedNames.length)
      fieldNames = new Array(encodedNames.length)
      simpleMethodNames = new Array(encodedNames.length)
      classNames = new Array(encodedNames.length)
      methodNames = Array.fill(readInt()) {
        val simpleName = readSimpleMethodName()
        val paramTypeRefs = List.fill(readInt())(readTypeRef())
        val resultTypeRef = readTypeRef()
        val isReflectiveProxy = readBoolean()
        MethodName(simpleName, paramTypeRefs, resultTypeRef, isReflectiveProxy)
      }
      strings = Array.fill(readInt())(readUTF())
      readClassDef()
    }

    /** Reads the Scala.js IR header and verifies the version compatibility.
     *
     *  @return the binary version that was read
     */
    private def readHeader(): String = {
      // Check magic number
      if (readInt() != IRMagicNumber)
        throw new IOException("Not a Scala.js IR file")

      // Check that we support this version of the IR
      val version = readUTF()
      val supported = ScalaJSVersions.binarySupported
      if (!supported.contains(version)) {
        throw new IRVersionNotSupportedException(version, supported,
            s"This version ($version) of Scala.js IR is not supported. " +
            s"Supported versions are: ${supported.mkString(", ")}")
      }

      version
    }

    private def readEntryPointsInfo(): EntryPointsInfo = {
      val encodedNameLen = readInt()
      val encodedName = new Array[Byte](encodedNameLen)
      buf.get(encodedName)
      val name = ClassName(UTF8String.createAcquiringByteArray(encodedName))
      val hasEntryPoint = readBoolean()
      new EntryPointsInfo(name, hasEntryPoint)
    }

    def readTree(): Tree = {
      val pos = readPosition()
      readTreeFromTag(readByte())(pos)
    }

    def readOptTree(): Option[Tree] = {
      // TODO switch tag and position when we can break binary compat.
      val pos = readPosition()
      val tag = readByte()
      if (tag == TagEmptyTree) None
      else Some(readTreeFromTag(tag)(pos))
    }

    def readTreeOrJSSpread(): TreeOrJSSpread = {
      val pos = readPosition()
      val tag = readByte()
      if (tag == TagJSSpread)
        JSSpread(readTree())(pos)
      else
        readTreeFromTag(tag)(pos)
    }

    def readTreeOrJSSpreads(): List[TreeOrJSSpread] =
      List.fill(readInt())(readTreeOrJSSpread())

    private def readTreeFromTag(tag: Byte)(implicit pos: Position): Tree = {
      val result = (tag: @switch) match {
        case TagEmptyTree =>
          throw new IOException("Found invalid TagEmptyTree")

        case TagVarDef  => VarDef(readLocalIdent(), readType(), readBoolean(), readTree())
        case TagSkip    => Skip()
        case TagBlock   => Block(readTrees())
        case TagLabeled => Labeled(readLabelIdent(), readType(), readTree())
        case TagAssign  => Assign(readTree(), readTree())
        case TagReturn  => Return(readTree(), readLabelIdent())
        case TagIf      => If(readTree(), readTree(), readTree())(readType())
        case TagWhile   => While(readTree(), readTree())
        case TagDoWhile => DoWhile(readTree(), readTree())
        case TagForIn   => ForIn(readTree(), readLocalIdent(), readTree())

        case TagTryCatch =>
          TryCatch(readTree(), readLocalIdent(), readTree())(readType())

        case TagTryFinally =>
          TryFinally(readTree(), readTree())

        case TagThrow    => Throw(readTree())
        case TagMatch    =>
          Match(readTree(), List.fill(readInt()) {
            (readTrees().map(_.asInstanceOf[IntLiteral]), readTree())
          }, readTree())(readType())
        case TagDebugger => Debugger()

        case TagNew          => New(readClassName(), readMethodIdent(), readTrees())
        case TagLoadModule   => LoadModule(readClassName())
        case TagStoreModule  => StoreModule(readClassName(), readTree())
        case TagSelect       => Select(readTree(), readClassName(), readFieldIdent())(readType())
        case TagSelectStatic => SelectStatic(readClassName(), readFieldIdent())(readType())

        case TagApply =>
          Apply(readApplyFlags(), readTree(), readMethodIdent(), readTrees())(
              readType())
        case TagApplyStatically =>
          ApplyStatically(readApplyFlags(), readTree(), readClassName(),
              readMethodIdent(), readTrees())(readType())
        case TagApplyStatic =>
          ApplyStatic(readApplyFlags(), readClassName(), readMethodIdent(),
              readTrees())(readType())

        case TagUnaryOp      => UnaryOp(readByte(), readTree())
        case TagBinaryOp     => BinaryOp(readByte(), readTree(), readTree())
        case TagNewArray     => NewArray(readArrayTypeRef(), readTrees())
        case TagArrayValue   => ArrayValue(readArrayTypeRef(), readTrees())
        case TagArrayLength  => ArrayLength(readTree())
        case TagArraySelect  => ArraySelect(readTree(), readTree())(readType())
        case TagRecordValue  => RecordValue(readType().asInstanceOf[RecordType], readTrees())
        case TagIsInstanceOf => IsInstanceOf(readTree(), readType())
        case TagAsInstanceOf => AsInstanceOf(readTree(), readType())
        case TagGetClass     => GetClass(readTree())

        case TagJSNew                => JSNew(readTree(), readTreeOrJSSpreads())
        case TagJSPrivateSelect      => JSPrivateSelect(readTree(), readClassName(), readFieldIdent())
        case TagJSSelect             => JSSelect(readTree(), readTree())
        case TagJSFunctionApply      => JSFunctionApply(readTree(), readTreeOrJSSpreads())
        case TagJSMethodApply        => JSMethodApply(readTree(), readTree(), readTreeOrJSSpreads())
        case TagJSSuperSelect        => JSSuperSelect(readTree(), readTree(), readTree())
        case TagJSSuperMethodCall    =>
          JSSuperMethodCall(readTree(), readTree(), readTree(), readTreeOrJSSpreads())
        case TagJSSuperConstructorCall => JSSuperConstructorCall(readTreeOrJSSpreads())
        case TagJSImportCall         => JSImportCall(readTree())
        case TagLoadJSConstructor    => LoadJSConstructor(readClassName())
        case TagLoadJSModule         => LoadJSModule(readClassName())
        case TagJSDelete             => JSDelete(readTree(), readTree())
        case TagJSUnaryOp            => JSUnaryOp(readInt(), readTree())
        case TagJSBinaryOp           => JSBinaryOp(readInt(), readTree(), readTree())
        case TagJSArrayConstr        => JSArrayConstr(readTreeOrJSSpreads())
        case TagJSObjectConstr       =>
          JSObjectConstr(List.fill(readInt())((readTree(), readTree())))
        case TagJSGlobalRef          => JSGlobalRef(readString())
        case TagJSTypeOfGlobalRef    => JSTypeOfGlobalRef(readTree().asInstanceOf[JSGlobalRef])
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
          VarRef(readLocalIdent())(readType())
        case TagThis =>
          This()(readType())
        case TagClosure =>
          Closure(readBoolean(), readParamDefs(), readParamDefs(), readTree(),
              readTrees())
        case TagCreateJSClass =>
          CreateJSClass(readClassName(), readTrees())
      }
      if (UseDebugMagic) {
        val magic = readInt()
        assert(magic == DebugMagic,
            s"Bad magic after reading a ${result.getClass}!")
      }
      result
    }

    def readTrees(): List[Tree] =
      List.fill(readInt())(readTree())

    def readClassDef(): ClassDef = {
      implicit val pos = readPosition()
      val name = readClassIdent()
      val kind = ClassKind.fromByte(readByte())
      val hasJSClassCaptures = readBoolean()
      val jsClassCaptures =
        if (!hasJSClassCaptures) None
        else Some(readParamDefs())
      val superClass = readOptClassIdent()
      val parents = readClassIdents()
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
      implicit val pos = readPosition()
      val tag = readByte()

      (tag: @switch) match {
        case TagFieldDef =>
          FieldDef(MemberFlags.fromBits(readInt()), readFieldIdent(), readType())

        case TagJSFieldDef =>
          JSFieldDef(MemberFlags.fromBits(readInt()), readTree(), readType())

        case TagMethodDef =>
          val optHash = readOptHash()
          // read and discard the length
          val len = readInt()
          assert(len >= 0)
          MethodDef(MemberFlags.fromBits(readInt()), readMethodIdent(),
              readParamDefs(), readType(), readOptTree())(
              OptimizerHints.fromBits(readInt()), optHash)

        case TagJSMethodDef =>
          val optHash = readOptHash()
          // read and discard the length
          val len = readInt()
          assert(len >= 0)
          JSMethodDef(MemberFlags.fromBits(readInt()), readTree(),
              readParamDefs(), readTree())(
              OptimizerHints.fromBits(readInt()), optHash)

        case TagJSPropertyDef =>
          val flags = MemberFlags.fromBits(readInt())
          val name = readTree()
          val getterBody = readOptTree()
          val setterArgAndBody = {
            if (readBoolean())
              Some((readParamDef(), readTree()))
            else
              None
          }
          JSPropertyDef(flags, name, getterBody, setterArgAndBody)
      }
    }

    def readMemberDefs(): List[MemberDef] =
      List.fill(readInt())(readMemberDef())

    def readTopLevelExportDef(): TopLevelExportDef = {
      implicit val pos = readPosition()
      val tag = readByte()

      (tag: @switch) match {
        case TagTopLevelJSClassExportDef => TopLevelJSClassExportDef(readString())
        case TagTopLevelModuleExportDef  => TopLevelModuleExportDef(readString())
        case TagTopLevelMethodExportDef  => TopLevelMethodExportDef(readMemberDef().asInstanceOf[JSMethodDef])
        case TagTopLevelFieldExportDef   => TopLevelFieldExportDef(readString(), readFieldIdent())
      }
    }

    def readTopLevelExportDefs(): List[TopLevelExportDef] =
      List.fill(readInt())(readTopLevelExportDef())

    def readLocalIdent(): LocalIdent = {
      implicit val pos = readPosition()
      val name = readLocalName()
      val originalName = readString()
      LocalIdent(name, if (originalName.isEmpty) None else Some(originalName))
    }

    def readLabelIdent(): LabelIdent = {
      implicit val pos = readPosition()
      LabelIdent(readLabelName())
    }

    def readFieldIdent(): FieldIdent = {
      implicit val pos = readPosition()
      val name = readFieldName()
      val originalName = readString()
      FieldIdent(name, if (originalName.isEmpty) None else Some(originalName))
    }

    def readMethodIdent(): MethodIdent = {
      implicit val pos = readPosition()
      val name = readMethodName()
      val originalName = readString()
      MethodIdent(name, if (originalName.isEmpty) None else Some(originalName))
    }

    def readClassIdent(): ClassIdent = {
      implicit val pos = readPosition()
      val name = readClassName()
      val originalName = readString()
      ClassIdent(name, if (originalName.isEmpty) None else Some(originalName))
    }

    def readClassIdents(): List[ClassIdent] =
      List.fill(readInt())(readClassIdent())

    def readOptClassIdent(): Option[ClassIdent] = {
      if (readBoolean()) Some(readClassIdent())
      else None
    }

    def readParamDef(): ParamDef = {
      implicit val pos = readPosition()
      ParamDef(readLocalIdent(), readType(), readBoolean(), readBoolean())
    }

    def readParamDefs(): List[ParamDef] =
      List.fill(readInt())(readParamDef())

    def readType(): Type = {
      val tag = readByte()
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

        case TagClassType => ClassType(readClassName())
        case TagArrayType => ArrayType(readArrayTypeRef())

        case TagRecordType =>
          RecordType(List.fill(readInt()) {
            val name = readFieldName()
            val originalName = readString()
            val tpe = readType()
            val mutable = readBoolean()
            RecordType.Field(name,
                if (originalName.isEmpty) None else Some(originalName),
                tpe, mutable)
          })
      }
    }

    def readTypeRef(): TypeRef = {
      readByte() match {
        case TagVoidRef      => VoidRef
        case TagBooleanRef   => BooleanRef
        case TagCharRef      => CharRef
        case TagByteRef      => ByteRef
        case TagShortRef     => ShortRef
        case TagIntRef       => IntRef
        case TagLongRef      => LongRef
        case TagFloatRef     => FloatRef
        case TagDoubleRef    => DoubleRef
        case TagNullRef      => NullRef
        case TagNothingRef   => NothingRef
        case TagClassRef     => ClassRef(readClassName())
        case TagArrayTypeRef => readArrayTypeRef()
      }
    }

    def readArrayTypeRef(): ArrayTypeRef =
      ArrayTypeRef(readTypeRef().asInstanceOf[NonArrayTypeRef], readInt())

    def readApplyFlags(): ApplyFlags =
      ApplyFlags.fromBits(readInt())

    def readPosition(): Position = {
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

      (readByte(): @switch) match {
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
      if (readBoolean()) {
        val hash = new Array[Byte](20)
        buf.get(hash)
        Some(new TreeHash(hash))
      } else {
        None
      }
    }

    def readString(): String = {
      strings(readInt())
    }

    def readStrings(): List[String] =
      List.fill(readInt())(readString())

    private def readLocalName(): LocalName = {
      val i = readInt()
      val existing = localNames(i)
      if (existing ne null) {
        existing
      } else {
        val result = LocalName(encodedNames(i))
        localNames(i) = result
        result
      }
    }

    private def readLabelName(): LabelName = {
      val i = readInt()
      val existing = labelNames(i)
      if (existing ne null) {
        existing
      } else {
        val result = LabelName(encodedNames(i))
        labelNames(i) = result
        result
      }
    }

    private def readFieldName(): FieldName = {
      val i = readInt()
      val existing = fieldNames(i)
      if (existing ne null) {
        existing
      } else {
        val result = FieldName(encodedNames(i))
        fieldNames(i) = result
        result
      }
    }

    private def readSimpleMethodName(): SimpleMethodName = {
      val i = readInt()
      val existing = simpleMethodNames(i)
      if (existing ne null) {
        existing
      } else {
        val result = SimpleMethodName(encodedNames(i))
        simpleMethodNames(i) = result
        result
      }
    }

    private def readClassName(): ClassName = {
      val i = readInt()
      val existing = classNames(i)
      if (existing ne null) {
        existing
      } else {
        val result = ClassName(encodedNames(i))
        classNames(i) = result
        result
      }
    }

    private def readMethodName(): MethodName =
      methodNames(readInt())

    private def readBoolean() = buf.get() != 0
    private def readByte() = buf.get()
    private def readChar() = buf.getChar()
    private def readShort() = buf.getShort()
    private def readInt() = buf.getInt()
    private def readLong() = buf.getLong()
    private def readFloat() = buf.getFloat()
    private def readDouble() = buf.getDouble()

    private def readUTF(): String = {
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
              badFormat("Expected 2 bytes, found: %#02x (init: %#02x)".format(b, a))

            (((a & 0x1F) << 6) | (b & 0x3F)).toChar
          } else if ((a & 0xF0) == 0xE0 && i < length - 1) { // 1110xxxx
            val b = buf.get()
            val c = buf.get()
            i += 2

            if ((b & 0xC0) != 0x80)   // 10xxxxxx
              badFormat("Expected 3 bytes, found: %#02x (init: %#02x)".format(b, a))

            if ((c & 0xC0) != 0x80)   // 10xxxxxx
              badFormat("Expected 3 bytes, found: %#02x, %#02x (init: %#02x)".format(b, c, a))

            (((a & 0x0F) << 12) | ((b & 0x3F) << 6) | (c & 0x3F)).toChar
          } else {
            val rem = length - i
            badFormat("Unexpected start of char: %#02x (%d bytes to go)".format(a, rem))
          }
        }

        res += char
      }

      res
    }
  }
}
