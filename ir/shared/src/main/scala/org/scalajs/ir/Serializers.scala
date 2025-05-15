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
import OriginalName.NoOriginalName
import Position._
import Trees._
import LinkTimeProperty.{ProductionMode, ESVersion, UseECMAScript2015Semantics, IsWebAssembly, LinkerVersion}
import Types._
import Tags._
import Version.Unversioned
import WellKnownNames._

import Utils.JumpBackByteArrayOutputStream

object Serializers {
  /** Scala.js IR File Magic Number
   *
   *    CA FE : first part of magic number of Java class files
   *    4A 53 : "JS" in ASCII
   *
   */
  final val IRMagicNumber = 0xCAFE4A53

  /** A regex for a compatible stable binary IR version from which we may need
   *  to migrate things with hacks.
   */
  private val CompatibleStableIRVersionRegex = {
    val prefix = java.util.regex.Pattern.quote(ScalaJSVersions.binaryCross + ".")
    new scala.util.matching.Regex(prefix + "(\\d+)")
  }

  // For deserialization hack
  private final val DynamicImportThunkClass =
    ClassName("scala.scalajs.runtime.DynamicImportThunk")

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
    private val bufferUnderlying = new JumpBackByteArrayOutputStream
    private val buffer = new DataOutputStream(bufferUnderlying)

    private val files = mutable.ListBuffer.empty[URI]
    private val fileIndexMap = mutable.Map.empty[URI, Int]
    private def fileToIndex(file: URI): Int =
      fileIndexMap.getOrElseUpdate(file, (files += file).size - 1)

    private val encodedNames = mutable.ListBuffer.empty[UTF8String]
    private val encodedNameIndexMap = mutable.Map.empty[EncodedNameKey, Int]
    private def encodedNameToIndex(encoded: UTF8String): Int = {
      val byteString = new EncodedNameKey(encoded)
      encodedNameIndexMap.getOrElseUpdate(byteString,
          (encodedNames += encoded).size - 1)
    }

    private val methodNames = mutable.ListBuffer.empty[MethodName]
    private val methodNameIndexMap = mutable.Map.empty[MethodName, Int]
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
          case typeRef: TransientTypeRef =>
            throw new InvalidIRException(s"Cannot serialize a transient type ref: $typeRef")
        }

        encodedNameToIndex(methodName.simpleName.encoded)
        methodName.paramTypeRefs.foreach(reserveTypeRef(_))
        reserveTypeRef(methodName.resultTypeRef)
        (methodNames += methodName).size - 1
      })
    }

    private val strings = mutable.ListBuffer.empty[String]
    private val stringIndexMap = mutable.Map.empty[String, Int]
    private def stringToIndex(str: String): Int =
      stringIndexMap.getOrElseUpdate(str, (strings += str).size - 1)

    private var lastPosition: Position = Position.NoPosition

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
            case VoidType    => s.writeByte(TagVoidRef)
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
        case typeRef: TransientTypeRef =>
          throw new InvalidIRException(s"Cannot serialize a transient type ref: $typeRef")
      }

      def writeTypeRefs(typeRefs: List[TypeRef]): Unit = {
        s.writeInt(typeRefs.size)
        typeRefs.foreach(writeTypeRef(_))
      }

      // Emit the method names
      s.writeInt(methodNames.size)
      methodNames.foreach { methodName =>
        s.writeInt(encodedNameIndexMap(
            new EncodedNameKey(methodName.simpleName.encoded)))
        writeTypeRefs(methodName.paramTypeRefs)
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

      def writeTagAndPos(tag: Int): Unit = {
        writeByte(tag)
        writePosition(tree.pos)
      }

      tree match {
        case VarDef(ident, originalName, vtpe, mutable, rhs) =>
          writeTagAndPos(TagVarDef)
          writeLocalIdent(ident); writeOriginalName(originalName)
          writeType(vtpe); writeBoolean(mutable); writeTree(rhs)

        case Skip() =>
          writeTagAndPos(TagSkip)

        case Block(stats) =>
          writeTagAndPos(TagBlock)
          writeTrees(stats)

        case Labeled(label, tpe, body) =>
          writeTagAndPos(TagLabeled)
          writeName(label); writeType(tpe); writeTree(body)

        case Assign(lhs, rhs) =>
          writeTagAndPos(TagAssign)
          writeTree(lhs); writeTree(rhs)

        case Return(expr, label) =>
          writeTagAndPos(TagReturn)
          writeTree(expr); writeName(label)

        case If(cond, thenp, elsep) =>
          writeTagAndPos(TagIf)
          writeTree(cond); writeTree(thenp); writeTree(elsep)
          writeType(tree.tpe)

        case LinkTimeIf(cond, thenp, elsep) =>
          writeTagAndPos(TagLinkTimeIf)
          writeTree(cond); writeTree(thenp); writeTree(elsep)
          writeType(tree.tpe)

        case While(cond, body) =>
          writeTagAndPos(TagWhile)
          writeTree(cond); writeTree(body)

        case ForIn(obj, keyVar, keyVarOriginalName, body) =>
          writeTagAndPos(TagForIn)
          writeTree(obj); writeLocalIdent(keyVar)
          writeOriginalName(keyVarOriginalName); writeTree(body)

        case TryCatch(block, errVar, errVarOriginalName, handler) =>
          writeTagAndPos(TagTryCatch)
          writeTree(block); writeLocalIdent(errVar)
          writeOriginalName(errVarOriginalName); writeTree(handler)
          writeType(tree.tpe)

        case TryFinally(block, finalizer) =>
          writeTagAndPos(TagTryFinally)
          writeTree(block); writeTree(finalizer)

        case Match(selector, cases, default) =>
          writeTagAndPos(TagMatch)
          writeTree(selector)
          writeInt(cases.size)
          cases foreach { caze =>
            writeTrees(caze._1); writeTree(caze._2)
          }
          writeTree(default)
          writeType(tree.tpe)

        case JSAwait(arg) =>
          writeTagAndPos(TagJSAwait)
          writeTree(arg)

        case Debugger() =>
          writeTagAndPos(TagDebugger)

        case New(className, ctor, args) =>
          writeTagAndPos(TagNew)
          writeName(className); writeMethodIdent(ctor); writeTrees(args)

        case LoadModule(className) =>
          writeTagAndPos(TagLoadModule)
          writeName(className)

        case StoreModule() =>
          writeTagAndPos(TagStoreModule)

        case Select(qualifier, field) =>
          writeTagAndPos(TagSelect)
          writeTree(qualifier); writeFieldIdent(field)
          writeType(tree.tpe)

        case SelectStatic(field) =>
          writeTagAndPos(TagSelectStatic)
          writeFieldIdent(field)
          writeType(tree.tpe)

        case SelectJSNativeMember(className, member) =>
          writeTagAndPos(TagSelectJSNativeMember)
          writeName(className); writeMethodIdent(member)

        case Apply(flags, receiver, method, args) =>
          writeTagAndPos(TagApply)
          writeApplyFlags(flags); writeTree(receiver); writeMethodIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyStatically(flags, receiver, className, method, args) =>
          writeTagAndPos(TagApplyStatically)
          writeApplyFlags(flags); writeTree(receiver); writeName(className); writeMethodIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyStatic(flags, className, method, args) =>
          writeTagAndPos(TagApplyStatic)
          writeApplyFlags(flags); writeName(className); writeMethodIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case ApplyDynamicImport(flags, className, method, args) =>
          writeTagAndPos(TagApplyDynamicImport)
          writeApplyFlags(flags); writeName(className); writeMethodIdent(method); writeTrees(args)

        case ApplyTypedClosure(flags, fun, args) =>
          writeTagAndPos(TagApplyTypedClosure)
          writeApplyFlags(flags); writeTree(fun); writeTrees(args)

        case NewLambda(descriptor, fun) =>
          val NewLambda.Descriptor(superClass, interfaces, methodName, paramTypes, resultType) =
            descriptor
          writeTagAndPos(TagNewLambda)
          writeName(superClass)
          writeNames(interfaces)
          writeMethodName(methodName)
          writeTypes(paramTypes)
          writeType(resultType)
          writeTree(fun)
          writeType(tree.tpe)

        case UnaryOp(op, lhs) =>
          writeTagAndPos(TagUnaryOp)
          writeByte(op); writeTree(lhs)

        case BinaryOp(op, lhs, rhs) =>
          writeTagAndPos(TagBinaryOp)
          writeByte(op); writeTree(lhs); writeTree(rhs)

        case NewArray(tpe, length) =>
          writeTagAndPos(TagNewArray)
          writeArrayTypeRef(tpe)
          writeTrees(length :: Nil) // written as a list of historical reasons

        case ArrayValue(tpe, elems) =>
          writeTagAndPos(TagArrayValue)
          writeArrayTypeRef(tpe); writeTrees(elems)

        case ArraySelect(array, index) =>
          writeTagAndPos(TagArraySelect)
          writeTree(array); writeTree(index)
          writeType(tree.tpe)

        case RecordValue(tpe, elems) =>
          writeTagAndPos(TagRecordValue)
          writeType(tpe); writeTrees(elems)

        case RecordSelect(record, field) =>
          writeTagAndPos(TagRecordSelect)
          writeTree(record); writeSimpleFieldIdent(field)
          writeType(tree.tpe)

        case IsInstanceOf(expr, testType) =>
          writeTagAndPos(TagIsInstanceOf)
          writeTree(expr); writeType(testType)

        case AsInstanceOf(expr, tpe) =>
          writeTagAndPos(TagAsInstanceOf)
          writeTree(expr); writeType(tpe)

        case JSNew(ctor, args) =>
          writeTagAndPos(TagJSNew)
          writeTree(ctor); writeTreeOrJSSpreads(args)

        case JSPrivateSelect(qualifier, field) =>
          writeTagAndPos(TagJSPrivateSelect)
          writeTree(qualifier); writeFieldIdent(field)

        case JSSelect(qualifier, item) =>
          writeTagAndPos(TagJSSelect)
          writeTree(qualifier); writeTree(item)

        case JSFunctionApply(fun, args) =>
          writeTagAndPos(TagJSFunctionApply)
          writeTree(fun); writeTreeOrJSSpreads(args)

        case JSMethodApply(receiver, method, args) =>
          writeTagAndPos(TagJSMethodApply)
          writeTree(receiver); writeTree(method); writeTreeOrJSSpreads(args)

        case JSSuperSelect(superClass, qualifier, item) =>
          writeTagAndPos(TagJSSuperSelect)
          writeTree(superClass); writeTree(qualifier); writeTree(item)

        case JSSuperMethodCall(superClass, receiver, method, args) =>
          writeTagAndPos(TagJSSuperMethodCall)
          writeTree(superClass); writeTree(receiver); writeTree(method); writeTreeOrJSSpreads(args)

        case JSSuperConstructorCall(args) =>
          writeTagAndPos(TagJSSuperConstructorCall)
          writeTreeOrJSSpreads(args)

        case JSImportCall(arg) =>
          writeTagAndPos(TagJSImportCall)
          writeTree(arg)

        case JSNewTarget() =>
          writeTagAndPos(TagJSNewTarget)

        case JSImportMeta() =>
          writeTagAndPos(TagJSImportMeta)

        case LoadJSConstructor(className) =>
          writeTagAndPos(TagLoadJSConstructor)
          writeName(className)

        case LoadJSModule(className) =>
          writeTagAndPos(TagLoadJSModule)
          writeName(className)

        case JSDelete(qualifier, item) =>
          writeTagAndPos(TagJSDelete)
          writeTree(qualifier)
          writeTree(item)

        case JSUnaryOp(op, lhs) =>
          writeTagAndPos(TagJSUnaryOp)
          writeInt(op); writeTree(lhs)

        case JSBinaryOp(op, lhs, rhs) =>
          writeTagAndPos(TagJSBinaryOp)
          writeInt(op); writeTree(lhs); writeTree(rhs)

        case JSArrayConstr(items) =>
          writeTagAndPos(TagJSArrayConstr)
          writeTreeOrJSSpreads(items)

        case JSObjectConstr(fields) =>
          writeTagAndPos(TagJSObjectConstr)
          writeInt(fields.size)
          fields.foreach { field =>
            writeTree(field._1); writeTree(field._2)
          }

        case JSGlobalRef(name) =>
          writeTagAndPos(TagJSGlobalRef)
          writeString(name)

        case JSTypeOfGlobalRef(globalRef) =>
          writeTagAndPos(TagJSTypeOfGlobalRef)
          writeTree(globalRef)

        case Undefined() =>
          writeTagAndPos(TagUndefined)

        case Null() =>
          writeTagAndPos(TagNull)

        case BooleanLiteral(value) =>
          writeTagAndPos(TagBooleanLiteral)
          writeBoolean(value)

        case CharLiteral(value) =>
          writeTagAndPos(TagCharLiteral)
          writeChar(value)

        case ByteLiteral(value) =>
          writeTagAndPos(TagByteLiteral)
          writeByte(value)

        case ShortLiteral(value) =>
          writeTagAndPos(TagShortLiteral)
          writeShort(value)

        case IntLiteral(value) =>
          writeTagAndPos(TagIntLiteral)
          writeInt(value)

        case LongLiteral(value) =>
          writeTagAndPos(TagLongLiteral)
          writeLong(value)

        case FloatLiteral(value) =>
          writeTagAndPos(TagFloatLiteral)
          writeFloat(value)

        case DoubleLiteral(value) =>
          writeTagAndPos(TagDoubleLiteral)
          writeDouble(value)

        case StringLiteral(value) =>
          writeTagAndPos(TagStringLiteral)
          writeString(value)

        case ClassOf(typeRef) =>
          writeTagAndPos(TagClassOf)
          writeTypeRef(typeRef)

        case VarRef(name) =>
          if (name.isThis) {
            // "Optimized" representation that is compatible with IR < 1.18
            writeTagAndPos(TagThis)
          } else {
            writeTagAndPos(TagVarRef)
            writeName(name)
          }
          writeType(tree.tpe)

        case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
          writeTagAndPos(TagClosure)
          writeClosureFlags(flags)
          writeParamDefs(captureParams)
          writeParamDefs(params)

          // Compatible with IR < v1.19, which had no `resultType`
          if (flags.typed) {
            if (restParam.isDefined)
              throw new InvalidIRException(tree, "Cannot serialize a typed closure with a rest param")
            writeType(resultType)
          } else {
            if (resultType != AnyType)
              throw new InvalidIRException(tree, "Cannot serialize a JS closure with a result type != AnyType")
            writeOptParamDef(restParam)
          }

          writeTree(body)
          writeTrees(captureValues)

        case CreateJSClass(className, captureValues) =>
          writeTagAndPos(TagCreateJSClass)
          writeName(className)
          writeTrees(captureValues)

        case LinkTimeProperty(name) =>
          writeTagAndPos(TagLinkTimeProperty)
          writeString(name)
          writeType(tree.tpe)

        case Transient(value) =>
          throw new InvalidIRException(tree,
              "Cannot serialize a transient IR node (its value is of class " +
              s"${value.getClass})")
      }
    }

    def writeTrees(trees: List[Tree]): Unit = {
      buffer.writeInt(trees.size)
      trees.foreach(writeTree)
    }

    def writeOptTree(optTree: Option[Tree]): Unit = {
      optTree.fold {
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
          buffer.writeByte(TagJSSpread)
          writePosition(tree.pos)
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
      writeOriginalName(originalName)
      writeByte(ClassKind.toByte(kind))
      writeBoolean(jsClassCaptures.isDefined)
      jsClassCaptures.foreach(writeParamDefs(_))
      writeOptClassIdent(superClass)
      writeClassIdents(interfaces)
      writeOptTree(jsSuperClass)
      writeJSNativeLoadSpec(jsNativeLoadSpec)
      writeMemberDefs(fields ::: methods ::: jsConstructor.toList ::: jsMethodProps ::: jsNativeMembers)
      writeTopLevelExportDefs(topLevelExportDefs)
      writeInt(OptimizerHints.toBits(optimizerHints))
    }

    def writeMemberDef(memberDef: MemberDef): Unit = {
      import buffer._
      writePosition(memberDef.pos)
      memberDef match {
        case FieldDef(flags, name, originalName, ftpe) =>
          writeByte(TagFieldDef)
          writeInt(MemberFlags.toBits(flags))
          writeFieldIdentForEnclosingClass(name)
          writeOriginalName(originalName)
          writeType(ftpe)

        case JSFieldDef(flags, name, ftpe) =>
          writeByte(TagJSFieldDef)
          writeInt(MemberFlags.toBits(flags))
          writeTree(name)
          writeType(ftpe)

        case methodDef: MethodDef =>
          val MethodDef(flags, name, originalName, args, resultType, body) = methodDef

          writeByte(TagMethodDef)
          writeOptHash(methodDef.version)

          // Prepare for back-jump and write dummy length
          bufferUnderlying.markJump()
          writeInt(-1)

          // Write out method def
          writeInt(MemberFlags.toBits(flags)); writeMethodIdent(name)
          writeOriginalName(originalName)
          writeParamDefs(args); writeType(resultType); writeOptTree(body)
          writeInt(OptimizerHints.toBits(methodDef.optimizerHints))

          // Jump back and write true length
          val length = bufferUnderlying.jumpBack()
          writeInt(length)
          bufferUnderlying.continue()

        case ctorDef: JSConstructorDef =>
          val JSConstructorDef(flags, args, restParam, body) = ctorDef

          writeByte(TagJSConstructorDef)
          writeOptHash(ctorDef.version)

          // Prepare for back-jump and write dummy length
          bufferUnderlying.markJump()
          writeInt(-1)

          // Write out ctor def
          writeInt(MemberFlags.toBits(flags))
          writeParamDefs(args); writeOptParamDef(restParam)
          writePosition(body.pos)
          writeTrees(body.beforeSuper)
          writeTree(body.superCall)
          writeTrees(body.afterSuper)
          writeInt(OptimizerHints.toBits(ctorDef.optimizerHints))

          // Jump back and write true length
          val length = bufferUnderlying.jumpBack()
          writeInt(length)
          bufferUnderlying.continue()

        case methodDef: JSMethodDef =>
          val JSMethodDef(flags, name, args, restParam, body) = methodDef

          writeByte(TagJSMethodDef)
          writeOptHash(methodDef.version)

          // Prepare for back-jump and write dummy length
          bufferUnderlying.markJump()
          writeInt(-1)

          // Write out method def
          writeInt(MemberFlags.toBits(flags)); writeTree(name)
          writeParamDefs(args); writeOptParamDef(restParam); writeTree(body)
          writeInt(OptimizerHints.toBits(methodDef.optimizerHints))

          // Jump back and write true length
          val length = bufferUnderlying.jumpBack()
          writeInt(length)
          bufferUnderlying.continue()

        case propDef: JSPropertyDef =>
          val JSPropertyDef(flags, name, getter, setterArgAndBody) = propDef

          writeByte(TagJSPropertyDef)
          writeOptHash(propDef.version)

          // Prepare for back-jump and write dummy length
          bufferUnderlying.markJump()
          writeInt(-1)

          // Write out prop def
          writeInt(MemberFlags.toBits(flags))
          writeTree(name)
          writeOptTree(getter)
          writeBoolean(setterArgAndBody.isDefined)
          setterArgAndBody foreach { case (arg, body) =>
            writeParamDef(arg); writeTree(body)
          }

          // Jump back and write true length
          val length = bufferUnderlying.jumpBack()
          writeInt(length)
          bufferUnderlying.continue()

        case JSNativeMemberDef(flags, name, jsNativeLoadSpec) =>
          writeByte(TagJSNativeMemberDef)
          writeInt(MemberFlags.toBits(flags))
          writeMethodIdent(name)
          writeJSNativeLoadSpec(Some(jsNativeLoadSpec))
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
        case TopLevelJSClassExportDef(moduleID, exportName) =>
          writeByte(TagTopLevelJSClassExportDef)
          writeString(moduleID); writeString(exportName)

        case TopLevelModuleExportDef(moduleID, exportName) =>
          writeByte(TagTopLevelModuleExportDef)
          writeString(moduleID); writeString(exportName)

        case TopLevelMethodExportDef(moduleID, methodDef) =>
          writeByte(TagTopLevelMethodExportDef)
          writeString(moduleID); writeMemberDef(methodDef)

        case TopLevelFieldExportDef(moduleID, exportName, field) =>
          writeByte(TagTopLevelFieldExportDef)
          writeString(moduleID); writeString(exportName); writeFieldIdentForEnclosingClass(field)
      }
    }

    def writeTopLevelExportDefs(
        topLevelExportDefs: List[TopLevelExportDef]): Unit = {
      buffer.writeInt(topLevelExportDefs.size)
      topLevelExportDefs.foreach(writeTopLevelExportDef)
    }

    def writeLocalIdent(ident: LocalIdent): Unit = {
      writePosition(ident.pos)
      writeName(ident.name)
    }

    def writeSimpleFieldIdent(ident: SimpleFieldIdent): Unit = {
      writePosition(ident.pos)
      writeName(ident.name)
    }

    def writeFieldIdent(ident: FieldIdent): Unit = {
      // For historical reasons, the className comes *before* the position
      writeName(ident.name.className)
      writePosition(ident.pos)
      writeName(ident.name.simpleName)
    }

    def writeFieldIdentForEnclosingClass(ident: FieldIdent): Unit = {
      writePosition(ident.pos)
      writeName(ident.name.simpleName)
    }

    def writeMethodIdent(ident: MethodIdent): Unit = {
      writePosition(ident.pos)
      writeMethodName(ident.name)
    }

    def writeClassIdent(ident: ClassIdent): Unit = {
      writePosition(ident.pos)
      writeName(ident.name)
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

    def writeNames(names: List[Name]): Unit = {
      buffer.writeInt(names.size)
      names.foreach(writeName(_))
    }

    def writeMethodName(name: MethodName): Unit =
      buffer.writeInt(methodNameToIndex(name))

    def writeOriginalName(originalName: OriginalName): Unit = {
      buffer.writeBoolean(originalName.isDefined)
      if (originalName.isDefined)
        buffer.writeInt(encodedNameToIndex(originalName.get))
    }

    def writeParamDef(paramDef: ParamDef): Unit = {
      writePosition(paramDef.pos)
      writeLocalIdent(paramDef.name)
      writeOriginalName(paramDef.originalName)
      writeType(paramDef.ptpe)
      buffer.writeBoolean(paramDef.mutable)
    }

    def writeParamDefs(paramDefs: List[ParamDef]): Unit = {
      buffer.writeInt(paramDefs.size)
      paramDefs.foreach(writeParamDef(_))
    }

    def writeOptParamDef(paramDef: Option[ParamDef]): Unit = {
      buffer.writeBoolean(paramDef.isDefined)
      paramDef.foreach(writeParamDef(_))
    }

    def writeType(tpe: Type): Unit = {
      tpe match {
        case AnyType        => buffer.write(TagAnyType)
        case AnyNotNullType => buffer.write(TagAnyNotNullType)
        case NothingType    => buffer.write(TagNothingType)
        case UndefType      => buffer.write(TagUndefType)
        case BooleanType    => buffer.write(TagBooleanType)
        case CharType       => buffer.write(TagCharType)
        case ByteType       => buffer.write(TagByteType)
        case ShortType      => buffer.write(TagShortType)
        case IntType        => buffer.write(TagIntType)
        case LongType       => buffer.write(TagLongType)
        case FloatType      => buffer.write(TagFloatType)
        case DoubleType     => buffer.write(TagDoubleType)
        case StringType     => buffer.write(TagStringType)
        case NullType       => buffer.write(TagNullType)
        case VoidType       => buffer.write(TagVoidType)

        case ClassType(className, nullable) =>
          buffer.write(if (nullable) TagClassType else TagNonNullClassType)
          writeName(className)

        case ArrayType(arrayTypeRef, nullable) =>
          buffer.write(if (nullable) TagArrayType else TagNonNullArrayType)
          writeArrayTypeRef(arrayTypeRef)

        case ClosureType(paramTypes, resultType, nullable) =>
          buffer.write(if (nullable) TagClosureType else TagNonNullClosureType)
          writeTypes(paramTypes)
          writeType(resultType)

        case RecordType(fields) =>
          buffer.write(TagRecordType)
          buffer.writeInt(fields.size)
          for (RecordType.Field(name, originalName, tpe, mutable) <- fields) {
            writeName(name)
            writeOriginalName(originalName)
            writeType(tpe)
            buffer.writeBoolean(mutable)
          }
      }
    }

    def writeTypes(tpes: List[Type]): Unit = {
      buffer.writeInt(tpes.size)
      tpes.foreach(writeType)
    }

    def writeTypeRef(typeRef: TypeRef): Unit = typeRef match {
      case PrimRef(tpe) =>
        tpe match {
          case VoidType    => buffer.writeByte(TagVoidRef)
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
      case typeRef: TransientTypeRef =>
        throw new InvalidIRException(s"Cannot serialize a transient type ref: $typeRef")
    }

    def writeArrayTypeRef(typeRef: ArrayTypeRef): Unit = {
      writeTypeRef(typeRef.base)
      buffer.writeInt(typeRef.dimensions)
    }

    def writeTypeRefs(typeRefs: List[TypeRef]): Unit = {
      buffer.writeInt(typeRefs.size)
      typeRefs.foreach(writeTypeRef(_))
    }

    def writeApplyFlags(flags: ApplyFlags): Unit =
      buffer.writeInt(ApplyFlags.toBits(flags))

    def writeClosureFlags(flags: ClosureFlags): Unit =
      buffer.writeByte(ClosureFlags.toBits(flags))

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

    def writeOptHash(version: Version): Unit = {
      val isHash = version.isHash
      buffer.writeBoolean(isHash)
      if (isHash)
        version.writeHash(buffer)
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

    private var hacks: Hacks = null
    private var files: Array[URI] = null
    private var encodedNames: Array[UTF8String] = null
    private var localNames: Array[LocalName] = null
    private var labelNames: Array[LabelName] = null
    private var simpleFieldNames: Array[SimpleFieldName] = null
    private var simpleMethodNames: Array[SimpleMethodName] = null
    private var classNames: Array[ClassName] = null
    private var methodNames: Array[MethodName] = null
    private var strings: Array[String] = null

    /** Uniqueness cache for FieldName's.
     *
     *  For historical reasons, the `ClassName` and `SimpleFieldName`
     *  components of `FieldName`s are store separately in the `.sjsir` format.
     *  Since most if not all occurrences of any particular `FieldName`
     *  typically come from a single `.sjsir` file, we use a uniqueness cache
     *  to make them all `eq`, consuming less memory and speeding up equality
     *  tests.
     */
    private val uniqueFieldNames = mutable.AnyRefMap.empty[FieldName, FieldName]

    private var lastPosition: Position = Position.NoPosition

    private var enclosingClassName: ClassName = null
    private var thisTypeForHack: Option[Type] = None
    private var patchDynamicImportThunkSuperCtorCall: Boolean = false

    def deserializeEntryPointsInfo(): EntryPointsInfo = {
      hacks = new Hacks(sourceVersion = readHeader())
      readEntryPointsInfo()
    }

    def deserialize(): ClassDef = {
      hacks = new Hacks(sourceVersion = readHeader())
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
      simpleFieldNames = new Array(encodedNames.length)
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
      ScalaJSVersions.checkSupported(version)

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

    def readTree(): Tree =
      readTreeFromTag(readByte())

    def readOptTree(): Option[Tree] = {
      val tag = readByte()
      if (tag == TagEmptyTree) None
      else Some(readTreeFromTag(tag))
    }

    def readTreeOrJSSpread(): TreeOrJSSpread = {
      val tag = readByte()
      if (tag == TagJSSpread) {
        implicit val pos = readPosition()
        JSSpread(readTree())
      } else {
        readTreeFromTag(tag)
      }
    }

    def readTreeOrJSSpreads(): List[TreeOrJSSpread] =
      List.fill(readInt())(readTreeOrJSSpread())

    private def readTreeFromTag(tag: Byte): Tree = {
      implicit val pos = readPosition()
      (tag: @switch) match {
        case TagEmptyTree =>
          throw new IOException("Found invalid TagEmptyTree")

        case TagVarDef  => VarDef(readLocalIdent(), readOriginalName(), readType(), readBoolean(), readTree())
        case TagSkip    => Skip()
        case TagBlock   => Block(readTrees())
        case TagLabeled => Labeled(readLabelName(), readType(), readTree())

        case TagAssign =>
          val lhs0 = readTree()
          val lhs = if (hacks.useBelow(5) && lhs0.tpe == NothingType) {
            /* Note [Nothing FieldDef rewrite]
             * (throw qual.field[null]) = rhs  -->  qual.field[null] = rhs
             */
            lhs0 match {
              case UnaryOp(UnaryOp.Throw, sel: Select) if sel.tpe == NullType => sel
              case _                                                          => lhs0
            }
          } else {
            lhs0
          }

          val rhs = readTree()

          Assign(lhs.asInstanceOf[AssignLhs], rhs)

        case TagReturn =>
          Return(readTree(), readLabelName())
        case TagIf =>
          If(readTree(), readTree(), readTree())(readType())
        case TagLinkTimeIf =>
          LinkTimeIf(readTree(), readTree(), readTree())(readType())
        case TagWhile =>
          While(readTree(), readTree())

        case TagDoWhile =>
          if (!hacks.useBelow(13))
            throw new IOException(s"Found invalid pre-1.13 DoWhile loop at $pos")
          // Rewrite `do { body } while (cond)` to `while ({ body; cond }) {}`
          val body = readTree()
          val cond = readTree()
          While(Block(body, cond), Skip())

        case TagForIn   => ForIn(readTree(), readLocalIdent(), readOriginalName(), readTree())

        case TagTryCatch =>
          TryCatch(readTree(), readLocalIdent(), readOriginalName(), readTree())(readType())

        case TagTryFinally =>
          TryFinally(readTree(), readTree())

        case TagMatch =>
          Match(readTree(), List.fill(readInt()) {
            (readTrees().map(_.asInstanceOf[MatchableLiteral]), readTree())
          }, readTree())(readType())

        case TagJSAwait =>
          JSAwait(readTree())

        case TagDebugger => Debugger()

        case TagNew =>
          val tree = New(readClassName(), readMethodIdent(), readTrees())
          if (hacks.useBelow(19))
            anonFunctionNewNodeHackBelow19(tree)
          else
            tree

        case TagLoadModule =>
          LoadModule(readClassName())

        case TagStoreModule =>
          if (hacks.useBelow(16)) {
            val cls = readClassName()
            val rhs = readTree()
            rhs match {
              case This() if cls == enclosingClassName =>
                // ok
              case _ =>
                throw new IOException(
                    s"Illegal legacy StoreModule(${cls.nameString}, $rhs) " +
                    s"found in class ${enclosingClassName.nameString}")
            }
          }
          StoreModule()

        case TagSelect =>
          val qualifier = readTree()
          val field = readFieldIdent()
          val tpe = readType()

          if (hacks.useBelow(5) && tpe == NothingType) {
            /* Note [Nothing FieldDef rewrite]
             * qual.field[nothing]  -->  throw qual.field[null]
             */
            UnaryOp(UnaryOp.Throw, Select(qualifier, field)(NullType))
          } else {
            Select(qualifier, field)(tpe)
          }

        case TagSelectStatic => SelectStatic(readFieldIdent())(readType())
        case TagSelectJSNativeMember => SelectJSNativeMember(readClassName(), readMethodIdent())

        case TagApply =>
          Apply(readApplyFlags(), readTree(), readMethodIdent(), readTrees())(
              readType())

        case TagApplyStatically =>
          val flags = readApplyFlags()
          val receiver = readTree()
          val className0 = readClassName()
          val method = readMethodIdent()
          val args = readTrees()
          val tpe = readType()

          val className = {
            if (patchDynamicImportThunkSuperCtorCall && method.name.isConstructor)
              DynamicImportThunkClass
            else
              className0
          }

          ApplyStatically(flags, receiver, className, method, args)(tpe)

        case TagApplyStatic =>
          ApplyStatic(readApplyFlags(), readClassName(), readMethodIdent(),
              readTrees())(readType())
        case TagApplyDynamicImport =>
          ApplyDynamicImport(readApplyFlags(), readClassName(),
              readMethodIdent(), readTrees())
        case TagApplyTypedClosure =>
          ApplyTypedClosure(readApplyFlags(), readTree(), readTrees())
        case TagNewLambda =>
          val descriptor = NewLambda.Descriptor(readClassName(),
              readClassNames(), readMethodName(), readTypes(), readType())
          NewLambda(descriptor, readTree())(readType())

        case TagUnaryOp  => UnaryOp(readByte(), readTree())
        case TagBinaryOp => BinaryOp(readByte(), readTree(), readTree())

        case TagArrayLength | TagGetClass | TagClone | TagIdentityHashCode |
            TagWrapAsThrowable | TagUnwrapFromThrowable | TagThrow =>
          if (!hacks.useBelow(18)) {
            throw new IOException(
                s"Illegal legacy node $tag found in class ${enclosingClassName.nameString}")
          }

          val lhs = readTree()
          def checkNotNullLhs: Tree = UnaryOp(UnaryOp.CheckNotNull, lhs)

          (tag: @switch) match {
            case TagArrayLength =>
              UnaryOp(UnaryOp.Array_length, checkNotNullLhs)
            case TagGetClass =>
              UnaryOp(UnaryOp.GetClass, checkNotNullLhs)
            case TagClone =>
              UnaryOp(UnaryOp.Clone, checkNotNullLhs)
            case TagIdentityHashCode =>
              UnaryOp(UnaryOp.IdentityHashCode, lhs)
            case TagWrapAsThrowable =>
              UnaryOp(UnaryOp.WrapAsThrowable, lhs)
            case TagUnwrapFromThrowable =>
              UnaryOp(UnaryOp.UnwrapFromThrowable, checkNotNullLhs)
            case TagThrow =>
              val patchedLhs =
                if (hacks.useBelow(11)) throwArgumentHackBelow11(lhs)
                else lhs
              UnaryOp(UnaryOp.Throw, patchedLhs)
          }

        case TagNewArray =>
          val arrayTypeRef = readArrayTypeRef()
          val lengths = readTrees()
          lengths match {
            case length :: Nil =>
              NewArray(arrayTypeRef, length)

            case _ =>
              if (hacks.useBelow(17)) {
                // Rewrite as a call to j.l.r.Array.newInstance
                val ArrayTypeRef(base, origDims) = arrayTypeRef
                val newDims = origDims - lengths.size
                if (newDims < 0) {
                  throw new IOException(
                      s"Illegal legacy NewArray node with ${lengths.size} lengths but dimension $origDims at $pos")
                }
                val newBase =
                  if (newDims == 0) base
                  else ArrayTypeRef(base, newDims)

                ApplyStatic(
                    ApplyFlags.empty,
                    HackNames.ReflectArrayClass,
                    MethodIdent(HackNames.newInstanceMultiName),
                    List(ClassOf(newBase), ArrayValue(ArrayTypeRef(IntRef, 1), lengths)))(
                    AnyType)
              } else {
                throw new IOException(
                    s"Illegal NewArray node with multiple lengths for IR version 1.17+ at $pos")
              }
          }

        case TagArrayValue  => ArrayValue(readArrayTypeRef(), readTrees())
        case TagArraySelect => ArraySelect(readTree(), readTree())(readType())
        case TagRecordValue => RecordValue(readType().asInstanceOf[RecordType], readTrees())

        case TagIsInstanceOf =>
          val expr = readTree()
          val testType0 = readType()
          val testType = if (hacks.useBelow(17)) {
            testType0 match {
              case ClassType(className, true)    => ClassType(className, nullable = false)
              case ArrayType(arrayTypeRef, true) => ArrayType(arrayTypeRef, nullable = false)
              case AnyType                       => AnyNotNullType
              case _                             => testType0
            }
          } else {
            testType0
          }
          IsInstanceOf(expr, testType)

        case TagAsInstanceOf     => AsInstanceOf(readTree(), readType())

        case TagJSNew           => JSNew(readTree(), readTreeOrJSSpreads())
        case TagJSPrivateSelect => JSPrivateSelect(readTree(), readFieldIdent())

        case TagJSSelect =>
          if (hacks.useBelow(18) && buf.get(buf.position()) == TagJSLinkingInfo) {
            val jsLinkingInfo = readTree()
            readTree() match {
              case StringLiteral("productionMode") =>
                LinkTimeProperty(ProductionMode)(BooleanType)
              case StringLiteral("esVersion") =>
                LinkTimeProperty(ESVersion)(IntType)
              case StringLiteral("assumingES6") =>
                LinkTimeProperty(UseECMAScript2015Semantics)(BooleanType)
              case StringLiteral("isWebAssembly") =>
                LinkTimeProperty(IsWebAssembly)(BooleanType)
              case StringLiteral("linkerVersion") =>
                LinkTimeProperty(LinkerVersion)(StringType)
              case StringLiteral("fileLevelThis") =>
                JSGlobalRef(JSGlobalRef.FileLevelThis)
              case otherItem =>
                JSSelect(jsLinkingInfo, otherItem)
          }
        } else {
          JSSelect(readTree(), readTree())
        }

        case TagJSFunctionApply      => JSFunctionApply(readTree(), readTreeOrJSSpreads())
        case TagJSMethodApply        => JSMethodApply(readTree(), readTree(), readTreeOrJSSpreads())
        case TagJSSuperSelect        => JSSuperSelect(readTree(), readTree(), readTree())
        case TagJSSuperMethodCall    =>
          JSSuperMethodCall(readTree(), readTree(), readTree(), readTreeOrJSSpreads())
        case TagJSSuperConstructorCall => JSSuperConstructorCall(readTreeOrJSSpreads())
        case TagJSImportCall         => JSImportCall(readTree())
        case TagJSNewTarget          => JSNewTarget()
        case TagJSImportMeta         => JSImportMeta()
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

        case TagJSLinkingInfo =>
          if (hacks.useBelow(18)) {
            JSObjectConstr(List(
              (StringLiteral("productionMode"), LinkTimeProperty(ProductionMode)(BooleanType)),
              (StringLiteral("esVersion"), LinkTimeProperty(ESVersion)(IntType)),
              (StringLiteral("assumingES6"), LinkTimeProperty(UseECMAScript2015Semantics)(BooleanType)),
              (StringLiteral("isWebAssembly"), LinkTimeProperty(IsWebAssembly)(BooleanType)),
              (StringLiteral("linkerVersion"), LinkTimeProperty(LinkerVersion)(StringType)),
              (StringLiteral("fileLevelThis"), JSGlobalRef(JSGlobalRef.FileLevelThis))
            ))
          } else {
            throw new IOException(
                s"Found invalid pre-1.18 JSLinkingInfo def at ${pos}")
          }

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
          val name =
            if (hacks.useBelow(18)) readLocalIdent().name
            else readLocalName()
          VarRef(name)(readType())

        case TagThis =>
          val tpe = readType()
          This()(thisTypeForHack.getOrElse(tpe))

        case TagClosure =>
          val flags = readClosureFlags()
          val captureParams = readParamDefs()

          val (params, restParam, resultType) = if (flags.typed) {
            (readParamDefs(), None, readType())
          } else {
            val (params, restParam) = readParamDefsWithRest()
            (params, restParam, AnyType)
          }

          val body = if (thisTypeForHack.isEmpty) {
            // Fast path; always taken for IR >= 1.17
            readTree()
          } else {
            val prevThisTypeForHack = thisTypeForHack
            thisTypeForHack = None
            try {
              readTree()
            } finally {
              thisTypeForHack = prevThisTypeForHack
            }
          }
          val captureValues = readTrees()
          Closure(flags, captureParams, params, restParam, resultType, body, captureValues)

        case TagCreateJSClass =>
          CreateJSClass(readClassName(), readTrees())

        case TagLinkTimeProperty =>
          LinkTimeProperty(readString())(readType())
      }
    }

    /** Patches the argument of a `Throw` for IR version below 1.11.
     *
     *  Prior to Scala.js 1.11, `Throw(e)` was emitted by the compiler with
     *  the somewhat implied assumption that it would "throw an NPE" (but
     *  subject to UB so not really) when `e` is a `null` `Throwable`.
     *
     *  Moreover, there was no other user-space way to emit a `Throw(e)` in the
     *  IR (`js.special.throw` was introduced in 1.11), so *all* `Throw` nodes
     *  are part of the semantics of a Scala `throw expr` or of an implicit
     *  re-throw in a Scala `try..catch`.
     *
     *  In Scala.js 1.11, we explicitly ruled out the NPE behavior of `Throw`,
     *  so that `Throw(e)` only ever throws the value of `e`, while the NPE UB
     *  is specified by `UnwrapFromThrowable`. Among other things, this allows
     *  the user-space code `js.special.throw(e)` to indiscriminately throw `e`
     *  even if it is `null`. Later, in Scala.js 1.18, we further separated the
     *  null check of `UnwrapFromThrowable` to be an explicit `CheckNotNull`.
     *
     *  With this hack, we patch `Throw(e)` by inserting an appropriate
     *  `CheckNotNull`.
     *
     *  However, we must not do that when the previous Scala.js compiler
     *  already provides the *unwrapped* exception. This happened in two
     *  situations:
     *
     *  - when automatically re-throwing an unhandled exception at the end of a
     *    `try..catch`, or
     *  - when throwing a maybe-JavaScriptException, with an explicit call to
     *    `runtime.package$.unwrapJavaScriptException(x)`.
     *
     *  Fortunately, in both situations, the type of the `expr` is always
     *  `AnyType`. We can accurately use that test to know whether we need to
     *  apply the patch.
     */
    private def throwArgumentHackBelow11(expr: Tree)(implicit pos: Position): Tree = {
      if (expr.tpe == AnyType)
        expr
      else if (!expr.tpe.isNullable)
        expr // no CheckNotNull needed; common case because of `throw new ...`
      else
        UnaryOp(UnaryOp.CheckNotNull, expr)
    }

    /** Rewrites `New` nodes of `AnonFunctionN`s coming from before 1.19 into `NewLambda` nodes.
     *
     *  Before 1.19, the codegen for `scala.FunctionN` lambda was of the following shape:
     *  {{{
     *  new scala.scalajs.runtime.AnonFunctionN(arrow-lambda<...captures>(...args: any): any = {
     *    body
     *  })
     *  }}}
     *
     *  This function rewrites such calls to `NewLambda` nodes, using the new
     *  definition of these classes:
     *  {{{
     *  <newLambda>(scala.scalajs.runtime.AnonFunctionN,
     *     apply;Ljava.lang.Object;...;Ljava.lang.Object,
     *     any, any, (typed-lambda<...captures>(...args: any): any = {
     *    body
     *  }))
     *  }}}
     *
     *  The rewrite ensures that previously published lambdas get the same
     *  optimizations on Wasm as those recompiled with 1.19+.
     *
     *  The rewrite also applies to Scala 3's `AnonFunctionXXL` classes, with
     *  an additional adaptation of the parameter's type. It rewrites
     *  {{{
     *  new scala.scalajs.runtime.AnonFunctionXXL(arrow-lambda<...captures>(argArray: any): any = {
     *    body
     *  })
     *  }}}
     *  to
     *  {{{
     *  <newLambda>(scala.scalajs.runtime.AnonFunctionXXL,
     *     apply;Ljava.lang.Object[];Ljava.lang.Object,
     *     any, any, (typed-lambda<...captures>(argArray: jl.Object[]): any = {
     *    newBody
     *  }))
     *  }}}
     *  where `newBody` is `body` transformed to adapt the type of `argArray`
     *  everywhere.
     *
     *  Tests are in `sbt-plugin/src/sbt-test/linker/anonfunction-compat/`.
     *
     *  ---
     *
     *  In case the argument is not an arrow-lambda of the expected shape, we
     *  use a fallback. This never happens for our published codegens, but
     *  could happen for other valid IR. We rewrite
     *  {{{
     *  new scala.scalajs.runtime.AnonFunctionN(jsFunctionArg)
     *  }}}
     *  to
     *  {{{
     *  <newLambda>(scala.scalajs.runtime.AnonFunctionN,
     *     apply;Ljava.lang.Object;...;Ljava.lang.Object,
     *     any, any, (typed-lambda<f: any = jsFunctionArg>(...args: any): any = {
     *    f(...args)
     *  }))
     *  }}}
     *
     *  This code path is not tested in the CI, but can be locally tested by
     *  commenting out the `case Closure(...) =>`.
     */
    private def anonFunctionNewNodeHackBelow19(tree: New): Tree = {
      tree match {
        case New(cls, _, funArg :: Nil) =>
          def makeFallbackTypedClosure(paramTypes: List[Type]): Closure = {
            implicit val pos = funArg.pos
            val fParamDef = ParamDef(LocalIdent(LocalName("f")), NoOriginalName, AnyType, mutable = false)
            val xParamDefs = paramTypes.zipWithIndex.map { case (ptpe, i) =>
              ParamDef(LocalIdent(LocalName(s"x$i")), NoOriginalName, ptpe, mutable = false)
            }
            Closure(ClosureFlags.typed, List(fParamDef), xParamDefs, None, AnyType,
                JSFunctionApply(fParamDef.ref, xParamDefs.map(_.ref)),
                List(funArg))
          }

          cls match {
            case HackNames.AnonFunctionClass(arity) =>
              val typedClosure = funArg match {
                // The shape produced by our earlier compilers, which we can optimally rewrite
                case Closure(ClosureFlags.arrow, captureParams, params, None, AnyType, body, captureValues)
                    if params.lengthCompare(arity) == 0 =>
                  Closure(ClosureFlags.typed, captureParams, params, None, AnyType,
                      body, captureValues)(funArg.pos)

                // Fallback for other shapes (theoretically required; dead code in practice)
                case _ =>
                  makeFallbackTypedClosure(List.fill(arity)(AnyType))
              }

              NewLambda(HackNames.anonFunctionDescriptors(arity), typedClosure)(tree.tpe)(tree.pos)

            case HackNames.AnonFunctionXXLClass =>
              val typedClosure = funArg match {
                // The shape produced by our earlier compilers, which we can optimally rewrite
                case Closure(ClosureFlags.arrow, captureParams, oldParam :: Nil, None, AnyType, body, captureValues) =>
                  // Here we need to adapt the type of the parameter from `any` to `jl.Object[]`.
                  val newParam = oldParam.copy(ptpe = HackNames.ObjectArrayType)(oldParam.pos)
                  val newBody = new Transformers.LocalScopeTransformer {
                    override def transform(tree: Tree): Tree = tree match {
                      case tree @ VarRef(newParam.name.name) => tree.copy()(newParam.ptpe)(tree.pos)
                      case _                                 => super.transform(tree)
                    }
                  }.transform(body)
                  Closure(ClosureFlags.typed, captureParams, List(newParam), None, AnyType,
                      newBody, captureValues)(funArg.pos)

                // Fallback for other shapes (theoretically required; dead code in practice)
                case _ =>
                  makeFallbackTypedClosure(List(HackNames.ObjectArrayType))
              }

              NewLambda(HackNames.anonFunctionXXLDescriptor, typedClosure)(tree.tpe)(tree.pos)

            case _ =>
              tree
          }

        case _ =>
          tree
      }
    }

    def readTrees(): List[Tree] =
      List.fill(readInt())(readTree())

    def readClassDef(): ClassDef = {
      implicit val pos = readPosition()

      val name = readClassIdent()
      val cls = name.name
      enclosingClassName = cls

      val originalName = readOriginalName()
      val kind = ClassKind.fromByte(readByte())

      if (hacks.useBelow(17)) {
        thisTypeForHack = kind match {
          case ClassKind.Class | ClassKind.ModuleClass | ClassKind.Interface =>
            Some(ClassType(cls, nullable = false))
          case ClassKind.HijackedClass if hacks.useBelow(11) =>
            // Use getOrElse as safety guard for otherwise invalid inputs
            Some(BoxedClassToPrimType.getOrElse(cls, ClassType(cls, nullable = false)))
          case _ =>
            None
        }
      }

      val hasJSClassCaptures = readBoolean()
      val jsClassCaptures =
        if (!hasJSClassCaptures) None
        else Some(readParamDefs())
      val superClass = readOptClassIdent()
      val parents = readClassIdents()

      if (hacks.useBelow(18) && kind.isClass) {
        /* In 1.18, we started enforcing the constructor chaining discipline.
         * Unfortunately, we used to generate a wrong super constructor call in
         * synthetic classes extending `DynamicImportThunk`, so we patch them.
         */
        patchDynamicImportThunkSuperCtorCall =
          superClass.exists(_.name == DynamicImportThunkClass)
      }

      /* jsSuperClass is not hacked like in readMemberDef.bodyHackBelow6. The
       * compilers before 1.6 always use a simple VarRef() as jsSuperClass,
       * when there is one, so no hack is required.
       */
      val jsSuperClass = readOptTree()

      val jsNativeLoadSpec = readJSNativeLoadSpec()

      // Read member defs
      val fieldsBuilder = List.newBuilder[AnyFieldDef]
      val methodsBuilder = List.newBuilder[MethodDef]
      val jsConstructorBuilder = new OptionBuilder[JSConstructorDef]
      val jsMethodPropsBuilder = List.newBuilder[JSMethodPropDef]
      val jsNativeMembersBuilder = List.newBuilder[JSNativeMemberDef]

      for (_ <- 0 until readInt()) {
        implicit val pos = readPosition()
        readByte() match {
          case TagFieldDef          => fieldsBuilder += readFieldDef()
          case TagJSFieldDef        => fieldsBuilder += readJSFieldDef()
          case TagMethodDef         => methodsBuilder += readMethodDef(cls, kind)
          case TagJSConstructorDef  => jsConstructorBuilder += readJSConstructorDef(kind)
          case TagJSMethodDef       => jsMethodPropsBuilder += readJSMethodDef()
          case TagJSPropertyDef     => jsMethodPropsBuilder += readJSPropertyDef()
          case TagJSNativeMemberDef => jsNativeMembersBuilder += readJSNativeMemberDef()
        }
      }

      val topLevelExportDefs = readTopLevelExportDefs()
      val optimizerHints = OptimizerHints.fromBits(readInt())

      val fields = fieldsBuilder.result()

      val methods = {
        val methods0 = methodsBuilder.result()
        if (hacks.useBelow(5) && kind.isJSClass) {
          // #4409: Filter out abstract methods in non-native JS classes for version < 1.5
          methods0.filter(_.body.isDefined)
        } else if (hacks.useBelow(17) && cls == ClassClass) {
          jlClassMethodsHackBelow17(methods0)
        } else if (hacks.useBelow(17) && cls == HackNames.ReflectArrayModClass) {
          jlReflectArrayMethodsHackBelow17(methods0)
        } else {
          methods0
        }
      }

      val (jsConstructor, jsMethodProps) = {
        if (hacks.useBelow(11) && kind.isJSClass) {
          assert(jsConstructorBuilder.result().isEmpty, "found JSConstructorDef in pre 1.11 IR")
          jsConstructorHackBelow11(kind, jsMethodPropsBuilder.result())
        } else {
          (jsConstructorBuilder.result(), jsMethodPropsBuilder.result())
        }
      }

      val jsNativeMembers = jsNativeMembersBuilder.result()

      val classDef = ClassDef(name, originalName, kind, jsClassCaptures, superClass, parents,
          jsSuperClass, jsNativeLoadSpec, fields, methods, jsConstructor,
          jsMethodProps, jsNativeMembers, topLevelExportDefs)(
          optimizerHints)

      if (hacks.useBelow(19))
        anonFunctionClassDefHackBelow19(classDef)
      else
        classDef
    }

    private def jlClassMethodsHackBelow17(methods: List[MethodDef]): List[MethodDef] = {
      for (method <- methods) yield {
        implicit val pos = method.pos

        val methodName = method.methodName
        val methodSimpleNameString = methodName.simpleName.nameString

        val thisJLClass = This()(ClassType(ClassClass, nullable = false))

        if (methodName.isConstructor) {
          val newName = MethodIdent(NoArgConstructorName)(method.name.pos)
          val newBody = ApplyStatically(ApplyFlags.empty.withConstructor(true),
              thisJLClass, ObjectClass, newName, Nil)(VoidType)
          MethodDef(method.flags, newName, method.originalName,
              Nil, VoidType, Some(newBody))(
              method.optimizerHints, method.version)
        } else {
          def argRef = method.args.head.ref
          def argRefNotNull = UnaryOp(UnaryOp.CheckNotNull, argRef)

          var forceInline = true // reset to false in the `case _ =>`

          val newBody: Tree = methodSimpleNameString match {
            case "getName"          => UnaryOp(UnaryOp.Class_name, thisJLClass)
            case "isPrimitive"      => UnaryOp(UnaryOp.Class_isPrimitive, thisJLClass)
            case "isInterface"      => UnaryOp(UnaryOp.Class_isInterface, thisJLClass)
            case "isArray"          => UnaryOp(UnaryOp.Class_isArray, thisJLClass)
            case "getComponentType" => UnaryOp(UnaryOp.Class_componentType, thisJLClass)
            case "getSuperclass"    => UnaryOp(UnaryOp.Class_superClass, thisJLClass)

            case "isInstance"       => BinaryOp(BinaryOp.Class_isInstance, thisJLClass, argRef)
            case "isAssignableFrom" => BinaryOp(BinaryOp.Class_isAssignableFrom, thisJLClass, argRefNotNull)
            case "cast"             => BinaryOp(BinaryOp.Class_cast, thisJLClass, argRef)

            case _ =>
              forceInline = false

              /* Unfortunately, some of the other methods directly referred to
               * `this.data["name"]`, instead of building on `this.getName()`.
               * We must replace those occurrences with a `Class_name` as well.
               */
              val transformer = new Transformers.Transformer {
                override def transform(tree: Tree): Tree = tree match {
                  case JSSelect(_, StringLiteral("name")) =>
                    implicit val pos = tree.pos
                    UnaryOp(UnaryOp.Class_name, thisJLClass)
                  case _ =>
                    super.transform(tree)
                }
              }
              transformer.transform(method.body.get)
          }

          val newOptimizerHints =
            if (forceInline) method.optimizerHints.withInline(true)
            else method.optimizerHints

          MethodDef(method.flags, method.name, method.originalName,
              method.args, method.resultType, Some(newBody))(
              newOptimizerHints, method.version)
        }
      }
    }

    private def jlReflectArrayMethodsHackBelow17(methods: List[MethodDef]): List[MethodDef] = {
      /* Basically this method hard-codes new implementations for the two
       * overloads of newInstance.
       * It is horrible, but better than pollute everything else in the linker.
       */

      import HackNames._

      def paramDef(name: String, ptpe: Type)(implicit pos: Position): ParamDef =
        ParamDef(LocalIdent(LocalName(name)), NoOriginalName, ptpe, mutable = false)

      def varDef(name: String, vtpe: Type, rhs: Tree, mutable: Boolean = false)(
          implicit pos: Position): VarDef = {
        VarDef(LocalIdent(LocalName(name)), NoOriginalName, vtpe, mutable, rhs)
      }

      def arrayLength(t: Tree)(implicit pos: Position): Tree =
        UnaryOp(UnaryOp.Array_length, UnaryOp(UnaryOp.CheckNotNull, t))

      def getClass(t: Tree)(implicit pos: Position): Tree =
        UnaryOp(UnaryOp.GetClass, UnaryOp(UnaryOp.CheckNotNull, t))

      val jlClassRef = ClassRef(ClassClass)
      val intArrayTypeRef = ArrayTypeRef(IntRef, 1)
      val objectRef = ClassRef(ObjectClass)
      val objectArrayTypeRef = ArrayTypeRef(objectRef, 1)

      val jlClassType = ClassType(ClassClass, nullable = true)

      val newInstanceRecName = MethodName("newInstanceRec",
          List(jlClassRef, intArrayTypeRef, IntRef), objectRef)

      val EAF = ApplyFlags.empty

      val newInstanceRecMethod = {
        /* def newInstanceRec(componentType: jl.Class, dimensions: int[], offset: int): any = {
         *   val length: int = dimensions[offset]
         *   val result: any = newInstance(componentType, length)
         *   val innerOffset = offset + 1
         *   if (innerOffset < dimensions.length) {
         *     val result2: Object[] = result.asInstanceOf[Object[]]
         *     val innerComponentType: jl.Class = componentType.getComponentType()
         *     var i: Int = 0
         *     while (i != length)
         *       result2[i] = newInstanceRec(innerComponentType, dimensions, innerOffset)
         *       i = i + 1
         *     }
         *   }
         *   result
         * }
         */

        implicit val pos = Position.NoPosition

        val getComponentTypeName = MethodName("getComponentType", Nil, jlClassRef)

        val ths = This()(ClassType(ReflectArrayModClass, nullable = false))

        val componentType = paramDef("componentType", jlClassType)
        val dimensions = paramDef("dimensions", ArrayType(intArrayTypeRef, nullable = true))
        val offset = paramDef("offset", IntType)

        val length = varDef("length", IntType, ArraySelect(dimensions.ref, offset.ref)(IntType))
        val result = varDef("result", AnyType,
            Apply(EAF, ths, MethodIdent(newInstanceSingleName), List(componentType.ref, length.ref))(AnyType))
        val innerOffset = varDef("innerOffset", IntType,
            BinaryOp(BinaryOp.Int_+, offset.ref, IntLiteral(1)))

        val result2 = varDef("result2", ArrayType(objectArrayTypeRef, nullable = true),
            AsInstanceOf(result.ref, ArrayType(objectArrayTypeRef, nullable = true)))
        val innerComponentType = varDef("innerComponentType", jlClassType,
            Apply(EAF, componentType.ref, MethodIdent(getComponentTypeName), Nil)(jlClassType))
        val i = varDef("i", IntType, IntLiteral(0), mutable = true)

        val body = {
          Block(
            length,
            result,
            innerOffset,
            If(BinaryOp(BinaryOp.Int_<, innerOffset.ref, arrayLength(dimensions.ref)), {
              Block(
                result2,
                innerComponentType,
                i,
                While(BinaryOp(BinaryOp.Int_!=, i.ref, length.ref), {
                  Block(
                    Assign(
                      ArraySelect(result2.ref, i.ref)(AnyType),
                      Apply(EAF, ths, MethodIdent(newInstanceRecName),
                          List(innerComponentType.ref, dimensions.ref, innerOffset.ref))(AnyType)
                    ),
                    Assign(
                      i.ref,
                      BinaryOp(BinaryOp.Int_+, i.ref, IntLiteral(1))
                    )
                  )
                })
              )
            }, Skip())(VoidType),
            result.ref
          )
        }

        MethodDef(MemberFlags.empty, MethodIdent(newInstanceRecName),
            NoOriginalName, List(componentType, dimensions, offset), AnyType,
            Some(body))(
            OptimizerHints.empty, Version.fromInt(1))
      }

      val newMethods = for (method <- methods) yield {
        method.methodName match {
          case `newInstanceSingleName` =>
            // newInstance(jl.Class, int)  -->  newArray(jlClass.notNull, length)

            implicit val pos = method.pos

            val List(jlClassParam, lengthParam) = method.args

            val newBody = BinaryOp(BinaryOp.Class_newArray,
                UnaryOp(UnaryOp.CheckNotNull, jlClassParam.ref),
                lengthParam.ref)

            MethodDef(method.flags, method.name, method.originalName,
                method.args, method.resultType, Some(newBody))(
                method.optimizerHints.withInline(true), method.version)

          case `newInstanceMultiName` =>
            /* newInstance(jl.Class, int[])  -->
             * var outermostComponentType: jl.Class = jlClassParam
             * var i: int = 1
             * while (i != lengths.length) {
             *   outermostComponentType = getClass(this.newInstance(outermostComponentType, 0))
             *   i = i + 1
             * }
             * newInstanceRec(outermostComponentType, lengths, 0)
             */

            implicit val pos = method.pos

            val List(jlClassParam, lengthsParam) = method.args

            val newBody = {
              val outermostComponentType = varDef("outermostComponentType",
                  jlClassType, jlClassParam.ref, mutable = true)
              val i = varDef("i", IntType, IntLiteral(1), mutable = true)

              Block(
                outermostComponentType,
                i,
                While(BinaryOp(BinaryOp.Int_!=, i.ref, arrayLength(lengthsParam.ref)), {
                  Block(
                    Assign(
                      outermostComponentType.ref,
                      getClass(Apply(EAF, This()(ClassType(ReflectArrayModClass, nullable = false)),
                          MethodIdent(newInstanceSingleName),
                          List(outermostComponentType.ref, IntLiteral(0)))(AnyType))
                    ),
                    Assign(
                      i.ref,
                      BinaryOp(BinaryOp.Int_+, i.ref, IntLiteral(1))
                    )
                  )
                }),
                Apply(EAF, This()(ClassType(ReflectArrayModClass, nullable = false)),
                    MethodIdent(newInstanceRecName),
                    List(outermostComponentType.ref, lengthsParam.ref, IntLiteral(0)))(
                    AnyType)
              )
            }

            MethodDef(method.flags, method.name, method.originalName,
                method.args, method.resultType, Some(newBody))(
                method.optimizerHints, method.version)

          case _ =>
            method
        }
      }

      newInstanceRecMethod :: newMethods
    }

    private def jsConstructorHackBelow11(ownerKind: ClassKind,
        jsMethodProps: List[JSMethodPropDef]): (Option[JSConstructorDef], List[JSMethodPropDef]) = {
      val jsConstructorBuilder = new OptionBuilder[JSConstructorDef]
      val jsMethodPropsBuilder = List.newBuilder[JSMethodPropDef]

      jsMethodProps.foreach {
        case methodDef @ JSMethodDef(flags, StringLiteral("constructor"), args, restParam, body)
            if flags.namespace == MemberNamespace.Public =>
          val bodyStats = body match {
            case Block(stats) => stats
            case _            => body :: Nil
          }

          bodyStats.span(!_.isInstanceOf[JSSuperConstructorCall]) match {
            case (beforeSuper, (superCall: JSSuperConstructorCall) :: afterSuper0) =>
              val newFlags = flags.withNamespace(MemberNamespace.Constructor)
              val afterSuper = maybeHackJSConstructorDefAfterSuper(ownerKind, afterSuper0, superCall.pos)
              val newBody = JSConstructorBody(beforeSuper, superCall, afterSuper)(body.pos)
              val ctorDef = JSConstructorDef(newFlags, args, restParam, newBody)(
                  methodDef.optimizerHints, Unversioned)(methodDef.pos)
              jsConstructorBuilder += Hashers.hashJSConstructorDef(ctorDef)

            case _ =>
              /* This is awkward: we have an old-style JS constructor that is
               * structurally invalid. We crash in order not to silently
               * ignore errors.
               */
              throw new IOException(
                  s"Found invalid pre-1.11 JS constructor def at ${methodDef.pos}:\n${methodDef.show}")
          }

        case exportedMember =>
          jsMethodPropsBuilder += exportedMember
      }

      (jsConstructorBuilder.result(), jsMethodPropsBuilder.result())
    }

    /** Rewrites `scala.scalajs.runtime.AnonFunctionN`s from before 1.19.
     *
     *  Before 1.19, these classes were defined as
     *  {{{
     *  // final in source code
     *  class AnonFunctionN extends AbstractFunctionN {
     *    val f: any
     *    def this(f: any) = {
     *      this.f = f;
     *      super()
     *    }
     *    def apply(...args: any): any = f(...args)
     *  }
     *  }}}
     *
     *  Starting with 1.19, they were rewritten to be used as SAM classes for
     *  `NewLambda` nodes. The new IR shape is
     *  {{{
     *  // sealed abstract in source code
     *  class AnonFunctionN extends AbstractFunctionN {
     *    def this() = super()
     *  }
     *  }}}
     *
     *  This function rewrites those classes to the new shape.
     *
     *  The rewrite also applies to Scala 3's `AnonFunctionXXL`.
     *
     *  Tests are in `sbt-plugin/src/sbt-test/linker/anonfunction-compat/`.
     */
    private def anonFunctionClassDefHackBelow19(classDef: ClassDef): ClassDef = {
      import classDef._

      if (!HackNames.allAnonFunctionClasses.contains(className)) {
        classDef
      } else {
        val newCtor: MethodDef = {
          // Find the old constructor to get its position and version
          val oldCtor = methods.find(_.methodName.isConstructor).getOrElse {
            throw new InvalidIRException(classDef,
                s"Did not find a constructor in ${className.nameString}")
          }
          implicit val pos = oldCtor.pos

          // constructor def <init>() = this.superClass::<init>()
          MethodDef(
            MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
            MethodIdent(NoArgConstructorName),
            NoOriginalName,
            Nil,
            VoidType,
            Some {
              ApplyStatically(
                ApplyFlags.empty.withConstructor(true),
                This()(ClassType(className, nullable = false)),
                superClass.get.name,
                MethodIdent(NoArgConstructorName),
                Nil
              )(VoidType)
            }
          )(OptimizerHints.empty, oldCtor.version)
        }

        ClassDef(
          name,
          originalName,
          kind,
          jsClassCaptures,
          superClass,
          interfaces,
          jsSuperClass,
          jsNativeLoadSpec,
          fields = Nil, // throws away the `f` field
          methods = List(newCtor), // throws away the old constructor and `apply` method
          jsConstructor,
          jsMethodProps,
          jsNativeMembers,
          topLevelExportDefs
        )(OptimizerHints.empty)(pos) // throws away the `@inline`
      }
    }

    private def readFieldDef()(implicit pos: Position): FieldDef = {
      val flags = MemberFlags.fromBits(readInt())
      val name = readFieldIdentForEnclosingClass()
      val originalName = readOriginalName()

      val ftpe0 = readType()
      val ftpe = if (hacks.useBelow(5) && ftpe0 == NothingType) {
        /* Note [Nothing FieldDef rewrite]
         * val field: nothing  -->  val field: null
         */
        NullType
      } else {
        ftpe0
      }

      FieldDef(flags, name, originalName, ftpe)
    }

    private def readJSFieldDef()(implicit pos: Position): JSFieldDef =
      JSFieldDef(MemberFlags.fromBits(readInt()), readTree(), readType())

    private def readMethodDef(owner: ClassName, ownerKind: ClassKind)(
        implicit pos: Position): MethodDef = {
      val optHash = readOptHash()
      // read and discard the length
      val len = readInt()
      assert(len >= 0)

      val flags = MemberFlags.fromBits(readInt())

      val name = {
        /* In versions 1.0 and 1.1 of the IR, static initializers and
         * class initializers were conflated into one concept, which was
         * handled differently in the linker based on whether the owner
         * was a JS type or not. They were serialized as `<clinit>`.
         * Starting with 1.2, `<clinit>` is only for class initializers.
         * If we read a definition for a `<clinit>` in a non-JS type, we
         * rewrite it as a static initializers instead (`<stinit>`).
         */
        val name0 = readMethodIdent()
        if (hacks.useBelow(2) &&
            name0.name == ClassInitializerName &&
            !ownerKind.isJSType) {
          MethodIdent(StaticInitializerName)(name0.pos)
        } else {
          name0
        }
      }

      val originalName = readOriginalName()
      val args = readParamDefs()
      val resultType = readType()
      val body = readOptTree()
      val optimizerHints = OptimizerHints.fromBits(readInt())

      if (hacks.useBelow(1) &&
          flags.namespace == MemberNamespace.Public &&
          owner == HackNames.SystemModule &&
          name.name == HackNames.identityHashCodeName) {
        /* #3976: Before 1.1, the javalib relied on wrong linker dispatch.
         * We simply replace it with a correct implementation.
         */
        assert(args.size == 1)

        val patchedBody = Some(UnaryOp(UnaryOp.IdentityHashCode, args(0).ref))
        val patchedOptimizerHints = OptimizerHints.empty.withInline(true)

        MethodDef(flags, name, originalName, args, resultType, patchedBody)(
            patchedOptimizerHints, optHash)
      } else if (hacks.useBelow(5) &&
          flags.namespace == MemberNamespace.Public &&
          owner == ObjectClass &&
          name.name == HackNames.cloneName) {
        /* #4391: In version 1.5, we introduced a dedicated IR node for the
         * primitive operation behind `Object.clone()`. This allowed to
         * simplify the linker by removing several special-cases that
         * treated it specially (for example, preventing it from being
         * inlined if the receiver could be an array). The simplifications
         * mean that the old implementation is not valid anymore, and so we
         * must force using the new implementation if we read IR from an
         * older version.
         */
        assert(args.isEmpty)

        val thisValue = This()(ClassType(ObjectClass, nullable = false))
        val cloneableClassType = ClassType(CloneableClass, nullable = true)

        val patchedBody = Some {
          If(IsInstanceOf(thisValue, cloneableClassType.toNonNullable),
              UnaryOp(UnaryOp.Clone,
                  UnaryOp(UnaryOp.CheckNotNull, AsInstanceOf(thisValue, cloneableClassType))),
              UnaryOp(UnaryOp.Throw,
                  New(
                    HackNames.CloneNotSupportedExceptionClass,
                    MethodIdent(NoArgConstructorName),
                    Nil)))(cloneableClassType)
        }
        val patchedOptimizerHints = OptimizerHints.empty.withInline(true)

        MethodDef(flags, name, originalName, args, resultType, patchedBody)(
            patchedOptimizerHints, optHash)
      } else {
        val patchedBody = body.map(bodyHackBelow6(_, isStat = resultType == VoidType))
        MethodDef(flags, name, originalName, args, resultType, patchedBody)(
            optimizerHints, optHash)
      }
    }

    private def readJSConstructorDef(ownerKind: ClassKind)(
        implicit pos: Position): JSConstructorDef = {

      val optHash = readOptHash()
      // read and discard the length
      val len = readInt()
      assert(len >= 0)

      /* JSConstructorDef was introduced in 1.11. Therefore, by
       * construction, they never need the body hack below 1.6.
       */

      val flags = MemberFlags.fromBits(readInt())
      val (params, restParam) = readParamDefsWithRest()
      val bodyPos = readPosition()
      val beforeSuper = readTrees()
      val superCall = readTree().asInstanceOf[JSSuperConstructorCall]
      val afterSuper0 = readTrees()
      val afterSuper = maybeHackJSConstructorDefAfterSuper(ownerKind, afterSuper0, superCall.pos)
      val body = JSConstructorBody(beforeSuper, superCall, afterSuper)(bodyPos)
      JSConstructorDef(flags, params, restParam, body)(
          OptimizerHints.fromBits(readInt()), optHash)
    }

    private def maybeHackJSConstructorDefAfterSuper(ownerKind: ClassKind,
        afterSuper0: List[Tree], superCallPos: Position): List[Tree] = {
      if (hacks.useBelow(18) && ownerKind == ClassKind.JSModuleClass) {
        afterSuper0 match {
          case StoreModule() :: _ => afterSuper0
          case _                  => StoreModule()(superCallPos) :: afterSuper0
        }
      } else {
        afterSuper0
      }
    }

    private def readJSMethodDef()(implicit pos: Position): JSMethodDef = {
      val optHash = readOptHash()
      // read and discard the length
      val len = readInt()
      assert(len >= 0)

      val flags = MemberFlags.fromBits(readInt())
      val name = bodyHackBelow6Expr(readTree())
      val (params, restParam) = readParamDefsWithRest()
      val body = bodyHackBelow6Expr(readTree())
      JSMethodDef(flags, name, params, restParam, body)(
          OptimizerHints.fromBits(readInt()), optHash)
    }

    private def readJSPropertyDef()(implicit pos: Position): JSPropertyDef = {
      val optHash: Version = {
        if (hacks.useBelow(13)) {
          Unversioned
        } else {
          val optHash = readOptHash()
          // read and discard the length
          val len = readInt()
          assert(len >= 0)
          optHash
        }
      }

      val flags = MemberFlags.fromBits(readInt())
      val name = bodyHackBelow6Expr(readTree())
      val getterBody = readOptTree().map(bodyHackBelow6Expr(_))
      val setterArgAndBody = {
        if (readBoolean())
          Some((readParamDef(), bodyHackBelow6Expr(readTree())))
        else
          None
      }
      JSPropertyDef(flags, name, getterBody, setterArgAndBody)(optHash)
    }

    private def readJSNativeMemberDef()(implicit pos: Position): JSNativeMemberDef = {
      val flags = MemberFlags.fromBits(readInt())
      val name = readMethodIdent()
      val jsNativeLoadSpec = readJSNativeLoadSpec().get
      JSNativeMemberDef(flags, name, jsNativeLoadSpec)
    }

    /* #4442 and #4601: Patch Labeled, If, Match and TryCatch nodes in
     * statement position to have type VoidType. These 4 nodes are the
     * control structures whose result type is explicitly specified (and
     * not derived from their children like Block or TryFinally, or
     * constant like While).
     */
    private object BodyHackBelow6Transformer extends Transformers.Transformer {
      def transformStat(tree: Tree): Tree = {
        implicit val pos = tree.pos

        tree match {
          // Nodes that we actually need to alter
          case Labeled(label, _, body) =>
            Labeled(label, VoidType, transformStat(body))
          case If(cond, thenp, elsep) =>
            If(transform(cond), transformStat(thenp), transformStat(elsep))(VoidType)
          case Match(selector, cases, default) =>
            Match(transform(selector), cases.map(c => (c._1, transformStat(c._2))),
                transformStat(default))(VoidType)
          case TryCatch(block, errVar, errVarOriginalName, handler) =>
            TryCatch(transformStat(block), errVar, errVarOriginalName,
                transformStat(handler))(VoidType)

          // Nodes for which we need to forward the statement position
          case Block(stats) =>
            Block(stats.map(transformStat(_)))
          case TryFinally(block, finalizer) =>
            Block(transformStat(block), transformStat(finalizer))

          // For everything else, delegate to transform
          case _ =>
            transform(tree)
        }
      }

      override def transform(tree: Tree): Tree = {
        implicit val pos = tree.pos

        tree match {
          // Nodes that force a statement position for some of their parts
          case Block(stats) =>
            Block(stats.init.map(transformStat(_)), transform(stats.last))
          case While(cond, body) =>
            While(transform(cond), transformStat(body))
          case ForIn(obj, keyVar, keyVarOriginalName, body) =>
            ForIn(transform(obj), keyVar, keyVarOriginalName, transformStat(body))
          case TryFinally(block, finalizer) =>
            TryFinally(transform(block), transformStat(finalizer))

          case _ =>
            super.transform(tree)
        }
      }

      def transform(tree: Tree, isStat: Boolean): Tree =
        if (isStat) transformStat(tree)
        else transform(tree)
    }

    private def bodyHackBelow6(body: Tree, isStat: Boolean): Tree =
      if (!hacks.useBelow(6)) body
      else BodyHackBelow6Transformer.transform(body, isStat)

    private def bodyHackBelow6Expr(body: Tree): Tree = bodyHackBelow6(body, isStat = false)

    def readTopLevelExportDef(): TopLevelExportDef = {
      implicit val pos = readPosition()
      val tag = readByte()

      def readJSMethodDef(): JSMethodDef = {
        implicit val pos = readPosition()
        val tag = readByte()
        assert(tag == TagJSMethodDef, s"unexpected tag $tag")
        this.readJSMethodDef()
      }

      def readModuleID(): String =
        if (hacks.useBelow(3)) DefaultModuleID
        else readString()

      (tag: @switch) match {
        case TagTopLevelJSClassExportDef => TopLevelJSClassExportDef(readModuleID(), readString())
        case TagTopLevelModuleExportDef  => TopLevelModuleExportDef(readModuleID(), readString())
        case TagTopLevelMethodExportDef  => TopLevelMethodExportDef(readModuleID(), readJSMethodDef())

        case TagTopLevelFieldExportDef =>
          TopLevelFieldExportDef(readModuleID(), readString(), readFieldIdentForEnclosingClass())
      }
    }

    def readTopLevelExportDefs(): List[TopLevelExportDef] =
      List.fill(readInt())(readTopLevelExportDef())

    def readLocalIdent(): LocalIdent = {
      implicit val pos = readPosition()
      LocalIdent(readLocalName())
    }

    def readFieldIdent(): FieldIdent = {
      // For historical reasons, the className comes *before* the position
      val className = readClassName()
      implicit val pos = readPosition()
      val simpleName = readSimpleFieldName()
      FieldIdent(makeFieldName(className, simpleName))
    }

    def readFieldIdentForEnclosingClass(): FieldIdent = {
      implicit val pos = readPosition()
      val simpleName = readSimpleFieldName()
      FieldIdent(makeFieldName(enclosingClassName, simpleName))
    }

    private def makeFieldName(className: ClassName, simpleName: SimpleFieldName): FieldName = {
      val newFieldName = FieldName(className, simpleName)
      uniqueFieldNames.getOrElseUpdate(newFieldName, newFieldName)
    }

    def readMethodIdent(): MethodIdent = {
      implicit val pos = readPosition()
      MethodIdent(readMethodName())
    }

    def readClassIdent(): ClassIdent = {
      implicit val pos = readPosition()
      ClassIdent(readClassName())
    }

    def readClassIdents(): List[ClassIdent] =
      List.fill(readInt())(readClassIdent())

    def readOptClassIdent(): Option[ClassIdent] = {
      if (readBoolean()) Some(readClassIdent())
      else None
    }

    def readParamDef(): ParamDef = {
      implicit val pos = readPosition()
      val name = readLocalIdent()
      val originalName = readOriginalName()
      val ptpe = readType()
      val mutable = readBoolean()

      if (hacks.useBelow(5)) {
        val rest = readBoolean()
        assert(!rest, "Illegal rest parameter")
      }

      ParamDef(name, originalName, ptpe, mutable)
    }

    def readParamDefs(): List[ParamDef] =
      List.fill(readInt())(readParamDef())

    def readParamDefsWithRest(): (List[ParamDef], Option[ParamDef]) = {
      if (hacks.useBelow(5)) {
        val (params, isRest) = List.fill(readInt()) {
          implicit val pos = readPosition()
          (ParamDef(readLocalIdent(), readOriginalName(), readType(), readBoolean()), readBoolean())
        }.unzip

        if (isRest.forall(!_)) {
          (params, None)
        } else {
          assert(isRest.init.forall(!_), "illegal non-last rest parameter")
          (params.init, Some(params.last))
        }
      } else {
        val params = readParamDefs()

        val restParam =
          if (readBoolean()) Some(readParamDef())
          else None

        (params, restParam)
      }
    }

    def readType(): Type = {
      val tag = readByte()
      (tag: @switch) match {
        case TagAnyType        => AnyType
        case TagAnyNotNullType => AnyNotNullType
        case TagNothingType    => NothingType
        case TagUndefType      => UndefType
        case TagBooleanType    => BooleanType
        case TagCharType       => CharType
        case TagByteType       => ByteType
        case TagShortType      => ShortType
        case TagIntType        => IntType
        case TagLongType       => LongType
        case TagFloatType      => FloatType
        case TagDoubleType     => DoubleType
        case TagStringType     => StringType
        case TagNullType       => NullType
        case TagVoidType       => VoidType

        case TagClassType => ClassType(readClassName(), nullable = true)
        case TagArrayType => ArrayType(readArrayTypeRef(), nullable = true)

        case TagNonNullClassType => ClassType(readClassName(), nullable = false)
        case TagNonNullArrayType => ArrayType(readArrayTypeRef(), nullable = false)

        case TagClosureType | TagNonNullClosureType =>
          val paramTypes = readTypes()
          val resultType = readType()
          ClosureType(paramTypes, resultType, nullable = tag == TagClosureType)

        case TagRecordType =>
          RecordType(List.fill(readInt()) {
            val name = readSimpleFieldName()
            val originalName = readString()
            val tpe = readType()
            val mutable = readBoolean()
            RecordType.Field(name, readOriginalName(), tpe, mutable)
          })
      }
    }

    def readTypes(): List[Type] =
      List.fill(readInt())(readType())

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

    def readClosureFlags(): ClosureFlags = {
      /* Before 1.19, the `flags` were a single `Boolean` for the `arrow` flag.
       * The bit pattern of `flags` was crafted so that it matches the old
       * boolean encoding for common values.
       */
      ClosureFlags.fromBits(readByte())
    }

    def readPosition(): Position = {
      import PositionFormat._

      val first = readByte()

      if (first == FormatNoPositionValue) {
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

    def readOptHash(): Version = {
      if (readBoolean()) {
        val hash = new Array[Byte](20)
        buf.get(hash)
        Version.fromHash(hash)
      } else {
        Unversioned
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
      /* Before 1.18, `LabelName`s were always wrapped in `LabelIdent`s, whose
       * encoding was a `Position` followed by the actual `LabelName`.
       */
      if (hacks.useBelow(18))
        readPosition() // intentional discard

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

    private def readSimpleFieldName(): SimpleFieldName = {
      val i = readInt()
      val existing = simpleFieldNames(i)
      if (existing ne null) {
        existing
      } else {
        val result = SimpleFieldName(encodedNames(i))
        simpleFieldNames(i) = result
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

    private def readClassNames(): List[ClassName] =
      List.fill(readInt())(readClassName())

    private def readMethodName(): MethodName =
      methodNames(readInt())

    def readOriginalName(): OriginalName =
      if (readBoolean()) OriginalName(encodedNames(readInt()))
      else OriginalName.NoOriginalName

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

  /** Hacks for backwards compatible deserializing.
   *
   *  `private[ir]` for testing purposes only.
   */
  private[ir] final class Hacks(sourceVersion: String) {
    private val fromVersion = sourceVersion match {
      case CompatibleStableIRVersionRegex(minorDigits) => minorDigits.toInt
      case _                                           => Int.MaxValue // never use any hack
    }

    /** Should we use the hacks to migrate from an IR version below `targetVersion`? */
    def useBelow(targetVersion: Int): Boolean =
      fromVersion < targetVersion
  }

  /** Names needed for hacks. */
  private object HackNames {
    val AnonFunctionXXLClass =
      ClassName("scala.scalajs.runtime.AnonFunctionXXL") // from the Scala 3 library
    val CloneNotSupportedExceptionClass =
      ClassName("java.lang.CloneNotSupportedException")
    val SystemModule: ClassName =
      ClassName("java.lang.System$")
    val ReflectArrayClass =
      ClassName("java.lang.reflect.Array")
    val ReflectArrayModClass =
      ClassName("java.lang.reflect.Array$")

    val ObjectArrayType = ArrayType(ArrayTypeRef(ObjectRef, 1), nullable = true)

    private val applySimpleName = SimpleMethodName("apply")

    val cloneName: MethodName =
      MethodName("clone", Nil, ObjectRef)
    val identityHashCodeName: MethodName =
      MethodName("identityHashCode", List(ObjectRef), IntRef)
    val newInstanceSingleName: MethodName =
      MethodName("newInstance", List(ClassRef(ClassClass), IntRef), ObjectRef)
    val newInstanceMultiName: MethodName =
      MethodName("newInstance", List(ClassRef(ClassClass), ArrayTypeRef(IntRef, 1)), ObjectRef)

    private val anonFunctionArities: Map[ClassName, Int] =
      (0 to 22).map(arity => ClassName(s"scala.scalajs.runtime.AnonFunction$arity") -> arity).toMap
    val allAnonFunctionClasses: Set[ClassName] =
      anonFunctionArities.keySet + AnonFunctionXXLClass

    object AnonFunctionClass {
      def unapply(cls: ClassName): Option[Int] =
        anonFunctionArities.get(cls)
    }

    lazy val anonFunctionDescriptors: IndexedSeq[NewLambda.Descriptor] = {
      anonFunctionArities.toIndexedSeq.sortBy(_._2).map { case (className, arity) =>
        NewLambda.Descriptor(
          superClass = className,
          interfaces = Nil,
          methodName = MethodName(applySimpleName, List.fill(arity)(ObjectRef), ObjectRef),
          paramTypes = List.fill(arity)(AnyType),
          resultType = AnyType
        )
      }
    }

    lazy val anonFunctionXXLDescriptor: NewLambda.Descriptor = {
      NewLambda.Descriptor(
        superClass = AnonFunctionXXLClass,
        interfaces = Nil,
        methodName = MethodName(applySimpleName, List(ObjectArrayType.arrayTypeRef), ObjectRef),
        paramTypes = List(ObjectArrayType),
        resultType = AnyType
      )
    }
  }

  private class OptionBuilder[T] {
    private var value: Option[T] = None

    def +=(x: T): Unit = {
      require(value.isEmpty)
      value = Some(x)
    }

    def result(): Option[T] = value
  }

  /* Note [Nothing FieldDef rewrite]
   *
   * Prior to Scala.js 1.5.0, the compiler back-end emitted `FieldDef`s with
   * type `nothing` (`NothingType`). Until Scala.js 1.3.1, such fields happened
   * to link by chance. Scala.js 1.4.0 changed the Emitter in a way that they
   * did not link anymore (#4370), which broke some existing code.
   *
   * In Scala.js 1.5.0, we declared that such definitions are invalid IR, since
   * fields need a zero value to initialize them, and `nothing` doesn't have
   * one.
   *
   * To preserve backward binary compatibility of IR produced by earlier
   * versions, we use the following rewrites as a deserialization hack:
   *
   * - `FieldDef`s with type `nothing` are rewritten with type `null`:
   *   val field: nothing  -->  val field: null
   * - `Select`s with type `nothing` are rewritten with type `null`, but are
   *   then wrapped in a `throw` to preserve the well-typedness of the
   *   surrounding IR:
   *   qual.field[nothing]  -->  throw qual.field[null]
   * - In an `Assign`, the inserted `throw` would be invalid. Therefore we have
   *   to unwrap the `throw`. The rhs being of type `nothing` (in IR that was
   *   originally well typed), it can be assigned to a field of type `null`.
   *   (throw qual.field[null]) = rhs  -->  qual.field[null] = rhs
   */
}
