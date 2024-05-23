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

package org.scalajs.linker.backend.webassembly

import scala.annotation.tailrec

import java.nio.{ByteBuffer, ByteOrder}

import org.scalajs.ir.{Position, UTF8String}
import org.scalajs.linker.backend.javascript.SourceMapWriter

import Instructions._
import Identitities._
import Modules._
import Types._

private sealed class BinaryWriter(module: Module, emitDebugInfo: Boolean) {
  import BinaryWriter._

  /** The big output buffer. */
  private[BinaryWriter] val buf = new Buffer()

  private val typeIdxValues: Map[TypeID, Int] =
    module.types.flatMap(_.subTypes).map(_.id).zipWithIndex.toMap

  private val dataIdxValues: Map[DataID, Int] =
    module.datas.map(_.id).zipWithIndex.toMap

  private val funcIdxValues: Map[FunctionID, Int] = {
    val importedFunctionIDs = module.imports.collect {
      case Import(_, _, ImportDesc.Func(id, _, _)) => id
    }
    val allIDs = importedFunctionIDs ::: module.funcs.map(_.id)
    allIDs.zipWithIndex.toMap
  }

  private val tagIdxValues: Map[TagID, Int] = {
    val importedTagIDs = module.imports.collect { case Import(_, _, ImportDesc.Tag(id, _, _)) =>
      id
    }
    val allIDs = importedTagIDs ::: module.tags.map(_.id)
    allIDs.zipWithIndex.toMap
  }

  private val globalIdxValues: Map[GlobalID, Int] = {
    val importedGlobalIDs = module.imports.collect {
      case Import(_, _, ImportDesc.Global(id, _, _, _)) => id
    }
    val allIDs = importedGlobalIDs ::: module.globals.map(_.id)
    allIDs.zipWithIndex.toMap
  }

  private val fieldIdxValues: Map[TypeID, Map[FieldID, Int]] = {
    (for {
      recType <- module.types
      SubType(typeID, _, _, _, StructType(fields)) <- recType.subTypes
    } yield {
      typeID -> fields.map(_.id).zipWithIndex.toMap
    }).toMap
  }

  private var localIdxValues: Option[Map[LocalID, Int]] = None

  /** A stack of the labels in scope (innermost labels are on top of the stack). */
  private var labelsInScope: List[Option[LabelID]] = Nil

  private def withLocalIdxValues(values: Map[LocalID, Int])(f: => Unit): Unit = {
    val saved = localIdxValues
    localIdxValues = Some(values)
    try f
    finally localIdxValues = saved
  }

  protected def emitStartFuncPosition(pos: Position): Unit = ()
  protected def emitPosition(pos: Position): Unit = ()
  protected def emitEndFuncPosition(): Unit = ()
  protected def emitSourceMapSection(): Unit = ()

  def write(): ByteBuffer = {
    // magic header: null char + "asm"
    buf.byte(0)
    buf.byte('a')
    buf.byte('s')
    buf.byte('m')

    // version
    buf.byte(1)
    buf.byte(0)
    buf.byte(0)
    buf.byte(0)

    writeSection(SectionType)(writeTypeSection())
    writeSection(SectionImport)(writeImportSection())
    writeSection(SectionFunction)(writeFunctionSection())
    writeSection(SectionTag)(writeTagSection())
    writeSection(SectionGlobal)(writeGlobalSection())
    writeSection(SectionExport)(writeExportSection())
    if (module.start.isDefined)
      writeSection(SectionStart)(writeStartSection())
    writeSection(SectionElement)(writeElementSection())
    if (module.datas.nonEmpty)
      writeSection(SectionDataCount)(writeDataCountSection())
    writeSection(SectionCode)(writeCodeSection())
    writeSection(SectionData)(writeDataSection())

    if (emitDebugInfo)
      writeCustomSection("name")(writeNameCustomSection())

    emitSourceMapSection()

    buf.result()
  }

  private def writeSection(sectionID: Byte)(sectionContent: => Unit): Unit = {
    buf.byte(sectionID)
    buf.byteLengthSubSection(sectionContent)
  }

  protected final def writeCustomSection(customSectionName: String)(
      sectionContent: => Unit): Unit = {
    writeSection(SectionCustom) {
      buf.name(customSectionName)
      sectionContent
    }
  }

  private def writeTypeSection(): Unit = {
    buf.vec(module.types) { recType =>
      recType.subTypes match {
        case singleSubType :: Nil =>
          writeSubType(singleSubType)
        case subTypes =>
          buf.byte(0x4E) // `rectype`
          buf.vec(subTypes)(writeSubType(_))
      }
    }
  }

  private def writeSubType(subType: SubType): Unit = {
    subType match {
      case SubType(_, _, true, None, compositeType) =>
        writeCompositeType(compositeType)
      case _ =>
        buf.byte(if (subType.isFinal) 0x4F else 0x50)
        buf.opt(subType.superType)(writeTypeIdx(_))
        writeCompositeType(subType.compositeType)
    }
  }

  private def writeCompositeType(compositeType: CompositeType): Unit = {
    def writeFieldType(fieldType: FieldType): Unit = {
      writeType(fieldType.tpe)
      buf.boolean(fieldType.isMutable)
    }

    compositeType match {
      case ArrayType(fieldType) =>
        buf.byte(0x5E) // array
        writeFieldType(fieldType)
      case StructType(fields) =>
        buf.byte(0x5F) // struct
        buf.vec(fields)(field => writeFieldType(field.fieldType))
      case FunctionType(params, results) =>
        buf.byte(0x60) // func
        writeResultType(params)
        writeResultType(results)
    }
  }

  private def writeImportSection(): Unit = {
    buf.vec(module.imports) { imprt =>
      buf.name(imprt.module)
      buf.name(imprt.name)

      imprt.desc match {
        case ImportDesc.Func(_, _, typeID) =>
          buf.byte(0x00) // func
          writeTypeIdx(typeID)
        case ImportDesc.Global(_, _, isMutable, tpe) =>
          buf.byte(0x03) // global
          writeType(tpe)
          buf.boolean(isMutable)
        case ImportDesc.Tag(_, _, typeID) =>
          buf.byte(0x04) // tag
          buf.byte(0x00) // exception kind (that is the only valid kind for now)
          writeTypeIdx(typeID)
      }
    }
  }

  private def writeFunctionSection(): Unit = {
    buf.vec(module.funcs) { fun =>
      writeTypeIdx(fun.typeID)
    }
  }

  private def writeTagSection(): Unit = {
    buf.vec(module.tags) { tag =>
      buf.byte(0x00) // exception kind (that is the only valid kind for now)
      writeTypeIdx(tag.typeID)
    }
  }

  private def writeGlobalSection(): Unit = {
    buf.vec(module.globals) { global =>
      writeType(global.tpe)
      buf.boolean(global.isMutable)
      writeExpr(global.init)
    }
  }

  private def writeExportSection(): Unit = {
    buf.vec(module.exports) { exp =>
      buf.name(exp.name)
      exp.desc match {
        case ExportDesc.Func(id) =>
          buf.byte(0x00)
          writeFuncIdx(id)
        case ExportDesc.Global(id) =>
          buf.byte(0x03)
          writeGlobalIdx(id)
      }
    }
  }

  private def writeStartSection(): Unit = {
    writeFuncIdx(module.start.get)
  }

  private def writeElementSection(): Unit = {
    buf.vec(module.elems) { element =>
      element.mode match {
        case Element.Mode.Declarative => buf.u32(7)
      }
      writeType(element.tpe)
      buf.vec(element.init) { expr =>
        writeExpr(expr)
      }
    }
  }

  private def writeDataSection(): Unit = {
    buf.vec(module.datas) { data =>
      data.mode match {
        case Data.Mode.Passive => buf.u32(1)
      }
      buf.vec(data.bytes)(buf.byte)
    }
  }

  private def writeDataCountSection(): Unit =
    buf.u32(module.datas.size)

  private def writeCodeSection(): Unit = {
    buf.vec(module.funcs) { func =>
      buf.byteLengthSubSection(writeFunc(func))
    }
  }

  private def writeNameCustomSection(): Unit = {
    // Currently, we only emit the function names

    val importFunctionNames = module.imports.collect {
      case Import(_, _, ImportDesc.Func(id, origName, _)) if origName.isDefined =>
        id -> origName
    }
    val definedFunctionNames =
      module.funcs.filter(_.originalName.isDefined).map(f => f.id -> f.originalName)
    val allFunctionNames = importFunctionNames ::: definedFunctionNames

    buf.byte(0x01) // function names
    buf.byteLengthSubSection {
      buf.vec(allFunctionNames) { elem =>
        writeFuncIdx(elem._1)
        buf.name(elem._2.get)
      }
    }
  }

  private def writeFunc(func: Function): Unit = {
    emitStartFuncPosition(func.pos)

    buf.vec(func.locals) { local =>
      buf.u32(1)
      writeType(local.tpe)
    }

    withLocalIdxValues((func.params ::: func.locals).map(_.id).zipWithIndex.toMap) {
      writeExpr(func.body)
    }

    emitEndFuncPosition()
  }

  private def writeType(tpe: StorageType): Unit = {
    tpe match {
      case tpe: SimpleType => buf.byte(tpe.binaryCode)
      case tpe: PackedType => buf.byte(tpe.binaryCode)

      case RefType(true, heapType: HeapType.AbsHeapType) =>
        buf.byte(heapType.binaryCode)

      case RefType(nullable, heapType) =>
        buf.byte(if (nullable) 0x63 else 0x64)
        writeHeapType(heapType)
    }
  }

  private def writeHeapType(heapType: HeapType): Unit = {
    heapType match {
      case HeapType.Type(typeID)          => writeTypeIdxs33(typeID)
      case heapType: HeapType.AbsHeapType => buf.byte(heapType.binaryCode)
    }
  }

  private def writeResultType(resultType: List[Type]): Unit =
    buf.vec(resultType)(writeType(_))

  private def writeTypeIdx(typeID: TypeID): Unit =
    buf.u32(typeIdxValues(typeID))

  private def writeFieldIdx(typeID: TypeID, fieldID: FieldID): Unit =
    buf.u32(fieldIdxValues(typeID)(fieldID))

  private def writeDataIdx(dataID: DataID): Unit =
    buf.u32(dataIdxValues(dataID))

  private def writeTypeIdxs33(typeID: TypeID): Unit =
    buf.s33OfUInt(typeIdxValues(typeID))

  private def writeFuncIdx(funcID: FunctionID): Unit =
    buf.u32(funcIdxValues(funcID))

  private def writeTagIdx(tagID: TagID): Unit =
    buf.u32(tagIdxValues(tagID))

  private def writeGlobalIdx(globalID: GlobalID): Unit =
    buf.u32(globalIdxValues(globalID))

  private def writeLocalIdx(localID: LocalID): Unit = {
    localIdxValues match {
      case Some(values) => buf.u32(values(localID))
      case None         => throw new IllegalStateException("Local name table is not available")
    }
  }

  private def writeLabelIdx(labelID: LabelID): Unit = {
    val relativeNumber = labelsInScope.indexOf(Some(labelID))
    if (relativeNumber < 0)
      throw new IllegalStateException(s"Cannot find $labelID in scope")
    buf.u32(relativeNumber)
  }

  private def writeExpr(expr: Expr): Unit = {
    for (instr <- expr.instr)
      writeInstr(instr)
    buf.byte(0x0B) // end
  }

  private def writeInstr(instr: Instr): Unit = {
    instr match {
      case PositionMark(pos) =>
        emitPosition(pos)

      case _ =>
        val opcode = instr.opcode
        if (opcode <= 0xFF) {
          buf.byte(opcode.toByte)
        } else {
          assert(opcode <= 0xFFFF,
              s"cannot encode an opcode longer than 2 bytes yet: ${opcode.toHexString}")
          buf.byte((opcode >>> 8).toByte)
          buf.byte(opcode.toByte)
        }

        writeInstrImmediates(instr)

        instr match {
          case instr: StructuredLabeledInstr =>
            // We must register even the `None` labels, because they contribute to relative numbering
            labelsInScope ::= instr.label
          case End =>
            labelsInScope = labelsInScope.tail
          case _ =>
            ()
        }
    }
  }

  private def writeInstrImmediates(instr: Instr): Unit = {
    def writeBrOnCast(labelIdx: LabelID, from: RefType, to: RefType): Unit = {
      val castFlags = ((if (from.nullable) 1 else 0) | (if (to.nullable) 2 else 0)).toByte
      buf.byte(castFlags)
      writeLabelIdx(labelIdx)
      writeHeapType(from.heapType)
      writeHeapType(to.heapType)
    }

    instr match {
      // Convenience categories

      case instr: SimpleInstr =>
        ()
      case instr: BlockTypeLabeledInstr =>
        writeBlockType(instr.blockTypeArgument)
      case instr: LabelInstr =>
        writeLabelIdx(instr.labelArgument)
      case instr: FuncInstr =>
        writeFuncIdx(instr.funcArgument)
      case instr: TypeInstr =>
        writeTypeIdx(instr.typeArgument)
      case instr: TagInstr =>
        writeTagIdx(instr.tagArgument)
      case instr: LocalInstr =>
        writeLocalIdx(instr.localArgument)
      case instr: GlobalInstr =>
        writeGlobalIdx(instr.globalArgument)
      case instr: HeapTypeInstr =>
        writeHeapType(instr.heapTypeArgument)
      case instr: RefTypeInstr =>
        writeHeapType(instr.refTypeArgument.heapType)
      case instr: StructFieldInstr =>
        writeTypeIdx(instr.structTypeID)
        writeFieldIdx(instr.structTypeID, instr.fieldID)

      // Specific instructions with unique-ish shapes

      case I32Const(v) => buf.i32(v)
      case I64Const(v) => buf.i64(v)
      case F32Const(v) => buf.f32(v)
      case F64Const(v) => buf.f64(v)

      case BrTable(labelIdxVector, defaultLabelIdx) =>
        buf.vec(labelIdxVector)(writeLabelIdx(_))
        writeLabelIdx(defaultLabelIdx)

      case TryTable(blockType, clauses, _) =>
        writeBlockType(blockType)
        buf.vec(clauses)(writeCatchClause(_))

      case ArrayNewData(typeIdx, dataIdx) =>
        writeTypeIdx(typeIdx)
        writeDataIdx(dataIdx)

      case ArrayNewFixed(typeIdx, length) =>
        writeTypeIdx(typeIdx)
        buf.u32(length)

      case ArrayCopy(destType, srcType) =>
        writeTypeIdx(destType)
        writeTypeIdx(srcType)

      case BrOnCast(labelIdx, from, to) =>
        writeBrOnCast(labelIdx, from, to)
      case BrOnCastFail(labelIdx, from, to) =>
        writeBrOnCast(labelIdx, from, to)

      case PositionMark(pos) =>
        throw new AssertionError(s"Unexpected $instr")
    }
  }

  private def writeCatchClause(clause: CatchClause): Unit = {
    buf.byte(clause.opcode.toByte)
    clause.tag.foreach(tag => writeTagIdx(tag))
    writeLabelIdx(clause.label)
  }

  private def writeBlockType(blockType: BlockType): Unit = {
    blockType match {
      case BlockType.ValueType(None)      => buf.byte(0x40)
      case BlockType.ValueType(Some(tpe)) => writeType(tpe)
      case BlockType.FunctionType(typeID) => writeTypeIdxs33(typeID)
    }
  }
}

object BinaryWriter {
  private final val SectionCustom = 0x00
  private final val SectionType = 0x01
  private final val SectionImport = 0x02
  private final val SectionFunction = 0x03
  private final val SectionTable = 0x04
  private final val SectionMemory = 0x05
  private final val SectionGlobal = 0x06
  private final val SectionExport = 0x07
  private final val SectionStart = 0x08
  private final val SectionElement = 0x09
  private final val SectionCode = 0x0A
  private final val SectionData = 0x0B
  private final val SectionDataCount = 0x0C
  private final val SectionTag = 0x0D

  def write(module: Module, emitDebugInfo: Boolean): ByteBuffer =
    new BinaryWriter(module, emitDebugInfo).write()

  def writeWithSourceMap(module: Module, emitDebugInfo: Boolean,
      sourceMapWriter: SourceMapWriter, sourceMapURI: String): ByteBuffer = {
    new WithSourceMap(module, emitDebugInfo, sourceMapWriter, sourceMapURI).write()
  }

  private[BinaryWriter] final class Buffer {
    private var buf: ByteBuffer =
      ByteBuffer.allocate(1024 * 1024).order(ByteOrder.LITTLE_ENDIAN)

    private def ensureRemaining(requiredRemaining: Int): Unit = {
      if (buf.remaining() < requiredRemaining) {
        buf.flip()
        val newCapacity = Integer.highestOneBit(buf.capacity() + requiredRemaining) << 1
        val newBuf = ByteBuffer.allocate(newCapacity).order(ByteOrder.LITTLE_ENDIAN)
        newBuf.put(buf)
        buf = newBuf
      }
    }

    def currentGlobalOffset: Int = buf.position()

    def result(): ByteBuffer = {
      buf.flip()
      buf
    }

    def byte(b: Byte): Unit = {
      ensureRemaining(1)
      buf.put(b)
    }

    def rawByteArray(array: Array[Byte]): Unit = {
      ensureRemaining(array.length)
      buf.put(array)
    }

    def boolean(b: Boolean): Unit =
      byte(if (b) 1 else 0)

    def u32(value: Int): Unit = unsignedLEB128(Integer.toUnsignedLong(value))

    def s32(value: Int): Unit = signedLEB128(value.toLong)

    def i32(value: Int): Unit = s32(value)

    def s33OfUInt(value: Int): Unit = signedLEB128(Integer.toUnsignedLong(value))

    def u64(value: Long): Unit = unsignedLEB128(value)

    def s64(value: Long): Unit = signedLEB128(value)

    def i64(value: Long): Unit = s64(value)

    def f32(value: Float): Unit = {
      ensureRemaining(4)
      buf.putFloat(value)
    }

    def f64(value: Double): Unit = {
      ensureRemaining(8)
      buf.putDouble(value)
    }

    def vec[A](elems: Iterable[A])(op: A => Unit): Unit = {
      u32(elems.size)
      for (elem <- elems)
        op(elem)
    }

    def opt[A](elemOpt: Option[A])(op: A => Unit): Unit =
      vec(elemOpt.toList)(op)

    def name(s: String): Unit =
      name(UTF8String(s))

    def name(utf8: UTF8String): Unit = {
      val len = utf8.length
      u32(len)
      ensureRemaining(len)
      utf8.writeTo(buf)
    }

    def byteLengthSubSection(subSectionContent: => Unit): Unit = {
      // Reserve 4 bytes at the current offset to store the byteLength later
      val byteLengthOffset = buf.position()
      ensureRemaining(4)
      val startOffset = buf.position() + 4
      buf.position(startOffset) // do not write the 4 bytes for now

      subSectionContent

      // Compute byteLength
      val endOffset = buf.position()
      val byteLength = endOffset - startOffset

      /* Because we limited ourselves to 4 bytes, we cannot represent a size
       * greater than 2^(4*7).
       */
      assert(byteLength < (1 << 28),
          s"Implementation restriction: Cannot write a subsection that large: $byteLength")

      /* Write the byteLength in the reserved slot. Note that we *always* use
       * 4 bytes to store the byteLength, even when less bytes are necessary in
       * the unsigned LEB encoding. The WebAssembly spec specifically calls out
       * this choice as valid. We leverage it to have predictable total offsets
       * when we write the code section, which is important to efficiently
       * generate source maps.
       */
      buf.put(byteLengthOffset, ((byteLength & 0x7F) | 0x80).toByte)
      buf.put(byteLengthOffset + 1, (((byteLength >>> 7) & 0x7F) | 0x80).toByte)
      buf.put(byteLengthOffset + 2, (((byteLength >>> 14) & 0x7F) | 0x80).toByte)
      buf.put(byteLengthOffset + 3, ((byteLength >>> 21) & 0x7F).toByte)
    }

    @tailrec
    private def unsignedLEB128(value: Long): Unit = {
      val next = value >>> 7
      if (next == 0) {
        byte(value.toByte)
      } else {
        byte(((value.toInt & 0x7F) | 0x80).toByte)
        unsignedLEB128(next)
      }
    }

    @tailrec
    private def signedLEB128(value: Long): Unit = {
      val chunk = value.toInt & 0x7F
      val next = value >> 7
      if (next == (if ((chunk & 0x40) != 0) -1 else 0)) {
        byte(chunk.toByte)
      } else {
        byte((chunk | 0x80).toByte)
        signedLEB128(next)
      }
    }
  }

  private final class WithSourceMap(module: Module, emitDebugInfo: Boolean,
      sourceMapWriter: SourceMapWriter, sourceMapURI: String)
      extends BinaryWriter(module, emitDebugInfo) {

    override protected def emitStartFuncPosition(pos: Position): Unit =
      sourceMapWriter.startNode(buf.currentGlobalOffset, pos)

    override protected def emitPosition(pos: Position): Unit = {
      sourceMapWriter.endNode(buf.currentGlobalOffset)
      sourceMapWriter.startNode(buf.currentGlobalOffset, pos)
    }

    override protected def emitEndFuncPosition(): Unit =
      sourceMapWriter.endNode(buf.currentGlobalOffset)

    override protected def emitSourceMapSection(): Unit = {
      // See https://github.com/WebAssembly/tool-conventions/blob/main/Debugging.md#source-maps
      writeCustomSection("sourceMappingURL") {
        buf.name(sourceMapURI)
      }
    }
  }
}
