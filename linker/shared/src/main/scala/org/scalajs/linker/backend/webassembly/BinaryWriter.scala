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

import org.scalajs.ir.{Position, UTF8String}
import org.scalajs.linker.backend.javascript.SourceMapWriter

import Instructions._
import Identitities._
import Modules._
import Types._

class BinaryWriter(module: Module, emitDebugInfo: Boolean) {
  import BinaryWriter._

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
  private var labelsInScope: List[Option[LabelID]] = Nil

  private def withLocalIdxValues(values: Map[LocalID, Int])(f: => Unit): Unit = {
    val saved = localIdxValues
    localIdxValues = Some(values)
    try f
    finally localIdxValues = saved
  }

  protected def emitStartFuncPosition(buf: Buffer, pos: Position): Unit = ()
  protected def emitPosition(buf: Buffer, pos: Position): Unit = ()
  protected def emitEndFuncPosition(buf: Buffer): Unit = ()
  protected def emitSourceMapSection(buf: Buffer): Unit = ()

  def write(): Array[Byte] = {
    val fullOutput = new Buffer()

    // magic header: null char + "asm"
    fullOutput.byte(0)
    fullOutput.byte('a')
    fullOutput.byte('s')
    fullOutput.byte('m')

    // version
    fullOutput.byte(1)
    fullOutput.byte(0)
    fullOutput.byte(0)
    fullOutput.byte(0)

    writeSection(fullOutput, SectionType)(writeTypeSection(_))
    writeSection(fullOutput, SectionImport)(writeImportSection(_))
    writeSection(fullOutput, SectionFunction)(writeFunctionSection(_))
    writeSection(fullOutput, SectionTag)(writeTagSection(_))
    writeSection(fullOutput, SectionGlobal)(writeGlobalSection(_))
    writeSection(fullOutput, SectionExport)(writeExportSection(_))
    if (module.start.isDefined)
      writeSection(fullOutput, SectionStart)(writeStartSection(_))
    writeSection(fullOutput, SectionElement)(writeElementSection(_))
    if (module.datas.nonEmpty)
      writeSection(fullOutput, SectionDataCount)(writeDataCountSection(_))
    writeSection(fullOutput, SectionCode)(writeCodeSection(_))
    writeSection(fullOutput, SectionData)(writeDataSection(_))

    if (emitDebugInfo)
      writeCustomSection(fullOutput, "name")(writeNameCustomSection(_))

    emitSourceMapSection(fullOutput)

    fullOutput.result()
  }

  private def writeSection(fullOutput: Buffer, sectionID: Byte)(f: Buffer => Unit): Unit = {
    fullOutput.byte(sectionID)
    fullOutput.byteLengthSubSection(f)
  }

  protected final def writeCustomSection(fullOutput: Buffer, customSectionName: String)(
      f: Buffer => Unit
  ): Unit = {
    writeSection(fullOutput, SectionCustom) { buf =>
      buf.name(customSectionName)
      f(buf)
    }
  }

  private def writeTypeSection(buf: Buffer): Unit = {
    buf.vec(module.types) { recType =>
      recType.subTypes match {
        case singleSubType :: Nil =>
          writeSubType(buf, singleSubType)
        case subTypes =>
          buf.byte(0x4E) // `rectype`
          buf.vec(subTypes)(writeSubType(buf, _))
      }
    }
  }

  private def writeSubType(buf: Buffer, subType: SubType): Unit = {
    subType match {
      case SubType(_, _, true, None, compositeType) =>
        writeCompositeType(buf, compositeType)
      case _ =>
        buf.byte(if (subType.isFinal) 0x4F else 0x50)
        buf.opt(subType.superType)(writeTypeIdx(buf, _))
        writeCompositeType(buf, subType.compositeType)
    }
  }

  private def writeCompositeType(buf: Buffer, compositeType: CompositeType): Unit = {
    def writeFieldType(fieldType: FieldType): Unit = {
      writeType(buf, fieldType.tpe)
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
        writeResultType(buf, params)
        writeResultType(buf, results)
    }
  }

  private def writeImportSection(buf: Buffer): Unit = {
    buf.vec(module.imports) { imprt =>
      buf.name(imprt.module)
      buf.name(imprt.name)

      imprt.desc match {
        case ImportDesc.Func(_, _, typeID) =>
          buf.byte(0x00) // func
          writeTypeIdx(buf, typeID)
        case ImportDesc.Global(_, _, tpe, isMutable) =>
          buf.byte(0x03) // global
          writeType(buf, tpe)
          buf.boolean(isMutable)
        case ImportDesc.Tag(_, _, typeID) =>
          buf.byte(0x04) // tag
          buf.byte(0x00) // exception kind (that is the only valid kind for now)
          writeTypeIdx(buf, typeID)
      }
    }
  }

  private def writeFunctionSection(buf: Buffer): Unit = {
    buf.vec(module.funcs) { fun =>
      writeTypeIdx(buf, fun.typeID)
    }
  }

  private def writeTagSection(buf: Buffer): Unit = {
    buf.vec(module.tags) { tag =>
      buf.byte(0x00) // exception kind (that is the only valid kind for now)
      writeTypeIdx(buf, tag.typeID)
    }
  }

  private def writeGlobalSection(buf: Buffer): Unit = {
    buf.vec(module.globals) { global =>
      writeType(buf, global.tpe)
      buf.boolean(global.isMutable)
      writeExpr(buf, global.init)
    }
  }

  private def writeExportSection(buf: Buffer): Unit = {
    buf.vec(module.exports) { exp =>
      buf.name(exp.name)
      exp.desc match {
        case ExportDesc.Func(id, _) =>
          buf.byte(0x00)
          writeFuncIdx(buf, id)
        case ExportDesc.Global(id, _) =>
          buf.byte(0x03)
          writeGlobalIdx(buf, id)
      }
    }
  }

  private def writeStartSection(buf: Buffer): Unit = {
    writeFuncIdx(buf, module.start.get)
  }

  private def writeElementSection(buf: Buffer): Unit = {
    buf.vec(module.elems) { element =>
      element.mode match {
        case Element.Mode.Passive     => buf.byte(5)
        case Element.Mode.Declarative => buf.byte(7)
      }
      writeType(buf, element.tpe)
      buf.vec(element.init) { expr =>
        writeExpr(buf, expr)
      }
    }
  }

  private def writeDataSection(buf: Buffer): Unit = {
    buf.vec(module.datas) { data =>
      data.mode match {
        case Data.Mode.Passive => buf.byte(1)
      }
      buf.vec(data.bytes)(buf.byte)
    }
  }

  private def writeDataCountSection(buf: Buffer): Unit =
    buf.u32(module.datas.size)

  private def writeCodeSection(buf: Buffer): Unit = {
    buf.vec(module.funcs) { func =>
      buf.byteLengthSubSection(writeFunc(_, func))
    }
  }

  private def writeNameCustomSection(buf: Buffer): Unit = {
    // Currently, we only emit the function names

    val importFunctionNames = module.imports.collect {
      case Import(_, _, ImportDesc.Func(id, origName, _)) if origName.isDefined =>
        id -> origName
    }
    val definedFunctionNames =
      module.funcs.filter(_.originalName.isDefined).map(f => f.id -> f.originalName)
    val allFunctionNames = importFunctionNames ::: definedFunctionNames

    buf.byte(0x01) // function names
    buf.byteLengthSubSection { buf =>
      buf.vec(allFunctionNames) { elem =>
        writeFuncIdx(buf, elem._1)
        buf.name(elem._2.get)
      }
    }
  }

  private def writeFunc(buf: Buffer, func: Function): Unit = {
    emitStartFuncPosition(buf, func.pos)

    buf.vec(func.locals) { local =>
      buf.u32(1)
      writeType(buf, local.tpe)
    }

    withLocalIdxValues((func.params ::: func.locals).map(_.id).zipWithIndex.toMap) {
      writeExpr(buf, func.body)
    }

    emitEndFuncPosition(buf)
  }

  private def writeType(buf: Buffer, tpe: StorageType): Unit = {
    tpe match {
      case tpe: SimpleType => buf.byte(tpe.binaryCode)
      case tpe: PackedType => buf.byte(tpe.binaryCode)

      case RefType(true, heapType: HeapType.AbsHeapType) =>
        buf.byte(heapType.binaryCode)

      case RefType(nullable, heapType) =>
        buf.byte(if (nullable) 0x63 else 0x64)
        writeHeapType(buf, heapType)
    }
  }

  private def writeHeapType(buf: Buffer, heapType: HeapType): Unit = {
    heapType match {
      case HeapType.Type(typeID)          => writeTypeIdxs33(buf, typeID)
      case heapType: HeapType.AbsHeapType => buf.byte(heapType.binaryCode)
    }
  }

  private def writeResultType(buf: Buffer, resultType: List[Type]): Unit =
    buf.vec(resultType)(writeType(buf, _))

  private def writeTypeIdx(buf: Buffer, typeID: TypeID): Unit =
    buf.u32(typeIdxValues(typeID))

  private def writeFieldIdx(buf: Buffer, typeID: TypeID, fieldID: FieldID): Unit =
    buf.u32(fieldIdxValues(typeID)(fieldID))

  private def writeDataIdx(buf: Buffer, dataID: DataID): Unit =
    buf.u32(dataIdxValues(dataID))

  private def writeTypeIdxs33(buf: Buffer, typeID: TypeID): Unit =
    buf.s33OfUInt(typeIdxValues(typeID))

  private def writeFuncIdx(buf: Buffer, funcID: FunctionID): Unit =
    buf.u32(funcIdxValues(funcID))

  private def writeTagIdx(buf: Buffer, tagID: TagID): Unit =
    buf.u32(tagIdxValues(tagID))

  private def writeGlobalIdx(buf: Buffer, globalID: GlobalID): Unit =
    buf.u32(globalIdxValues(globalID))

  private def writeLocalIdx(buf: Buffer, localID: LocalID): Unit = {
    localIdxValues match {
      case Some(values) => buf.u32(values(localID))
      case None         => throw new IllegalStateException("Local name table is not available")
    }
  }

  private def writeLabelIdx(buf: Buffer, labelID: LabelID): Unit = {
    val relativeNumber = labelsInScope.indexOf(Some(labelID))
    if (relativeNumber < 0)
      throw new IllegalStateException(s"Cannot find $labelID in scope")
    buf.u32(relativeNumber)
  }

  private def writeExpr(buf: Buffer, expr: Expr): Unit = {
    for (instr <- expr.instr)
      writeInstr(buf, instr)
    buf.byte(0x0B) // end
  }

  private def writeInstr(buf: Buffer, instr: Instr): Unit = {
    instr match {
      case PositionMark(pos) =>
        emitPosition(buf, pos)

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

        writeInstrImmediates(buf, instr)

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

  private def writeInstrImmediates(buf: Buffer, instr: Instr): Unit = {
    def writeBrOnCast(labelIdx: LabelID, from: RefType, to: RefType): Unit = {
      val castFlags = ((if (from.nullable) 1 else 0) | (if (to.nullable) 2 else 0)).toByte
      buf.byte(castFlags)
      writeLabelIdx(buf, labelIdx)
      writeHeapType(buf, from.heapType)
      writeHeapType(buf, to.heapType)
    }

    instr match {
      // Convenience categories

      case instr: SimpleInstr =>
        ()
      case instr: BlockTypeLabeledInstr =>
        writeBlockType(buf, instr.blockTypeArgument)
      case instr: LabelInstr =>
        writeLabelIdx(buf, instr.labelArgument)
      case instr: FuncInstr =>
        writeFuncIdx(buf, instr.funcArgument)
      case instr: TypeInstr =>
        writeTypeIdx(buf, instr.typeArgument)
      case instr: TagInstr =>
        writeTagIdx(buf, instr.tagArgument)
      case instr: LocalInstr =>
        writeLocalIdx(buf, instr.localArgument)
      case instr: GlobalInstr =>
        writeGlobalIdx(buf, instr.globalArgument)
      case instr: HeapTypeInstr =>
        writeHeapType(buf, instr.heapTypeArgument)
      case instr: RefTypeInstr =>
        writeHeapType(buf, instr.refTypeArgument.heapType)
      case instr: StructFieldInstr =>
        writeTypeIdx(buf, instr.structTypeID)
        writeFieldIdx(buf, instr.structTypeID, instr.fieldID)

      // Specific instructions with unique-ish shapes

      case I32Const(v) => buf.i32(v)
      case I64Const(v) => buf.i64(v)
      case F32Const(v) => buf.f32(v)
      case F64Const(v) => buf.f64(v)

      case BrTable(labelIdxVector, defaultLabelIdx) =>
        buf.vec(labelIdxVector)(writeLabelIdx(buf, _))
        writeLabelIdx(buf, defaultLabelIdx)

      case TryTable(blockType, clauses, _) =>
        writeBlockType(buf, blockType)
        buf.vec(clauses) { clause =>
          buf.byte(clause.opcode.toByte)
          clause.tag.foreach(tag => writeTagIdx(buf, tag))
          writeLabelIdx(buf, clause.label)
        }

      case ArrayNewData(typeIdx, dataIdx) =>
        writeTypeIdx(buf, typeIdx)
        writeDataIdx(buf, dataIdx)

      case ArrayNewFixed(typeIdx, length) =>
        writeTypeIdx(buf, typeIdx)
        buf.u32(length)

      case ArrayCopy(destType, srcType) =>
        writeTypeIdx(buf, destType)
        writeTypeIdx(buf, srcType)

      case BrOnCast(labelIdx, from, to) =>
        writeBrOnCast(labelIdx, from, to)
      case BrOnCastFail(labelIdx, from, to) =>
        writeBrOnCast(labelIdx, from, to)

      case PositionMark(pos) =>
        throw new AssertionError(s"Unexpected $instr")
    }
  }

  private def writeBlockType(buf: Buffer, blockType: BlockType): Unit = {
    blockType match {
      case BlockType.ValueType(None)      => buf.byte(0x40)
      case BlockType.ValueType(Some(tpe)) => writeType(buf, tpe)
      case BlockType.FunctionType(typeID) => writeTypeIdxs33(buf, typeID)
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

  private final class Buffer {
    private var buf: Array[Byte] = new Array[Byte](1024 * 1024)
    private var size: Int = 0

    private def ensureCapacity(capacity: Int): Unit = {
      if (buf.length < capacity) {
        val newCapacity = Integer.highestOneBit(capacity) << 1
        buf = java.util.Arrays.copyOf(buf, newCapacity)
      }
    }

    def currentGlobalOffset: Int = size

    def result(): Array[Byte] =
      java.util.Arrays.copyOf(buf, size)

    def byte(b: Byte): Unit = {
      val newSize = size + 1
      ensureCapacity(newSize)
      buf(size) = b
      size = newSize
    }

    def rawByteArray(array: Array[Byte]): Unit = {
      val newSize = size + array.length
      ensureCapacity(newSize)
      System.arraycopy(array, 0, buf, size, array.length)
      size = newSize
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
      val bits = java.lang.Float.floatToIntBits(value)
      byte(bits.toByte)
      byte((bits >>> 8).toByte)
      byte((bits >>> 16).toByte)
      byte((bits >>> 24).toByte)
    }

    def f64(value: Double): Unit = {
      val bits = java.lang.Double.doubleToLongBits(value)
      byte(bits.toByte)
      byte((bits >>> 8).toByte)
      byte((bits >>> 16).toByte)
      byte((bits >>> 24).toByte)
      byte((bits >>> 32).toByte)
      byte((bits >>> 40).toByte)
      byte((bits >>> 48).toByte)
      byte((bits >>> 56).toByte)
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
      var i = 0
      while (i != len) {
        byte(utf8(i))
        i += 1
      }
    }

    def byteLengthSubSection(f: Buffer => Unit): Unit = {
      // Reserve 4 bytes at the current offset to store the byteLength later
      val byteLengthOffset = size
      val startOffset = byteLengthOffset + 4
      ensureCapacity(startOffset)
      size = startOffset // do not write the 4 bytes for now

      f(this)

      // Compute byteLength
      val endOffset = size
      val byteLength = endOffset - startOffset

      assert(byteLength < (1 << 28), s"Cannot write a subsection that large: $byteLength")

      /* Write the byteLength in the reserved slot. Note that we *always* use
       * 4 bytes to store the byteLength, even when less bytes are necessary in
       * the unsigned LEB encoding. The WebAssembly spec specifically calls out
       * this choice as valid. We leverage it to have predictable total offsets
       * when write the code section, which is important to efficiently
       * generate source maps.
       */
      buf(byteLengthOffset) = ((byteLength & 0x7F) | 0x80).toByte
      buf(byteLengthOffset + 1) = (((byteLength >>> 7) & 0x7F) | 0x80).toByte
      buf(byteLengthOffset + 2) = (((byteLength >>> 14) & 0x7F) | 0x80).toByte
      buf(byteLengthOffset + 3) = ((byteLength >>> 21) & 0x7F).toByte
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

  final class WithSourceMap(module: Module, emitDebugInfo: Boolean,
      sourceMapWriter: SourceMapWriter, sourceMapURI: String)
      extends BinaryWriter(module, emitDebugInfo) {

    override protected def emitStartFuncPosition(buf: Buffer, pos: Position): Unit =
      sourceMapWriter.startNode(buf.currentGlobalOffset, pos)

    override protected def emitPosition(buf: Buffer, pos: Position): Unit = {
      sourceMapWriter.endNode(buf.currentGlobalOffset)
      sourceMapWriter.startNode(buf.currentGlobalOffset, pos)
    }

    override protected def emitEndFuncPosition(buf: Buffer): Unit =
      sourceMapWriter.endNode(buf.currentGlobalOffset)

    override protected def emitSourceMapSection(buf: Buffer): Unit = {
      writeCustomSection(buf, "sourceMappingURL") { buf =>
        buf.name(sourceMapURI)
      }
    }
  }
}
