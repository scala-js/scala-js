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

import scala.collection.mutable

import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName

import Instructions._
import Identitities._
import Modules._
import Types._

private class TextWriter(module: Module) {
  import TextWriter._

  private val b = new WatBuilder()

  private val typeNames: Map[TypeID, String] = {
    val nameGen = new FreshNameGenerator
    module.types.flatMap(_.subTypes).map(st => st.id -> nameGen.genName(st.originalName)).toMap
  }

  private val dataNames: Map[DataID, String] = {
    val nameGen = new FreshNameGenerator
    module.datas.map(data => data.id -> nameGen.genName(data.originalName)).toMap
  }

  private val funcNames: Map[FunctionID, String] = {
    val nameGen = new FreshNameGenerator
    val importedFunctionNames = module.imports.collect {
      case Import(_, _, ImportDesc.Func(id, origName, _)) => id -> nameGen.genName(origName)
    }
    val definedFunctionNames = module.funcs.map(f => f.id -> nameGen.genName(f.originalName))
    (importedFunctionNames ::: definedFunctionNames).toMap
  }

  private val tagNames: Map[TagID, String] = {
    val nameGen = new FreshNameGenerator
    val importedTagNames = module.imports.collect {
      case Import(_, _, ImportDesc.Tag(id, origName, _)) =>
        id -> nameGen.genName(origName)
    }
    val definedTagNames = module.tags.map(t => t.id -> nameGen.genName(t.originalName))
    (importedTagNames ::: definedTagNames).toMap
  }

  private val globalNames: Map[GlobalID, String] = {
    val nameGen = new FreshNameGenerator
    val importedGlobalNames = module.imports.collect {
      case Import(_, _, ImportDesc.Global(id, origName, _, _)) => id -> nameGen.genName(origName)
    }
    val definedGlobalNames = module.globals.map(g => g.id -> nameGen.genName(g.originalName))
    (importedGlobalNames ::: definedGlobalNames).toMap
  }

  private val fieldNames: Map[TypeID, Map[FieldID, String]] = {
    (for {
      recType <- module.types
      SubType(typeID, _, _, _, StructType(fields)) <- recType.subTypes
    } yield {
      val nameGen = new FreshNameGenerator
      typeID -> fields.map(f => f.id -> nameGen.genName(f.originalName)).toMap
    }).toMap
  }

  private var localNames: Option[Map[LocalID, String]] = None
  private var labelNames: Option[mutable.Map[LabelID, String]] = None
  private var labelNameGen: Option[FreshNameGenerator] = None

  def write(): String = {
    b.topLevel("module") {
      module.types.foreach(writeRecType)
      module.imports.foreach(writeImport)
      module.funcs.foreach(writeFunction)
      module.tags.foreach(writeTag)
      module.globals.foreach(writeGlobal)
      module.exports.foreach(writeExport)
      module.start.foreach(writeStart)
      module.elems.foreach(writeElement)
      module.datas.foreach(writeData)
    }

    b.toString()
  }

  private def appendName(typeID: TypeID): Unit =
    b.appendElement(typeNames(typeID))

  private def appendName(dataID: DataID): Unit =
    b.appendElement(dataNames(dataID))

  private def appendName(functionID: FunctionID): Unit =
    b.appendElement(funcNames(functionID))

  private def appendName(tagID: TagID): Unit =
    b.appendElement(tagNames(tagID))

  private def appendName(globalID: GlobalID): Unit =
    b.appendElement(globalNames(globalID))

  private def appendName(typeID: TypeID, fieldID: FieldID): Unit =
    b.appendElement(fieldNames(typeID)(fieldID))

  private def appendName(localID: LocalID): Unit =
    b.appendElement(localNames.get(localID))

  private def appendName(labelID: LabelID): Unit =
    b.appendElement(labelNames.get(labelID))

  private def writeRecType(recType: RecType): Unit = {
    val RecType(subTypes) = recType

    subTypes match {
      case singleSubType :: Nil =>
        writeTypeDefinition(singleSubType)
      case subTypes =>
        b.newLineList("rec") {
          subTypes.foreach(writeTypeDefinition)
        }
    }
  }

  private def writeTypeDefinition(subType: SubType): Unit = {
    val SubType(id, _, isFinal, superType, compositeType) = subType

    b.newLineList("type") {
      appendName(id)
      subType match {
        case SubType(_, _, true, None, _) =>
          writeCompositeType(id, compositeType)
        case _ =>
          b.sameLineList("sub") {
            if (subType.isFinal)
              b.appendElement("final")
            superType.foreach(appendName(_))
            writeCompositeType(id, compositeType)
          }
      }
    }
  }

  private def writeCompositeType(typeID: TypeID, t: CompositeType): Unit = {
    def writeFieldType(fieldType: FieldType): Unit = {
      val FieldType(tpe, isMutable) = fieldType

      if (isMutable) {
        b.sameLineList("mut") {
          writeType(tpe)
        }
      } else {
        writeType(tpe)
      }
    }

    def writeField(field: StructField): Unit = {
      val StructField(id, _, fieldType) = field

      b.sameLineList("field") {
        appendName(typeID, id)
        writeFieldType(fieldType)
      }
    }

    t match {
      case FunctionType(params, results) =>
        b.sameLineList("func") {
          params.foreach { ty =>
            b.sameLineList("param")(writeType(ty))
          }
          results.foreach { ty =>
            b.sameLineList("result")(writeType(ty))
          }
        }

      case ArrayType(fieldType) =>
        b.sameLineList("array") {
          writeFieldType(fieldType)
        }

      case StructType(fields) =>
        b.sameLineList("struct") {
          fields.foreach(writeField)
        }
    }
  }

  private def writeImport(i: Import): Unit = {
    val Import(module, name, desc) = i

    b.newLineList("import") {
      b.appendStringElement(module)
      b.appendStringElement(name)

      desc match {
        case ImportDesc.Func(id, _, typeID) =>
          b.sameLineList("func") {
            appendName(id)
            writeTypeUse(typeID)
          }
        case ImportDesc.Global(id, _, isMutable, tpe) =>
          b.sameLineList("global") {
            appendName(id)
            if (isMutable)
              b.sameLineList("mut")(writeType(tpe))
            else
              writeType(tpe)
          }
        case ImportDesc.Tag(id, _, typeID) =>
          b.sameLineList("tag") {
            appendName(id)
            writeTypeUse(typeID)
          }
      }
    }
  }

  private def writeFunction(f: Function): Unit = {
    def writeParam(l: Local): Unit = {
      b.sameLineList("param") {
        appendName(l.id)
        writeType(l.tpe)
      }
    }

    def writeLocal(l: Local): Unit = {
      b.sameLineList("local") {
        appendName(l.id)
        writeType(l.tpe)
      }
    }

    val Function(id, _, typeID, params, results, locals, body, _) = f

    localNames = {
      val nameGen = new FreshNameGenerator
      Some((params ::: locals).map(l => l.id -> nameGen.genName(l.originalName)).toMap)
    }
    labelNames = Some(mutable.HashMap.empty)
    labelNameGen = Some(new FreshNameGenerator)

    b.newLineList("func") {
      appendName(id)
      writeTypeUse(typeID)

      b.newLine()
      params.foreach(writeParam)
      results.foreach(r => b.sameLineList("result")(writeType(r)))

      b.newLine()
      locals.foreach(writeLocal)
      writeExpr(body)
    }

    localNames = None
    labelNames = None
    labelNameGen = None
  }

  private def writeTag(tag: Tag): Unit = {
    val Tag(id, _, typeID) = tag

    b.newLineList("tag") {
      appendName(id)
      writeTypeUse(typeID)
    }
  }

  private def writeGlobal(g: Global): Unit = {
    val Global(id, _, isMutable, tpe, init) = g

    b.newLineList("global") {
      appendName(id)
      if (isMutable)
        b.sameLineList("mut")(writeType(tpe))
      else
        writeType(tpe)
      writeExpr(init)
    }
  }

  private def writeExport(e: Export): Unit = {
    val Export(name, desc) = e

    b.newLineList("export") {
      b.appendStringElement(name)
      desc match {
        case ExportDesc.Func(id) =>
          b.sameLineList("func") {
            appendName(id)
          }
        case ExportDesc.Global(id) =>
          b.sameLineList("global") {
            appendName(id)
          }
      }
    }
  }

  private def writeStart(startFunction: FunctionID): Unit = {
    b.newLineList("start") {
      appendName(startFunction)
    }
  }

  private def writeElement(element: Element): Unit = {
    val Element(tpe, init, mode) = element

    b.newLineList("elem") {
      mode match {
        case Element.Mode.Declarative => b.appendElement("declare")
      }
      writeType(tpe)
      init.foreach { item =>
        b.newLineList("item") {
          writeExpr(item)
        }
      }
    }
  }

  private def writeData(data: Data): Unit = {
    val Data(id, _, bytes, mode) = data

    b.newLineList("data") {
      appendName(id)
      mode match {
        case Data.Mode.Passive =>
          // do nothing
      }
      b.appendElement("\"" + bytes.map("\\%02x".format(_)).mkString + "\"")
    }
  }

  /** Writes a `typeuse`.
   *
   *  @see
   *    [[https://webassembly.github.io/gc/core/text/modules.html#type-uses]]
   */
  private def writeTypeUse(typeID: TypeID): Unit = {
    b.sameLineList("type")(appendName(typeID))
  }

  private def writeType(tpe: StorageType): Unit = {
    tpe match {
      case tpe: SimpleType => b.appendElement(tpe.textName)
      case tpe: PackedType => b.appendElement(tpe.textName)

      case RefType(true, heapType: HeapType.AbsHeapType) =>
        b.appendElement(heapType.nullableRefTextName)

      case RefType(nullable, heapType) =>
        b.sameLineList("ref") {
          if (nullable)
            b.appendElement("null")
          writeHeapType(heapType)
        }
    }
  }

  private def writeHeapType(heapType: HeapType): Unit = {
    heapType match {
      case HeapType.Type(typeID)          => appendName(typeID)
      case heapType: HeapType.AbsHeapType => b.appendElement(heapType.textName)
    }
  }

  private def writeFloatString(v: Double): Unit = {
    val stringRepr = {
      if (v.isNaN()) "nan"
      else if (v == Double.PositiveInfinity) "inf"
      else if (v == Double.NegativeInfinity) "-inf"
      else if (v.equals(-0.0)) "-0.0"
      else v.toString()
    }
    b.appendElement(stringRepr)
  }

  private def writeBlockType(blockType: BlockType): Unit = {
    blockType match {
      case BlockType.FunctionType(typeID) =>
        writeTypeUse(typeID)
      case BlockType.ValueType(None) =>
        // do nothing
      case BlockType.ValueType(Some(tpe)) =>
        b.sameLineList("result")(writeType(tpe))
    }
  }

  private def writeLabelIdx(labelIdx: LabelID): Unit =
    appendName(labelIdx)

  private def writeExpr(expr: Expr): Unit = {
    val Expr(instrs) = expr

    instrs.foreach(writeInstr(_))
  }

  private def writeInstr(instr: Instr): Unit = {
    instr match {
      case PositionMark(_) =>
        // ignore

      case _ =>
        instr match {
          case End | Else | _: Catch => b.deindent()
          case _                     => // do nothing
        }
        b.newLine()
        b.appendElement(instr.mnemonic)
        instr match {
          case instr: StructuredLabeledInstr =>
            for (label <- instr.label) {
              labelNames.get += label -> labelNameGen.get.genName(NoOriginalName)
              appendName(label)
            }
          case _ =>
            // do nothing
        }

        writeInstrImmediates(instr)

        instr match {
          case _: StructuredLabeledInstr | Else | _: Catch => b.indent()
          case _                                           => // do nothing
        }
    }
  }

  private def writeInstrImmediates(instr: Instr): Unit = {
    instr match {
      // Convenience categories

      case instr: SimpleInstr =>
        // do nothing
      case instr: BlockTypeLabeledInstr =>
        writeBlockType(instr.blockTypeArgument)
      case instr: LabelInstr =>
        writeLabelIdx(instr.labelArgument)
      case instr: FuncInstr =>
        appendName(instr.funcArgument)
      case instr: TypeInstr =>
        appendName(instr.typeArgument)
      case instr: TagInstr =>
        appendName(instr.tagArgument)
      case instr: LocalInstr =>
        appendName(instr.localArgument)
      case instr: GlobalInstr =>
        appendName(instr.globalArgument)
      case instr: HeapTypeInstr =>
        writeHeapType(instr.heapTypeArgument)
      case instr: RefTypeInstr =>
        writeType(instr.refTypeArgument)
      case instr: StructFieldInstr =>
        appendName(instr.structTypeID)
        appendName(instr.structTypeID, instr.fieldID)

      // Specific instructions with unique-ish shapes

      case I32Const(v) => b.appendElement(v.toString())
      case I64Const(v) => b.appendElement(v.toString())
      case F32Const(v) => writeFloatString(v.toDouble)
      case F64Const(v) => writeFloatString(v)

      case BrTable(labelIdxVector, defaultLabelIdx) =>
        labelIdxVector.foreach(writeLabelIdx(_))
        writeLabelIdx(defaultLabelIdx)

      case TryTable(blockType, clauses, _) =>
        writeBlockType(blockType)
        for (clause <- clauses) {
          b.sameLineList(clause.mnemonic) {
            clause.tag.foreach(tag => appendName(tag))
            writeLabelIdx(clause.label)
          }
        }

      case ArrayNewData(typeIdx, dataIdx) =>
        appendName(typeIdx)
        appendName(dataIdx)

      case ArrayNewFixed(typeIdx, length) =>
        appendName(typeIdx)
        b.appendElement(Integer.toUnsignedString(length))

      case ArrayCopy(destType, srcType) =>
        appendName(destType)
        appendName(srcType)

      case BrOnCast(labelIdx, from, to) =>
        writeLabelIdx(labelIdx)
        writeType(from)
        writeType(to)
      case BrOnCastFail(labelIdx, from, to) =>
        writeLabelIdx(labelIdx)
        writeType(from)
        writeType(to)

      case PositionMark(_) =>
        throw new AssertionError(s"Unexpected $instr")
    }
  }
}

object TextWriter {
  def write(module: Module): String =
    new TextWriter(module).write()

  private class FreshNameGenerator {
    private val generated = mutable.HashSet.empty[String]

    def genName(originalName: OriginalName): String = {
      val base =
        if (originalName.isDefined) "$" + sanitizeWatIdentifier(originalName.get.toString())
        else "$"
      if (originalName.isDefined && generated.add(base)) {
        base
      } else {
        var index = 1
        while (!generated.add(base + index))
          index += 1
        base + index
      }
    }
  }

  private class WatBuilder {
    private val builder = new StringBuilder
    private var level = 0
    private val indentStr = "  "

    private def indented(body: => Unit): Unit = {
      indent()
      body
      deindent()
    }

    def indent(): Unit = level += 1
    def deindent(): Unit = level -= 1

    def newLine(): Unit = {
      builder.append("\n")
      builder.append(indentStr * level)
    }

    def topLevel(name: String)(body: => Unit): Unit = {
      builder.append(s"($name")
      indented(body)
      builder.append(")")
      newLine()
    }

    def newLineList(name: String)(body: => Unit): Unit = {
      newLine()
      builder.append(s"($name")
      indented(body)
      builder.append(")")
    }

    def sameLineList(name: String)(body: => Unit): Unit = {
      builder.append(s" ($name")
      body
      builder.append(")")
    }

    def sameLineListOne(name: String)(value: String): Unit =
      sameLineList(name)(appendElement(value))

    def appendElement(value: String): Unit = {
      builder.append(" ")
      builder.append(value)
    }

    /** Appends a `string` element.
     *
     *  @see
     *    [[https://webassembly.github.io/gc/core/text/values.html#strings]]
     */
    def appendStringElement(str: String): Unit = {
      builder.append(" \"")
      val len = str.length()
      var i = 0
      while (i != len) {
        str.charAt(i) match {
          case '"'                        => builder.append("\\\"")
          case '\\'                       => builder.append("\\\\")
          case c if c < 0x20 || c == 0x7f => builder.append("\\%02x".format(c))
          case c                          => builder.append(c)
        }
        i += 1
      }
      builder.append("\"")
    }

    override def toString: String =
      builder.toString()
  }

  /** @see https://webassembly.github.io/spec/core/text/values.html#text-id */
  private def sanitizeWatIdentifier(name: String): String = {
    if (name.isEmpty) "_"
    else if (name.forall(isValidWatIdentifierChar)) name
    else name.map(c => if (isValidWatIdentifierChar(c)) c else '_').mkString
  }

  private def isValidWatIdentifierChar(c: Char): Boolean = {
    c.isDigit || c.isLetter ||
    "!#$%&'*+-./:<=>?@\\^_`|~".contains(c) ||
    "$.@_".contains(c)
  }

}
