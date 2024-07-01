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

class TextWriter(module: Module) {
  import TextWriter._

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
    implicit val b = new WatBuilder()
    writeModule(module)
    b.toString()
  }

  private def appendName(typeID: TypeID)(implicit b: WatBuilder): Unit =
    b.appendElement(typeNames(typeID))

  private def appendName(dataID: DataID)(implicit b: WatBuilder): Unit =
    b.appendElement(dataNames(dataID))

  private def appendName(functionID: FunctionID)(implicit b: WatBuilder): Unit =
    b.appendElement(funcNames(functionID))

  private def appendName(tagID: TagID)(implicit b: WatBuilder): Unit =
    b.appendElement(tagNames(tagID))

  private def appendName(globalID: GlobalID)(implicit b: WatBuilder): Unit =
    b.appendElement(globalNames(globalID))

  private def appendName(typeID: TypeID, fieldID: FieldID)(implicit b: WatBuilder): Unit =
    b.appendElement(fieldNames(typeID)(fieldID))

  private def appendName(localID: LocalID)(implicit b: WatBuilder): Unit =
    b.appendElement(localNames.get(localID))

  private def appendName(labelID: LabelID)(implicit b: WatBuilder): Unit =
    b.appendElement(labelNames.get(labelID))

  private def writeModule(module: Module)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "module", {
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
    )
  }

  private def writeRecType(recType: RecType)(implicit b: WatBuilder): Unit = {
    recType.subTypes match {
      case singleSubType :: Nil =>
        writeTypeDefinition(singleSubType)
      case subTypes =>
        b.newLineList(
          "rec", {
            subTypes.foreach(writeTypeDefinition)
          }
        )
    }
  }

  private def writeTypeDefinition(subType: SubType)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "type", {
        appendName(subType.id)
        subType match {
          case SubType(_, _, true, None, compositeType) =>
            writeCompositeType(subType.id, compositeType)
          case _ =>
            b.sameLineList(
              "sub", {
                if (subType.isFinal)
                  b.appendElement("final")
                for (superType <- subType.superType)
                  appendName(superType)
                writeCompositeType(subType.id, subType.compositeType)
              }
            )
        }
      }
    )
  }

  private def writeCompositeType(typeID: TypeID, t: CompositeType)(implicit b: WatBuilder): Unit = {
    def writeFieldType(fieldType: FieldType): Unit = {
      if (fieldType.isMutable)
        b.sameLineList(
          "mut", {
            writeType(fieldType.tpe)
          }
        )
      else writeType(fieldType.tpe)
    }

    def writeField(field: StructField): Unit = {
      b.sameLineList(
        "field", {
          appendName(typeID, field.id)
          writeFieldType(field.fieldType)
        }
      )
    }

    t match {
      case FunctionType(params, results) =>
        b.sameLineList(
          "func", {
            params.foreach { ty =>
              b.sameLineList("param", writeType(ty))
            }
            results.foreach { ty =>
              b.sameLineList("result", writeType(ty))
            }
          }
        )

      case ArrayType(fieldType) =>
        b.sameLineList(
          "array", {
            writeFieldType(fieldType)
          }
        )

      case StructType(fields) =>
        b.sameLineList(
          "struct", {
            fields.foreach(writeField)
          }
        )
    }
  }

  private def writeImport(i: Import)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "import", {
        b.appendElement("\"" + i.module + "\"")
        b.appendElement("\"" + i.name + "\"")

        i.desc match {
          case ImportDesc.Func(id, _, typeID) =>
            b.sameLineList(
              "func", {
                appendName(id)
                writeTypeUse(typeID)
              }
            )
          case ImportDesc.Global(id, _, isMutable, tpe) =>
            b.sameLineList(
              "global", {
                appendName(id)
                if (isMutable)
                  b.sameLineList("mut", writeType(tpe))
                else
                  writeType(tpe)
              }
            )
          case ImportDesc.Tag(id, _, typeID) =>
            b.sameLineList(
              "tag", {
                appendName(id)
                writeTypeUse(typeID)
              }
            )
        }
      }
    )
  }

  private def writeSig(params: List[Type], results: List[Type])(
      implicit b: WatBuilder): Unit = {
    params.foreach(tpe => b.sameLineList("param", writeType(tpe)))
    results.foreach(tpe => b.sameLineList("result", writeType(tpe)))
  }

  private def writeFunction(f: Function)(implicit b: WatBuilder): Unit = {
    def writeParam(l: Local)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "param", {
          appendName(l.id)
          writeType(l.tpe)
        }
      )
    }

    def writeLocal(l: Local)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "local", {
          appendName(l.id)
          writeType(l.tpe)
        }
      )
    }

    localNames = {
      val nameGen = new FreshNameGenerator
      Some((f.params ::: f.locals).map(l => l.id -> nameGen.genName(l.originalName)).toMap)
    }
    labelNames = Some(mutable.HashMap.empty)
    labelNameGen = Some(new FreshNameGenerator)

    b.newLineList(
      "func", {
        appendName(f.id)
        writeTypeUse(f.typeID)

        b.newLine()
        f.params.foreach(writeParam)
        f.results.foreach(r => { b.sameLineList("result", writeType(r)) })

        b.newLine()
        f.locals.foreach(writeLocal)
        f.body.instr.foreach(writeInstr)
      }
    )

    localNames = None
    labelNames = None
    labelNameGen = None
  }

  private def writeTag(tag: Tag)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "tag", {
        appendName(tag.id)
        writeTypeUse(tag.typeID)
      }
    )
  }

  private def writeGlobal(g: Global)(implicit b: WatBuilder) =
    b.newLineList(
      "global", {
        appendName(g.id)
        if (g.isMutable)
          b.sameLineList("mut", writeType(g.tpe))
        else writeType(g.tpe)
        g.init.instr.foreach(writeInstr)
      }
    )

  private def writeExport(e: Export)(implicit
      b: WatBuilder
  ) = b.newLineList(
    "export", {
      b.appendElement("\"" + e.name + "\"")
      e.desc match {
        case ExportDesc.Func(id) =>
          b.sameLineList(
            "func",
            { appendName(id) }
          )
        case ExportDesc.Global(id) =>
          b.sameLineList(
            "global",
            { appendName(id) }
          )
      }
    }
  )

  private def writeStart(startFunction: FunctionID)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "start", {
        appendName(startFunction)
      }
    )
  }

  private def writeElement(element: Element)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "elem", {
        element.mode match {
          case Element.Mode.Declarative => b.appendElement("declare")
        }
        writeType(element.tpe)
        element.init.foreach { item =>
          b.newLineList(
            "item",
            item.instr.foreach(writeInstr(_))
          )
        }
      }
    )
  }

  private def writeData(data: Data)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "data", {
        appendName(data.id)
        data.mode match {
          case Data.Mode.Passive => ()
        }
        b.appendElement("\"" + data.bytes.map("\\%02x".format(_)).mkString + "\"")
      }
    )
  }

  private def writeTypeUse(typeID: TypeID)(implicit b: WatBuilder): Unit = {
    b.sameLineList("type", appendName(typeID))
  }

  private def writeType(tpe: StorageType)(implicit b: WatBuilder): Unit = {
    tpe match {
      case tpe: SimpleType => b.appendElement(tpe.textName)
      case tpe: PackedType => b.appendElement(tpe.textName)

      case RefType(true, heapType: HeapType.AbsHeapType) =>
        b.appendElement(heapType.nullableRefTextName)

      case RefType(nullable, heapType) =>
        b.sameLineList(
          "ref", {
            if (nullable)
              b.appendElement("null")
            writeHeapType(heapType)
          }
        )
    }
  }

  private def writeHeapType(heapType: HeapType)(implicit b: WatBuilder): Unit = {
    heapType match {
      case HeapType.Type(typeID)          => appendName(typeID)
      case heapType: HeapType.AbsHeapType => b.appendElement(heapType.textName)
    }
  }

  private def floatString(v: Double): String = {
    if (v.isNaN()) "nan"
    else if (v == Double.PositiveInfinity) "inf"
    else if (v == Double.NegativeInfinity) "-inf"
    else if (v.equals(-0.0)) "-0.0"
    else v.toString()
  }

  private def writeBlockType(blockType: BlockType)(implicit b: WatBuilder): Unit = {
    blockType match {
      case BlockType.FunctionType(typeID) =>
        writeTypeUse(typeID)
      case BlockType.ValueType(optType) =>
        for (tpe <- optType)
          b.sameLineList("result", writeType(tpe))
    }
  }

  private def writeLabelIdx(labelIdx: LabelID)(implicit b: WatBuilder): Unit =
    appendName(labelIdx)

  private def writeInstr(instr: Instr)(implicit b: WatBuilder): Unit = {
    instr match {
      case PositionMark(_) =>
        // ignore
        ()

      case _ =>
        instr match {
          case End | Else | _: Catch => b.deindent()
          case _                     => ()
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
            ()
        }

        writeInstrImmediates(instr)

        instr match {
          case _: StructuredLabeledInstr | Else | _: Catch => b.indent()
          case _                                           => ()
        }
    }
  }

  private def writeInstrImmediates(instr: Instr)(implicit b: WatBuilder): Unit = {
    instr match {
      // Convenience categories

      case instr: SimpleInstr =>
        ()
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
      case F32Const(v) => b.appendElement(floatString(v.toDouble))
      case F64Const(v) => b.appendElement(floatString(v))

      case BrTable(labelIdxVector, defaultLabelIdx) =>
        labelIdxVector.foreach(writeLabelIdx(_))
        writeLabelIdx(defaultLabelIdx)

      case TryTable(blockType, clauses, _) =>
        writeBlockType(blockType)
        for (clause <- clauses) {
          b.sameLineList(
            clause.mnemonic, {
              clause.tag.foreach(tag => appendName(tag))
              writeLabelIdx(clause.label)
            }
          )
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

      case PositionMark(pos) =>
        throw new AssertionError(s"Unexpected $instr")
    }
  }
}

object TextWriter {
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
      level += 1
      body
      level -= 1
    }

    def indent(): Unit = level += 1
    def deindent(): Unit = level -= 1

    def newLine(): Unit = {
      builder.append("\n")
      builder.append(indentStr * level)
    }

    def newLineList(name: String, body: => Unit): Unit = {
      newLine()
      builder.append(s"($name")
      indented(body)
      builder.append(")")
    }

    def sameLineList(name: String, body: => Unit): Unit = {
      builder.append(s" ($name")
      body
      builder.append(")")
    }

    def sameLineListOne(name: String, value: String): Unit =
      sameLineList(name, { appendElement(value) })

    def appendElement(value: String): Unit = {
      builder.append(" ")
      builder.append(value)
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
