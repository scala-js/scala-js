/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.sourcemap

import java.io.Writer
import java.net.URI

import scala.collection.mutable.{ ListBuffer, HashMap, Stack, StringBuilder }

import org.scalajs.core.ir
import ir.Position
import ir.Position._
import ir.Utils

object SourceMapWriter {
  private val Base64Map =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "abcdefghijklmnopqrstuvwxyz" +
      "0123456789+/"

  // Some constants for writeBase64VLQ
  // Each base-64 digit covers 6 bits, but 1 is used for the continuation
  private final val VLQBaseShift = 5
  private final val VLQBase = 1 << VLQBaseShift
  private final val VLQBaseMask = VLQBase - 1
  private final val VLQContinuationBit = VLQBase

  private def jsonString(s: String) =
    "\"" + Utils.escapeJS(s) + "\""
}

class SourceMapWriter(
    val out: Writer,
    val generatedFile: String,
    val relativizeBaseURI: Option[URI] = None) {

  import SourceMapWriter._

  private val sources = new ListBuffer[String]
  private val _srcToIndex = new HashMap[SourceFile, Int]

  private val names = new ListBuffer[String]
  private val _nameToIndex = new HashMap[String, Int]

  // Strings are nullable in this stack
  private val nodePosStack = new Stack[(Position, String)]
  nodePosStack.push((NoPosition, null))

  private var lineCountInGenerated = 0
  private var lastColumnInGenerated = 0
  private var firstSegmentOfLine = true
  private var lastSource: SourceFile = null
  private var lastSourceIndex = 0
  private var lastLine: Int = 0
  private var lastColumn: Int = 0
  private var lastNameIndex: Int = 0

  private var pendingColumnInGenerated: Int = -1
  private var pendingPos: Position = NoPosition
  // pendingName string is nullable
  private var pendingName: String = null

  writeHeader()

  private def sourceToIndex(source: SourceFile): Int = {
    if (_srcToIndex.contains(source)) {
      _srcToIndex(source)
    } else {
      val index = sources.size
      _srcToIndex.put(source, index)
      sources += sourceToURI(source)
      index
    }
  }

  /** Relatively hacky way to get a Web-friendly URI to the source file */
  private def sourceToURI(source: SourceFile): String = {
    val uri = source
    val relURI = relativizeBaseURI.fold(uri)(Utils.relativize(_, uri))

    Utils.fixFileURI(relURI).toASCIIString
  }

  private def nameToIndex(name: String): Int = {
    if (_nameToIndex.contains(name)) {
      _nameToIndex(name)
    } else {
      val index = names.size
      _nameToIndex.put(name, index)
      names += name
      index
    }
  }

  private def writeHeader(): Unit = {
    out.write("{\n")
    out.write("\"version\": 3,\n")
    out.write("\"file\": " + jsonString(generatedFile) + ",\n")
    out.write("\"mappings\": \"")
  }

  def nextLine(): Unit = {
    writePendingSegment()
    out.write(';')
    lineCountInGenerated += 1
    lastColumnInGenerated = 0
    firstSegmentOfLine = true
    pendingColumnInGenerated = -1
    pendingPos = nodePosStack.top._1
    pendingName = nodePosStack.top._2
  }

  def startNode(column: Int, originalPos: Position): Unit = {
    nodePosStack.push((originalPos, null))
    startSegment(column, originalPos, null)
  }

  def startNode(column: Int, originalPos: Position,
      optOriginalName: Option[String]): Unit = {
    val originalName =
      if (optOriginalName.isDefined) optOriginalName.get
      else null
    nodePosStack.push((originalPos, originalName))
    startSegment(column, originalPos, originalName)
  }

  def endNode(column: Int): Unit = {
    nodePosStack.pop()
    startSegment(column, nodePosStack.top._1, nodePosStack.top._2)
  }

  private def startSegment(startColumn: Int, originalPos: Position,
      originalName: String): Unit = {
    // There is no point in outputting a segment with the same information
    if ((originalPos == pendingPos) && (originalName == pendingName))
      return

    // Write pending segment if it covers a non-empty range
    if (startColumn != pendingColumnInGenerated)
      writePendingSegment()

    // New pending
    pendingColumnInGenerated = startColumn
    pendingPos = originalPos
    pendingName =
      if (startColumn != pendingColumnInGenerated) originalName
      else if (pendingName != null) pendingName
      else originalName
  }

  private def writePendingSegment() {
    if (pendingColumnInGenerated < 0)
      return

    // Segments of a line are separated by ','
    if (firstSegmentOfLine) firstSegmentOfLine = false
    else out.write(',')

    // Generated column field
    writeBase64VLQ(pendingColumnInGenerated-lastColumnInGenerated)
    lastColumnInGenerated = pendingColumnInGenerated

    // If the position is NoPosition, stop here
    val pendingPos1 = pendingPos
    if (pendingPos1.isEmpty)
      return

    // Extract relevant properties of pendingPos
    val source = pendingPos1.source
    val line = pendingPos1.line
    val column = pendingPos1.column

    // Source index field
    if (source eq lastSource) { // highly likely
      writeBase64VLQ0()
    } else {
      val sourceIndex = sourceToIndex(source)
      writeBase64VLQ(sourceIndex-lastSourceIndex)
      lastSource = source
      lastSourceIndex = sourceIndex
    }

    // Line field
    writeBase64VLQ(line - lastLine)
    lastLine = line

    // Column field
    writeBase64VLQ(column - lastColumn)
    lastColumn = column

    // Name field
    if (pendingName != null) {
      val nameIndex = nameToIndex(pendingName)
      writeBase64VLQ(nameIndex-lastNameIndex)
      lastNameIndex = nameIndex
    }
  }

  def complete(): Unit = {
    writePendingSegment()

    out.write("\",\n")

    var restSources = sources
    out.write("\"sources\": [")
    while (restSources.nonEmpty) {
      out.write(jsonString(restSources.head))
      restSources = restSources.tail
      if (restSources.nonEmpty)
        out.write(", ")
    }
    out.write("],\n")

    var restNames = names
    out.write("\"names\": [")
    while (restNames.nonEmpty) {
      out.write(jsonString(restNames.head))
      restNames = restNames.tail
      if (restNames.nonEmpty)
        out.write(", ")
    }
    out.write("],\n")

    out.write("\"lineCount\": ")
    out.write(lineCountInGenerated.toString)
    out.write("\n}\n")
  }

  /** Write the Base 64 VLQ of an integer to the mappings
   *  Inspired by the implementation in Closure Compiler:
   *  http://code.google.com/p/closure-compiler/source/browse/src/com/google/debugging/sourcemap/Base64VLQ.java
   */
  private def writeBase64VLQ(value0: Int): Unit = {
    /* The sign is encoded in the least significant bit, while the
     * absolute value is shifted one bit to the left.
     * So in theory the "definition" of `value` is:
     *   val value =
     *     if (value0 < 0) ((-value0) << 1) | 1
     *     else value0 << 1
     * The following code is a branchless version of that spec.
     * It is valid because:
     * - if value0 < 0:
     *   signExtended == value0 >> 31 == 0xffffffff
     *   value0 ^ signExtended == ~value0
     *   (value0 ^ signExtended) - signExtended == ~value0 - (-1) == -value0
     *   signExtended & 1 == 1
     *   So we get ((-value0) << 1) | 1 as required
     * - if n >= 0:
     *   signExtended == value0 >> 31 == 0
     *   value0 ^ signExtended == value0
     *   (value0 ^ signExtended) - signExtended == value0 - 0 == value0
     *   signExtended & 1 == 0
     *   So we get (value0 << 1) | 0 == value0 << 1 as required
     */
    val signExtended = value0 >> 31
    val value = (((value0 ^ signExtended) - signExtended) << 1) | (signExtended & 1)

    // Write as many base-64 digits as necessary to encode value
    if (value < 26) {
      return out.write('A' + value)
    } else {
      def writeBase64VLQSlowPath(value0: Int): Unit = {
        var value = value0
        do {
          var digit = value & VLQBaseMask
          value = value >>> VLQBaseShift
          if (value != 0)
            digit |= VLQContinuationBit
          out.write(Base64Map.charAt(digit))
        } while (value != 0)
      }
      writeBase64VLQSlowPath(value)
    }
  }

  private def writeBase64VLQ0(): Unit =
    out.write('A')
}
