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

  private val nodePosStack = new Stack[(Position, Option[String])]
  nodePosStack.push((NoPosition, None))

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
  private var pendingName: Option[String] = None

  writeHeader()

  private def sourceToIndex(source: SourceFile) =
    _srcToIndex.getOrElseUpdate(source,
        (sources += sourceToURI(source)).size-1)

  /** Relatively hacky way to get a Web-friendly URI to the source file */
  private def sourceToURI(source: SourceFile): String = {
    val uri = source
    val relURI = relativizeBaseURI.fold(uri)(Utils.relativize(_, uri))

    Utils.fixFileURI(relURI).toASCIIString
  }

  private def nameToIndex(name: String) =
    _nameToIndex.getOrElseUpdate(name, (names += name).size-1)

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

  def startNode(column: Int, originalPos: Position,
      originalName: Option[String] = None): Unit = {
    nodePosStack.push((originalPos, originalName))
    startSegment(column, originalPos, originalName)
  }

  def endNode(column: Int): Unit = {
    nodePosStack.pop()
    startSegment(column, nodePosStack.top._1, nodePosStack.top._2)
  }

  private def startSegment(startColumn: Int, originalPos: Position,
      originalName: Option[String]): Unit = {
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
      else pendingName orElse originalName
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
    if (!pendingPos.isDefined)
      return

    // Extract relevant properties of pendingPos
    val source = pendingPos.source
    val line = pendingPos.line
    val column = pendingPos.column

    // Source index field
    if (source == lastSource) { // highly likely
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
    if (pendingName.isDefined) {
      val nameIndex = nameToIndex(pendingName.get)
      writeBase64VLQ(nameIndex-lastNameIndex)
      lastNameIndex = nameIndex
    }
  }

  def complete(): Unit = {
    writePendingSegment()

    out.write("\",\n")
    out.write(
        sources.map(jsonString(_)).mkString("\"sources\": [", ", ", "],\n"))
    out.write(
        names.map(jsonString(_)).mkString("\"names\": [", ", ", "],\n"))
    out.write("\"lineCount\": "+lineCountInGenerated+"\n")
    out.write("}\n")
  }

  /** Write the Base 64 VLQ of an integer to the mappings
   *  Inspired by the implementation in Closure Compiler:
   *  http://code.google.com/p/closure-compiler/source/browse/src/com/google/debugging/sourcemap/Base64VLQ.java
   */
  private def writeBase64VLQ(value0: Int): Unit = {
    // Sign is encoded in the least significant bit
    var value =
      if (value0 < 0) ((-value0) << 1) + 1
      else value0 << 1

    // Write as many base-64 digits as necessary to encode value
    do {
      var digit = value & VLQBaseMask
      value = value >>> VLQBaseShift
      if (value != 0)
        digit |= VLQContinuationBit
      out.write(Base64Map.charAt(digit))
    } while (value != 0)
  }

  private def writeBase64VLQ0(): Unit =
    out.write('A')
}
