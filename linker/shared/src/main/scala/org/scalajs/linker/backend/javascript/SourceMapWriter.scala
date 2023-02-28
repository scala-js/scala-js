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

package org.scalajs.linker.backend.javascript

import java.io._
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.{util => ju}

import scala.collection.mutable.{ ArrayBuffer, ListBuffer, HashMap, Stack, StringBuilder }

import org.scalajs.ir
import org.scalajs.ir.OriginalName
import org.scalajs.ir.Position
import org.scalajs.ir.Position._

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

  private def printJSONString(s: String, out: Writer) = {
    out.write('\"')
    Utils.printEscapeJS(s, out)
    out.write('\"')
  }

  private final class NodePosStack {
    private var topIndex: Int = -1
    private var posStack: Array[Position] = new Array(128)
    private var nameStack: Array[String] = new Array(128)

    def pop(): Unit =
      topIndex -= 1

    def topPos: Position =
      posStack(topIndex)

    def topName: String =
      nameStack(topIndex)

    def push(pos: Position, originalName: String): Unit = {
      val newTopIdx = topIndex + 1
      topIndex = newTopIdx
      if (newTopIdx >= posStack.length)
        growStack()
      posStack(newTopIdx) = pos
      nameStack(newTopIdx) = originalName
    }

    private def growStack(): Unit = {
      val newSize = 2 * posStack.length
      posStack = ju.Arrays.copyOf(posStack, newSize)
      nameStack = ju.Arrays.copyOf(nameStack, newSize)
    }
  }

  private sealed abstract class FragmentElement

  private object FragmentElement {
    case object NewLine extends FragmentElement

    // name is nullable
    final case class Segment(columnInGenerated: Int, pos: Position, name: String)
        extends FragmentElement
  }

  final class Fragment private[SourceMapWriter] (
      private[SourceMapWriter] val elements: Array[FragmentElement])

  object Fragment {
    val Empty: Fragment = new Fragment(new Array(0))
  }

  sealed abstract class Builder {
    // Strings are nullable in this stack
    private val nodePosStack = new SourceMapWriter.NodePosStack
    nodePosStack.push(NoPosition, null)

    private var pendingColumnInGenerated: Int = -1
    private var pendingPos: Position = NoPosition
    private var pendingIsIdent: Boolean = false
    // pendingName string is nullable
    private var pendingName: String = null

    final def nextLine(): Unit = {
      writePendingSegment()
      doWriteNewLine()
      pendingColumnInGenerated = -1
      pendingPos = nodePosStack.topPos
      pendingName = nodePosStack.topName
    }

    final def startNode(column: Int, originalPos: Position): Unit = {
      nodePosStack.push(originalPos, null)
      startSegment(column, originalPos, isIdent = false, null)
    }

    final def startIdentNode(column: Int, originalPos: Position,
        optOriginalName: OriginalName): Unit = {
      // TODO The then branch allocates a String; we should avoid that at some point
      val originalName =
        if (optOriginalName.isDefined) optOriginalName.get.toString()
        else null
      nodePosStack.push(originalPos, originalName)
      startSegment(column, originalPos, isIdent = true, originalName)
    }

    final def endNode(column: Int): Unit = {
      nodePosStack.pop()
      startSegment(column, nodePosStack.topPos, isIdent = false,
          nodePosStack.topName)
    }

    final def insertFragment(fragment: Fragment): Unit = {
      require(pendingColumnInGenerated < 0, s"Cannot add fragment when in the middle of a line")

      val elements = fragment.elements
      val len = elements.length
      var i = 0
      while (i != len) {
        elements(i) match {
          case FragmentElement.Segment(columnInGenerated, pos, name) =>
            doWriteSegment(columnInGenerated, pos, name)
          case FragmentElement.NewLine =>
            doWriteNewLine()
        }
        i += 1
      }
    }

    final def complete(): Unit = {
      writePendingSegment()
      doComplete()
    }

    private def startSegment(startColumn: Int, originalPos: Position,
        isIdent: Boolean, originalName: String): Unit = {
      // scalastyle:off return

      // There is no point in outputting a segment with the same information
      if ((originalPos == pendingPos) && (isIdent == pendingIsIdent) &&
          (originalName == pendingName)) {
        return
      }

      // Write pending segment if it covers a non-empty range
      if (startColumn != pendingColumnInGenerated)
        writePendingSegment()

      // New pending
      pendingColumnInGenerated = startColumn
      pendingPos = originalPos
      pendingIsIdent = isIdent
      pendingName = originalName

      // scalastyle:on return
    }

    private def writePendingSegment(): Unit = {
      if (pendingColumnInGenerated >= 0)
        doWriteSegment(pendingColumnInGenerated, pendingPos, pendingName)
    }

    protected def doWriteNewLine(): Unit

    protected def doWriteSegment(columnInGenerated: Int, pos: Position, name: String): Unit

    protected def doComplete(): Unit
  }

  final class FragmentBuilder extends Builder {
    private val elements = new ArrayBuffer[FragmentElement]

    protected def doWriteNewLine(): Unit =
      elements += FragmentElement.NewLine

    protected def doWriteSegment(columnInGenerated: Int, pos: Position, name: String): Unit =
      elements += FragmentElement.Segment(columnInGenerated, pos, name)

    protected def doComplete(): Unit = {
      if (elements.nonEmpty && elements.last != FragmentElement.NewLine)
        throw new IllegalStateException("Trying to complete a fragment in the middle of a line")
    }

    def result(): Fragment =
      new Fragment(elements.toArray)
  }
}

final class SourceMapWriter(out: Writer, jsFileName: String,
    relativizeBaseURI: Option[URI])
    extends SourceMapWriter.Builder {

  import SourceMapWriter._

  private val sources = new ListBuffer[String]
  private val _srcToIndex = new HashMap[SourceFile, Int]

  private val names = new ListBuffer[String]
  private val _nameToIndex = new HashMap[String, Int]

  private var lineCountInGenerated = 0
  private var lastColumnInGenerated = 0
  private var firstSegmentOfLine = true
  private var lastSource: SourceFile = null
  private var lastSourceIndex = 0
  private var lastLine: Int = 0
  private var lastColumn: Int = 0
  private var lastNameIndex: Int = 0

  writeHeader()

  private def sourceToIndex(source: SourceFile): Int = {
    if (_srcToIndex.contains(source)) {
      _srcToIndex(source)
    } else {
      val index = sources.size
      _srcToIndex.put(source, index)
      sources += SourceFileUtil.webURI(relativizeBaseURI, source)
      index
    }
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
    out.write("{\n\"version\": 3")
    out.write(",\n\"file\": ")
    printJSONString(jsFileName, out)
    out.write(",\n\"mappings\": \"")
  }

  protected def doWriteNewLine(): Unit = {
    out.write(';')
    lineCountInGenerated += 1
    lastColumnInGenerated = 0
    firstSegmentOfLine = true
  }

  protected def doWriteSegment(columnInGenerated: Int, pos: Position, name: String): Unit = {
    // scalastyle:off return

    // Segments of a line are separated by ','
    if (firstSegmentOfLine) firstSegmentOfLine = false
    else out.write(',')

    // Generated column field
    writeBase64VLQ(columnInGenerated-lastColumnInGenerated)
    lastColumnInGenerated = columnInGenerated

    // If the position is NoPosition, stop here
    if (pos.isEmpty)
      return

    // Extract relevant properties of pendingPos
    val source = pos.source
    val line = pos.line
    val column = pos.column

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
    if (name != null) {
      val nameIndex = nameToIndex(name)
      writeBase64VLQ(nameIndex-lastNameIndex)
      lastNameIndex = nameIndex
    }

    // scalastyle:on return
  }

  protected def doComplete(): Unit = {
    var restSources = sources.result()
    out.write("\",\n\"sources\": [")
    while (restSources.nonEmpty) {
      printJSONString(restSources.head, out)
      restSources = restSources.tail
      if (restSources.nonEmpty)
        out.write(", ")
    }

    var restNames = names.result()
    out.write("],\n\"names\": [")
    while (restNames.nonEmpty) {
      printJSONString(restNames.head, out)
      restNames = restNames.tail
      if (restNames.nonEmpty)
        out.write(", ")
    }
    out.write("],\n\"lineCount\": ")
    out.write(lineCountInGenerated.toString)
    out.write("\n}\n")
  }

  /** Write the Base 64 VLQ of an integer to the mappings
   *  Inspired by the implementation in Closure Compiler:
   *  http://code.google.com/p/closure-compiler/source/browse/src/com/google/debugging/sourcemap/Base64VLQ.java
   */
  private def writeBase64VLQ(value0: Int): Unit = {
    // scalastyle:off return

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

    // scalastyle:on return
  }

  private def writeBase64VLQ0(): Unit =
    out.write('A')
}
