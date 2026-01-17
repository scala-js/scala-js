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
import java.util.concurrent.ConcurrentHashMap
import java.{util => ju}

import scala.annotation.switch
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import org.scalajs.ir
import org.scalajs.ir.OriginalName
import org.scalajs.ir.Position
import org.scalajs.ir.Position._

object SourceMapWriter {
  private val Base64UpperMap: Array[Byte] =
    "ghijklmnopqrstuvwxyz0123456789+/".toArray.map(_.toByte)

  // Some constants for writeBase64VLQ
  // Each base-64 digit covers 6 bits, but 1 is used for the continuation
  private final val VLQBaseShift = 5
  private final val VLQBase = 1 << VLQBaseShift
  private final val VLQBaseMask = VLQBase - 1
  private final val VLQContinuationBit = VLQBase

  // Constants for fragments
  private final val FragNewLine = 0
  private final val FragColOnly = 1
  private final val FragColAndPos = 2
  private final val FragColPosName = 3

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

  final class Fragment private[SourceMapWriter] (
      private[SourceMapWriter] val data: Array[Byte])
      extends AnyVal

  object Fragment {
    val Empty: Fragment = new Fragment(new Array(0))
  }

  sealed abstract class Builder(fragmentIndex: Index) {
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

      val buf = ByteBuffer.wrap(fragment.data)

      var columnInGenerated = 0
      var sourceIndex = 0
      var line: Int = 0
      var column: Int = 0
      var nameIndex: Int = 0

      while (buf.hasRemaining()) {
        (buf.get(): @switch) match {
          case FragNewLine =>
            doWriteNewLine()

          case FragColOnly =>
            columnInGenerated += readRawVLQ(buf)
            doWriteSegment(columnInGenerated, null, 0, 0, null)

          case FragColAndPos =>
            columnInGenerated += readRawVLQ(buf)
            sourceIndex += readRawVLQ(buf)
            line += readRawVLQ(buf)
            column += readRawVLQ(buf)

            val source = fragmentIndex.sources(sourceIndex)
            doWriteSegment(columnInGenerated, source, line, column, null)

          case FragColPosName =>
            columnInGenerated += readRawVLQ(buf)
            sourceIndex += readRawVLQ(buf)
            line += readRawVLQ(buf)
            column += readRawVLQ(buf)
            nameIndex += readRawVLQ(buf)

            val source = fragmentIndex.sources(sourceIndex)
            val name = fragmentIndex.names(nameIndex)
            doWriteSegment(columnInGenerated, source, line, column, name)
        }
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
      if (pendingColumnInGenerated >= 0) {
        if (pendingPos.isEmpty) {
          doWriteSegment(pendingColumnInGenerated, null, 0, 0, null)
        } else {
          doWriteSegment(pendingColumnInGenerated,
              pendingPos.source, pendingPos.line, pendingPos.column, pendingName)
        }
      }
    }

    private def readRawVLQ(buf: ByteBuffer): Int = {
      var shift = 0
      var value = 0

      while ({
        val i = buf.get()
        value |= (i & 0x7f) << shift
        (i & 0x80) != 0
      }) {
        shift += 7
      }

      val neg = (value & 1) != 0
      value >>>= 1

      /* technically, in the neg branch, we'd need to map
       * value == 0 to Int.MinValue. However, given that this is not a realistic
       * value for what we are dealing with here, we skip that check to avoid a
       * branch.
       */
      if (neg) -value else value
    }

    protected def doWriteNewLine(): Unit

    protected def doWriteSegment(columnInGenerated: Int, source: SourceFile, line: Int, column: Int,
        name: String): Unit

    protected def doComplete(): Unit
  }

  final class FragmentBuilder(index: Index) extends Builder(index) {
    private val data = new ByteArrayWriter()

    private var lastColumnInGenerated = 0
    private var lastSource: SourceFile = null
    private var lastSourceIndex = 0
    private var lastLine: Int = 0
    private var lastColumn: Int = 0
    private var lastNameIndex: Int = 0

    protected def doWriteNewLine(): Unit =
      data.write(FragNewLine)

    protected def doWriteSegment(columnInGenerated: Int, source: SourceFile,
        line: Int, column: Int, name: String): Unit = {
      val MaxSegmentLength = 1 + 5 * 5 // segment type + max 5 rawVLQ of max 5 bytes each
      val buffer = data.unsafeStartDirectWrite(maxBytes = MaxSegmentLength)
      var offset = data.currentSize

      // Write segment type
      buffer(offset) = {
        if (source == null) FragColOnly
        else if (name == null) FragColAndPos
        else FragColPosName
      }
      offset += 1

      offset = writeRawVLQ(buffer, offset, columnInGenerated - lastColumnInGenerated)
      lastColumnInGenerated = columnInGenerated

      if (source != null) {
        if (source eq lastSource) { // highly likely
          buffer(offset) = 0
          offset += 1
        } else {
          val sourceIndex = index.sourceToIndex(source)
          offset = writeRawVLQ(buffer, offset, sourceIndex - lastSourceIndex)
          lastSource = source
          lastSourceIndex = sourceIndex
        }

        // Line field
        offset = writeRawVLQ(buffer, offset, line - lastLine)
        lastLine = line

        // Column field
        offset = writeRawVLQ(buffer, offset, column - lastColumn)
        lastColumn = column

        // Name field
        if (name != null) {
          val nameIndex = index.nameToIndex(name)
          offset = writeRawVLQ(buffer, offset, nameIndex - lastNameIndex)
          lastNameIndex = nameIndex
        }
      }
      data.unsafeEndDirectWrite(offset)
    }

    protected def doComplete(): Unit = ()

    private def writeRawVLQ(buffer: Array[Byte], offset0: Int, value0: Int): Int = {
      // See comment in writeBase64VLQ
      val signExtended = value0 >> 31
      var value = (((value0 ^ signExtended) - signExtended) << 1) | (signExtended & 1)

      var offset = offset0

      while ({
        if ((value & ~0x7f) != 0)
          buffer(offset) = ((value & 0x7f) | 0x80).toByte
        else
          buffer(offset) = (value & 0x7f).toByte

        offset += 1
        value >>>= 7

        value != 0
      }) ()

      offset
    }

    def result(): Fragment =
      new Fragment(data.toByteArray())
  }

  final class Index {
    private[SourceMapWriter] val sources = new ArrayBuffer[SourceFile]
    private val _srcToIndex = new ConcurrentHashMap[SourceFile, Integer]

    private[SourceMapWriter] val names = new ArrayBuffer[String]
    private val _nameToIndex = new ConcurrentHashMap[String, Integer]

    private[SourceMapWriter] def sourceToIndex(source: SourceFile): Int = {
      val existing = _srcToIndex.get(source)
      if (existing != null) {
        existing.intValue()
      } else {
        sources.synchronized {
          val index = sources.size
          _srcToIndex.put(source, index)
          sources += source
          index
        }
      }
    }

    private[SourceMapWriter] def nameToIndex(name: String): Int = {
      val existing = _nameToIndex.get(name)
      if (existing != null) {
        existing.intValue()
      } else {
        names.synchronized {
          val index = names.size
          _nameToIndex.put(name, index)
          names += name
          index
        }
      }
    }
  }
}

final class SourceMapWriter(out: ByteArrayWriter, jsFileName: String,
    relativizeBaseURI: Option[URI], fragmentIndex: SourceMapWriter.Index)
    extends SourceMapWriter.Builder(fragmentIndex) {

  import SourceMapWriter._

  private val outIndex = new Index

  private var lineCountInGenerated = 0
  private var lastColumnInGenerated = 0
  private var firstSegmentOfLine = true
  private var lastSource: SourceFile = null
  private var lastSourceIndex = 0
  private var lastLine: Int = 0
  private var lastColumn: Int = 0
  private var lastNameIndex: Int = 0

  writeHeader()

  private def writeJSONString(s: String): Unit = {
    out.write('\"')
    out.writeASCIIEscapedJSString(s)
    out.write('\"')
  }

  private def writeHeader(): Unit = {
    out.writeASCIIString("{\n\"version\": 3")
    out.writeASCIIString(",\n\"file\": ")
    writeJSONString(jsFileName)
    out.writeASCIIString(",\n\"mappings\": \"")
  }

  protected def doWriteNewLine(): Unit = {
    out.write(';')
    lineCountInGenerated += 1
    lastColumnInGenerated = 0
    firstSegmentOfLine = true
  }

  protected def doWriteSegment(columnInGenerated: Int, source: SourceFile,
      line: Int, column: Int, name: String): Unit = {
    // scalastyle:off return

    /* This method is incredibly performance-sensitive, so we resort to
     * "unsafe" direct access to the underlying array of `out`.
     */
    val MaxSegmentLength = 1 + 5 * 7 // ',' + max 5 base64VLQ of max 7 bytes each
    val buffer = out.unsafeStartDirectWrite(maxBytes = MaxSegmentLength)
    var offset = out.currentSize

    // Segments of a line are separated by ','
    if (firstSegmentOfLine) {
      firstSegmentOfLine = false
    } else {
      buffer(offset) = ','
      offset += 1
    }

    // Generated column field
    offset = writeBase64VLQ(buffer, offset, columnInGenerated - lastColumnInGenerated)
    lastColumnInGenerated = columnInGenerated

    if (source == null) {
      // The position was NoPosition, stop here
      out.unsafeEndDirectWrite(offset)
      return
    }

    // Source index field
    if (source eq lastSource) { // highly likely
      buffer(offset) = 'A' // 0 in Base64VLQ
      offset += 1
    } else {
      val sourceIndex = outIndex.sourceToIndex(source)
      offset = writeBase64VLQ(buffer, offset, sourceIndex - lastSourceIndex)
      lastSource = source
      lastSourceIndex = sourceIndex
    }

    // Line field
    offset = writeBase64VLQ(buffer, offset, line - lastLine)
    lastLine = line

    // Column field
    offset = writeBase64VLQ(buffer, offset, column - lastColumn)
    lastColumn = column

    // Name field
    if (name != null) {
      val nameIndex = outIndex.nameToIndex(name)
      offset = writeBase64VLQ(buffer, offset, nameIndex - lastNameIndex)
      lastNameIndex = nameIndex
    }

    out.unsafeEndDirectWrite(offset)

    // scalastyle:on return
  }

  protected def doComplete(): Unit = {
    val relativizeBaseURI = this.relativizeBaseURI // local copy
    val sources = outIndex.sources // local copy
    val sourcesLen = sources.length

    out.writeASCIIString("\",\n\"sources\": [")

    var i = 0
    while (i < sourcesLen) {
      writeJSONString(SourceFileUtil.webURI(relativizeBaseURI, sources(i)))
      i += 1
      if (i < sourcesLen)
        out.writeASCIIString(", ")
    }

    val names = outIndex.names // local copy
    val namesLen = names.length

    out.writeASCIIString("],\n\"names\": [")

    i = 0
    while (i < namesLen) {
      writeJSONString(names(i))
      i += 1
      if (i < namesLen)
        out.writeASCIIString(", ")
    }
    out.writeASCIIString("],\n\"lineCount\": ")
    out.writeASCIIString(lineCountInGenerated.toString)
    out.writeASCIIString("\n}\n")
  }

  /** Write the Base 64 VLQ of an integer to the mappings.
   *
   *  !!! This method is surprisingly performance-sensitive. In an incremental
   *  run of the linker, it takes half of the time of the `BasicLinkerBackend`
   *  and systematically shows up on performance profiles. If you change it,
   *  profile it and measure performance of source map generation.
   *
   *  @return
   *    the offset past the written bytes in the `buffer`, i.e., `offset + x`
   *    where `x` is the amount of bytes written
   */
  private def writeBase64VLQ(buffer: Array[Byte], offset: Int, value0: Int): Int = {
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

    /* Now that we have a non-negative `value`, we encode it in base64 by
     * blocks of 5 bits. Each base64 digit stores 6 bits, but the most
     * significant one is used as a continuation bit (1 to continue, 0 to
     * indicate the last block). The payload is stored in little endian, with
     * the least significant blocks first.
     *
     * We could use a unique lookup table for the 64 base64 digits. However,
     * since in every path we either always pick in the lower half (for the
     * last byte) or the upper half (for continuation bytes), we use two
     * distinct functions, and omit the implicit `| VLQContinuationBit` in the
     * upper half.
     *
     * The upper half, in `continuationByte`, actually uses a lookup table.
     *
     * The lower half, in `lastByte`, uses a branchless, memory access-free
     * algorithm. The logical way to write it would be
     *   if (v < 26) v + 'A' else (v - 26) + 'a'
     * Because 'a' == 'A' + 32, this is equivalent to
     *   if (v < 26) v + 'A' else v - 26 + 'A' + 32
     * Factoring out v + 'A' and adding constants, we get
     *   v + 'A' + (if (v < 26) 0 else 6)
     * We rewrite the condition as the following branchless algorithm:
     *   ((25 - v) >> 31) & 6
     * It is equivalent because:
     *   * (25 - v) is < 0 iff v >= 26
     *   * i.e., its sign bit is 1 iff v >= 26
     *   * (25 - v) >> 31 is all-1's if v >= 26, and all-0's if v < 26
     *   * ((25 - v) >> 31) & 6 is 6 if v >= 26, and 0 if v < 26
     * This gives us the algorithm used in `lastByte`:
     *   v + 'A' + (((25 - v) >> 31) & 6)
     *
     * Compared to the lookup table, this seems to exhibit a 5-10% speedup for
     * the source map generation.
     */

    // Precondition: 0 <= v < 32, i.e., (v & 31) == v
    def continuationByte(v: Int): Byte =
      Base64UpperMap(v)

    // Precondition: 0 <= v < 32, i.e., (v & 31) == v
    def lastByte(v: Int): Byte =
      (v + 'A' + (((25 - v) >> 31) & 6)).toByte

    // Write as many base-64 digits as necessary to encode `value`
    if ((value & ~31) == 0) {
      // fast path for value < 32 -- store as a single byte (about 7/8 of the time for the test suite)
      buffer(offset) = lastByte(value)
      offset + 1
    } else if ((value & ~1023) == 0) {
      // fast path for 32 <= value < 1024 -- store as two bytes (about 1/8 of the time for the test suite)
      buffer(offset) = continuationByte(value & VLQBaseMask)
      buffer(offset + 1) = lastByte(value >>> 5)
      offset + 2
    } else {
      // slow path for 1024 <= value -- store as 3 bytes or more (a negligible fraction of the time)
      def writeBase64VLQSlowPath(value0: Int): Int = {
        var offset1 = offset
        var value = value0
        var digit = 0
        while ({
          digit = value & VLQBaseMask
          value = value >>> VLQBaseShift
          value != 0
        }) {
          buffer(offset1) = continuationByte(digit)
          offset1 += 1
        }
        buffer(offset1) = lastByte(digit)
        offset1 + 1
      }
      writeBase64VLQSlowPath(value)
    }
  }
}
