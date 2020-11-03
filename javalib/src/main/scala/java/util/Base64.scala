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

package java.util

import scala.annotation.tailrec

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer

import ScalaOps._

object Base64 {

  private val basicEncodeTable: Array[Byte] = {
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    val table = new Array[Byte](64)
    var i = 0
    while (i != 64) {
      table(i) = chars.charAt(i).toByte
      i += 1
    }
    table
  }

  private val urlSafeEncodeTable: Array[Byte] = {
    val table = basicEncodeTable.clone()
    table(62) = '-'.toByte
    table(63) = '_'.toByte
    table
  }

  private def decodeTable(encode: Array[Byte]): Array[Int] = {
    val decode = new Array[Int](256)
    var i = 0
    while (i != 256) {
      decode(i) = -1
      i += 1
    }
    val len = encode.length
    var j = 0
    while (j != len) {
      decode(encode(j)) = j
      j += 1
    }
    decode('=') = -2
    decode
  }

  private val basicDecodeTable = decodeTable(basicEncodeTable)
  private val urlSafeDecodeTable = decodeTable(urlSafeEncodeTable)

  private val mimeLineSeparators = Array('\r'.toByte, '\n'.toByte)
  private final val mimeLineLength = 76

  private val basicEncoder =
    new Encoder(basicEncodeTable)

  private val basicDecoder =
    new Decoder(basicDecodeTable, ignoreInvalid = false)

  private val mimeEncoder =
    new Encoder(basicEncodeTable, mimeLineLength, mimeLineSeparators)

  private val mimeDecoder =
    new Decoder(basicDecodeTable, ignoreInvalid = true)

  private val urlSafeEncoder =
    new Encoder(urlSafeEncodeTable)

  private val urlSafeDecoder =
    new Decoder(urlSafeDecodeTable, ignoreInvalid = false)

  // --------------------------------------------------------------------------

  def getEncoder(): Encoder = basicEncoder

  def getUrlEncoder(): Encoder = urlSafeEncoder

  def getMimeEncoder(): Encoder = mimeEncoder

  def getMimeEncoder(lineLength: Int, lineSeparator: Array[Byte]): Encoder = {
    for (i <- 0 until lineSeparator.length) {
      val b = lineSeparator(i) & 0xff
      if (basicDecodeTable(b) != -1) {
        throw new IllegalArgumentException(
            "Illegal base64 line separator character 0x" + Integer.toHexString(b))
      }
    }
    new Encoder(basicEncodeTable, lineLength / 4 * 4, lineSeparator)
  }

  def getDecoder(): Decoder = basicDecoder

  def getUrlDecoder(): Decoder = urlSafeDecoder

  def getMimeDecoder(): Decoder = mimeDecoder

  // --------------------------------------------------------------------------

  class Decoder private[Base64] (table: Array[Int], ignoreInvalid: Boolean) {

    def decode(src: Array[Byte]): Array[Byte] = {
      val dst = new Array[Byte](dstRequiredLength(src))
      doDecode(new Wrapper(src), new Wrapper(dst))
      dst
    }

    def decode(src: String): Array[Byte] =
      decode(src.getBytes(StandardCharsets.ISO_8859_1))

    def decode(src: Array[Byte], dst: Array[Byte]): Int = {
      if (dst.length < dstMaxLength(src.length) && // dst is possibly too small
          dst.length < dstRequiredLength(src)) { // dst is actually too small
        throw new IllegalArgumentException(
            "Output byte array is too small for decoding all input bytes")
      }

      doDecode(new Wrapper(src), new Wrapper(dst))
    }

    def decode(buffer: ByteBuffer): ByteBuffer = {
      val start = buffer.position()
      try {
        val src = new Array[Byte](buffer.remaining())
        buffer.get(src)
        val dst = new Array[Byte](dstRequiredLength(src))
        val written = doDecode(new Wrapper(src), new Wrapper(dst))
        ByteBuffer.wrap(dst, 0, written)
      } catch {
        case e: IllegalArgumentException =>
          buffer.position(start)
          throw e
      }
    }

    def wrap(is: InputStream): InputStream =
      new DecodingInputStream(is, table, ignoreInvalid)

    // ------------------------------------------------------------------------
    // PRIVATE
    // ------------------------------------------------------------------------

    private def doDecode(src: Wrapper, dst: Wrapper): Int = {
      val srcBuffer = new Wrapper(new Array[Byte](4))

      @inline
      def inputData(): Unit = {
        srcBuffer.position = 0
        var shift = 18
        var i = 0
        while (srcBuffer.hasRemaining) {
          i |= ((srcBuffer.get() & 0xff) << shift)
          shift -= 6
        }

        if (shift == 12) {
          throw new IllegalArgumentException("Last unit does not have enough valid bits")
        }

        if (shift <= 6)
          dst.put((i >> 16).toByte)
        if (shift <= 0)
          dst.put((i >> 8).toByte)
        if (shift <= -6)
          dst.put(i.toByte)
        srcBuffer.clear()
      }

      @tailrec
      def iterate(): Unit = {
        if (src.hasRemaining) {
          if (srcBuffer.hasRemaining) {
            val int = src.get() & 0xff
            table(int) match {
              case -2 =>

              case -1 =>
                if (!ignoreInvalid) {
                  throw new IllegalArgumentException(
                      "Illegal base64 character " + Integer.toHexString(int))
                }
                iterate()

              case i =>
                srcBuffer.put(i.toByte)
                iterate()
            }
          } else {
            inputData()
            iterate()
          }
        }
      }

      iterate()

      // end or padding
      srcBuffer.flip()
      inputData()
      while (src.hasRemaining) {
        val int = src.get() & 0xff
        val value = table(int)
        if (value != -2 && (!ignoreInvalid || value > 0)) {
          throw new IllegalArgumentException(s"Input byte array has incorrect ending byte at $int")
        }
      }

      dst.position
    }

    private def dstRequiredLength(src: Array[Byte]): Int = {
      var validBytes = 0

      if (ignoreInvalid) {
        for (i <- 0 until src.length) {
          if (table(src(i) & 0xff) >= 0)
            validBytes += 1
        }
      } else {
        /* We check the end for padding and compute the length from there.
         * This is ok, if the rest contains garbage we'll have written
         * something before throwing but the spec says "If the input byte array
         * is not in valid Base64 encoding scheme then some bytes may have been
         * written to the output byte array before IllegalArgumentException is
         * thrown."
         */
        validBytes = src.length
        if (src.length >= 1 && src(src.length - 1) == '=') {
          validBytes -= 1
          if (src.length >= 2 && src(src.length - 2) == '=')
            validBytes -= 1
        }

        if (src.length >= 1 && validBytes == 0) {
          throw new IllegalArgumentException("Input byte array has wrong 4-byte ending unit")
        }
      }

      dstMaxLength(validBytes)
    }

    /** Computes the destination length solely based on the source length,
     *  without knowing about padding.
     */
    private def dstMaxLength(srcLength: Int): Int =
      (srcLength + 3) / 4 * 3 - (if (srcLength % 4 == 0) 0 else 4 - (srcLength % 4))

  }

  private object DecodingInputStream {
    private final val DecodeState18 = 0
    private final val DecodeState12 = 1
    private final val DecodeState14 = 2
    private final val DecodeState16 = 3
  }

  private class DecodingInputStream(in: InputStream, table: Array[Int], ignoreInvalid: Boolean)
      extends FilterInputStream(in) {

    import DecodingInputStream._

    private val oneBuf = new Array[Byte](1)

    private var closed = false
    private var eof = false
    private var out = 0
    private var shift = DecodeState18

    override def read(): Int =
      if (read(oneBuf, 0, 1) == -1) -1
      else oneBuf(0) & 0xff

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      var written = 0

      @inline
      def writeValue(i: Int): Int = {
        /* Max value means we're writing remaining bytes after EOF, no table
         * lookup.
         */
        if (i == Int.MaxValue) {
          0
        } else {
          table(i) match {
            case -1 =>
              if (!ignoreInvalid) {
                throw new IOException("Illegal base64 character " + Integer.toHexString(i))
              }
              0

            case v =>
              shift match {
                case DecodeState18 =>
                  out |= v << 18
                  shift = DecodeState12
                  0

                case DecodeState12 =>
                  out |= v << 12
                  b(off + written) = (out >> 16).toByte
                  out <<= 8
                  shift = DecodeState14
                  1

                case DecodeState14 =>
                  out |= v << 14
                  b(off + written) = (out >> 16).toByte
                  out <<= 8
                  shift = DecodeState16
                  1

                case DecodeState16 =>
                  out |= v << 16
                  b(off + written) = (out >> 16).toByte
                  out = 0
                  shift = DecodeState18
                  1
              }
          }
        }
      }

      @inline
      def endOfFile(): Int = {
        eof = true
        shift match {
          case DecodeState18 =>
            0 // nothing
          case DecodeState12 =>
            throw new IOException("Base64 stream has one un-decoded dangling byte.")
          case _ =>
            writeValue(Int.MaxValue)
        }
      }

      @inline
      def padding(): Int = {
        eof = true
        val s = shift
        if (s == DecodeState18 || s == DecodeState12 ||
            (s == DecodeState14 && in.read() != '=' && !ignoreInvalid)) {
          throw new IOException("Illegal base64 ending sequence")
        }
        writeValue(Int.MaxValue)
      }

      @tailrec
      def iterate(): Unit = {
        if (written < len) {
          in.read() match {
            case -1 =>
              written += endOfFile()

            case '=' =>
              written += padding()
              iterate()

            case int =>
              written += writeValue(int)
              iterate()
          }
        }
      }

      if (closed)
        throw new IOException("Stream is closed")

      if (off < 0 || len < 0 || len > b.length - off)
        throw new IndexOutOfBoundsException()

      if (eof) {
        -1
      } else {
        iterate()
        written
      }
    }

    override def close(): Unit = if (!closed) {
      closed = true
      in.close()
    }
  }

  // --------------------------------------------------------------------------

  class Encoder private[Base64] (table: Array[Byte], lineLength: Int = 0,
      lineSeparator: Array[Byte] = Array.empty, withPadding: Boolean = true) {

    def encode(src: Array[Byte]): Array[Byte] = {
      val dst = new Array[Byte](dstLength(src.length))
      doEncode(src, dst, dst.length)
      dst
    }

    def encode(src: Array[Byte], dst: Array[Byte]): Int = {
      val dstLen = dstLength(src.length)
      if (dst.length < dstLen) {
        throw new IllegalArgumentException(
            "Output byte array is too small for encoding all input bytes")
      }
      doEncode(src, dst, dstLen)
    }

    def encodeToString(src: Array[Byte]): String =
      new String(encode(src), StandardCharsets.ISO_8859_1)

    def encode(buffer: ByteBuffer): ByteBuffer = {
      val result = new Array[Byte](dstLength(buffer.remaining()))
      val src = new Array[Byte](buffer.remaining())
      buffer.get(src)
      val written = doEncode(new Wrapper(src), new Wrapper(result))
      ByteBuffer.wrap(result, 0, written)
    }

    def wrap(os: OutputStream): OutputStream =
      new EncodingOutputStream(os, table, lineLength, lineSeparator, withPadding)

    def withoutPadding(): Encoder =
      if (withPadding) new Encoder(table, lineLength, lineSeparator, false)
      else this

    // ------------------------------------------------------------------------
    // PRIVATE
    // ------------------------------------------------------------------------

    private def doEncode(src: Array[Byte], dst: Array[Byte], dstLength: Int): Int = {
      doEncode(new Wrapper(src), new Wrapper(dst, 0, dstLength))
    }

    // dst position must always be 0 here
    private def doEncode(src: Wrapper, dst: Wrapper): Int = {
      val length = src.remaining
      var currentLine = 0

      @inline
      def encode(a: Byte, b: Byte, c: Byte): Unit = {
        val bits = (a & 0xff) << 16 | (b & 0xff) << 8 | (c & 0xff)
        dst.put(table((bits >>> 18) & 0x3f))
        dst.put(table((bits >>> 12) & 0x3f))
        if (dst.hasRemaining)
          dst.put(table((bits >>> 6) & 0x3f))
        if (dst.hasRemaining)
          dst.put(table(bits & 0x3f))

        currentLine += 4
        if (lineSeparator.length > 0 && lineLength > 0 &&
            currentLine == lineLength && dst.hasRemaining) {
          for (i <- 0 until lineSeparator.length)
            dst.put(lineSeparator(i))
          currentLine = 0
        }
      }

      while (src.remaining >= 3)
        encode(src.get(), src.get(), src.get())

      (length % 3) match {
        case 0 =>
        case 1 =>
          encode(src.get(), 0, 0)
          if (withPadding) {
            dst.position = dst.position - 2
            dst.put('='.toByte)
            dst.put('='.toByte)
          }
        case 2 =>
          encode(src.get(), src.get(), 0)
          if (withPadding) {
            dst.position = dst.position - 1
            dst.put('='.toByte)
          }
      }

      dst.position
    }

    private def dstLength(srcLength: Int): Int = {
      val withPad = ((srcLength + 2) / 3) * 4
      val toRemove = if (withPadding) 0 else (3 - (srcLength % 3)) % 3
      val withoutEndLines = withPad - toRemove
      val endLines =
        if (lineLength <= 0) 0
        else ((withoutEndLines - 1) / lineLength) * lineSeparator.length
      withoutEndLines + endLines
    }
  }

  // --------------------------------------------------------------------------

  private class EncodingOutputStream(out: OutputStream, table: Array[Byte], lineLength: Int,
      lineSeparator: Array[Byte], withPadding: Boolean)
      extends FilterOutputStream(out) {

    private val inputBuf = new Wrapper(new Array[Byte](3))
    private var currentLineLength = 0
    private var closed = false

    override def write(b: Int): Unit =
      write(Array(b.toByte), 0, 1)

    @inline
    private def addLineSeparators(): Unit = {
      if (lineSeparator.length > 0 && lineLength > 0 &&
          currentLineLength == lineLength) {
        out.write(lineSeparator)
        currentLineLength = 0
      }
    }

    @inline
    private def writeBuffer(count: Int): Unit = {
      inputBuf.clear()
      val bits = {
        ((inputBuf.get() & 0xff) << 16) |
          ((inputBuf.get() & 0xff) << 8) |
          (inputBuf.get() & 0xff)
      }
      var shift = 18
      for (_ <- 0 until count) {
        out.write(table((bits >>> shift) & 0x3f))
        shift -= 6
        currentLineLength += 1
      }
      inputBuf.clear()
    }

    override def write(bytes: Array[Byte], off: Int, len: Int): Unit = {
      if (closed)
        throw new IOException("Stream is closed")
      if (off < 0 || len < 0 || len > bytes.length - off)
        throw new IndexOutOfBoundsException()

      if (len != 0) {
        addLineSeparators()
        for (i <- off until (off + len)) {
          inputBuf.put(bytes(i))
          if (!inputBuf.hasRemaining) {
            writeBuffer(4)
            if (i < (off + len - 1))
              addLineSeparators()
          }
        }
      }
    }

    override def close(): Unit = {
      @inline
      def fillAndWrite(count: Int): Unit = {
        addLineSeparators()
        while (inputBuf.hasRemaining)
          inputBuf.put(0.toByte)
        writeBuffer(count)
        if (withPadding) {
          for (_ <- count until 4)
            out.write('=')
        }
      }

      if (!closed) {
        inputBuf.position match {
          case 0 =>
          case 1 => fillAndWrite(2)
          case 2 => fillAndWrite(3)
        }
        out.close()
        closed = true
      }
    }
  }

  /** An Array augmented with a position and a limit.
   *
   *  This is modeled after `java.nio.ByteBuffer`, but is more lightweight.
   */
  private class Wrapper(array: Array[Byte], var position: Int, var limit: Int) {

    def this(array: Array[Byte]) = this(array, 0, array.length)

    def hasRemaining: Boolean = position < limit

    def remaining: Int = limit - position

    def put(b: Byte): Unit = {
      array(position) = b
      position += 1
    }

    def get(): Byte = {
      position += 1
      array(position - 1)
    }

    def clear(): Unit = {
      position = 0
      limit = array.length
    }

    def flip(): Unit = {
      limit = position
      position = 0
    }
  }
}
