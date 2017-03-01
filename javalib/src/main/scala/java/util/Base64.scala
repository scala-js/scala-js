package java.util

import java.io.{IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, IntBuffer}

// scalastyle:off return
object Base64 {

  private type ByteTable = Array[Byte]
  private type IntTable = Array[Int]

  private val basicEncodeTable: ByteTable = Array[Byte](
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'
  )

  private val urlSafeEncodeTable: ByteTable = Array[Byte](
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '_'
  )

  private def decodeTable(encode: ByteTable): IntTable = {
    val decode: IntTable = Array.fill(256)(-1)
    encode.zipWithIndex.foreach{ case (b,i) => decode(b) = i}
    decode('=') = -2
    decode
  }

  private val basicDecodeTable: IntTable = decodeTable(basicEncodeTable)
  private val urlSafeDecodeTable: IntTable = decodeTable(urlSafeEncodeTable)

  private val mimeLineSeparators = Array[Byte]('\r','\n')
  private val mimeLineLength = 76

  private val basicEncoder = new Encoder(basicEncodeTable)
  private val basicDecoder = new Decoder(basicDecodeTable, false)

  private val mimeEncoder = new Encoder(basicEncodeTable, mimeLineLength, mimeLineSeparators)
  private val mimeDecoder = new Decoder(basicDecodeTable, true)

  private val urlSafeEncoder = new Encoder(urlSafeEncodeTable)
  private val urlSafeDecoder = new Decoder(urlSafeDecodeTable, false)

  // --------------------------------------------------------------------------

  def getDecoder: Decoder = basicDecoder

  def getEncoder: Encoder = basicEncoder

  def getMimeDecoder: Decoder = mimeDecoder

  def getMimeEncoder: Encoder = mimeEncoder

  def getMimeEncoder(lineLength: Int, lineSeparator: Array[Byte]): Encoder =
    new Encoder(basicEncodeTable, lineLength / 4 * 4, lineSeparator)

  def getUrlDecoder: Decoder = urlSafeDecoder

  def getUrlEncoder: Encoder = urlSafeEncoder

  // --------------------------------------------------------------------------

  class Decoder private[Base64](table: IntTable, ignoreInvalid: Boolean) {

    def decode(src: Array[Byte]): Array[Byte] = {
      val dst = ByteBuffer.allocate(dstMaxLength(src.length))
      doDecode(ByteBuffer.wrap(src), dst)
      val result = new Array[Byte](dst.position)
      dst.flip()
      dst.get(result)
      result
    }

    def decode(src: Array[Byte], dst: Array[Byte]): Int = {
      val dstBb = ByteBuffer.allocate(dstMaxLength(src.length))
      val written = doDecode(ByteBuffer.wrap(src), dstBb)
      if(written > dst.length)
        throw new IllegalArgumentException("Output byte array is too small for decoding all input bytes")
      dstBb.flip()
      dstBb.get(dst)
      written
    }

    def decode(buffer: ByteBuffer): ByteBuffer = {
      val result = ByteBuffer.allocate(dstMaxLength(buffer.remaining()))
      doDecode(buffer, result)
      result.flip()
      result
    }

    def decode(src: String): Array[Byte] = decode(src.getBytes(StandardCharsets.ISO_8859_1))

    def wrap(is: InputStream): InputStream = new DecodingInputStream(is, table, ignoreInvalid)

    private def doDecode(src: ByteBuffer, dst: ByteBuffer): Int = {
      val srcBuffer = IntBuffer.allocate(4)

      @inline
      def inputData(): Unit = {
        srcBuffer.rewind()
        var shift = 18
        var i = 0
        while(srcBuffer.hasRemaining) {
          i |= (srcBuffer.get() << shift)
          shift -= 6
        }
        if(shift == 12) throw new IllegalArgumentException("Last unit does not have enough valid bits")
        if(shift <= 6) dst.put((i >> 16).toByte)
        if(shift <= 0) dst.put((i >> 8).toByte)
        if(shift <= -6) dst.put(i.toByte)
        srcBuffer.clear()
      }

      @inline
      def iterate(): Unit = {
        while (src.hasRemaining) {
          if (srcBuffer.hasRemaining) {
            val int = src.get() & 0xff
            table(int) match {
              case -2 => return
              case -1 if ignoreInvalid =>
              case -1 => throw new IllegalArgumentException("Illegal base64 character " + Integer.toString(int, 16))
              case i => srcBuffer.put(i)
            }
          } else inputData()
        }
      }
      iterate()
      // end or padding
      srcBuffer.flip()
      inputData()
      while(src.hasRemaining) {
        val int = src.get() & 0xff
        if(table(int) == -2) {
          // ok
        } else if (!ignoreInvalid || table(int) > 0)
          throw new IllegalArgumentException(s"Input byte array has incorrect ending byte at $int")
      }
      dst.position()
    }

    private def dstMaxLength(srcLength: Int): Int = (srcLength+3) / 4 * 3

  }

  private class DecodingInputStream(in: InputStream, table: IntTable, ignoreInvalid: Boolean) extends InputStream {

    private var inShifts = Stream.continually(Seq(18,12,14,16)).flatten
    private val oneBuf = new Array[Byte](1)

    private var closed = false
    private var eof = false
    private var out = 0

    override def read(): Int = if (read(oneBuf, 0, 1) == -1) -1 else oneBuf(0) & 0xff

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      if (closed) throw new IOException("Stream is closed")
      if (off < 0 || len < 0 || len > b.length - off) throw new IndexOutOfBoundsException
      if (eof) return -1
      var written = 0

      @inline
      def writeValue(i: Int): Int = {
        // max value means we're writing remaining bytes after eof, no table lookup
        if (i == Integer.MAX_VALUE) 0 else table(i) match {
          case -1 =>
            if (ignoreInvalid) 0
            else throw new IOException("Illegal base64 character " + Integer.toString(i, 16))
          case v =>
            inShifts.head match {
              case 18 =>
                out |= v << 18
                inShifts = inShifts.tail
                0
              case x if x == 12 || x == 14 =>
                out |= v << x
                b(off+written) = (out >> 16).toByte
                out <<= 8
                inShifts = inShifts.tail
                1
              case 16 =>
                out |= v << 16
                b(off+written) = (out >> 16).toByte
                out = 0
                inShifts = inShifts.tail
                1
            }
        }
      }

      @inline
      def endOfFile(): Int = {
        eof = true
        inShifts.head match {
          case 18 => 0 // nothing
          case 12 => throw new IOException("Base64 stream has one un-decoded dangling byte.")
          case _ => writeValue(Integer.MAX_VALUE)
        }
      }

      @inline
      def padding(): Int = {
        eof = true
        val s = inShifts.head
        if(s == 18 || s == 12 || (s == 14 && in.read() != '=' && !ignoreInvalid))
          throw new IOException (s"Illegal base64 ending sequence")
        else {
          writeValue(Integer.MAX_VALUE)
        }
      }

      def iterate(): Unit = {
        while (written < len)
          in.read() match {
            case -1 =>
              written += endOfFile()
              return
            case '=' => written += padding()
            case int => written += writeValue(int)
          }
      }

      iterate()
      written
    }

    override def close(): Unit = if (!closed) {
      closed = true
      in.close()
    }
  }

  // --------------------------------------------------------------------------

  class Encoder private[Base64] (table: ByteTable,
                                 lineLength: Int = Int.MaxValue,
                                 lineSeparator: Array[Byte] = Array.empty,
                                 withPadding: Boolean = true) {

    lineSeparator.foreach { b =>
      if (basicDecodeTable(b & 0xff) != -1)
        throw new IllegalArgumentException("Illegal base64 line separator character 0x" + Integer.toString(b, 16))
    }

    def encode(src: Array[Byte]): Array[Byte] = {
      val dst = new Array[Byte](dstLength(src.length))
      doEncode(src, dst, dst.length)
      dst
    }

    def encode(src: Array[Byte], dst: Array[Byte]): Int = {
      val dstLen = dstLength(src.length)
      if (dst.length < dstLen)
        throw new IllegalArgumentException("Output byte array is too small for encoding all input bytes")
      doEncode(src, dst, dstLen)
    }

    def encode(buffer: ByteBuffer): ByteBuffer = {
      val result = ByteBuffer.allocate(dstLength(buffer.remaining()))
      doEncode(buffer, result)
      result.flip()
      result
    }

    def encodeToString(src: Array[Byte]): String = new String(encode(src), StandardCharsets.ISO_8859_1)

    def withoutPadding(): Encoder = if (withPadding) new Encoder(table, lineLength, lineSeparator, false) else this

    def wrap(os: OutputStream): OutputStream =
      new EncodingOutputStream(os, table, lineLength, lineSeparator, withPadding)

    private def doEncode(src: Array[Byte], dst: Array[Byte], dstLength: Int): Int =
      doEncode(ByteBuffer.wrap(src), ByteBuffer.wrap(dst, 0, dstLength))

    // dst position must always be 0 here
    private def doEncode(src: ByteBuffer, dst: ByteBuffer): Int = {
      val length = src.remaining()
      var currentLine = 0

      @inline
      def encode(a: Byte, b: Byte, c: Byte): Unit = {
        val bits = (a & 0xff) << 16 | (b & 0xff) << 8 | (c & 0xff)
        dst.put(table((bits >>> 18) & 0x3f))
        dst.put(table((bits >>> 12) & 0x3f))
        if(dst.hasRemaining) dst.put(table((bits >>> 6) & 0x3f))
        if(dst.hasRemaining) dst.put(table(bits & 0x3f))
        currentLine += 4
        if(lineSeparator.length > 0 && lineLength > 0 && currentLine == lineLength && dst.hasRemaining) {
          lineSeparator.foreach(dst.put)
          currentLine = 0
        }
      }

      while(src.remaining() >= 3) encode(src.get(), src.get(), src.get())

      (3 - length % 3) % 3 match {
        case 0 =>
        case 1 =>
          encode(src.get(), src.get(), 0)
          if(withPadding) {
            dst.position(dst.position()-1)
            dst.put('='.toByte)
          }
        case 2 =>
          encode(src.get(), 0, 0)
          if(withPadding) {
            dst.position(dst.position()-2)
            dst.put('='.toByte)
            dst.put('='.toByte)
          }
      }
      dst.position()
    }

    private def dstLength(srcLength: Int): Int = {
      val withPad = ((srcLength + 2) / 3) * 4
      val toRemove = if (withPad == 0 || withPadding) 0 else (3 - (srcLength % 3)) % 3
      val withoutEndLines = withPad - toRemove
      withoutEndLines + (if (lineLength > 0) ((withoutEndLines-1) / lineLength) * lineSeparator.length else 0)
    }
  }

  // --------------------------------------------------------------------------

  private class EncodingOutputStream(out: OutputStream,
                                     table: ByteTable,
                                     ll: Int,
                                     ls: Array[Byte],
                                     withPadding: Boolean) extends OutputStream {

    override def write(b: Int): Unit = write(Array((b & 0xff).toByte), 0, 1)

    private val inputBuf = ByteBuffer.allocate(3)
    private var currentLine = 0
    private var closed = false

    @inline
    private def addLs(): Unit = {
      if (ls.length > 0 && ll > 0 && currentLine == ll) {
        out.write(ls)
        currentLine = 0
      }
    }

    @inline
    private def writeBuffer(count: Int): Unit = {
      inputBuf.rewind()
      val bits = (inputBuf.get() & 0xff) << 16 | (inputBuf.get() & 0xff) << 8 | (inputBuf.get() & 0xff)
      var shift = 18
      for(_ <- 0 until count) {
        out.write(table((bits >>> shift) & 0x3f))
        shift -= 6
        currentLine += 1
      }
      inputBuf.rewind()
    }

    override def write(bytes: Array[Byte], off: Int, len: Int): Unit = {
      if (closed) throw new IOException("Stream is closed")
      else if ((off < 0) || (len < 0) || (off > bytes.length) || ((off + len) < 0) || ((off + len) > bytes.length))
        throw new IndexOutOfBoundsException
      else if (len == 0) return

      addLs()
      for(i <- off until (off+len)) {
        inputBuf.put(bytes(i))
        if(!inputBuf.hasRemaining) {
          writeBuffer(4)
          if(i < (off+len-1)) addLs()
        }
      }
    }

    override def close(): Unit = {

      @inline
      def fillAndWrite(count: Int): Unit = {
        addLs()
        while(inputBuf.hasRemaining) inputBuf.put(0.toByte)
        writeBuffer(count)
        if(withPadding) for (_ <- count until 4) out.write('=')
      }

      if(!closed) {
        inputBuf.position() match {
          case 0 =>
          case 1 => fillAndWrite(2)
          case 2 => fillAndWrite(3)
        }
        out.close()
        closed = true
      }
    }
  }
}