/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.niocharset

import scala.annotation.tailrec

import java.nio._
import java.nio.charset._

/** This is a very specific common implementation for UTF_16BE and UTF_16LE.
 */
private[niocharset] abstract class UTF_16_Common protected ( // scalastyle:ignore
    name: String, aliases: Array[String],
    private val endianness: Int) extends Charset(name, aliases) {

  import UTF_16_Common._

  def contains(that: Charset): Boolean = true

  def newDecoder(): CharsetDecoder = new Decoder
  def newEncoder(): CharsetEncoder = new Encoder

  private class Decoder extends CharsetDecoder(
      UTF_16_Common.this, 0.5f, 1.0f) {
    private var endianness = UTF_16_Common.this.endianness

    override protected def implReset(): Unit = {
      super.implReset()
      endianness = UTF_16_Common.this.endianness
    }

    def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult = {
      @inline
      @tailrec
      def loop(): CoderResult = {
        if (in.remaining < 2) CoderResult.UNDERFLOW
        else {
          val b1 = in.get() & 0xff
          val b2 = in.get() & 0xff

          val wasBOM = if (endianness == AutoEndian) {
            // Read BOM
            if (b1 == 0xfe && b2 == 0xff) {
              endianness = BigEndian
              true
            } else if (b1 == 0xff && b2 == 0xfe) {
              endianness = LittleEndian
              true
            } else {
              // Not a valid BOM: default to BigEndian and start reading
              endianness = BigEndian
              false
            }
          } else false

          if (wasBOM) {
            loop()
          } else {
            val bigEndian = endianness == BigEndian

            @inline def bytes2char(hi: Int, lo: Int): Char =
              (if (bigEndian) (hi << 8) | lo else (lo << 8) | hi).toChar

            val c1 = bytes2char(b1, b2)

            if (Character.isLowSurrogate(c1)) {
              in.position(in.position - 2)
              CoderResult.malformedForLength(2)
            } else if (!Character.isHighSurrogate(c1)) {
              if (out.remaining == 0) {
                in.position(in.position - 2)
                CoderResult.OVERFLOW
              } else {
                out.put(c1)
                loop()
              }
            } else {
              if (in.remaining < 2) {
                in.position(in.position - 2)
                CoderResult.UNDERFLOW
              } else {
                val b3 = in.get() & 0xff
                val b4 = in.get() & 0xff
                val c2 = bytes2char(b3, b4)

                if (!Character.isLowSurrogate(c2)) {
                  in.position(in.position - 4)
                  CoderResult.malformedForLength(2)
                } else {
                  if (out.remaining < 2) {
                    in.position(in.position - 4)
                    CoderResult.OVERFLOW
                  } else {
                    out.put(c1)
                    out.put(c2)
                    loop()
                  }
                }
              }
            }
          }
        }
      }

      loop()
    }
  }

  private class Encoder extends CharsetEncoder(
      UTF_16_Common.this, 2.0f, 2.0f,
      // Character 0xfffd
      if (endianness == LittleEndian) Array(-3, -1) else Array(-1, -3)) {

    private var needToWriteBOM: Boolean = endianness == AutoEndian

    override protected def implReset(): Unit = {
      super.implReset()
      needToWriteBOM = endianness == AutoEndian
    }

    def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult = {
      if (needToWriteBOM) {
        if (out.remaining < 2) {
          return CoderResult.OVERFLOW // scalastyle:ignore
        } else {
          // Always encode in big endian
          out.put(0xfe.toByte)
          out.put(0xff.toByte)
          needToWriteBOM = false
        }
      }

      val bigEndian = endianness != LittleEndian

      @inline
      def putChar(c: Char): Unit = {
        if (bigEndian) {
          out.put((c >> 8).toByte)
          out.put(c.toByte)
        } else {
          out.put(c.toByte)
          out.put((c >> 8).toByte)
        }
      }

      @inline
      @tailrec
      def loop(): CoderResult = {
        if (in.remaining == 0) CoderResult.UNDERFLOW
        else {
          val c1 = in.get()

          if (Character.isLowSurrogate(c1)) {
            in.position(in.position - 1)
            CoderResult.malformedForLength(1)
          } else if (!Character.isHighSurrogate(c1)) {
            if (out.remaining < 2) {
              in.position(in.position - 1)
              CoderResult.OVERFLOW
            } else {
              putChar(c1)
              loop()
            }
          } else {
            if (in.remaining < 1) {
              in.position(in.position - 1)
              CoderResult.UNDERFLOW
            } else {
              val c2 = in.get()

              if (!Character.isLowSurrogate(c2)) {
                in.position(in.position - 2)
                CoderResult.malformedForLength(1)
              } else {
                if (out.remaining < 4) {
                  in.position(in.position - 2)
                  CoderResult.OVERFLOW
                } else {
                  putChar(c1)
                  putChar(c2)
                  loop()
                }
              }
            }
          }
        }
      }

      loop()
    }
  }
}

private[niocharset] object UTF_16_Common { // scalastyle:ignore
  final val AutoEndian = 0
  final val BigEndian = 1
  final val LittleEndian = 2
}
