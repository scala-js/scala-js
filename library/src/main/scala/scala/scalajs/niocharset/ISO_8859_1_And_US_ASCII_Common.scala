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

/** This is a very specific common implementation for ISO_8859_1 and US_ASCII.
 *  Only a single constant changes between the two algorithms (`maxValue`).
 *  No attempt was made at generalizing this to other potential charsets.
 *
 *  `maxValue` is therefore either 0xff (ISO_8859_1) or 0x7f (US_ASCII).
 */
private[niocharset] abstract class ISO_8859_1_And_US_ASCII_Common protected ( // scalastyle:ignore
    name: String, aliases: Array[String],
    private val maxValue: Int) extends Charset(name, aliases) {

  def contains(that: Charset): Boolean = that match {
    case that: ISO_8859_1_And_US_ASCII_Common => this.maxValue >= that.maxValue
    case _                                    => false
  }

  def newDecoder(): CharsetDecoder = new Decoder
  def newEncoder(): CharsetEncoder = new Encoder

  private class Decoder extends CharsetDecoder(
      ISO_8859_1_And_US_ASCII_Common.this, 1.0f, 1.0f) {
    def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult = {
      // scalastyle:off return
      val maxValue = ISO_8859_1_And_US_ASCII_Common.this.maxValue
      val inRemaining = in.remaining
      if (inRemaining == 0) {
        CoderResult.UNDERFLOW
      } else {
        val outRemaining = out.remaining
        val overflow = outRemaining < inRemaining
        val rem = if (overflow) outRemaining else inRemaining

        if (in.hasArray && out.hasArray) {
          val inArr = in.array
          val inOffset = in.arrayOffset
          val inStart = in.position + inOffset
          val inEnd = inStart + rem

          val outArr = out.array
          val outOffset = out.arrayOffset
          val outStart = out.position + outOffset

          var inPos = inStart
          var outPos = outStart
          while (inPos != inEnd) {
            val c = inArr(inPos).toInt & 0xff

            if (c > maxValue) {
              // Can only happen in US_ASCII
              in.position(inPos - inOffset)
              out.position(outPos - outOffset)
              return CoderResult.malformedForLength(1)
            }

            outArr(outPos) = c.toChar
            inPos += 1
            outPos += 1
          }

          in.position(inPos - inOffset)
          out.position(outPos - outOffset)
        } else {
          var i = 0
          while (i != rem) {
            val c = in.get().toInt & 0xff

            if (c > maxValue) {
              // Can only happen in US_ASCII
              in.position(in.position() - 1)
              return CoderResult.malformedForLength(1)
            }

            out.put(c.toChar)
            i += 1
          }
        }

        if (overflow) CoderResult.OVERFLOW
        else CoderResult.UNDERFLOW
      }
      // scalastyle:on return
    }
  }

  private class Encoder extends CharsetEncoder(
      ISO_8859_1_And_US_ASCII_Common.this, 1.0f, 1.0f) {
    def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult = {
      import java.lang.Character.{MIN_SURROGATE, MAX_SURROGATE}

      val maxValue = ISO_8859_1_And_US_ASCII_Common.this.maxValue
      val inRemaining = in.remaining
      if (inRemaining == 0) {
        CoderResult.UNDERFLOW
      } else {
        if (in.hasArray && out.hasArray) {
          val outRemaining = out.remaining
          val overflow = outRemaining < inRemaining
          val rem = if (overflow) outRemaining else inRemaining

          val inArr = in.array
          val inOffset = in.arrayOffset
          val inStart = in.position + inOffset
          val inEnd = inStart + rem

          val outArr = out.array
          val outOffset = out.arrayOffset
          val outStart = out.position + outOffset

          @inline
          @tailrec
          def loop(inPos: Int, outPos: Int): CoderResult = {
            @inline
            def finalize(result: CoderResult): CoderResult = {
              in.position(inPos - inOffset)
              out.position(outPos - outOffset)
              result
            }

            if (inPos == inEnd) {
              finalize {
                if (overflow) CoderResult.OVERFLOW
                else CoderResult.UNDERFLOW
              }
            } else {
              val c = inArr(inPos)
              if (c <= maxValue) {
                outArr(outPos) = c.toByte
                loop(inPos+1, outPos+1)
              } else {
                finalize {
                  if (Character.isLowSurrogate(c)) {
                    CoderResult.malformedForLength(1)
                  } else if (Character.isHighSurrogate(c)) {
                    if (inPos + 1 < in.limit) {
                      val c2 = inArr(inPos+1)
                      if (Character.isLowSurrogate(c2))
                        CoderResult.unmappableForLength(2)
                      else
                        CoderResult.malformedForLength(1)
                    } else {
                      CoderResult.UNDERFLOW
                    }
                  } else {
                    CoderResult.unmappableForLength(1)
                  }
                }
              }
            }
          }

          loop(inStart, outStart)
        } else {
          // Not both have arrays
          @inline
          @tailrec
          def loop(): CoderResult = {
            if (!in.hasRemaining) {
              CoderResult.UNDERFLOW
            } else if (!out.hasRemaining) {
              CoderResult.OVERFLOW
            } else {
              val c = in.get()
              if (c <= maxValue) {
                out.put(c.toByte)
                loop()
              } else {
                if (Character.isLowSurrogate(c)) {
                  in.position(in.position - 1)
                  CoderResult.malformedForLength(1)
                } else if (Character.isHighSurrogate(c)) {
                  if (in.hasRemaining) {
                    val c2 = in.get()
                    in.position(in.position - 2)
                    if (Character.isLowSurrogate(c2)) {
                      CoderResult.unmappableForLength(2)
                    } else {
                      CoderResult.malformedForLength(1)
                    }
                  } else {
                    in.position(in.position - 1)
                    CoderResult.UNDERFLOW
                  }
                } else {
                  in.position(in.position - 1)
                  CoderResult.unmappableForLength(1)
                }
              }
            }
          }

          loop()
        }
      }
    }
  }
}
