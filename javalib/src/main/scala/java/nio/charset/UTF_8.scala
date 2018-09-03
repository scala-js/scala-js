package java.nio.charset

import scala.annotation.{switch, tailrec}

import java.nio._

private[charset] object UTF_8 extends Charset("UTF-8", Array(
    "UTF8", "unicode-1-1-utf-8")) {

  import java.lang.Character._

  def contains(that: Charset): Boolean = true

  def newDecoder(): CharsetDecoder = new Decoder
  def newEncoder(): CharsetEncoder = new Encoder

  /* The next table contains information about UTF-8 charset and
   * correspondence of 1st byte to the length of sequence
   * For information please visit http://www.ietf.org/rfc/rfc3629.txt
   *
   * -------------------------------------------------------------------
   * 0         1         2         3          Value
   * -------------------------------------------------------------------
   * 0xxxxxxx                                 00000000 00000000 0xxxxxxx
   * 110yyyyy  10xxxxxx                       00000000 00000yyy yyxxxxxx
   * 1110zzzz  10yyyyyy  10xxxxxx             00000000 zzzzyyyy yyxxxxxx
   * 11110uuu  10zzzzzz  10yyyyyy  10xxxxxx   000uuuzz zzzzyyyy yyxxxxxx
   */

  private val lengthByLeading: Array[Int] = Array(
      // 10wwwwww
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      // 110yyyyy
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      // 1110zzzz
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      // 11110uuu
      4, 4, 4, 4, 4, 4, 4, 4,
      // > 11110111
      -1, -1, -1, -1, -1, -1, -1, -1
  )

  @inline
  private class DecodedMultiByte(val failure: CoderResult,
      val high: Char, val low: Char)

  private object DecodedMultiByte {
    @inline def apply(failure: CoderResult): DecodedMultiByte =
      new DecodedMultiByte(failure, 0, 0)

    @inline def apply(single: Char): DecodedMultiByte =
      new DecodedMultiByte(null, single, 0)

    @inline def apply(high: Char, low: Char): DecodedMultiByte =
      new DecodedMultiByte(null, high, low)
  }

  private class Decoder extends CharsetDecoder(UTF_8, 1.0f, 1.0f) {
    def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult = {
      if (in.hasArray && out.hasArray)
        decodeLoopArray(in, out)
      else
        decodeLoopNoArray(in, out)
    }

    private def decodeLoopArray(in: ByteBuffer, out: CharBuffer): CoderResult = {
      val inArray = in.array
      val inOffset = in.arrayOffset
      val inStart = in.position() + inOffset
      val inEnd = in.limit() + inOffset

      val outArray = out.array
      val outOffset = out.arrayOffset
      val outStart = out.position() + outOffset
      val outEnd = out.limit() + outOffset

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
          finalize(CoderResult.UNDERFLOW)
        } else {
          val leading = inArray(inPos).toInt
          if (leading >= 0) {
            // US-ASCII repertoire
            if (outPos == outEnd) {
              finalize(CoderResult.OVERFLOW)
            } else {
              outArray(outPos) = leading.toChar
              loop(inPos+1, outPos+1)
            }
          } else {
            // Multi-byte
            val length = lengthByLeading(leading & 0x7f)
            if (length == -1) {
              finalize(CoderResult.malformedForLength(1))
            } else {
              val decoded = {
                if (inPos + 1 >= inEnd) {
                  DecodedMultiByte(CoderResult.UNDERFLOW)
                } else {
                  val b2 = inArray(inPos + 1)
                  if (isInvalidNextByte(b2)) {
                    DecodedMultiByte(CoderResult.malformedForLength(1))
                  } else if (length == 2) {
                    decode2(leading, b2)
                  } else if (inPos + 2 >= inEnd) {
                    DecodedMultiByte(CoderResult.UNDERFLOW)
                  } else {
                    val b3 = inArray(inPos + 2)
                    if (isInvalidNextByte(b3)) {
                      DecodedMultiByte(CoderResult.malformedForLength(2))
                    } else if (length == 3) {
                      decode3(leading, b2, b3)
                    } else if (inPos + 3 >= inEnd) {
                      DecodedMultiByte(CoderResult.UNDERFLOW)
                    } else {
                      val b4 = inArray(inPos + 3)
                      if (isInvalidNextByte(b4))
                        DecodedMultiByte(CoderResult.malformedForLength(3))
                      else
                        decode4(leading, b2, b3, b4)
                    }
                  }
                }
              }

              if (decoded.failure != null) {
                finalize(decoded.failure)
              } else if (decoded.low == 0) {
                // not a surrogate pair
                if (outPos == outEnd)
                  finalize(CoderResult.OVERFLOW)
                else {
                  outArray(outPos) = decoded.high
                  loop(inPos+length, outPos+1)
                }
              } else {
                // a surrogate pair
                if (outPos + 2 > outEnd)
                  finalize(CoderResult.OVERFLOW)
                else {
                  outArray(outPos) = decoded.high
                  outArray(outPos+1) = decoded.low
                  loop(inPos+length, outPos+2)
                }
              }
            }
          }
        }
      }

      loop(inStart, outStart)
    }

    private def decodeLoopNoArray(in: ByteBuffer, out: CharBuffer): CoderResult = {
      @inline
      @tailrec
      def loop(): CoderResult = {
        // Mark the input position so that we can reset on multi-byte failure
        val startPosition = in.position()

        @inline
        def fail(result: CoderResult): CoderResult = {
          in.position(startPosition)
          result
        }

        if (!in.hasRemaining) {
          CoderResult.UNDERFLOW
        } else {
          val leading = in.get().toInt
          if (leading >= 0) {
            // US-ASCII repertoire
            if (!out.hasRemaining) {
              fail(CoderResult.OVERFLOW)
            } else {
              out.put(leading.toChar)
              loop()
            }
          } else {
            // Multi-byte
            val length = lengthByLeading(leading & 0x7f)
            if (length == -1) {
              fail(CoderResult.malformedForLength(1))
            } else {
              val decoded = {
                if (in.hasRemaining) {
                  val b2 = in.get()
                  if (isInvalidNextByte(b2)) {
                    DecodedMultiByte(CoderResult.malformedForLength(1))
                  } else if (length == 2) {
                    decode2(leading, b2)
                  } else if (in.hasRemaining) {
                    val b3 = in.get()
                    if (isInvalidNextByte(b3)) {
                      DecodedMultiByte(CoderResult.malformedForLength(2))
                    } else if (length == 3) {
                      decode3(leading, b2, b3)
                    } else if (in.hasRemaining) {
                      val b4 = in.get()
                      if (isInvalidNextByte(b4))
                        DecodedMultiByte(CoderResult.malformedForLength(3))
                      else
                        decode4(leading, b2, b3, b4)
                    } else {
                      DecodedMultiByte(CoderResult.UNDERFLOW)
                    }
                  } else {
                    DecodedMultiByte(CoderResult.UNDERFLOW)
                  }
                } else {
                  DecodedMultiByte(CoderResult.UNDERFLOW)
                }
              }

              if (decoded.failure != null) {
                fail(decoded.failure)
              } else if (decoded.low == 0) {
                // not a surrogate pair
                if (!out.hasRemaining)
                  fail(CoderResult.OVERFLOW)
                else {
                  out.put(decoded.high)
                  loop()
                }
              } else {
                // a surrogate pair
                if (out.remaining < 2)
                  fail(CoderResult.OVERFLOW)
                else {
                  out.put(decoded.high)
                  out.put(decoded.low)
                  loop()
                }
              }
            }
          }
        }
      }

      loop()
    }

    @inline private def isInvalidNextByte(b: Int): Boolean =
      (b & 0xc0) != 0x80

    /** Requires the input bytes to be a valid byte sequence. */
    @inline private def decode2(b1: Int, b2: Int): DecodedMultiByte = {
      val codePoint = (((b1 & 0x1f) << 6) | (b2 & 0x3f))
      // By construction, 0 <= codePoint <= 0x7ff < MIN_SURROGATE
      if (codePoint < 0x80) {
        // Should have been encoded with only 1 byte
        DecodedMultiByte(CoderResult.malformedForLength(1))
      } else {
        DecodedMultiByte(codePoint.toChar)
      }
    }

    /** Requires the input bytes to be a valid byte sequence. */
    @inline private def decode3(b1: Int, b2: Int, b3: Int): DecodedMultiByte = {
      val codePoint = (((b1 & 0xf) << 12) | ((b2 & 0x3f) << 6) | (b3 & 0x3f))
      // By construction, 0 <= codePoint <= 0xffff < MIN_SUPPLEMENTARY_CODE_POINT
      if (codePoint < 0x800) {
        // Should have been encoded with only 1 or 2 bytes
        DecodedMultiByte(CoderResult.malformedForLength(1))
      } else if (codePoint >= MIN_SURROGATE && codePoint <= MAX_SURROGATE) {
        // It is a surrogate, which is not a valid code point
        DecodedMultiByte(CoderResult.malformedForLength(3))
      } else {
        DecodedMultiByte(codePoint.toChar)
      }
    }

    /** Requires the input bytes to be a valid byte sequence. */
    @inline private def decode4(b1: Int, b2: Int, b3: Int, b4: Int): DecodedMultiByte = {
      val codePoint = (((b1 & 0x7) << 18) | ((b2 & 0x3f) << 12) |
          ((b3 & 0x3f) << 6) | (b4 & 0x3f))
      // By construction, 0 <= codePoint <= 0x1fffff
      if (codePoint < 0x10000 || codePoint > MAX_CODE_POINT) {
        // It should have been encoded with 1, 2, or 3 bytes
        // or it is not a valid code point
        DecodedMultiByte(CoderResult.malformedForLength(1))
      } else {
        // Here, we need to encode the code point as a surrogate pair.
        // http://en.wikipedia.org/wiki/UTF-16
        val offsetCodePoint = codePoint - 0x10000
        DecodedMultiByte(
            ((offsetCodePoint >> 10) | 0xd800).toChar,
            ((offsetCodePoint & 0x3ff) | 0xdc00).toChar)
      }
    }
  }

  private class Encoder extends CharsetEncoder(UTF_8, 1.1f, 4.0f) {
    def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult = {
      if (in.hasArray && out.hasArray)
        encodeLoopArray(in, out)
      else
        encodeLoopNoArray(in, out)
    }

    private def encodeLoopArray(in: CharBuffer, out: ByteBuffer): CoderResult = {
      val inArray = in.array
      val inOffset = in.arrayOffset
      val inStart = in.position() + inOffset
      val inEnd = in.limit() + inOffset

      val outArray = out.array
      val outOffset = out.arrayOffset
      val outStart = out.position() + outOffset
      val outEnd = out.limit() + outOffset

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
          finalize(CoderResult.UNDERFLOW)
        } else {
          val c1 = inArray(inPos)

          if (c1 < 0x80) {
            // Encoding in one byte
            if (outPos == outEnd)
              finalize(CoderResult.OVERFLOW)
            else {
              outArray(outPos) = c1.toByte
              loop(inPos+1, outPos+1)
            }
          } else if (c1 < 0x800) {
            // Encoding in 2 bytes (by construction, not a surrogate)
            if (outPos + 2 > outEnd)
              finalize(CoderResult.OVERFLOW)
            else {
              outArray(outPos) = ((c1 >> 6) | 0xc0).toByte
              outArray(outPos+1) = ((c1 & 0x3f) | 0x80).toByte
              loop(inPos+1, outPos+2)
            }
          } else if (!isSurrogate(c1)) {
            // Not a surrogate, encoding in 3 bytes
            if (outPos + 3 > outEnd)
              finalize(CoderResult.OVERFLOW)
            else {
              outArray(outPos) = ((c1 >> 12) | 0xe0).toByte
              outArray(outPos+1) = (((c1 >> 6) & 0x3f) | 0x80).toByte
              outArray(outPos+2) = ((c1 & 0x3f) | 0x80).toByte
              loop(inPos+1, outPos+3)
            }
          } else if (isHighSurrogate(c1)) {
            // Should have a low surrogate that follows
            if (inPos + 1 == inEnd)
              finalize(CoderResult.UNDERFLOW)
            else {
              val c2 = inArray(inPos+1)
              if (!isLowSurrogate(c2)) {
                finalize(CoderResult.malformedForLength(1))
              } else {
                // Surrogate pair, encoding in 4 bytes
                if (outPos + 4 > outEnd)
                  finalize(CoderResult.OVERFLOW)
                else {
                  val cp = toCodePoint(c1, c2)
                  outArray(outPos) = ((cp >> 18) | 0xf0).toByte
                  outArray(outPos+1) = (((cp >> 12) & 0x3f) | 0x80).toByte
                  outArray(outPos+2) = (((cp >> 6) & 0x3f) | 0x80).toByte
                  outArray(outPos+3) = ((cp & 0x3f) | 0x80).toByte
                  loop(inPos+2, outPos+4)
                }
              }
            }
          } else {
            finalize(CoderResult.malformedForLength(1))
          }
        }
      }

      loop(inStart, outStart)
    }

    private def encodeLoopNoArray(in: CharBuffer, out: ByteBuffer): CoderResult = {
      @inline
      @tailrec
      def loop(): CoderResult = {
        @inline
        def finalize(read: Int, result: CoderResult): CoderResult = {
          in.position(in.position() - read)
          result
        }

        if (!in.hasRemaining) {
          CoderResult.UNDERFLOW
        } else {
          val c1 = in.get()

          if (c1 < 0x80) {
            // Encoding in one byte
            if (!out.hasRemaining)
              finalize(1, CoderResult.OVERFLOW)
            else {
              out.put(c1.toByte)
              loop()
            }
          } else if (c1 < 0x800) {
            // Encoding in 2 bytes (by construction, not a surrogate)
            if (out.remaining < 2)
              finalize(1, CoderResult.OVERFLOW)
            else {
              out.put(((c1 >> 6) | 0xc0).toByte)
              out.put(((c1 & 0x3f) | 0x80).toByte)
              loop()
            }
          } else if (!isSurrogate(c1)) {
            // Not a surrogate, encoding in 3 bytes
            if (out.remaining < 3)
              finalize(1, CoderResult.OVERFLOW)
            else {
              out.put(((c1 >> 12) | 0xe0).toByte)
              out.put((((c1 >> 6) & 0x3f) | 0x80).toByte)
              out.put(((c1 & 0x3f) | 0x80).toByte)
              loop()
            }
          } else if (isHighSurrogate(c1)) {
            // Should have a low surrogate that follows
            if (!in.hasRemaining)
              finalize(1, CoderResult.UNDERFLOW)
            else {
              val c2 = in.get()
              if (!isLowSurrogate(c2)) {
                finalize(2, CoderResult.malformedForLength(1))
              } else {
                // Surrogate pair, encoding in 4 bytes
                if (out.remaining < 4)
                  finalize(2, CoderResult.OVERFLOW)
                else {
                  val cp = toCodePoint(c1, c2)
                  out.put(((cp >> 18) | 0xf0).toByte)
                  out.put((((cp >> 12) & 0x3f) | 0x80).toByte)
                  out.put((((cp >> 6) & 0x3f) | 0x80).toByte)
                  out.put(((cp & 0x3f) | 0x80).toByte)
                  loop()
                }
              }
            }
          } else {
            finalize(1, CoderResult.malformedForLength(1))
          }
        }
      }

      loop()
    }
  }

  private final val SurrogateMask = 0xf800 // 11111 0 00  00000000
  private final val SurrogateID   = 0xd800 // 11011 0 00  00000000

  @inline private def isSurrogate(c: Char): Boolean =
    (c & SurrogateMask) == SurrogateID
}
