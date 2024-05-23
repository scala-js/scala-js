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

package org.scalajs.ir

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.CharacterCodingException
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets.UTF_8

/** An immutable UTF-8 string.
 *
 *  The contents of a `UTF8String` is guaranteed to be a well-formed UTF-8
 *  string.
 *
 *  @note
 *    `equals()` and `hashCode()`, along with `==` and `##`, are just as
 *    broken for `UTF8String` as for `Array`s. Use the methods in the
 *    companion object instead. This is unavoidable because we cannot override
 *    `equals` nor `hashCode` in an `AnyVal`.
 */
final class UTF8String private (private[ir] val bytes: Array[Byte])
    extends AnyVal {

  import UTF8String._

  /** Returns the length in UTF-8 code units of this string. */
  @inline def length: Int = bytes.length

  /** Returns the `i`th UTF-8 code unit of this string. */
  @inline def apply(i: Int): Byte = bytes(i)

  @inline override def toString(): String = decodeUTF8(bytes)

  def ++(that: UTF8String): UTF8String = {
    val thisLen = this.length
    val thatLen = that.length
    val result = java.util.Arrays.copyOf(this.bytes, thisLen + thatLen)
    System.arraycopy(that.bytes, 0, result, thisLen, thatLen)
    new UTF8String(result)
  }

  def writeTo(buffer: ByteBuffer): Unit =
    buffer.put(bytes)
}

object UTF8String {
  /** Unsafely creates a `UTF8String` from a byte array.
   *
   *  This method does not validate the input array nor copies its contents. It
   *  should only be used to recreate a `UTF8String` from a byte array that has
   *  been extracted from a correctly validated `UTF8String`.
   */
  private[ir] def unsafeCreate(bytes: Array[Byte]): UTF8String =
    new UTF8String(bytes)

  /** Creates a UTF-8 string from a byte array.
   *
   *  The input byte array will be copied to ensure the immutability of
   *  `UTF8String`.
   *
   *  @throws java.lang.IllegalArgumentException
   *    if the input byte array is not a valid UTF-8 string
   */
  def apply(bytes: Array[Byte]): UTF8String =
    new UTF8String(validateUTF8(bytes).clone())

  /** Creates a UTF-8 string from a string.
   *
   *  @throws java.lang.IllegalArgumentException
   *    if the input string is not a valid UTF-16 string, i.e., if it
   *    contains unpaired surrogates
   */
  def apply(str: String): UTF8String =
    new UTF8String(encodeUTF8(str))

  /** Creates a UTF-8 string from a byte array without copying.
   *
   *  After calling this method, the input byte array must not be mutated by
   *  the caller anymore.
   *
   *  @throws java.lang.IllegalArgumentException
   *    if the input byte array is not a valid UTF-8 string
   */
  private[ir] def createAcquiringByteArray(bytes: Array[Byte]): UTF8String =
    new UTF8String(validateUTF8(bytes))

  def equals(x: UTF8String, y: UTF8String): Boolean =
    java.util.Arrays.equals(x.bytes, y.bytes)

  def hashCode(x: UTF8String): Int =
    scala.util.hashing.MurmurHash3.bytesHash(x.bytes)

  // -----------------------------------------------------------------
  // ----- Private helpers for validation, encoding and decoding -----
  // -----------------------------------------------------------------

  // --- Validation ---

  private def validateUTF8(bytes: Array[Byte]): Array[Byte] = {
    val len = bytes.length

    var i = 0
    while (i != len) {
      val b = bytes(i).toInt
      if (b >= 0) {
        // fast path: single-byte code point, ASCII repertoire
        i += 1
      } else {
        // slow path: multi-byte code point
        i += validateMultibyteCodePointAndGetByteLen(bytes, len, i, b)
      }
    }

    bytes
  }

  private def validateMultibyteCodePointAndGetByteLen(bytes: Array[Byte],
      end: Int, i: Int, b1: Int): Int = {

    @inline def isInvalidNextByte(b: Int): Boolean =
      (b & 0xc0) != 0x80

    def throwInvalid(): Nothing = {
      throw new IllegalArgumentException(
          "Invalid UTF-8 byte sequence " + bytes.mkString("[", ",", "]") +
          s" (error at index $i)")
    }

    if ((b1 & 0xe0) == 0xc0) { // 110xxxxx
      if (i > end - 2) {
        throwInvalid()
      } else {
        val b2 = bytes(i + 1) & 0xff
        if (isInvalidNextByte(b2)) {
          throwInvalid()
        } else {
          val cp = (((b1 & 0x1f) << 6) | (b2 & 0x3f))
          if (cp >= 0x80)
            2
          else
            throwInvalid()
        }
      }
    } else if ((b1 & 0xf0) == 0xe0) { // 1110xxxx
      if (i > end - 3) {
        throwInvalid()
      } else {
        val b2 = bytes(i + 1) & 0xff
        val b3 = bytes(i + 2) & 0xff
        if (isInvalidNextByte(b2) || isInvalidNextByte(b3)) {
          throwInvalid()
        } else {
          val cp = (((b1 & 0xf) << 12) | ((b2 & 0x3f) << 6) | (b3 & 0x3f))
          if (cp >= 0x800 && !Character.isSurrogate(cp.toChar))
            3
          else
            throwInvalid()
        }
      }
    } else if ((b1 & 0xf8) == 0xf0) { // 11110xxx
      if (i > end - 4) {
        throwInvalid()
      } else {
        val b2 = bytes(i + 1) & 0xff
        val b3 = bytes(i + 2) & 0xff
        val b4 = bytes(i + 3) & 0xff
        if (isInvalidNextByte(b2) || isInvalidNextByte(b3) || isInvalidNextByte(b4)) {
          throwInvalid()
        } else {
          val cp = (((b1 & 0x7) << 18) | ((b2 & 0x3f) << 12) |
              ((b3 & 0x3f) << 6) | (b4 & 0x3f))
          if (cp >= 0x10000 && cp <= Character.MAX_CODE_POINT)
            4
          else
            throwInvalid()
        }
      }
    } else {
      throwInvalid()
    }
  }

  // --- Encoding ---

  private def encodeUTF8(str: String): Array[Byte] = {
    // scalastyle:off return
    val len = str.length()

    /* We optimistically assume that all characters are ASCII, and backtrack if
     * we find a non-ASCII character.
     */
    val result = new Array[Byte](len)
    var i = 0
    while (i != len) {
      val c = str.charAt(i).toInt
      if ((c & 0x7f) != c)
        return encodeUTF8WithNonASCII(str)
      result(i) = c.toByte
      i += 1
    }
    result
    // scalastyle:on return
  }

  private def encodeUTF8WithNonASCII(str: String): Array[Byte] = {
    // Note: a UTF-8 encoder can never encounter an "unmappable" character
    val encoder = UTF_8.newEncoder().onMalformedInput(CodingErrorAction.REPORT)
    try {
      val outputBuffer = encoder.encode(CharBuffer.wrap(str))
      val result = new Array[Byte](outputBuffer.remaining())
      outputBuffer.get(result)
      result
    } catch {
      case _: CharacterCodingException =>
        throw new IllegalArgumentException("Not a valid UTF-16 string: " + str)
    }
  }

  // --- Decoding ---

  private def decodeUTF8(bytes: Array[Byte]): String = {
    // scalastyle:off return
    /* We optimistically assume that all characters are single-byte (i.e., in
     * the ASCII repertoire), and fall back to a full UTF-8 decoder if we find
     * a multi-byte character.
     */
    val len = bytes.length
    val result = new Array[Char](len)
    var i = 0
    while (i != len) {
      val b = bytes(i)
      if (b < 0)
        return new String(bytes, UTF_8)
      result(i) = (b & 0xff).toChar
      i += 1
    }
    new String(result)
    // scalastyle:on return
  }
}
