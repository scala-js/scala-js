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

package org.scalajs.linker.backend.emitter

import org.scalajs.ir._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.{ESFeatures, ESVersion}

/** Compression of constant arrays using various encoding strategies.
 *
 *  We use 5 strategies, depending on the type of array and the values we
 *  find in the array. In all cases, we use some form of base-64 encoding,
 *  though we don't use the standard Base64 alphabet. One version is
 *  fixed-length, while the others are variable-length.
 *
 *  - Raw: fixed-length encoding of chunks of 3 bytes with 4 chars for each chunk.
 *  - UVals: varlen encoding of unsigned values.
 *  - UDiffs: varlen encoding of unsigned *diffs* of each value wrt. the previous one.
 *  - SVals: varlen encoding of signed values.
 *  - SDiffs: varlen encoding of signed *diffs* of each value wrt. the previous one.
 *
 *  When choosing our base-64 alphabets, we must only use printable ASCII
 *  chars that need no escaping in a JS string. That ensures that each char
 *  is actually a single byte in the .js file.
 *  The critical ASCII chars to avoid are
 *  0x00-0x1f (control chars), 0x22 ("), 0x5c (\) and 0x7f (DEL).
 *  Therefore, we can pick within 0x23-0x5b and 0x5d-0x7e.
 *
 *  ---
 *
 *  For the raw variant, we use the ranges 0x34-0x3b, 0x40-0x5b and 0x60-0x7b.
 *
 *  The gap between the ranges is 4 (0x60 - 4 = 0x5c = 0x5b + 1, likewise
 *  0x40 - 4 = 0x3b + 1). The bits 0x60 = 0110_0000b identify in which
 *  range we are: 01, 10 or 11. If we shift these two bits by 3 to be
 *  aligned on bits 2-3 (values 4 and 8), we get 0x04, 0x08 or 0x0c, whose
 *  values are 4, 8 and 12. Therefore, if `x` is the original value and
 *  `y = (x & 0x60) >>> 3`, we compute `z = x - y` which will be:
 *
 *  - `x - 4` in the first range, offsetting the range to 0x30-0x37
 *  - `x - 8` in the second range, offsetting the range to 0x38-0x53
 *  - `x - 12` in the third range, offsetting the range to 0x54-0x6f.
 *
 *  Lo and behold, the offset ranges are consecutive! Offsetting them by
 *  an additional constant term `- 0x30` gives the full range 0x00-0x3f.
 *  That covers the 6 bits encoded in an base-64 byte.
 *
 *  The full decoding of a byte `x` is therefore
 *  `(x - 0x30) - ((x & 0x60) >>> 3)`. By construction, all the
 *  intermediate results stay within the byte, without causing any carry
 *  across the byte boundary. That means we can compute them in parallel
 *  over the 4 bytes in a 32-bit integer.
 *
 *  ---
 *
 *  For the varlen encodings, we use the ranges 0x30-0x4f (for
 *  continuation chunks) and 0x5d-0x7c (for final chunks).
 *
 *  We need a branch for continuation/final anyway. Therefore, it is not
 *  useful to have a fancy branchless way to decode a byte. Instead, it
 *  is better to have two ranges of 32 chars each.
 *
 *  While we can in theory do this for bytes and shorts, currently we only
 *  use these strategies for ints and longs (i.e., for an Int32Array).
 *
 *  For bytes, the best case of varlen encodings is only 3/4 times the
 *  raw encoding.
 *
 *  For shorts, in theory it is 3/8 if most values fit in 31 bits. In
 *  practice it is also probably just 3/4. It's also worth noting that
 *  the raw encoding is almost never worse than the naive "text" encoding.
 *  That only happens when most values are in the (positive) range [0, 9].
 *
 *  The current trade-off is not to pay the constant price of defining
 *  the decoding functions for bytes and shorts.
 *
 *  The unsigned variant is straightforward. The signed variant
 *  sign-extends the first (most significant) chunk, then processes the
 *  other chunks normally.
 *
 *  We use the same function for the "Vals" and "Diffs" variant. The
 *  parameter `prevMask` tells them apart: it is 0 for Vals (masking off
 *  the `prev` value) and -1 for Diffs (keeping all the bits of the
 *  `prev` value). This trick reduces code size and has virtually no
 *  run-time cost.
 */
private[emitter] object ConstantArrays {

  /** Minimum amount of elements in a constant array to use the encoded strategy.
   *
   *  This is a trade-off. For small arrays, it is probably more important
   *  that they are created quickly, rather than saving some bytes.
   *
   *  Must not be 0. The constant array decoder functions assume that the
   *  resulting array is not empty.
   */
  private final val ConstantArrayThreshold = 8

  /* Strategies - ordered by decreasing run-time performance.
   * All of them use a base64 (6-bit) target encoding.
   */

  /** Raw: fixed-length encoding of chunks of 3 bytes with 4 chars each. */
  private final val CAStrategyRaw = 0

  /** UVals: varlen encoding of unsigned values. */
  private final val CAStrategyUVals = 1

  /** UDiffs: varlen encoding of unsigned diffs of each value wrt. the previous one. */
  private final val CAStrategyUDiffs = 2

  /** SVals: varlen encoding of signed values. */
  private final val CAStrategySVals = 3

  /** SDiffs: varlen encoding of signed diffs of each value wrt. the previous one. */
  private final val CAStrategySDiffs = 4

  /** Should we a use a constant array strategy for the given array elems.
   *
   *  This true when all of the following apply:
   *
   *  - We emit for ES2015+ (because we assume that arrays use TypedArrays),
   *  - The elem type is an integer type,
   *  - There are at least `ConstantArrayThreshold` elements, and
   *  - All the elements are Literals.
   *
   *  In the future, we could add floating point types and booleans.
   */
  def shouldGenerateAsConstantArray(primRef: PrimRef, elems: List[Tree],
      esFeatures: ESFeatures): Boolean = {

    primRef match {
      case _ if esFeatures.esVersion < ESVersion.ES2015 =>
        false
      case CharRef | ByteRef | ShortRef | IntRef | LongRef =>
        elems.lengthCompare(ConstantArrayThreshold) >= 0 &&
        elems.forall(_.isInstanceOf[Literal])
      case _ =>
        false
    }
  }

  /** Generates a constant array using a base-64-encoded string.
   *
   *  See CoreJSLib.defineConstantArrayMakers() for more information on the
   *  encoding.
   *
   *  @return
   *    Tuple of (helperVarField, encoded)
   */
  def genConstantArray(primRef: PrimRef, elems: List[Tree])(
      implicit pos: Position): (VarField, String) = {

    val elemCount = elems.size
    val elemByteSize = primRef match {
      case ByteRef            => 1
      case CharRef | ShortRef => 2
      case IntRef             => 4
      case LongRef            => 8
    }

    val rawBuffer = java.nio.ByteBuffer.allocate(elemCount * elemByteSize)
      .order(java.nio.ByteOrder.LITTLE_ENDIAN)

    for (elem <- elems) {
      /* Recall that the run-time subtyping relationship for numbers
       * (e.g., Short <: Int) does not apply to static typing.
       * Literals must be exactly of the right type.
       */
      (elem: @unchecked) match {
        case CharLiteral(v)  => rawBuffer.putChar(v)
        case ByteLiteral(v)  => rawBuffer.put(v)
        case ShortLiteral(v) => rawBuffer.putShort(v)
        case IntLiteral(v)   => rawBuffer.putInt(v)
        case LongLiteral(v)  => rawBuffer.putLong(v)
      }
    }
    rawBuffer.rewind()

    // Currently, only ints and longs have the non-raw strategies
    val strategy = primRef match {
      case IntRef | LongRef =>
        pickConstantInt32ArrayStrategy(rawBuffer)
      case _ =>
        CAStrategyRaw
    }
    rawBuffer.rewind()

    val encoded =
      if (strategy == CAStrategyRaw) encodeConstantArrayRaw(rawBuffer)
      else encodeConstantInt32ArrayVarLen(rawBuffer, strategy)

    val helperVarField = strategy match {
      case CAStrategyRaw    => VarField.constArrRaw
      case CAStrategyUVals  => VarField.constArrUVals
      case CAStrategyUDiffs => VarField.constArrUDiffs
      case CAStrategySVals  => VarField.constArrSVals
      case CAStrategySDiffs => VarField.constArrSDiffs
    }

    (helperVarField, encoded)
  }

  private def ceilDiv(x: Int, y: Int): Int =
    Integer.divideUnsigned(x + y - 1, y)

  /** The number of bytes required to encode `unencodedByteLength` bytes using
   *  the raw encoding.
   *
   *  We need 4 encoded bytes for every group of 3 unencoded bytes.
   */
  private def totalRawSize(unencodedByteLength: Int): Int =
    ceilDiv(unencodedByteLength, 3) * 4

  /** The number of base-64 chars required to varlen-encode an unsigned int.
   *
   *  That is 1 char for every 5 bits required for the theoretical encoding,
   *  with a minimum of 1 char.
   */
  private def uCharSize(x: Int): Int = Math.max(ceilDiv(uBitSize(x), 5), 1)

  /** The number of base-64 chars required to varlen-encode a signed int.
   *
   *  That is 1 char for every 5 bits required for the theoretical encoding,
   *  with a minimum of 1 char.
   */
  private def sCharSize(x: Int): Int = Math.max(ceilDiv(sBitSize(x), 5), 1)

  /** The number of bits theoretically required to encode an unsigned int. */
  private def uBitSize(x: Int): Int = 32 - Integer.numberOfLeadingZeros(x)

  /** The number of bits theoretically required to encode a signed int.
   *
   *  1 for the sign bit, plus the number of bits required to encode the
   *  "bit-absolute" value of x (i.e., ~x if x < 0).
   */
  private def sBitSize(x: Int): Int = 1 + uBitSize(x ^ (x >> 31))

  /** Pick an encoding strategy for an Int32Array-based array.
   *
   *  This is used for longs as well, since we store their lo and hi fields
   *  separately. This is true even when using bigints for longs: we decode
   *  as pairs of ints, then reinterpret as a BigInt64Array.
   */
  private def pickConstantInt32ArrayStrategy(buffer: java.nio.ByteBuffer): Int = {
    val sizeRaw = totalRawSize(buffer.remaining())

    var sizeUVals = 0
    var sizeUDiffs = 0
    var sizeSVals = 0
    var sizeSDiffs = 0

    var prevElem = 0

    while (buffer.hasRemaining()) {
      val elem = buffer.getInt()
      val diff = elem - prevElem
      prevElem = elem

      sizeUVals += uCharSize(elem)
      sizeUDiffs += uCharSize(diff)
      sizeSVals += sCharSize(elem)
      sizeSDiffs += sCharSize(diff)
    }

    // Indexed by CAStrategyX constants; the order matters
    val sizes = Array(
      sizeRaw,
      sizeUVals,
      sizeUDiffs,
      sizeSVals,
      sizeSDiffs
    )

    sizes.zipWithIndex.min._2
  }

  private def encodeConstantArrayRaw(buffer: java.nio.ByteBuffer): String = {
    val len = buffer.remaining()

    val encoded = new java.lang.StringBuilder(totalRawSize(len))

    while (buffer.hasRemaining()) {
      val a = buffer.get() & 0xff
      val b = if (buffer.hasRemaining()) buffer.get() & 0xff else 0
      val c = if (buffer.hasRemaining()) buffer.get() & 0xff else 0

      def encode6Bits(x: Int): Char = {
        if (x < 8) (0x34 + x).toChar
        else if (x < 36) (0x40 + (x - 8)).toChar
        else (0x60 + (x - 36)).toChar
      }

      encoded.append(encode6Bits(a & 0x3f))
      encoded.append(encode6Bits((a >>> 6) | ((b & 0x0f) << 2)))
      encoded.append(encode6Bits((b >>> 4) | ((c & 0x03) << 4)))
      encoded.append(encode6Bits(c >>> 2))
    }

    encoded.toString()
  }

  private def encodeConstantInt32ArrayVarLen(buffer: java.nio.ByteBuffer,
      strategy: Int): String = {

    val LowRangeStart = 0x30
    val HighRangeStart = 0x5d

    val isSigned = strategy == CAStrategySVals || strategy == CAStrategySDiffs
    val isDiffs = strategy == CAStrategyUDiffs || strategy == CAStrategySDiffs

    val encoded = new java.lang.StringBuilder()
    var prev = 0

    while (buffer.hasRemaining()) {
      val elemValue = buffer.getInt()
      var valueToEncode = if (isDiffs) elemValue - prev else elemValue
      prev = elemValue

      /* We must encode the chars from most significant to least significant.
       * Therefore, we must first compute how many chars we need, then
       * encode backwards.
       * That order is expensive to encode but cheap to decode.
       */

      val charCount =
        if (isSigned) sCharSize(valueToEncode)
        else uCharSize(valueToEncode)

      for (i <- (charCount - 1) to 1 by -1) {
        val shifted =
          if (isSigned) valueToEncode >> (5 * i)
          else valueToEncode >>> (5 * i)
        encoded.append((LowRangeStart + (shifted & 0x1f)).toChar)
      }
      encoded.append((HighRangeStart + (valueToEncode & 0x1f)).toChar)
    }

    encoded.toString()
  }

}
