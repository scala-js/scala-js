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

import scala.scalajs.js.typedarray._
import scala.scalajs.js.typedarray.DataViewExt._

/** An implementation of the SHA-1 algorithm for use in Hashers.
 *
 *  Reference: https://en.wikipedia.org/wiki/SHA-1#SHA-1_pseudocode
 *
 *  This implementation MUST NOT be used for any cryptographic purposes. It did
 *  not receive the care and attention required for security purposes. It is
 *  only meant as a best-effort hash for incremental linking.
 */
object SHA1 {
  final class DigestBuilder {
    /** The SHA-1 state.
     *
     *  It is an array of 5 Ints, initialized with a specific initialization
     *  vector specified by SHA-1, but we represent it as 5 individual Ints,
     *  since we always access them with static indices.
     */
    private var state0: Int = 0x67452301
    private var state1: Int = 0xefcdab89
    private var state2: Int = 0x98badcfe
    private var state3: Int = 0x10325476
    private var state4: Int = 0xc3d2e1f0

    /** The number of *bytes* integrated so far in the state.
     *
     *  This is used for two purposes:
     *
     *  - By taking `byteCount & 63`, we get the number of bytes already
     *    written in the current `buffer`, which has 64 bytes.
     *  - In the padding during `sha1Final`, where we have to write the total
     *    number of *bits* (i.e., `byteCount << 3`) integrated into the digest.
     */
    private var byteCount: Long = 0L

    /** The buffer for one 64-byte chunk to integrate into the state. */
    private val buffer = new Int8Array(64)

    /** A view of the buffer for accessing data in big endian. */
    private val bufferView = new DataView(buffer.buffer)

    /** The temporary array of 32-bit integers used by `sha1Transform()`,
     *  called `W` in the SHA-1 algorithm.
     *
     *  Even though it is only used inside `sha1Transform()`, we declare it
     *  here so that we don't have to allocate a new one every time.
     */
    private val W = new Int32Array(80)

    // Public API

    def update(b: Byte): Unit =
      sha1Update(Array(b), 0, 1)

    def update(b: Array[Byte]): Unit =
      sha1Update(b, 0, b.length)

    def update(b: Array[Byte], off: Int, len: Int): Unit = {
      if (off < 0 || len < 0 || off > b.length - len)
        throw new IndexOutOfBoundsException()

      sha1Update(b, off, len)
    }

    def updateUTF8String(str: UTF8String): Unit =
      update(str.bytes)

    def finalizeDigest(): Array[Byte] =
      sha1Final()

    // Private methods

    /** Hashes a single 512-bit block.
     *
     *  This is the core of the algorithm.
     */
    private def sha1Transform(): Unit = {
      import java.lang.Integer.{rotateLeft => rol}

      // Local copies
      val buffer = this.buffer
      val bufferView = this.bufferView
      val W = this.W

      /* Perform the initial expansion of `buffer` into 80 Ints.
       * The first 16 Ints are read from the buffer in big endian. The rest is
       * derived from previous values.
       */
      for (i <- 0 until 16)
        W(i) = bufferView.getInt32(i * 4)
      for (i <- 16 until 80)
        W(i) = rol(W(i - 3) ^ W(i - 8) ^ W(i - 14) ^ W(i - 16), 1)

      // Copy state to working vars
      var a = state0
      var b = state1
      var c = state2
      var d = state3
      var e = state4

      // Main loop

      @inline def performOneIteration(i: Int, fi: Int, Ki: Int): Unit = {
        val temp = rol(a, 5) + fi + e + Ki + W(i)
        e = d
        d = c
        c = rol(b, 30)
        b = a
        a = temp
      }

      for (i <- 0 until 20)
        performOneIteration(i, (b & c) | (~b & d), 0x5a827999)
      for (i <- 20 until 40)
        performOneIteration(i, b ^ c ^ d, 0x6ed9eba1)
      for (i <- 40 until 60)
        performOneIteration(i, (b & c) | (b & d) | (c & d), 0x8f1bbcdc)
      for (i <- 60 until 80)
        performOneIteration(i, b ^ c ^ d, 0xca62c1d6)

      // Add the working vars back into `state`
      state0 += a
      state1 += b
      state2 += c
      state3 += d
      state4 += e
    }

    private def sha1Update(data: Array[Byte], off: Int, len: Int): Unit = {
      // Local copies
      val buffer = this.buffer

      /* Update the byte count.
       * We use `Integer.toUnsignedLong(len)` because `len` is known to be
       * non-negative, and it's faster. It also results in a `hi` part which is
       * a constant 0, which makes the `+` operation faster as well.
       */
      val oldByteCount = byteCount
      byteCount = oldByteCount + Integer.toUnsignedLong(len)

      /* Decompose `data` into 64-byte chunks, taking into account bytes that
       * were previously stored in `buffer`.
       */

      var bufferPos = oldByteCount.toInt & 63
      var dataPos = off
      val dataEnd = off + len

      @inline def bufferRem = 64 - bufferPos

      while (dataEnd - dataPos >= bufferRem) {
        arraycopyToInt8Array(data, dataPos, buffer, bufferPos, bufferRem)
        sha1Transform()
        dataPos += bufferRem
        bufferPos = 0
      }

      arraycopyToInt8Array(data, dataPos, buffer, bufferPos, dataEnd - dataPos)
    }

    /** Add padding and return the message digest. */
    private def sha1Final(): Array[Byte] = {
      // Local copies
      val buffer = this.buffer
      val bufferView = this.bufferView

      // Snapshot the final bit count, before padding
      val finalByteCount = byteCount
      val finalBitCount = finalByteCount << 3

      /* The padding contains:
       *
       * - One bit '1'
       * - '0' bits to pad, until
       * - the `finalBitCount`, stored in 64-bit big-endian
       *
       * Converted to byte-size items, this means:
       *
       * - One byte '0x80'
       * - '0x00' bytes to pad, until
       * - the `finalBitCount`, stored in 8-byte big-endian
       *
       * Since the `buffer` is not full when we start, we can always write the
       * byte '0x80'. After that, if there are still at least 8 bytes left, we
       * can fit everything in the current 64-byte chunk. Otherwise, we have to
       * write '0x00' to fill the current chunk, transform, and then start a
       * new chunk.
       */

      var currentBufferPos = finalByteCount.toInt & 63

      // 0x80
      buffer(currentBufferPos) = 0x80.toByte
      currentBufferPos += 1

      // Finish one chunk if we don't have enough space in the current one
      if (currentBufferPos > 56) {
        buffer.fill(0x00, currentBufferPos, 64)
        sha1Transform()
        currentBufferPos = 0
      }

      // Write 0x00 until position 56 (64 - 8)
      buffer.fill(0, currentBufferPos, 56)

      // Write the final bit count in big-endian
      bufferView.setInt64(56, finalBitCount)

      // Transform one last time
      sha1Transform()

      /* Compute the result digest, which is the `state` stored in big-endian.
       * We reuse our internal buffer, because we can.
       */
      bufferView.setInt32(0, state0)
      bufferView.setInt32(4, state1)
      bufferView.setInt32(8, state2)
      bufferView.setInt32(12, state3)
      bufferView.setInt32(16, state4)
      buffer.subarray(0, 20).toArray
    }

    /** Like `System.arraycopy`, but the dest is an `Int8Array`. */
    private def arraycopyToInt8Array(src: Array[Byte], srcPos: Int,
        dest: Int8Array, destPos: Int, len: Int): Unit = {
      for (i <- 0 until len)
        dest(destPos + i) = src(srcPos + i)
    }
  }
}
