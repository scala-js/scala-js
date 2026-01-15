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

import java.io.Serializable
import java.lang.Cloneable
import java.lang.Integer.bitCount
import java.lang.Integer.toUnsignedLong
import java.nio.{ByteBuffer, LongBuffer}
import java.util
import java.util.ScalaOps.IntScalaOps

private object BitSet {
  private final val AddressBitsPerWord = 5 // Int Based 2^5 = 32
  private final val ElementSize = 1 << AddressBitsPerWord
  private final val RightBits = ElementSize - 1

  def valueOf(longs: Array[Long]): util.BitSet = {
    val bs = new util.BitSet

    for (i <- 0 until longs.length * 64) {
      val idx = i / 64
      if ((longs(idx) & (1L << (i % 64))) != 0)
        bs.set(i)
    }

    bs
  }

  def valueOf(lb: LongBuffer): BitSet = {
    val arr = new Array[Long](lb.remaining())
    lb.get(arr)
    lb.position(lb.position() - arr.length) // Restores the buffer position
    valueOf(arr)
  }

  def valueOf(bytes: Array[Byte]): BitSet = {
    val bs = new BitSet

    for (i <- 0 until bytes.length * 8) {
      val idx = i / 8
      if ((bytes(idx) & (1 << (i % 8))) != 0)
        bs.set(i)
    }

    bs
  }

  def valueOf(bb: ByteBuffer): BitSet = {
    val arr = new Array[Byte](bb.remaining())
    bb.get(arr)
    bb.position(bb.position() - arr.length) // Restores the buffer position
    valueOf(arr)
  }
}

class BitSet private (private var bits: Array[Int]) extends Serializable with Cloneable {
  import BitSet.{AddressBitsPerWord, ElementSize, RightBits}

  def this(nbits: Int) = {
    this(
      bits = {
        if (nbits < 0)
          throw new NegativeArraySizeException

        val length = (nbits + BitSet.RightBits) >> BitSet.AddressBitsPerWord

        new Array[Int](length)
      }
    )
  }

  def this() = {
    this(64)
  }

  def toByteArray(): Array[Byte] = {
    if (isEmpty()) {
      new Array[Byte](0)
    } else {
      val l = (length() + 7) / 8
      val array = new Array[Byte](l)

      for (i <- 0 until length()) {
        if (get(i))
          array(i / 8) = (array(i / 8) | (1 << (i % 8))).toByte
      }

      array
    }
  }

  def toLongArray(): Array[Long] = {
    if (isEmpty()) {
      new Array[Long](0)
    } else {
      val l = (length() + 63) / 64
      val array = new Array[Long](l)

      for (i <- 0 until length()) {
        if (get(i))
          array(i / 64) |= 1L << (i % 64)
      }

      array
    }
  }

  def flip(bitIndex: Int): Unit = {
    checkBitIndex(bitIndex)

    val len = (bitIndex >> AddressBitsPerWord) + 1
    ensureLength(len)

    bits(len - 1) ^= 1 << (bitIndex & RightBits)
  }

  def flip(fromIndex: Int, toIndex: Int): Unit = {
    checkToAndFromIndex(fromIndex, toIndex)

    if (fromIndex != toIndex) {
      val len2 = ((toIndex - 1) >> AddressBitsPerWord) + 1
      ensureLength(len2)
      val idx1 = fromIndex >> AddressBitsPerWord
      val idx2 = (toIndex - 1) >> AddressBitsPerWord
      val mask1 = (~0) << (fromIndex & RightBits)
      val mask2 = (~0) >>> (ElementSize - (toIndex & RightBits))

      if (idx1 == idx2) {
        bits(idx1) ^= (mask1 & mask2)
      } else {
        bits(idx1) ^= mask1
        bits(idx2) ^= mask2
        for (i <- idx1 + 1 until idx2)
          bits(i) ^= (~0)
      }
    }
  }

  def set(bitIndex: Int): Unit = {
    checkBitIndex(bitIndex)

    val len = (bitIndex >> AddressBitsPerWord) + 1
    ensureLength(len)

    bits(len - 1) |= 1 << (bitIndex & RightBits)
  }

  def set(bitIndex: Int, value: Boolean): Unit =
    if (value) set(bitIndex)
    else clear(bitIndex)

  // fromIndex is inclusive, toIndex is exclusive
  def set(fromIndex: Int, toIndex: Int): Unit = {
    checkToAndFromIndex(fromIndex, toIndex)

    if (fromIndex != toIndex) {
      val len2 = ((toIndex - 1) >> AddressBitsPerWord) + 1
      ensureLength(len2)

      val idx1 = fromIndex >> AddressBitsPerWord
      val idx2 = (toIndex - 1) >> AddressBitsPerWord
      val mask1 = (~0) << (fromIndex & RightBits)
      val mask2 = (~0) >>> (ElementSize - (toIndex & RightBits))

      if (idx1 == idx2) {
        bits(idx1) |= (mask1 & mask2)
      } else {
        bits(idx1) |= mask1
        bits(idx2) |= mask2

        for (i <- idx1 + 1 until idx2)
          bits(i) |= (~0)
      }
    }
  }

  def set(fromIndex: Int, toIndex: Int, value: Boolean): Unit =
    if (value) set(fromIndex, toIndex)
    else clear(fromIndex, toIndex)

  def clear(bitIndex: Int): Unit = {
    checkBitIndex(bitIndex)

    val arrayPos = bitIndex >> AddressBitsPerWord

    if (arrayPos < bits.length) {
      bits(arrayPos) &= ~(1 << (bitIndex & RightBits))
    }
  }

  def clear(fromIndex: Int, toIndex: Int): Unit = {
    checkToAndFromIndex(fromIndex, toIndex)

    val last = bits.length << AddressBitsPerWord
    if (fromIndex >= last || fromIndex == toIndex)
      return // scalastyle:ignore

    val toIndexOrLast =
      if (toIndex > last) last
      else toIndex

    val idx1 = fromIndex >> AddressBitsPerWord
    val idx2 = (toIndexOrLast - 1) >> AddressBitsPerWord
    val mask1 = (~0) << (fromIndex & RightBits)
    val mask2 = (~0) >>> (ElementSize - (toIndexOrLast & RightBits))

    if (idx1 == idx2) {
      bits(idx1) &= ~(mask1 & mask2)
    } else {
      bits(idx1) &= ~mask1
      bits(idx2) &= ~mask2

      for (i <- idx1 + 1 until idx2)
        bits(i) = 0
    }
  }

  def clear(): Unit = {
    for (i <- 0 until bits.length)
      bits(i) = 0
  }

  def get(bitIndex: Int): Boolean = {
    checkBitIndex(bitIndex)

    val arrayPos = bitIndex >> AddressBitsPerWord

    if (arrayPos < bits.length)
      (bits(arrayPos) & (1 << (bitIndex & RightBits))) != 0
    else
      false
  }

  def get(fromIndex: Int, toIndex: Int): BitSet = {
    // scalastyle:off return
    checkToAndFromIndex(fromIndex, toIndex)

    val last = bits.length << AddressBitsPerWord
    if (fromIndex >= last || fromIndex == toIndex)
      return new BitSet(0)

    val toIndexOrLast =
      if (toIndex > last) last
      else toIndex

    val idx1 = fromIndex >> AddressBitsPerWord
    val idx2 = (toIndexOrLast - 1) >> AddressBitsPerWord
    val mask1 = (~0) << (fromIndex & RightBits)
    val mask2 = (~0) >>> (ElementSize - (toIndexOrLast & RightBits))

    if (idx1 == idx2) {
      val result = (bits(idx1) & (mask1 & mask2)) >>> (fromIndex % ElementSize)
      if (result == 0)
        return new BitSet(0)

      new BitSet(Array(result))
    } else {
      val newbits = new Array[Int](idx2 - idx1 + 1)
      // first fill in the first and last indexes in the new bitset
      newbits(0) = bits(idx1) & mask1
      newbits(newbits.length - 1) = bits(idx2) & mask2
      // fill in the in between elements of the new bitset
      for (i <- 1 until idx2 - idx1)
        newbits(i) = bits(idx1 + i)

      val numBitsToShift = fromIndex & RightBits

      if (numBitsToShift != 0) {
        for (i <- 0 until newbits.length) {
          // shift the current element to the right
          newbits(i) = newbits(i) >>> numBitsToShift
          // apply the last x bits of newbits[i+1] to the current
          // element
          if (i != newbits.length - 1)
            newbits(i) |= newbits(i + 1) << (ElementSize - numBitsToShift)
        }
      }

      new BitSet(newbits)
    }
    // scalastyle:on return
  }

  def nextSetBit(fromIndex: Int): Int = {
    // scalastyle:off return
    checkFromIndex(fromIndex)

    if (fromIndex >= (bits.length << AddressBitsPerWord))
      return -1

    var idx = fromIndex >> AddressBitsPerWord

    // first check in the same bit set element
    if (bits(idx) != 0) {
      var j = fromIndex & RightBits
      while (j < ElementSize) {
        if ((bits(idx) & (1 << j)) != 0)
          return (idx << AddressBitsPerWord) + j
        j += 1
      }
    }

    idx += 1

    while (idx < bits.length && bits(idx) == 0)
      idx += 1

    if (idx == bits.length)
      return -1

    // we know for sure there is a bit set to true in this element
    // since the bitset value is not 0
    var j = 0
    while (j < ElementSize) {
      if ((bits(idx) & (1 << j)) != 0)
        return (idx << AddressBitsPerWord) + j
      j += 1
    }

    -1
    // scalastyle:on return
  }

  def nextClearBit(fromIndex: Int): Int = {
    // scalastyle:off return
    checkFromIndex(fromIndex)

    val length = bits.length
    val bssize = length << AddressBitsPerWord

    if (fromIndex >= bssize)
      return fromIndex

    var idx = fromIndex >> AddressBitsPerWord

    if (bits(idx) != (~0)) {
      var j = fromIndex % ElementSize
      while (j < ElementSize) {
        if ((bits(idx) & (1 << j)) == 0)
          return idx * ElementSize + j
        j += 1
      }
    }

    idx += 1

    while (idx < length && bits(idx) == (~0))
      idx += 1

    if (idx == length)
      return bssize

    var j = 0
    while (j < ElementSize) {
      if ((bits(idx) & (1 << j)) == 0)
        return (idx << AddressBitsPerWord) + j
      j += 1
    }

    bssize
    // scalastyle:on return
  }

  def previousSetBit(fromIndex: Int): Int = {
    // scalastyle:off return
    if (fromIndex == -1)
      return -1

    checkFromIndex(fromIndex)

    val bssize = bits.length << AddressBitsPerWord
    var idx = Math.min(bits.length - 1, fromIndex >> AddressBitsPerWord)

    if (bits(idx) != 0) {
      if (idx == bssize)
        return idx

      var j: Int = fromIndex % ElementSize
      while (j >= 0) {
        if ((bits(idx) & (1 << j)) != 0)
          return idx * ElementSize + j

        j -= 1
      }
    }

    idx -= 1

    while (idx >= 0 && bits(idx) == 0)
      idx -= 1

    if (idx == -1)
      return -1

    var j: Int = ElementSize - 1
    while (j >= 0) {
      if ((bits(idx) & (1 << j)) != 0)
        return (idx << AddressBitsPerWord) + j

      j -= 1
    }

    bssize
    // scalastyle:on return
  }

  def previousClearBit(fromIndex: Int): Int = {
    // scalastyle:off return
    if (fromIndex == -1)
      return -1

    checkFromIndex(fromIndex)

    val length = bits.length
    val bssize = length << AddressBitsPerWord

    if (fromIndex >= bssize)
      return fromIndex

    var idx = Math.min(bits.length - 1, fromIndex >> AddressBitsPerWord)

    if (bits(idx) != (~0)) {
      var j: Int = fromIndex % ElementSize
      while (j >= 0) {
        if ((bits(idx) & (1 << j)) == 0)
          return idx * ElementSize + j

        j -= 1
      }
    }

    idx -= 1

    while (idx >= 0 && bits(idx) == (~0))
      idx -= 1

    if (idx == -1)
      return -1

    var j: Int = ElementSize - 1
    while (j >= 0) {
      if ((bits(idx) & (1 << j)) == 0)
        return (idx << AddressBitsPerWord) + j

      j -= 1
    }

    bssize
    // scalastyle:on return
  }

  def length(): Int = {
    val len = getActualArrayLength()
    if (len == 0)
      0
    else
      (len << AddressBitsPerWord) - Integer.numberOfLeadingZeros(bits(len - 1))
  }

  def isEmpty(): Boolean = getActualArrayLength() == 0

  def intersects(set: BitSet): Boolean = {
    // scalastyle:off return
    val bsBits = set.bits
    val length1 = bits.length
    val length2 = set.bits.length

    if (length1 <= length2) {
      var i: Int = 0
      while (i < length1) {
        if ((bits(i) & bsBits(i)) != 0)
          return true

        i += 1
      }
    } else {
      var i: Int = 0
      while (i < length2) {
        if ((bits(i) & bsBits(i)) != 0)
          return true

        i += 1
      }
    }

    false
    // scalastyle:on return
  }

  def cardinality(): Int = {
    var count = 0

    val length = getActualArrayLength()

    for (idx <- 0 until length) {
      count += bitCount(bits(idx))
    }

    count
  }

  def and(set: BitSet): Unit = {
    val bsBits = set.bits
    val length1 = bits.length
    val length2 = set.bits.length

    if (length1 <= length2) {
      for (i <- 0 until length1)
        bits(i) &= bsBits(i)
    } else {
      for (i <- 0 until length2)
        bits(i) &= bsBits(i)

      for (i <- length2 until length1)
        bits(i) = 0
    }
  }

  def or(set: BitSet): Unit = {
    val bsActualLen = set.getActualArrayLength()

    if (bsActualLen > bits.length) {
      val tempBits = Arrays.copyOf(set.bits, bsActualLen)

      for (i <- 0 until bits.length)
        tempBits(i) |= bits(i)

      bits = tempBits
    } else {
      val bsBits = set.bits

      for (i <- 0 until bsActualLen)
        bits(i) |= bsBits(i)
    }
  }

  def xor(set: BitSet): Unit = {
    val bsActualLen = set.getActualArrayLength()

    if (bsActualLen > bits.length) {
      val tempBits = Arrays.copyOf(set.bits, bsActualLen)

      for (i <- 0 until bits.length)
        tempBits(i) ^= bits(i)

      bits = tempBits
    } else {
      val bsBits = set.bits

      for (i <- 0 until bsActualLen)
        bits(i) ^= bsBits(i)
    }
  }

  def andNot(set: BitSet): Unit = {
    if (bits.length != 0) {
      val bsBits = set.bits

      val minLength = Math.min(bits.length, set.bits.length)

      for (i <- 0 until minLength)
        bits(i) &= ~bsBits(i)
    }
  }

  override def hashCode(): Int = {
    var x: Long = 1234L
    var i: Int = 0

    while (i < bits.length) {
      x ^= toUnsignedLong(bits(i)) * toUnsignedLong(i + 1)
      i += 1
    }

    ((x >> 32) ^ x).toInt
  }

  def size(): Int = bits.length << AddressBitsPerWord

  /** If one of the BitSets is larger than the other, check to see if
   *  any of its extra bits are set. If so return false.
   */
  private def equalsImpl(other: BitSet): Boolean = {
    // scalastyle:off return
    val length1 = bits.length
    val length2 = other.bits.length

    val smallerBS: BitSet = if (length1 <= length2) this else other
    val smallerLength: Int = if (length1 <= length2) length1 else length2

    val largerBS: BitSet = if (length1 > length2) this else other
    val largerLength: Int = if (length1 > length2) length1 else length2

    var i: Int = 0
    while (i < smallerLength) {
      if (smallerBS.bits(i) != largerBS.bits(i))
        return false

      i += 1
    }

    // Check remainder bits, if they are zero these are equal
    while (i < largerLength) {
      if (largerBS.bits(i) != 0)
        return false

      i += 1
    }
    // scalastyle:on return

    true
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case bs: BitSet => equalsImpl(bs)
      case _          => false
    }
  }

  override def clone(): AnyRef =
    new BitSet(bits.clone())

  override def toString(): String = {
    var result: String = "{"
    var comma: Boolean = false

    for {
      i <- 0 until getActualArrayLength()
      j <- 0 until ElementSize
    } {
      if ((bits(i) & (1 << j)) != 0) {
        if (comma)
          result += ", "
        else
          comma = true
        result += (i << AddressBitsPerWord) + j
      }
    }

    result += "}"
    result
  }

  final private def ensureLength(len: Int): Unit = {
    if (len > bits.length)
      bits = Arrays.copyOf(bits, Math.max(len, bits.length * 2))
  }

  final private def getActualArrayLength(): Int = {
    var idx = bits.length - 1
    while (idx >= 0 && bits(idx) == 0)
      idx -= 1

    idx + 1
  }

  private def checkToAndFromIndex(fromIndex: Int, toIndex: Int): Unit = {
    if (fromIndex < 0)
      throw new IndexOutOfBoundsException(s"fromIndex < 0: $fromIndex")

    if (toIndex < 0)
      throw new IndexOutOfBoundsException(s"toIndex < 0: $toIndex")

    if (toIndex < fromIndex)
      throw new IndexOutOfBoundsException(s"fromIndex: $fromIndex > toIndex: $toIndex")
  }

  private def checkFromIndex(fromIndex: Int): Unit = {
    if (fromIndex < 0)
      throw new IndexOutOfBoundsException(s"fromIndex < 0: $fromIndex")
  }

  private def checkBitIndex(bitIndex: Int): Unit = {
    if (bitIndex < 0)
      throw new IndexOutOfBoundsException(s"bitIndex < 0: $bitIndex")
  }
}
