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

import java.io.ObjectInputStream
import java.io.Serializable
import java.nio.{ByteBuffer, LongBuffer}
import java.util
import java.util.ScalaOps.IntScalaOps

private object BitSet {
  private final val AddressBitsPerWord = 5 // Int Based 2^5 = 32
  private val ElementSize: Int = 1 << AddressBitsPerWord
  private val RightBits: Int = ElementSize - 1
  private def toIntBit(idx: Int): Int = 1 << idx

  def valueOf(longs: Array[Long]): util.BitSet = {
    // scalastyle:off return
    val bs = new util.BitSet

    if (longs.length == 0)
      return bs

    for (i <- 0 to longs.length * 64) {
      val idx = i / 64
      if (idx < longs.length && ((longs(idx) & (1L << (i % 64))) != 0))
        bs.set(i)
    }

    bs
    // scalastyle:on return
  }

  def valueOf(lb: LongBuffer): BitSet = valueOf(lb.array())

  def valueOf(bytes: Array[Byte]): BitSet = {
    // scalastyle:off return
    val bs = new BitSet

    if (bytes.length == 0)
      return bs

    for (i <- 0 to bytes.length * 8) {
      val idx = i / 8
      if (idx < bytes.length && ((bytes(idx) & (1 << (i % 8))) != 0))
        bs.set(i)
    }

    bs
    // scalastyle:on return
  }

  def valueOf(bb: ByteBuffer): BitSet = valueOf(bb.array())
}

@SerialVersionUID(7997698588986878753L)
class BitSet private (private var bits: Array[Int], private var needClear: Boolean,
    private var actualArrayLength: Int, private var isLengthActual: Boolean) extends Serializable with Cloneable {

  def this(nbits: Int) = {
    this(
      bits = {
        if (nbits < 0)
          throw new NegativeArraySizeException

        val length: Int = (nbits >> BitSet.AddressBitsPerWord) + {
          if ((nbits & BitSet.RightBits) > 0) 1
          else 0
        }

        new Array[Int](length)
      },
      needClear = false,
      actualArrayLength = 0,
      isLengthActual = false
    )
  }

  def this() = {
    this(64)
  }

  private def this(bits: Array[Int]) = {
    this(bits, false, bits.length, true)
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
          array(i / 64) |= 1L << (i.toLong % 64L)
      }

      array
    }
  }

  def flip(bitIndex: Int): Unit = {
    checkBitIndex(bitIndex)

    val len = (bitIndex >> BitSet.AddressBitsPerWord) + 1

    if (len > bits.length)
      growLength(len)

    bits(len - 1) ^= BitSet.toIntBit(bitIndex & BitSet.RightBits)

    if (len > actualArrayLength)
      actualArrayLength = len

    isLengthActual = !((actualArrayLength > 0) && (bits(actualArrayLength - 1) == 0))
    needClear = true
  }

  def flip(fromIndex: Int, toIndex: Int): Unit = {
    checkToAndFromIndex(fromIndex, toIndex)

    if (fromIndex != toIndex) {
      val len2 = ((toIndex - 1) >> BitSet.AddressBitsPerWord) + 1
      if (len2 > bits.length) growLength(len2)
      val idx1 = fromIndex >> BitSet.AddressBitsPerWord
      val idx2 = (toIndex - 1) >> BitSet.AddressBitsPerWord
      val factor1 = (~0) << (fromIndex & BitSet.RightBits)
      val factor2 = (~0) >>> (BitSet.ElementSize - (toIndex & BitSet.RightBits))

      if (idx1 == idx2) {
        bits(idx1) ^= (factor1 & factor2)
      } else {
        bits(idx1) ^= factor1
        bits(idx2) ^= factor2
        for (i <- idx1 + 1 until idx2)
          bits(i) ^= (~0)
      }

      if (len2 > actualArrayLength)
        actualArrayLength = len2

      isLengthActual = !((actualArrayLength > 0) && (bits(actualArrayLength - 1) == 0))
      needClear = true
    }
  }

  def set(bitIndex: Int): Unit = {
    checkBitIndex(bitIndex)

    val len = (bitIndex >> BitSet.AddressBitsPerWord) + 1

    if (len > bits.length)
      growLength(len)

    bits(len - 1) |= BitSet.toIntBit(bitIndex & BitSet.RightBits)

    if (len > actualArrayLength) {
      actualArrayLength = len
      isLengthActual = true
    }

    needClear = true
  }

  def set(bitIndex: Int, value: Boolean): Unit =
    if (value) set(bitIndex)
    else clear(bitIndex)

  // fromIndex is inclusive, toIndex is exclusive
  def set(fromIndex: Int, toIndex: Int): Unit = {
    checkToAndFromIndex(fromIndex, toIndex)

    if (fromIndex != toIndex) {
      val len2 = ((toIndex - 1) >> BitSet.AddressBitsPerWord) + 1

      if (len2 > bits.length)
        growLength(len2)

      val idx1 = fromIndex >> BitSet.AddressBitsPerWord
      val idx2 = (toIndex - 1) >> BitSet.AddressBitsPerWord
      val factor1 = (~0) << (fromIndex & BitSet.RightBits)
      val factor2 = (~0) >>> (BitSet.ElementSize - (toIndex & BitSet.RightBits))

      if (idx1 == idx2) {
        bits(idx1) |= (factor1 & factor2)
      } else {
        bits(idx1) |= factor1
        bits(idx2) |= factor2

        for (i <- idx1 + 1 until idx2)
          bits(i) |= (~0)
      }

      if (idx2 + 1 > actualArrayLength) {
        actualArrayLength = idx2 + 1
        isLengthActual = true
      }

      needClear = true
    }
  }

  def set(fromIndex: Int, toIndex: Int, value: Boolean): Unit =
    if (value) set(fromIndex, toIndex)
    else clear(fromIndex, toIndex)

  def clear(bitIndex: Int): Unit = {
    checkBitIndex(bitIndex)

    if (needClear) {
      val arrayPos = bitIndex >> BitSet.AddressBitsPerWord

      if (arrayPos < actualArrayLength) {
        bits(arrayPos) &= ~BitSet.toIntBit(bitIndex & BitSet.RightBits)

        if (bits(actualArrayLength - 1) == 0)
          isLengthActual = false
      }
    }
  }

  def clear(fromIndex: Int, toIndex: Int): Unit = {
    // scalastyle:off return
    checkToAndFromIndex(fromIndex, toIndex)

    if (!needClear)
      return

    val last = actualArrayLength << BitSet.AddressBitsPerWord
    if (fromIndex >= last || fromIndex == toIndex)
      return

    val toIndexOrLast =
      if (toIndex > last) last
      else toIndex

    val idx1 = fromIndex >> BitSet.AddressBitsPerWord
    val idx2 = (toIndexOrLast - 1) >> BitSet.AddressBitsPerWord
    val factor1 = (~0) << (fromIndex & BitSet.RightBits)
    val factor2 = (~0) >>> (BitSet.ElementSize - (toIndexOrLast & BitSet.RightBits))

    if (idx1 == idx2) {
      bits(idx1) &= ~(factor1 & factor2)
    } else {
      bits(idx1) &= ~factor1
      bits(idx2) &= ~factor2

      for (i <- idx1 + 1 until idx2)
        bits(i) = 0
    }

    if ((actualArrayLength > 0) && (bits(actualArrayLength - 1) == 0))
      isLengthActual = false

    // scalastyle:on return
  }

  def clear(): Unit = {
    if (needClear) {
      for (i <- 0 until bits.length)
        bits(i) = 0

      actualArrayLength = 0
      isLengthActual = true
      needClear = false
    }
  }

  def get(bitIndex: Int): Boolean = {
    checkBitIndex(bitIndex)

    val arrayPos = bitIndex >> BitSet.AddressBitsPerWord

    if (arrayPos < actualArrayLength)
      (bits(arrayPos) & BitSet.toIntBit(bitIndex & BitSet.RightBits)) != 0
    else
      false
  }

  def get(fromIndex: Int, toIndex: Int): BitSet = {
    // scalastyle:off return
    checkToAndFromIndex(fromIndex, toIndex)

    val last = actualArrayLength << BitSet.AddressBitsPerWord
    if (fromIndex >= last || fromIndex == toIndex)
      return new BitSet(0)

    val toIndexOrLast =
      if (toIndex > last) last
      else toIndex

    val idx1 = fromIndex >> BitSet.AddressBitsPerWord
    val idx2 = (toIndexOrLast - 1) >> BitSet.AddressBitsPerWord
    val factor1 = (~0) << (fromIndex & BitSet.RightBits)
    val factor2 = (~0) >>> (BitSet.ElementSize - (toIndexOrLast & BitSet.RightBits))

    if (idx1 == idx2) {
      val result = (bits(idx1) & (factor1 & factor2)) >>> (fromIndex % BitSet.ElementSize)
      if (result == 0)
        return new BitSet(0)

      new BitSet(Array[Int](result), needClear, 1, true)
    } else {
      val newbits = new Array[Int](idx2 - idx1 + 1)
      // first fill in the first and last indexes in the new bitset
      newbits(0) = bits(idx1) & factor1
      newbits(newbits.length - 1) = bits(idx2) & factor2
      // fill in the in between elements of the new bitset
      for (i <- 1 until idx2 - idx1)
        newbits(i) = bits(idx1 + i)

      // shift all the elements in the new bitset to the right by pos1 % ELM_SIZE
      val numBitsToShift = fromIndex & BitSet.RightBits
      var actualLen = newbits.length

      if (numBitsToShift != 0) {
        for (i <- 0 until newbits.length) {
          // shift the current element to the right regardless of sign
          newbits(i) = newbits(i) >>> numBitsToShift
          // apply the last x bits of newbits[i+1] to the current
          // element
          if (i != newbits.length - 1)
            newbits(i) |= newbits(i + 1) << (BitSet.ElementSize - numBitsToShift)

          if (newbits(i) != 0)
            actualLen = i + 1
        }
      }

      new BitSet(newbits, needClear, actualLen, newbits(actualLen - 1) != 0)
    }
    // scalastyle:on return
  }

  def nextSetBit(fromIndex: Int): Int = {
    // scalastyle:off return
    checkFromIndex(fromIndex)

    if (fromIndex >= (actualArrayLength << BitSet.AddressBitsPerWord))
      return -1

    var idx = fromIndex >> BitSet.AddressBitsPerWord

    // first check in the same bit set element
    if (bits(idx) != 0) {
      var j = fromIndex & BitSet.RightBits
      while (j < BitSet.ElementSize) {
        if ((bits(idx) & (BitSet.toIntBit(j))) != 0)
          return (idx << BitSet.AddressBitsPerWord) + j
        j += 1
      }
    }

    idx += 1

    while (idx < actualArrayLength && bits(idx) == 0)
      idx += 1

    if (idx == actualArrayLength)
      return -1

    // we know for sure there is a bit set to true in this element
    // since the bitset value is not 0
    var j = 0
    while (j < BitSet.ElementSize) {
      if ((bits(idx) & BitSet.toIntBit(j)) != 0)
        return (idx << BitSet.AddressBitsPerWord) + j
      j += 1
    }

    -1
    // scalastyle:on return
  }

  def nextClearBit(fromIndex: Int): Int = {
    // scalastyle:off return
    checkFromIndex(fromIndex)

    val length = actualArrayLength
    val bssize = length << BitSet.AddressBitsPerWord

    if (fromIndex >= bssize)
      return fromIndex

    var idx = fromIndex >> BitSet.AddressBitsPerWord

    if (bits(idx) != (~0)) {
      var j = fromIndex % BitSet.ElementSize
      while (j < BitSet.ElementSize) {
        if ((bits(idx) & (BitSet.toIntBit(j))) == 0)
          return idx * BitSet.ElementSize + j
        j += 1
      }
    }

    idx += 1

    while (idx < length && bits(idx) == (~0))
      idx += 1

    if (idx == length)
      return bssize

    var j = 0
    while (j < BitSet.ElementSize) {
      if ((bits(idx) & BitSet.toIntBit(j)) == 0)
        return (idx << BitSet.AddressBitsPerWord) + j
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

    val bssize = actualArrayLength << BitSet.AddressBitsPerWord
    var idx = Math.min(actualArrayLength, fromIndex >> BitSet.AddressBitsPerWord)

    if (bits(idx) != 0) {
      if (idx == bssize)
        return idx

      var j: Int = fromIndex % BitSet.ElementSize
      while (j >= 0) {
        if ((bits(idx) & BitSet.toIntBit(j)) != 0)
          return idx * BitSet.ElementSize + j

        j -= 1
      }
    }

    idx -= 1

    while (idx >= 0 && bits(idx) == 0)
      idx -= 1

    if (idx == -1)
      return -1

    var j: Int = BitSet.ElementSize - 1
    while (j >= 0) {
      if ((bits(idx) & BitSet.toIntBit(j)) != 0)
        return (idx << BitSet.AddressBitsPerWord) + j

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

    val length = actualArrayLength
    val bssize = length << BitSet.AddressBitsPerWord

    if (fromIndex >= bssize)
      return fromIndex

    var idx = Math.min(actualArrayLength - 1, fromIndex >> BitSet.AddressBitsPerWord)

    if (bits(idx) != (~0)) {
      var j: Int = fromIndex % BitSet.ElementSize
      while (j >= 0) {
        if ((bits(idx) & BitSet.toIntBit(j)) == 0)
          return idx * BitSet.ElementSize + j

        j -= 1
      }
    }

    idx -= 1

    while (idx >= 0 && bits(idx) == (~0))
      idx -= 1

    if (idx == -1)
      return -1

    var j: Int = BitSet.ElementSize - 1
    while (j >= 0) {
      if ((bits(idx) & (BitSet.toIntBit(j))) == 0)
        return (idx << BitSet.AddressBitsPerWord) + j

      j -= 1
    }

    bssize
    // scalastyle:on return
  }

  def length(): Int = {
    // scalastyle:off return
    var idx = actualArrayLength - 1

    while (idx >= 0 && bits(idx) == 0)
      idx -= 1

    actualArrayLength = idx + 1

    if (idx == -1)
      return 0

    var i = BitSet.ElementSize - 1
    val v = bits(idx)

    while ((v & BitSet.toIntBit(i)) == 0 && i > 0)
      i -= 1

    (idx << BitSet.AddressBitsPerWord) + i + 1
    // scalastyle:on return
  }

  def isEmpty(): Boolean = {
    // scalastyle:off return
    if (needClear) {
      var idx = 0
      while (idx < bits.length) {
        if (bits(idx) != 0)
          return false
        idx += 1
      }
    }

    true
    // scalastyle:on return
  }

  def intersects(set: BitSet): Boolean = {
    // scalastyle:off return
    val bsBits = set.bits
    val length1 = actualArrayLength
    val length2 = set.actualArrayLength

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

    if (needClear) {
      val length = bits.length

      for (idx <- 0 until length) {
        count += bitCount(bits(idx))
      }
    }

    count
  }

  def and(set: BitSet): Unit = {
    if (needClear) {
      val bsBits = set.bits
      val length1 = actualArrayLength
      val length2 = set.actualArrayLength

      if (length1 <= length2) {
        for (i <- 0 until length1)
          bits(i) &= bsBits(i)
      } else {
        for (i <- 0 until length2)
          bits(i) &= bsBits(i)

        for (i <- length2 until length1)
          bits(i) = 0

        actualArrayLength = length2
      }

      isLengthActual = !((actualArrayLength > 0) && (bits(actualArrayLength - 1) == 0))
    }
  }

  def or(set: BitSet): Unit = {
    val bsActualLen = set.getActualArrayLength()

    if (bsActualLen > bits.length) {
      val tempBits = new Array[Int](bsActualLen)
      System.arraycopy(set.bits, 0, tempBits, 0, set.actualArrayLength)

      for (i <- 0 until actualArrayLength)
        tempBits(i) |= bits(i)

      bits = tempBits
      actualArrayLength = bsActualLen
      isLengthActual = true
    } else {
      val bsBits = set.bits

      for (i <- 0 until bsActualLen)
        bits(i) |= bsBits(i)

      if (bsActualLen > actualArrayLength) {
        actualArrayLength = bsActualLen
        isLengthActual = true
      }
    }

    needClear = true
  }

  def xor(set: BitSet): Unit = {
    val bsActualLen = set.getActualArrayLength()

    if (bsActualLen > bits.length) {
      val tempBits = new Array[Int](bsActualLen)
      System.arraycopy(set.bits, 0, tempBits, 0, set.actualArrayLength)

      for (i <- 0 until actualArrayLength)
        tempBits(i) ^= bits(i)

      bits = tempBits
      actualArrayLength = bsActualLen
      isLengthActual = !((actualArrayLength > 0) && (bits(actualArrayLength - 1) == 0))
    } else {
      val bsBits = set.bits

      for (i <- 0 until bsActualLen)
        bits(i) ^= bsBits(i)

      if (bsActualLen > actualArrayLength) {
        actualArrayLength = bsActualLen
        isLengthActual = true
      }
    }

    needClear = true
  }

  def andNot(set: BitSet): Unit = {
    if (needClear) {
      val bsBits = set.bits
      val range =
        if (actualArrayLength < set.actualArrayLength) actualArrayLength
        else set.actualArrayLength

      for (i <- 0 until range)
        bits(i) &= ~bsBits(i)

      if (actualArrayLength < range)
        actualArrayLength = range

      isLengthActual = !((actualArrayLength > 0) && (bits(actualArrayLength - 1) == 0))
    }
  }

  override def hashCode(): Int = {
    var x: Long = 1234L
    var i: Int = 0

    while (i < actualArrayLength) {
      x ^= bits(i) * (i + 1)
      i += 1
    }

    ((x >> 32) ^ x).toInt
  }

  def size(): Int = bits.length << BitSet.AddressBitsPerWord

  override def equals(obj: Any): Boolean = {
    // scalastyle:off return
    if (obj.isInstanceOf[AnyRef] && (this eq obj.asInstanceOf[AnyRef]))
      return true

    if (!obj.isInstanceOf[BitSet])
      return false

    val bs: BitSet = obj.asInstanceOf[BitSet]
    val bsBits = bs.bits
    val length1 = actualArrayLength
    val length2 = bs.actualArrayLength

    if (isLengthActual && bs.isLengthActual && length1 != length2)
      return false

    // If one of the BitSets is larger than the other, check to see if
    // any of its extra bits are set. If so return false.
    if (length1 <= length2) {
      var i: Int = 0
      while (i < length1) {
        if (bits(i) != bsBits(i))
          return false

        i += 1
      }

      i = length1
      while (i < length2) {
        if (bsBits(i) != 0)
          return false

        i += 1
      }
    } else {
      var i: Int = 0
      while (i < length2) {
        if (bits(i) != bsBits(i))
          return false

        i += 1
      }

      i = length2
      while (i < length1) {
        if (bits(i) != 0)
          return false

        i += 1
      }
    }

    true
    // scalastyle:on return
  }

  override def clone(): AnyRef = try {
    val clone = super.clone.asInstanceOf[BitSet]
    clone.bits = bits.clone
    clone
  } catch {
    case _: CloneNotSupportedException => null
  }

  override def toString(): String = {
    val sb = new StringBuilder(bits.length / 2)
    var bitCount: Int = 0
    var comma: Boolean = false
    sb.append('{')

    for (i <- 0 until bits.length) {
      if (bits(i) == 0) {
        bitCount += BitSet.ElementSize
      } else {
        for (j <- 0 until BitSet.ElementSize) {
          if ((bits(i) & (BitSet.toIntBit(j))) != 0) {
            if (comma)
              sb.append(", ")
            else
              comma = true
            sb.append(bitCount)
          }
          bitCount += 1
        }
      }
    }

    sb.append('}')
    sb.toString
  }

  final private def growLength(len: Int): Unit = {
    val tempBits = new Array[Int](Math.max(len, bits.length * 2))
    System.arraycopy(bits, 0, tempBits, 0, actualArrayLength)
    bits = tempBits
  }

  final private def getActualArrayLength(): Int = {
    if (isLengthActual) {
      actualArrayLength
    } else {
      var idx = actualArrayLength - 1
      while (idx >= 0 && bits(idx) == 0)
        idx -= 1

      actualArrayLength = idx + 1
      isLengthActual = true
      actualArrayLength
    }
  }

  private def bitCount(v: Int): Int = java.lang.Integer.bitCount(v)

  private def readObject(ois: ObjectInputStream): Unit = {
    ois.defaultReadObject()
    isLengthActual = false
    actualArrayLength = bits.length
    needClear = getActualArrayLength() != 0
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
