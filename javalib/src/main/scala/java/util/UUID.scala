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

import scala.scalajs.js

final class UUID private (
    private val i1: Int, private val i2: Int,
    private val i3: Int, private val i4: Int)
    extends AnyRef with java.io.Serializable with Comparable[UUID] {

  import UUID._

  /* Most significant long:
   *
   *  0xFFFFFFFF00000000 time_low
   *  0x00000000FFFF0000 time_mid
   *  0x000000000000F000 version
   *  0x0000000000000FFF time_hi
   *
   * Least significant long:
   *
   *  0xC000000000000000 variant
   *  0x3FFF000000000000 clock_seq
   *  0x0000FFFFFFFFFFFF node
   */

  def this(mostSigBits: Long, leastSigBits: Long) = {
    this((mostSigBits >>> 32).toInt, mostSigBits.toInt,
        (leastSigBits >>> 32).toInt, leastSigBits.toInt)
  }

  @inline
  def getLeastSignificantBits(): Long =
    (i3.toLong << 32) | (i4.toLong & 0xffffffffL)

  @inline
  def getMostSignificantBits(): Long =
    (i1.toLong << 32) | (i2.toLong & 0xffffffffL)

  def version(): Int =
    (i2 & 0xf000) >> 12

  def variant(): Int = {
    if ((i3 & 0x80000000) == 0) {
      // MSB0 not set: NCS backwards compatibility variant
      0
    } else if ((i3 & 0x40000000) != 0) {
      // MSB1 set: either MS reserved or future reserved
      (i3 & 0xe0000000) >>> 29
    } else {
      // MSB1 not set: RFC 4122 variant
      2
    }
  }

  def timestamp(): Long = {
    if (version() != TimeBased)
      throw new UnsupportedOperationException("Not a time-based UUID")
    (((i2 >>> 16) | ((i2 & 0x0fff) << 16)).toLong << 32) | (i1.toLong & 0xffffffffL)
  }

  def clockSequence(): Int = {
    if (version() != TimeBased)
      throw new UnsupportedOperationException("Not a time-based UUID")
    (i3 & 0x3fff0000) >> 16
  }

  def node(): Long = {
    if (version() != TimeBased)
      throw new UnsupportedOperationException("Not a time-based UUID")
    ((i3 & 0xffff).toLong << 32) | (i4.toLong & 0xffffffffL)
  }

  override def toString(): String = {
    @inline def paddedHex8(i: Int): String = {
      val s = Integer.toHexString(i)
      "00000000".substring(s.length) + s
    }

    @inline def paddedHex4(i: Int): String = {
      val s = Integer.toHexString(i)
      "0000".substring(s.length) + s
    }

    paddedHex8(i1) + "-" + paddedHex4(i2 >>> 16) + "-" + paddedHex4(i2 & 0xffff) + "-" +
    paddedHex4(i3 >>> 16) + "-" + paddedHex4(i3 & 0xffff) + paddedHex8(i4)
  }

  override def hashCode(): Int =
    i1 ^ i2 ^ i3 ^ i4

  override def equals(that: Any): Boolean = that match {
    case that: UUID =>
      i1 == that.i1 && i2 == that.i2 && i3 == that.i3 && i4 == that.i4
    case _ =>
      false
  }

  def compareTo(that: UUID): Int = {
    // See #4882 and the test `UUIDTest.compareTo()` for context
    val thisHi = this.getMostSignificantBits()
    val thatHi = that.getMostSignificantBits()
    if (thisHi != thatHi) {
      if (thisHi < thatHi) -1
      else 1
    } else {
      val thisLo = this.getLeastSignificantBits()
      val thatLo = that.getLeastSignificantBits()
      if (thisLo != thatLo) {
        if (thisLo < thatLo) -1
        else 1
      } else {
        0
      }
    }
  }
}

object UUID {
  private final val TimeBased = 1
  private final val DCESecurity = 2
  private final val NameBased = 3
  private final val Random = 4

  // Typed as `Random` so that the IR typechecks when SecureRandom is not available
  private lazy val csprng: Random = new java.security.SecureRandom()
  private lazy val randomUUIDBuffer: Array[Byte] = new Array[Byte](16)

  def randomUUID(): UUID = {
    val buffer = randomUUIDBuffer // local copy

    /* We use nextBytes() because that is the primitive of most secure RNGs,
     * and therefore it allows to perform a unique call to the underlying
     * secure RNG.
     */
    csprng.nextBytes(randomUUIDBuffer)

    @inline def intFromBuffer(i: Int): Int =
      (buffer(i) << 24) | ((buffer(i + 1) & 0xff) << 16) | ((buffer(i + 2) & 0xff) << 8) | (buffer(i + 3) & 0xff)

    val i1 = intFromBuffer(0)
    val i2 = (intFromBuffer(4) & ~0x0000f000) | 0x00004000
    val i3 = (intFromBuffer(8) & ~0xc0000000) | 0x80000000
    val i4 = intFromBuffer(12)
    new UUID(i1, i2, i3, i4)
  }

  // Not implemented (requires messing with MD5 or SHA-1):
  //def nameUUIDFromBytes(name: Array[Byte]): UUID = ???

  def fromString(name: String): UUID = {
    import Integer.parseInt

    def fail(): Nothing =
      throw new IllegalArgumentException("Invalid UUID string: "+name)

    @inline def parseHex8(his: String, los: String): Int =
      (parseInt(his, 16) << 16) | parseInt(los, 16)

    if (name.length != 36 || name.charAt(8) != '-' ||
        name.charAt(13) != '-' || name.charAt(18) != '-' || name.charAt(23) != '-')
      fail()

    try {
      val i1 = parseHex8(name.substring(0, 4), name.substring(4, 8))
      val i2 = parseHex8(name.substring(9, 13), name.substring(14, 18))
      val i3 = parseHex8(name.substring(19, 23), name.substring(24, 28))
      val i4 = parseHex8(name.substring(28, 32), name.substring(32, 36))
      new UUID(i1, i2, i3, i4)
    } catch {
      case _: NumberFormatException => fail()
    }
  }
}
