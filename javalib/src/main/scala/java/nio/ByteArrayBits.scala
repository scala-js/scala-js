package java.nio

private[nio] object ByteArrayBits {
  def apply(array: Array[Byte], arrayOffset: Int,
      isBigEndian: Boolean, indexMultiplier: Int = 1): ByteArrayBits =
    new ByteArrayBits(array, arrayOffset, isBigEndian, indexMultiplier)
}

@inline
private[nio] final class ByteArrayBits(
    array: Array[Byte], arrayOffset: Int, isBigEndian: Boolean,
    indexMultiplier: Int) {

  /* We use tuples of bytes instead of, say, arrays, because they can be
   * completely stack-allocated.
   *
   * When used in a place where it can be stack-allocated, an "instance" of
   * this class has zero overhead.
   */

  // API

  def loadChar(index: Int): Char = makeChar(load2Bytes(index))
  def loadShort(index: Int): Short = makeShort(load2Bytes(index))
  def loadInt(index: Int): Int = makeInt(load4Bytes(index))
  def loadLong(index: Int): Long = makeLong(load8Bytes(index))
  def loadFloat(index: Int): Float = makeFloat(load4Bytes(index))
  def loadDouble(index: Int): Double = makeDouble(load8Bytes(index))

  def storeChar(index: Int, v: Char): Unit = store2Bytes(index, unmakeChar(v))
  def storeShort(index: Int, v: Short): Unit = store2Bytes(index, unmakeShort(v))
  def storeInt(index: Int, v: Int): Unit = store4Bytes(index, unmakeInt(v))
  def storeLong(index: Int, v: Long): Unit = store8Bytes(index, unmakeLong(v))
  def storeFloat(index: Int, v: Float): Unit = store4Bytes(index, unmakeFloat(v))
  def storeDouble(index: Int, v: Double): Unit = store8Bytes(index, unmakeDouble(v))

  // Making and unmaking values

  @inline
  private def makeChar(bs: (Byte, Byte)): Char =
    makeChar(bs._1, bs._2)

  @inline
  private def makeChar(b0: Byte, b1: Byte): Char =
    if (isBigEndian) makeCharBE(b0, b1)
    else             makeCharBE(b1, b0)

  @inline
  private def makeCharBE(b0: Byte, b1: Byte): Char =
    ((b0 << 8) | (b1 & 0xff)).toChar

  @inline
  private def makeShort(bs: (Byte, Byte)): Short =
    makeShort(bs._1, bs._2)

  @inline
  private def makeShort(b0: Byte, b1: Byte): Short =
    if (isBigEndian) makeShortBE(b0, b1)
    else             makeShortBE(b1, b0)

  @inline
  private def makeShortBE(b0: Byte, b1: Byte): Short =
    ((b0 << 8) | (b1 & 0xff)).toShort

  @inline
  private def makeInt(bs: (Byte, Byte, Byte, Byte)): Int =
    makeInt(bs._1, bs._2, bs._3, bs._4)

  @inline
  private def makeInt(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Int =
    if (isBigEndian) makeIntBE(b0, b1, b2, b3)
    else             makeIntBE(b3, b2, b1, b0)

  @inline
  private def makeIntBE(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Int =
    ((b0 << 24) | ((b1 & 0xff) << 16) | ((b2 & 0xff) << 8) | (b3 & 0xff))

  @inline
  private def makeLong(
      bs: (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte)): Long =
    makeLong(bs._1, bs._2, bs._3, bs._4, bs._5, bs._6, bs._7, bs._8)

  @inline
  private def makeLong(
      b0: Byte, b1: Byte, b2: Byte, b3: Byte,
      b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long =
    if (isBigEndian) makeLongBE(b0, b1, b2, b3, b4, b5, b6, b7)
    else             makeLongBE(b7, b6, b5, b4, b3, b2, b1, b0)

  @inline
  private def makeLongBE(
      b0: Byte, b1: Byte, b2: Byte, b3: Byte,
      b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long = {
    (makeIntBE(b0, b1, b2, b3).toLong << 32) |
      (makeIntBE(b4, b5, b6, b7).toLong & 0xffffffffL)
  }

  @inline
  private def makeFloat(bs: (Byte, Byte, Byte, Byte)): Float =
    makeFloat(bs._1, bs._2, bs._3, bs._4)

  @inline
  private def makeFloat(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float =
    java.lang.Float.intBitsToFloat(makeInt(b0, b1, b2, b3))

  @inline
  private def makeDouble(
      bs: (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte)): Double =
    makeDouble(bs._1, bs._2, bs._3, bs._4, bs._5, bs._6, bs._7, bs._8)

  @inline
  private def makeDouble(
      b0: Byte, b1: Byte, b2: Byte, b3: Byte,
      b4: Byte, b5: Byte, b6: Byte, b7: Byte): Double =
    java.lang.Double.longBitsToDouble(makeLong(b0, b1, b2, b3, b4, b5, b6, b7))

  @inline
  private def unmakeChar(c: Char): (Byte, Byte) = {
    val bs = unmakeCharBE(c)
    if (isBigEndian) bs
    else             (bs._2, bs._1)
  }

  @inline
  private def unmakeCharBE(c: Char): (Byte, Byte) =
    ((c >> 8).toByte, c.toByte)

  @inline
  private def unmakeShort(s: Short): (Byte, Byte) = {
    val bs = unmakeShortBE(s)
    if (isBigEndian) bs
    else             (bs._2, bs._1)
  }

  @inline
  private def unmakeShortBE(s: Short): (Byte, Byte) =
    ((s >> 8).toByte, s.toByte)

  @inline
  private def unmakeInt(i: Int): (Byte, Byte, Byte, Byte) = {
    val bs = unmakeIntBE(i)
    if (isBigEndian) bs
    else             (bs._4, bs._3, bs._2, bs._1)
  }

  @inline
  private def unmakeIntBE(i: Int): (Byte, Byte, Byte, Byte) =
    ((i >> 24).toByte, (i >> 16).toByte, (i >> 8).toByte, i.toByte)

  @inline
  private def unmakeLong(
      l: Long): (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte) = {
    val bs0 = unmakeIntBE((l >>> 32).toInt)
    val bs1 = unmakeIntBE(l.toInt)
    if (isBigEndian) (bs0._1, bs0._2, bs0._3, bs0._4, bs1._1, bs1._2, bs1._3, bs1._4)
    else             (bs1._4, bs1._3, bs1._2, bs1._1, bs0._4, bs0._3, bs0._2, bs0._1)
  }

  @inline
  private def unmakeFloat(f: Float): (Byte, Byte, Byte, Byte) =
    unmakeInt(java.lang.Float.floatToIntBits(f))

  @inline
  private def unmakeDouble(
      d: Double): (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte) =
    unmakeLong(java.lang.Double.doubleToLongBits(d))

  // Loading and storing bytes

  @inline
  private def load2Bytes(index: Int): (Byte, Byte) = {
    val idx = indexMultiplier*index + arrayOffset
    (array(idx), array(idx + 1))
  }

  @inline
  private def load4Bytes(index: Int): (Byte, Byte, Byte, Byte) = {
    val idx = indexMultiplier*index + arrayOffset
    (array(idx), array(idx + 1), array(idx + 2), array(idx + 3))
  }

  @inline
  private def load8Bytes(
      index: Int): (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte) = {
    val idx = indexMultiplier*index + arrayOffset
    (array(idx), array(idx + 1), array(idx + 2), array(idx + 3),
        array(idx + 4), array(idx + 5), array(idx + 6), array(idx + 7))
  }

  @inline
  private def store2Bytes(index: Int, bs: (Byte, Byte)): Unit = {
    val idx = indexMultiplier*index + arrayOffset
    array(idx) = bs._1
    array(idx + 1) = bs._2
  }

  @inline
  private def store4Bytes(index: Int, bs: (Byte, Byte, Byte, Byte)): Unit = {
    val idx = indexMultiplier*index + arrayOffset
    array(idx) = bs._1
    array(idx + 1) = bs._2
    array(idx + 2) = bs._3
    array(idx + 3) = bs._4
  }

  @inline
  private def store8Bytes(index: Int,
      bs: (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte)): Unit = {
    val idx = indexMultiplier*index + arrayOffset
    array(idx) = bs._1
    array(idx + 1) = bs._2
    array(idx + 2) = bs._3
    array(idx + 3) = bs._4
    array(idx + 4) = bs._5
    array(idx + 5) = bs._6
    array(idx + 6) = bs._7
    array(idx + 7) = bs._8
  }
}
