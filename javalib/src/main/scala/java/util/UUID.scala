package java.util

/* Scala implementation of Java UUID.
 * Jean Luc Chasseriau <jeanluc.chasseriau@crossing-tech.com> */

final class UUID(private val highBits: Long, private val lowBits: Long) extends Object with Serializable with Comparable[UUID] {

  private def this(data: Array[Byte]) = this(
      data.take(8).foldLeft(0: Long)((acc, b) => (acc << 8) | (b & 0xff)), // high
      data.drop(8).foldLeft(0: Long)((acc, b) => (acc << 8) | (b & 0xff))  // low
  )

  def getLeastSignificantBits(): Long = lowBits

  def getMostSignificantBits(): Long = highBits

  def version(): Int = ((highBits >> 12) & 0x0f).toInt

  def variant(): Int = (((lowBits >>> (64 - (lowBits >>> 62))) & (lowBits >> 63))).toInt

  def timestamp(): Long = {
    if (version() != 1)
      throw new UnsupportedOperationException("This UUID does not support timestamp")

    (highBits & 0x0FFFL) << 48 | ((highBits >> 16) & 0x0FFFL) << 32 | highBits >>> 32
  }

  def clockSequence(): Int = {
    if (version() != 1)
      throw new UnsupportedOperationException("This UUID does not support clock sequence")

    (((lowBits & 0x3FFF000000000000L) >>> 48)).toInt
  }

  def node(): Long = {
    if (version() != 1)
      throw new UnsupportedOperationException("This UUID does not support node")

    lowBits & 0x0000FFFFFFFFFFFFL
  }

  override def toString(): String = {
    val timeLow = (highBits >> 32) & 0xffffffffL
    val timeMid = (highBits >> 16) & 0xffff
    val timeHighVersion = highBits & 0xffff
    val variantSeq = (lowBits >> 48) & 0xffff
    val node = lowBits & 0xffffffffffffL

    "%08x".format(timeLow) + "-" +
    "%04x".format(timeMid) + "-" +
    "%04x".format(timeHighVersion) + "-" +
    "%04x".format(variantSeq) + "-" +
    "%012x".format(node)
  }

  override def hashCode: Int = {
    val x = highBits ^ lowBits
    (x >> 32).toInt ^ x.toInt
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: UUID => (this.highBits == that.highBits) && (this.lowBits == that.lowBits)
      case _ => false
    }
  }

  override def compareTo(that: UUID): Int = {
    if (this.highBits < that.highBits) -1
    else if (this.highBits > that.highBits) 1
    else if (this.lowBits < that.lowBits) -1
    else if (this.lowBits > that.lowBits) 1
    else 0
  }
}

object UUID {

  private val random = new Random

  //def nameUUIDFromBytes(name: Array[Byte]): UUID = ???

  def randomUUID(): UUID = {
    var high = random.nextLong
    var low = random.nextLong

    high &= 0xffffffffffff0fffL // clear version
    high |= 0x4000              // version 4
    low  &= 0x3fffffffffffffffL // clear variant
    low  |= 0x8000000000000000L // IETF variant

    new UUID(high, low)
  }

  def fromString(s: String): UUID = {
    val chars = s.replace("-", "")
    assert(chars.length == 32, "UUID string must be composed of 32 hexadecimal characters")

    val charPairs = s.replace("-", "").sliding(2, 2).toList
    val bytes = charPairs.map(cc => Character.digit(cc(0), 16) << 4 | Character.digit(cc(1), 16))

    val high = bytes.take(8).foldLeft(0: Long)((acc, i) => (acc << 8) | (i & 0xff))
    val low = bytes.drop(8).foldLeft(0: Long)((acc, i) => (acc << 8) | (i & 0xff))

    new UUID(high, low)
  }
}
