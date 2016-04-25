package scala.runtime

/** Not for public consumption.  Usage by the runtime only.
 */

object Statics {
  def mix(hash: Int, data: Int): Int = {
    var h = mixLast(hash, data)
    h = Integer.rotateLeft(h, 13)
    (h * 5) + 0xe6546b64
  }

  def mixLast(hash: Int, data: Int): Int = {
    var k = data
    k *= 0xcc9e2d51
    k = Integer.rotateLeft(k, 15)
    k *= 0x1b873593
    hash ^ k
  }

  def finalizeHash(hash: Int, length: Int): Int = {
    avalanche(hash ^ length)
  }

  /** Force all bits of the hash to avalanche. Used for finalizing the hash. */
  def avalanche(h0: Int): Int = {
    var h = h0
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    h
  }

  def longHash(lv: Long): Int = {
    val lo = lv.toInt
    val hi = (lv >>> 32).toInt
    if (hi == (lo >> 31)) lo // it is in the Int range
    else lo ^ hi
  }

  def doubleHash(dv: Double): Int = {
    /* This implementation is based on what 2.12.0-M5+ does on the JVM.
     * The 2.10/2.11 implementation on the JVM was not consistent with that of
     * BoxesRunTime, and most importantly was not consistent with the hash of
     * Long values.
     *
     * In Scala.js, we always use the version consistent with BoxesRunTime.
     * Note that, for values that happen to be valid floats but not valid
     * longs, this implementation is *not* consistent with the JVM (just like
     * that of BoxesRunTime).
     */
    val iv = dv.toInt
    if (iv == dv) {
      iv
    } else {
      // don't test the case dv.toFloat == dv
      val lv = dv.toLong
      if (lv == dv)
        lv.hashCode()
      else
        dv.hashCode()
    }
  }

  def floatHash(fv: Float): Int = {
    doubleHash(fv.toDouble)
  }

  def anyHash(x: Any): Int = {
    x match {
      case null      => 0
      case x: Double => doubleHash(x)
      case x: Long   => longHash(x)
      case _         => x.hashCode()
    }
  }
}
