package scala.runtime

/** Not for public consumption.  Usage by the runtime only.
 */

object Statics {
  def mix(hash: Int, data: Int): Int = {
    var h = mixLast(hash, data)
    h = Integer.rotateLeft(h, 13)
    (((h * 5) | 0) + 0xe6546b64) | 0
  }

  def mixLast(hash: Int, data: Int): Int = {
    var k = data
    k = (k * 0xcc9e2d51) | 0
    k = Integer.rotateLeft(k, 15)
    k = (k * 0x1b873593) | 0
    hash ^ k
  }

  def finalizeHash(hash: Int, length: Int): Int = {
    avalanche(hash ^ length)
  }

  /** Force all bits of the hash to avalanche. Used for finalizing the hash. */
  def avalanche(h0: Int): Int = {
    var h = h0
    h ^= h >>> 16
    h = (h * 0x85ebca6b) | 0
    h ^= h >>> 13
    h = (h * 0xc2b2ae35) | 0
    h ^= h >>> 16
    h
  }

  def longHash(lv: Long): Int = {
    (lv | 0).toInt
    /*
    val iv = lv.toInt | 0
    if (iv == lv) iv
    else (lv ^ (lv >>> 32)).toInt | 0
    */
  }

  def doubleHash(dv: Double): Int = {
    dv.toInt
    /*
    val iv = dv.toInt | 0
    if (iv == dv)
      return iv

    val fv = dv.toFloat
    if (fv == dv)
      return java.lang.Float.floatToIntBits(fv)

    val lv = dv.toLong
    if (lv == dv)
      return lv.toInt | 0

    val lv2 == java.lang.Double.doubleToLongBits(dv)
    return (lv2 ^ (lv2 >>> 32)).toInt | 0
    */
  }

  def floatHash(fv: Float): Int = {
    fv.toInt
    /*
    val iv = fv.toInt
    if (iv == fv)
      return iv

    val lv = fv.toLong
    if (lv == fv)
      return (lv ^ (lv >>> 32)).toInt | 0

    return java.lang.Float.floatToIntBits(fv)
    */
  }

  def anyHash(x: Any): Int = {
    x match {
      case null => 0
      case x: Long => longHash(x)
      case x: Double => doubleHash(x)
      case x: Float => floatHash(x)
      case _ => x.hashCode()
    }
  }
}
