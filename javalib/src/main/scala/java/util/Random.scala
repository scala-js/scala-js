package java.util

import scala.scalajs.js

import scala.annotation.tailrec

class Random(seed_in: Long) extends AnyRef with java.io.Serializable {

  private var seed: Long = _

  // see nextGaussian()
  private var nextNextGaussian: Double = _
  private var haveNextNextGaussian: Boolean = false

  setSeed(seed_in)

  def this() = this(Random.randomSeed())

  def setSeed(seed_in: Long): Unit = {
    seed = (seed_in ^ 0x5DEECE66DL) & ((1L << 48) - 1)
    haveNextNextGaussian = false
  }

  protected def next(bits: Int): Int = {
    seed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    (seed >>> (48 - bits)).toInt
  }

  def nextDouble(): Double =
    ((next(26).toLong << 27) + next(27)) / (1L << 53).toDouble

  def nextBoolean(): Boolean = next(1) != 0

  def nextInt(): Int = next(32)

  def nextInt(n: Int): Int = {
    if (n <= 0)
      throw new IllegalArgumentException("n must be positive");

    if ((n & -n) == n)  // i.e., n is a power of 2
      ((n * next(31).toLong) >> 31).toInt
    else {
      @tailrec
      def loop(): Int = {
        val bits = next(31)
        val value = bits % n
        if (bits - value + (n-1) < 0) loop()
        else value
      }

      loop()
    }
  }

  def nextLong(): Long = (next(32).toLong << 32) + next(32)

  def nextFloat(): Float = next(24) / (1 << 24).toFloat

  def nextBytes(bytes: Array[Byte]): Unit = {
    var i = 0
    while (i < bytes.length) {
      var rnd = nextInt()
      var n = js.Math.min(bytes.length - i, 4)
      while (n > 0) {
        bytes(i) = rnd.toByte
        rnd >>= 8
        n -= 1
        i += 1
      }
    }
  }

  def nextGaussian(): Double = {
    // See http://www.protonfish.com/jslib/boxmuller.shtml

    /* The Box-Muller algorithm produces two random numbers at once. We save
     * the second one in `nextNextGaussian` to be used by the next call to
     * nextGaussian().
     */

    if (haveNextNextGaussian) {
      haveNextNextGaussian = false
      return nextNextGaussian
    }

    var x, y, rds: Double = 0

    /* Get two random numbers from -1 to 1.
     * If the radius is zero or greater than 1, throw them out and pick two new
     * ones.
     * Rejection sampling throws away about 20% of the pairs.
     */
    do {
      x = nextDouble()*2-1
      y = nextDouble()*2-1
      rds = x*x + y*y
    } while (rds == 0 || rds > 1)

    val c = js.Math.sqrt(-2*js.Math.log(rds)/rds)

    // Save y*c for next time
    nextNextGaussian = y*c
    haveNextNextGaussian = true

    // And return x*c
    x*c
  }
}

object Random {

  /** Generate a random long from JS RNG to seed a new Random */
  private def randomSeed(): Long =
    (randomInt().toLong << 32) | (randomInt().toLong & 0xffffffffL)

  private def randomInt(): Int =
    (js.Math.floor(js.Math.random() * 4294967296.0) - 2147483648.0).toInt

}
