package java.util

import scala.scalajs.js.Math

class Random extends AnyRef with java.io.Serializable {

  // see nextGaussian()
  private var nextNextGaussian: Double = _
  private var haveNextNextGaussian: Boolean = false

  def this(seed: Long) = this() // ignore seed

  def setSeed(seed: Long): Unit = () // ignore seed

  /* This method is supposed to be overridable in subclasses to change the
   * origin of random numbers for other methods.
   * In this implementation, it is the other way around: the base method is
   * nextDouble(), because this one is offered by the ECMAScript library.
   */
  protected def next(bits: Int): Int =
    if (bits >= 32) nextInt()
    else nextInt(1 << bits)

  def nextDouble(): Double = Math.random()

  def nextBoolean(): Boolean = nextDouble() >= 0.5

  def nextInt(): Int =
    (Math.floor(nextDouble() * 4294967296.0) - 2147483648.0).toInt

  def nextInt(n: Int): Int =
    (Math.floor(nextDouble() * n)).toInt

  def nextLong(): Long = (Math.floor(nextDouble() * 18446744073709551616.0) - 9223372036854775808.0).toLong

  def nextFloat(): Float = nextDouble().toFloat

  def nextBytes(bytes: Array[Byte]): Unit = {
    var i = 0
    while (i < bytes.length) {
      bytes(i) = nextInt(256).toByte
      i += 1
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

    val c = Math.sqrt(-2*Math.log(rds)/rds)

    // Save y*c for next time
    nextNextGaussian = y*c
    haveNextNextGaussian = true

    // And return x*c
    x*c
  }
}
