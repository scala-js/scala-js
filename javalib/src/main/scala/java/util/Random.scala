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

import scala.annotation.tailrec

import scala.scalajs.LinkingInfo

import java.util.random.RandomGenerator

class Random(seed_in: Long)
    extends AnyRef with RandomGenerator with java.io.Serializable {

  /* This class has two different implementations of seeding and computing
   * bits, depending on whether we are on Wasm or JS.
   *
   * On Wasm, we use the implementation specified in the JavaDoc verbatim.
   *
   * On JS, the naive implementation is too slow, due to the use of `Long`s.
   * We use semantically equivalent formulas that better fold away.
   * We also separately store the 2x32 bits of the `Long`, in order not to
   * allocate a `RuntimeLong` when storing in the field.
   */

  private var seed: Long = _ // the full seed on Wasm (dce'ed on JS)
  private var seedHi: Int = _ // 32 msb of the seed in JS (dce'ed on Wasm)
  private var seedLo: Int = _ // 32 lsb of the seed in JS (dce'ed on Wasm)

  // see nextGaussian()
  private var nextNextGaussian: Double = _
  private var haveNextNextGaussian: Boolean = false

  setSeed(seed_in)

  def this() = this(Random.randomSeed())

  def setSeed(seed_in: Long): Unit = {
    val seed = ((seed_in ^ 0x5DEECE66DL) & ((1L << 48) - 1)) // as documented
    if (LinkingInfo.isWebAssembly) {
      this.seed = seed
    } else {
      seedHi = (seed >>> 32).toInt
      seedLo = seed.toInt
    }
    haveNextNextGaussian = false
  }

  @noinline
  protected def next(bits: Int): Int =
    if (LinkingInfo.isWebAssembly) nextWasm(bits)
    else nextJS(bits)

  @inline
  private def nextWasm(bits: Int): Int = {
    // as documented
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    seed = newSeed
    (newSeed >>> (48 - bits)).toInt
  }

  @inline
  private def nextJS(bits: Int): Int = {
    /* Spec: seed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
     *
     * Instead we compute the new seed << 16 (where 16 = 64 - 48).
     * This is done by shifting both constants by 16 (appending 0000 at the end
     * of their hex value) and removing the & ...
     *
     * Then we compute new values of `seedHi`, `seedLo` and the result by
     * adding 16 to all the shifts.
     *
     * By doing this, the `a0` part of the multiplicative constants is `0`.
     * That allows the optimizer to constant-fold away 2 of the 6 int
     * multiplications it would normally have to do.
     */

    val oldSeed = (seedHi.toLong << 32) | Integer.toUnsignedLong(seedLo) // free
    val newSeedShift16 = 0x5DEECE66D0000L * oldSeed + 0xB0000L
    seedHi = (newSeedShift16 >>> (16 + 32)).toInt
    seedLo = (newSeedShift16 >>> 16).toInt

    /* Spec:       (newSeed >>> (48 - bits)).toInt
     * with shift: (newSeedShift16 >>> (16 + 48 - bits)).toInt
     *
     * Since 1 <= bits <= 32 (by spec of next(bits)), the shift is
     * 32 <= 64 - bits <= 63, which should result in a branchless shift inside
     * RuntimeLong. The optimizer does not know that, though, so we help it by
     * first shifting by 32 (which is free), extracting the `toInt` (also free),
     * then shifting by `32 - bits`.
     */
    (newSeedShift16 >>> 32).toInt >>> (32 - bits)
  }

  override def nextDouble(): Double = {
    // ((next(26).toLong << 27) + next(27)) / (1L << 53).toDouble
    ((next(26).toDouble * (1L << 27).toDouble) + next(27).toDouble) / (1L << 53).toDouble
  }

  override def nextBoolean(): Boolean = next(1) != 0

  override def nextInt(): Int = next(32)

  override def nextInt(n: Int): Int = {
    if (n <= 0) {
      throw new IllegalArgumentException("n must be positive")
    } else if ((n & -n) == n) { // i.e., n is a power of 2
      /* The specification is
       *   ((n * next(31).toLong) >> 31).toInt
       *   == ((2**log2(n) * next(31).toLong) >> 31).toInt
       *   == ((next(31).toLong << log2(n)) >> 31).toInt
       *   == (next(31).toLong >> (31 - log2(n))).toInt
       *   == next(31) >> (31 - log2(n))
       * For a power of 2,
       *   log2(n) == numberOfTrailingZeros(n) == 31 - numberOfLeadingZeros(n)
       * hence, we simply get
       *   next(31) >> numberOfLeadingZeros(n)
       */
      next(31) >> Integer.numberOfLeadingZeros(n)
    } else {
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

  override def nextFloat(): Float = {
    // next(24).toFloat / (1 << 24).toFloat
    (next(24).toDouble / (1 << 24).toDouble).toFloat
  }

  override def nextBytes(bytes: Array[Byte]): Unit = {
    var i = 0
    while (i < bytes.length) {
      var rnd = nextInt()
      var n = Math.min(bytes.length - i, 4)
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
      nextNextGaussian
    } else {
      var x, y, rds: Double = 0

      /* Get two random numbers from -1 to 1.
       * If the radius is zero or greater than 1, throw them out and pick two
       * new ones.
       * Rejection sampling throws away about 20% of the pairs.
       */
      do {
        x = nextDouble()*2-1
        y = nextDouble()*2-1
        rds = x*x + y*y
      } while (rds == 0 || rds > 1)

      val c = Math.sqrt(-2 * Math.log(rds) / rds)

      // Save y*c for next time
      nextNextGaussian = y*c
      haveNextNextGaussian = true

      // And return x*c
      x*c
    }
  }
}

object Random {

  /** Generate a random long from JS RNG to seed a new Random */
  private def randomSeed(): Long =
    (randomInt().toLong << 32) | (randomInt().toLong & 0xffffffffL)

  private def randomInt(): Int =
    (Math.floor(Math.random() * 4294967296.0) - 2147483648.0).toInt

}
