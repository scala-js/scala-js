/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/publicdomain/zero/1.0/
 *
 * and translated to Scala
 */

package java.util.concurrent

import java.util.Random
import scala.annotation.tailrec

class ThreadLocalRandom extends Random {

  private var initialized: Boolean = _
  initialized = true

  override def setSeed(seed: Long): Unit = {
    if (initialized)
      throw new UnsupportedOperationException()

    super.setSeed(seed)
  }

  def nextInt(least: Int, bound: Int): Int = {
    if (least >= bound)
      throw new IllegalArgumentException()

    val difference = bound - least
    if (difference > 0) {
      nextInt(difference) + least
    } else {
      /* The interval size here is greater than Int.MaxValue,
       * so the loop will exit with a probability of at least 1/2.
       */
      @tailrec
      def loop(): Int = {
        val n = nextInt()
        if (n >= least && n < bound) n
        else loop()
      }

      loop()
    }
  }

  def nextLong(_n: Long): Long = {
    if (_n <= 0)
      throw new IllegalArgumentException("n must be positive")

    /*
     * Divide n by two until small enough for nextInt. On each
     * iteration (at most 31 of them but usually much less),
     * randomly choose both whether to include high bit in result
     * (offset) and whether to continue with the lower vs upper
     * half (which makes a difference only if odd).
     */

    var offset = 0L
    var n = _n

    while (n >= Integer.MAX_VALUE) {
      val bits = next(2)
      val halfn = n >>> 1
      val nextn =
        if ((bits & 2) == 0) halfn
        else n - halfn
      if ((bits & 1) == 0)
        offset += n - nextn
      n = nextn
    }
    offset + nextInt(n.toInt)
  }

  def nextLong(least: Long, bound: Long): Long = {
    if (least >= bound)
      throw new IllegalArgumentException()

    val difference = bound - least
    if (difference > 0) {
      nextLong(difference) + least
    } else {
      /* The interval size here is greater than Long.MaxValue,
       * so the loop will exit with a probability of at least 1/2.
       */
      @tailrec
      def loop(): Long = {
        val n = nextLong()
        if (n >= least && n < bound) n
        else loop()
      }

      loop()
    }
  }

  def nextDouble(n: Double): Double = {
    if (n <= 0)
      throw new IllegalArgumentException("n must be positive")

    nextDouble() * n
  }

  def nextDouble(least: Double, bound: Double): Double = {
    if (least >= bound)
      throw new IllegalArgumentException()

    /* Based on documentation for Random.doubles to avoid issue #2144 and other
     * possible rounding up issues:
     * https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#doubles-double-double-
     */
    val next = nextDouble() * (bound - least) + least
    if (next < bound) next
    else Math.nextAfter(bound, Double.NegativeInfinity)
  }
}

object ThreadLocalRandom {

  private val _current =
    new ThreadLocalRandom()

  def current(): ThreadLocalRandom = _current

}
