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

/*
 * This is a clean room implementation derived from the original paper
 * and Java implementation mentioned there:
 *
 * Fast Splittable Pseudorandom Number Generators
 * by Guy L. Steele Jr., Doug Lea, Christine H. Flood
 * http://gee.cs.oswego.edu/dl/papers/oopsla14.pdf
 *
 */
private object SplittableRandom {

  private final val DoubleULP = 1.0 / (1L << 53)
  private final val GoldenGamma = 0x9e3779b97f4a7c15L

  private var defaultGen: Long = new Random().nextLong()

  private def nextDefaultGen(): Long = {
    val s = defaultGen
    defaultGen = s + (2 * GoldenGamma)
    s
  }

  // This function implements the original MurmurHash 3 finalizer
  private final def mix64ForGamma(z: Long): Long = {
    val z1 = (z ^ (z >>> 33)) * 0xff51afd7ed558ccdL
    val z2 = (z1 ^ (z1 >>> 33)) * 0xc4ceb9fe1a85ec53L
    z2 ^ (z2 >>> 33)
  }

  /*
   * This function implements David Stafford's variant 4,
   * while the paper version uses the original MurmurHash3 finalizer
   * reference:
   * http://zimbry.blogspot.pt/2011/09/better-bit-mixing-improving-on.html
   */
  private final def mix32(z: Long): Int = {
    val z1 = (z ^ (z >>> 33)) * 0x62a9d9ed799705f5L
    val z2 = (z1 ^ (z1 >>> 28)) * 0xcb24d0a5c88c35b3L
    (z2 >>> 32).toInt
  }

  /*
   * This function implements Stafford's variant 13,
   * whereas the paper uses the original MurmurHash3 finalizer
   */
  private final def mix64(z: Long): Long = {
    val z1 = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    val z2 = (z1 ^ (z1 >>> 27)) * 0x94d049bb133111ebL
    z2 ^ (z2 >>> 31)
  }

  private final def mixGamma(z: Long): Long = {
    val z1 = mix64ForGamma(z) | 1L
    val n = java.lang.Long.bitCount(z1 ^ (z1 >>> 1))
    /* Reference implementation is wrong since we can read in the paper:
     *
     * ... Therefore we require that the number of such
     * pairs, as computed by Long.bitCount(z ^ (z >>> 1)),
     * exceed 24; if it does not, then the candidate z is replaced by
     * the XOR of z and 0xaaaaaaaaaaaaaaaaL ...
     * ... so the new value necessarily has more than 24 bit pairs whose bits differ
     */
    if (n <= 24) z1 ^ 0xaaaaaaaaaaaaaaaaL
    else z1
  }

}

final class SplittableRandom private (private var seed: Long, gamma: Long) {
  import SplittableRandom._

  def this(seed: Long) = {
    this(seed, SplittableRandom.GoldenGamma)
  }

  private def this(ll: (Long, Long)) = this(ll._1, ll._2)

  def this() = {
    this({
      val s = SplittableRandom.nextDefaultGen()

      (SplittableRandom.mix64(s),
          SplittableRandom.mixGamma(s + SplittableRandom.GoldenGamma))
    })
  }

  def split(): SplittableRandom =
    new SplittableRandom(mix64(nextSeed()), mixGamma(nextSeed()))

  private def nextSeed(): Long = {
    seed += gamma
    seed
  }

  def nextInt(): Int = mix32(nextSeed())

  //def nextInt(bound: Int): Int

  //def nextInt(origin: Int, bound: Int): Int

  def nextLong(): Long = mix64(nextSeed())

  //def nextLong(bound: Long): Long

  //def nextLong(origin: Long, bound: Long): Long

  def nextDouble(): Double =
    (nextLong() >>> 11).toDouble * DoubleULP

  //def nextDouble(bound: Double): Double

  //def nextDouble(origin: Double, bound: Double): Double

  // this should be properly tested
  // looks to work but just by chance maybe
  def nextBoolean(): Boolean = nextInt() < 0

}
