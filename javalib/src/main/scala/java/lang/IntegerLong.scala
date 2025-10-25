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

package java.lang

/** Common algorithms between `Integer` and `Long`. */
private[lang] object IntegerLong {

  @inline
  private def parseFail[I, F](s: String): Nothing =
    Integer.parseIntFail(s)

  /* Must be called only with a valid radix.
   *
   * The overflowBarrier must be divideUnsigned(ops.minInt, radix).
   * It will be used to detect overflow during the multiplication (and, a
   * posteriori, for the addition of `+ digit` from the previous iteration).
   *
   * `ops.minInt` is clearly the correct value for the negative case.
   *
   * For the positive case, in theory it should be `ops.maxInt`.
   * The only case where that would give a different quotient is when
   * `minInt = n * radix`. In that case, we will fail to detect the overflow if
   * `result == (n - 1) * radix` just before the multiplication. After the
   * multiplication, it will be `ops.minInt`, which is out of bounds. That's
   * fine, though, because that case will be caught either on the next
   * iteration of the loop, or in the final overflow check for the addition.
   *
   * That means we can always use the constant `ops.minInt` here.
   */
  @inline
  def parseSignedImpl[I, F](s: String, radix: Int, overflowBarrier: I)(
      implicit ops: IntFloatBits[I, F]): I = {
    import ops._

    // Keep the explicit IntegerLong; it makes sure we only load the module in that code path
    def fail(): Nothing = IntegerLong.parseFail(s)

    // Early checks: s non-null and non-empty
    if (s == null)
      fail()
    val len = s.length()
    if (len == 0)
      fail()

    // Load the module instance of Character once, instead of in every loop iteration
    val character = Character

    /* Process the sign character.
     * Set `sign` to `-1` if there is a leading '-', and `0` otherwise.
     * Set `i` to 1 if there was a leading '+' or '-', and 0 otherwise.
     */
    val firstChar = s.charAt(0)
    val negative = firstChar == '-'
    val sign = if (negative) minusOne else zero
    var i = if (negative || firstChar == '+') 1 else 0

    // We need at least one digit
    if (i >= len)
      fail()

    val result = newIntBox(zero)

    while (i != len) {
      val digit = fromInt32(character.digitWithValidRadix(s.charAt(i), radix))
      if (digit < zero || unsigned_>(result(), overflowBarrier))
        fail()
      result() = result() * fromUnsignedInt32(radix) + digit
      /* The above addition can overflow the range of valid results (but it
       * cannot overflow the unsigned long range). If that happens during the
       * last iteration, we catch it with the final overflow check. If it
       * happens during an earlier iteration, we catch it with the
       * `overflowBarrier`-based check.
       */
      i += 1
    }

    /* Final overflow check. So far we computed `result` as an unsigned
     * quantity. If negative, the maximum unsigned value allowed is
     * `minInt`. If non-negative, it is `maxInt`. We can compute
     * the right value without branches with `maxInt - sign`.
     */
    if (unsigned_>(result(), maxInt - sign))
      fail()

    /* Compute the final result. Use the standard trick to do this in a
     * branchless way.
     */
    (result() ^ sign) - sign
  }

  /* Must be called only with a valid radix.
   *
   * The overflowBarrier must be divideUnsigned(-1, radix). It will be used to
   * detect overflow during the multiplication.
   */
  @inline
  def parseUnsignedImpl[I, F](s: String, radix: Int, overflowBarrier: I)(
      implicit ops: IntFloatBits[I, F]): I = {
    import ops._

    // Keep the explicit IntegerLong; it makes sure we only load the module in that code path
    def fail(): Nothing = IntegerLong.parseFail(s)

    // Early checks: s non-null and non-empty
    if (s == null)
      fail()
    val len = s.length()
    if (len == 0)
      fail()

    // Load the module instance of Character once, instead of in every loop iteration
    val character = Character

    // Process a possible leading '+' sign
    var i = if (s.charAt(0) == '+') 1 else 0

    // We need at least one digit
    if (i >= len)
      fail()

    val result = newIntBox(zero)

    while (i != len) {
      val digit = fromInt32(character.digitWithValidRadix(s.charAt(i), radix))
      if (digit < zero || unsigned_>(result(), overflowBarrier))
        fail()
      result() = result() * fromUnsignedInt32(radix) + digit
      /* Unlike in `parseSignedImpl`, the addition overflows outside of the
       * unsigned integer range (obviously, otherwise it wouldn't be considered
       * an overflow for `parseUnsignedImpl`). We have to test for it at each
       * iteration, as the `overflowBarrier`-based check cannot detect it.
       */
      if (unsigned_<(result(), digit))
        fail()
      i += 1
    }

    result()
  }

  @inline
  def compress[I, F](i: I, mask: I)(implicit ops: IntFloatBits[I, F]): I = {
    // Hacker's Delight, Section 7-4, Figure 7-10

    import ops._

    val m = newIntBox(mask)
    val x = newIntBox(i & mask) // clear irrelevant bits
    val mk = newIntBox(~mask << 1) // we will count 0's to right

    var j = 0 // i in Hacker's Delight, but we already have an i
    while (j < logBitSize) {
      val mp = parallelSuffix(mk())
      val mv = mp & m() // bits to move
      m() = (m() ^ mv) | (mv >>> (1 << j)) // compress m
      val t = x() & mv
      x() = (x() ^ t) | (t >>> (1 << j)) // compress x
      mk() = mk() & ~mp
      j += 1
    }

    x()
  }

  @inline
  def expand[I, F](i: I, mask: I)(implicit ops: IntFloatBits[I, F]): I = {
    // Hacker's Delight, Section 7-5, Figure 7-12

    import ops._

    val array = intArrayOps.create(logBitSize)

    val m = newIntBox(mask)
    val x = newIntBox(i)
    val mk = newIntBox(~mask << 1) // we will count 0's to right

    var j = 0 // i in Hacker's Delight, but we already have an i
    while (j < logBitSize) {
      val mp = parallelSuffix(mk())
      val mv = mp & m() // bits to move
      intArrayOps.set(array, j, mv)
      m() = (m() ^ mv) | (mv >>> (1 << j)) // compress m
      mk() = mk() & ~mp
      j += 1
    }

    j = logBitSize - 1
    while (j >= 0) {
      val mv = intArrayOps.get(array, j)
      val t = x() << (1 << j)

      /* See the last line of the section text, but there is a mistake in the
       * book: y should be t. There is no y in this algorithm, so it doesn't
       * make sense. Plugging t instead matches the formula (c) of "Exchanging
       * Corresponding Fields of Registers" in Section 2-20.
       */
      x() = ((x() ^ t) & mv) ^ x()

      j -= 1
    }

    x() & mask // clear out extraneous bits
  }

  @inline
  private def parallelSuffix[I, F](x: I)(implicit ops: IntFloatBits[I, F]): I = {
    // Hacker's Delight, Section 5-2

    import ops._

    val y1 = x ^ (x << 1)
    val y2 = y1 ^ (y1 << 2)
    val y3 = y2 ^ (y2 << 4)
    val y4 = y3 ^ (y3 << 8)
    val y5 = y4 ^ (y4 << 16)

    // not so generic here
    if (bitSize == 32)
      y5
    else
      y5 ^ (y5 << 32)
  }

}
