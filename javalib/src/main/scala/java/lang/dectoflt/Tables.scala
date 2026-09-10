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

package java.lang.dectoflt

import java.math.BigInteger
import dectoflt.{FloatingPoint => FP}

/** A list of power of ten for 64-bit significant floating point numbers.
 *
 *  The `smallPowerOfTens` has exact floating point values, because 64-bit
 *  floating point numbers can exactly represent the 10^e up to e < 27.563...,
 *  because 10^e = 5^e * 2^e and the 2^e part can be absorbed into the binary
 *  exponent. Now 5^e < 2^64 is the requirement for significand, and
 *  e < log5(2^64) =~ 27.5632997...
 *  `largePowerOfTens` are the best approximations, and when we wanna calculate the
 *  10^e (where e > 16 or e < 0), we calculate it by 10^a * 10^b (where
 *  0 <= a < 16, and a+b=e) 10^a from `smallPowerOfTens` and 10^b from
 *  `largePowerOfTens`).
 *
 *  Since largest normal number for double precision (Double.MaxValue) is
 *  around 1.79...E308 and the smallest subnormal (java.lang.Double.longBitsToDouble(1L))
 *  is 4.9E-324, tables covers enough with 10^-320 ~ 10^320.
 *
 *  Generated with:
 *
 *  ```
 *  import java.math.BigInteger
 *
 *  val N = 64 // Size of the significand field in bits
 *  val MIN_SIG: BigInteger = BigInteger.ONE.shiftLeft(N - 1)
 *  val MAX_SIG: BigInteger = BigInteger.ONE.shiftLeft(N).subtract(BigInteger.ONE)
 *
 *  case class Fp(sig: BigInteger, exp: Int)
 *
 *  def algorithmM(f: BigInteger, e: Int): Fp = {
 *    var (u: BigInteger, v: BigInteger) = if (e < 0) {
 *      (f, BigInteger.TEN.pow(e.abs))
 *    } else {
 *      (f.multiply(BigInteger.TEN.pow(e)), BigInteger.ONE)
 *    }
 *    var k = 0
 *
 *    while (true) {
 *      val x = u.divide(v)
 *      if (x.compareTo(MIN_SIG) < 0) {
 *        u = u.shiftLeft(1)
 *        k -= 1
 *      } else if (x.compareTo(MAX_SIG) >= 0) {
 *        v = v.shiftLeft(1)
 *        k += 1
 *      } else {
 *        return ratioToFloat(u, v, k)
 *      }
 *    }
 *    throw new IllegalStateException("Unreachable")
 *  }
 *
 *  def ratioToFloat(u: BigInteger, v: BigInteger, k: Int): Fp = {
 *    val qr = u.divideAndRemainder(v)
 *    val q = qr(0)
 *    val r = qr(1)
 *    val v_r = v.subtract(r)
 *    val z = Fp(q, k)
 *
 *    if (r.compareTo(v_r) < 0) {
 *      z
 *    } else if (r.compareTo(v_r) > 0) {
 *      nextFloat(z)
 *    } else if (q.testBit(0)) { // q is odd
 *      nextFloat(z)
 *    } else { // q is even
 *      z
 *    }
 *  }
 *
 *  def nextFloat(z: Fp): Fp = {
 *    if (z.sig.equals(MAX_SIG)) {
 *      Fp(MIN_SIG, z.exp + 1)
 *    } else {
 *      Fp(z.sig.add(BigInteger.ONE), z.exp)
 *    }
 *  }
 *
 *  @main def main(): Unit = {
 *    val SmallTableSize = 16
 *    // Generate multiples of SmallTableSize (16) for the range -20 to 20.
 *    val LargeTableRange = 20
 *
 *    val powers = (0 until SmallTableSize).map { e =>
 *      algorithmM(BigInteger.ONE, e)
 *    }
 *    val largePowers = (-LargeTableRange to LargeTableRange).map { i =>
 *      (algorithmM(BigInteger.ONE, i*SmallTableSize), i)
 *    }
 *
 *    println(
 *    """
 *    |private[dec2flt] object Tables {
 *    |""".stripMargin)
 *
 *    println("  val smallPowerOfTens = Array[FP](")
 *    for ((z, i) <- powers.zipWithIndex) {
 *      println(f"    FP.normalized(${z.sig.longValue()}L, ${z.exp}), // $i")
 *    }
 *    println("  )") // end smallPowerOfTens
 *    println("")
 *    println("  val largePowerOfTens = Array[FP](")
 *    for ((z, i) <- largePowers) {
 *      println(f"    FP.normalized(${z.sig.longValue()}L, ${z.exp}), // ${i*SmallTableSize} ($i*$SmallTableSize)")
 *    }
 *    println("  )")
 *    println(s"  final val LargeTableRange = $LargeTableRange")
 *    println("}")
 *  }
 *  ```
 */
private[dectoflt] object Tables {

  val smallPowerOfTens = Array[FP](
    FP.normalized(-9223372036854775808L, -63), // 0
    FP.normalized(-6917529027641081856L, -60), // 1
    FP.normalized(-4035225266123964416L, -57), // 2
    FP.normalized(-432345564227567616L, -54), // 3
    FP.normalized(-7187745005283311616L, -50), // 4
    FP.normalized(-4372995238176751616L, -47), // 5
    FP.normalized(-854558029293551616L, -44), // 6
    FP.normalized(-7451627795949551616L, -40), // 7
    FP.normalized(-4702848726509551616L, -37), // 8
    FP.normalized(-1266874889709551616L, -34), // 9
    FP.normalized(-7709325833709551616L, -30), // 10
    FP.normalized(-5024971273709551616L, -27), // 11
    FP.normalized(-1669528073709551616L, -24), // 12
    FP.normalized(-7960984073709551616L, -20), // 13
    FP.normalized(-5339544073709551616L, -17), // 14
    FP.normalized(-2062744073709551616L, -14) // 15
  )

  val largePowerOfTens = Array[FP](
    FP.normalized(-215969822234494767L, -1127), // -320 (-20*16)
    FP.normalized(-8326631408344020698L, -1073), // -304 (-19*16)
    FP.normalized(-7211161980820077193L, -1020), // -288 (-18*16)
    FP.normalized(-5972742139117552793L, -967), // -272 (-17*16)
    FP.normalized(-4597819916706768582L, -914), // -256 (-16*16)
    FP.normalized(-3071349608317525545L, -861), // -240 (-15*16)
    FP.normalized(-1376627125537124674L, -808), // -224 (-14*16)
    FP.normalized(-8970925639256982432L, -754), // -208 (-13*16)
    FP.normalized(-7926472270612804602L, -701), // -192 (-12*16)
    FP.normalized(-6766896092596731856L, -648), // -176 (-11*16)
    FP.normalized(-5479507920956448621L, -595), // -160 (-10*16)
    FP.normalized(-4050219931171323191L, -542), // -144 (-9*16)
    FP.normalized(-2463391496091671391L, -489), // -128 (-8*16)
    FP.normalized(-701658031336336515L, -436), // -112 (-7*16)
    FP.normalized(-8596242524610931813L, -382), // -96 (-6*16)
    FP.normalized(-7510490449794491994L, -329), // -80 (-5*16)
    FP.normalized(-6305063497298744923L, -276), // -64 (-4*16)
    FP.normalized(-4966770740134231719L, -223), // -48 (-3*16)
    FP.normalized(-3480967307441105734L, -170), // -32 (-2*16)
    FP.normalized(-1831394126398103205L, -117), // -16 (-1*16)
    FP.normalized(-9223372036854775808L, -63), // 0 (0*16)
    FP.normalized(-8206744073709551616L, -10), // 16 (1*16)
    FP.normalized(-7078060301547948642L, 43), // 32 (2*16)
    FP.normalized(-5824969590173362729L, 96), // 48 (3*16)
    FP.normalized(-4433759430461380907L, 149), // 64 (4*16)
    FP.normalized(-2889205879056697348L, 202), // 80 (5*16)
    FP.normalized(-1174406963520662365L, 255), // 96 (6*16)
    FP.normalized(-8858670899299929442L, 309), // 112 (7*16)
    FP.normalized(-7801844473689174816L, 362), // 128 (8*16)
    FP.normalized(-6628531442943809817L, 415), // 144 (9*16)
    FP.normalized(-5325892301117581398L, 468), // 160 (10*16)
    FP.normalized(-3879672333084147821L, 521), // 176 (11*16)
    FP.normalized(-2274045625900771989L, 574), // 192 (12*16)
    FP.normalized(-491441886632713914L, 627), // 208 (13*16)
    FP.normalized(-8479549122611984080L, 681), // 224 (14*16)
    FP.normalized(-7380934748073420954L, 734), // 240 (15*16)
    FP.normalized(-6161227774276542834L, 787), // 256 (16*16)
    FP.normalized(-4807081008671376254L, 840), // 272 (17*16)
    FP.normalized(-3303676090774835316L, 893), // 288 (18*16)
    FP.normalized(-1634561335591402499L, 946), // 304 (19*16)
    FP.normalized(-9114107888677362826L, 1000) // 320 (20*16)
  )

  final val LargeTableRange = 20
}
