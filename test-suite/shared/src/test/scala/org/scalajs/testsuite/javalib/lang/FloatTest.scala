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

package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import java.lang.{Float => JFloat}

import scala.util.Try

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.{executingInJVM, hasAccurateFloats}

class FloatTest {

  @Test def properEquals(): Unit = {
    assertTrue(0.0f.equals(0.0f))
    assertTrue((-0.0f).equals(-0.0f))
    assertFalse(0.0f.equals(-0.0f))
    assertTrue(Float.NaN.equals(Float.NaN))
  }

  @Test def hashCodeTest(): Unit = {
    def hashCodeNotInlined(x: Any): Int = {
      var y = x // do not inline
      y.hashCode
    }

    def test(x: Float, expected: Int): Unit = {
      assertEquals(expected, x.hashCode)
      assertEquals(expected, hashCodeNotInlined(x))
    }

    if (!executingInJVM) {
      test(0.0f, 0)
      test(-0.0f, -2147483648)
      test(1234.0f, 1234)
      test(1.5f, 1073217536)
      test(-54f, -54)

      test(Float.MinPositiveValue, 916455424)
      test(Float.MinValue, 670040063)
      test(Float.MaxValue, -1477443585)

      test(Float.NaN, 2146959360)
      test(Float.PositiveInfinity, 2146435072)
      test(Float.NegativeInfinity, -1048576)
    }
  }

  @Test def toStringWithIntegerValuesWhenAnInteger(): Unit = {
    if (executingInJVM) {
      assertEquals("0.0", 0.0f.toString)
      assertEquals("-0.0", (-0.0f).toString)
    } else {
      assertEquals("0", 0.0f.toString)
      assertEquals("0", (-0.0f).toString)
    }
    assertEquals("NaN", Float.NaN.toString)
    assertEquals("Infinity", Float.PositiveInfinity.toString)
    assertEquals("-Infinity", Float.NegativeInfinity.toString)
    if (executingInJVM) {
      assertEquals("5.0", 5.0f.toString)
      assertEquals("-5.0", (-5.0f).toString)
    } else {
      assertEquals("5", 5.0f.toString)
      assertEquals("-5", (-5.0f).toString)
    }
    // We need to explicitly cut the string here, since floats are
    // represented by doubles (but the literal is emitted as
    // float). Therefore there may be some imprecision. This is
    // documented as semantic difference.
    assertEquals("1.2", 1.2f.toString.substring(0,3))
  }

  @Test def toHexStringTest(): Unit = {
    import java.lang.Float.toHexString

    assertEquals("NaN", toHexString(Float.NaN))
    assertEquals("Infinity", toHexString(Float.PositiveInfinity))
    assertEquals("-Infinity", toHexString(Float.NegativeInfinity))
    assertEquals("0x0.0p0", toHexString(0.0f))
    assertEquals("-0x0.0p0", toHexString(-0.0f))
    assertEquals("0x1.0p0", toHexString(1.0f))
    assertEquals("-0x1.0p0", toHexString(-1.0f))
    assertEquals("0x1.0p1", toHexString(2.0f))
    assertEquals("0x1.8p1", toHexString(3.0f))
    assertEquals("0x1.0p-1", toHexString(0.5f))
    assertEquals("0x1.0p-2", toHexString(0.25f))
    assertEquals("0x1.00204p3", toHexString(8.003937f))
    assertEquals("0x0.00204p-126", toHexString(5.785e-42f))
    assertEquals("0x1.fffffep127", toHexString(Float.MaxValue))
    assertEquals("0x1.0p-126", toHexString(java.lang.Float.MIN_NORMAL))
    assertEquals("0x0.fffffep-126", toHexString(1.1754942E-38f))
    assertEquals("0x0.000002p-126", toHexString(Float.MinPositiveValue))
  }

  @Test def parseStringMethods(): Unit = {
    def test(expected: Float, s: String): Unit = {
      if (hasAccurateFloats) {
        assertEquals(s, expected: Any, JFloat.parseFloat(s))
        assertEquals(s, expected: Any, JFloat.valueOf(s).floatValue())
        assertEquals(s, expected: Any, new JFloat(s).floatValue())
      } else {
        val epsilon = Math.ulp(expected)
        assertEquals(s, expected, JFloat.parseFloat(s), epsilon)
        assertEquals(s, expected, JFloat.valueOf(s).floatValue(), epsilon)
        assertEquals(s, expected, new JFloat(s).floatValue(), epsilon)
      }
    }

    // Specials

    test(Float.NaN, "NaN")
    test(Float.NaN, "  -NaN\r  ")
    test(Float.PositiveInfinity, "Infinity")
    test(Float.NegativeInfinity, "  -Infinity\r  ")

    // Zeros

    test(+0.0f, "0")
    test(+0.0f, "0.0")
    test(-0.0f, "-0.0")
    test(+0.0f, "  .0f  ")
    test(+0.0f, "0.e5")
    test(-0.0f, "  -0D\r  ")

    // Regular values

    test(5.3f, "5.3")
    test(12700.0f, "127e2")
    test(1.27f, "127E-2")
    test(10f, "1E+1")
    test(-123.4f, "-123.4")
    test(65432.10f, "65432.1")
    test(-87654.321f, "-87654.321")
    test(0.3f, "+.3f")

    // Corner cases that require the BigInteger arithmetics code paths

    // k >= 0, e >= 0, f*10^e < m*2^k
    test(1.72544037e18f, "1725440439005216752") // from the bug report
    test(1.72544037e18f, "1725440439005216767")

    // k >= 0, e >= 0, f*10^e = m*2^k, even is upwards
    test(1.72544051e18f, "1725440439005216768")

    // k >= 0, e >= 0, f*10^e > m*2^k
    test(1.72544051e18f, "1725440439005216775")

    // k >= 0, e >= 0, f*10^e = m*2^k, even is downwards
    test(1.72544051e18f, "1725440576444170240")
    test(1.72544051e18f, "172544057644417024e1")

    // k >= 0, e < 0, f*10^e < m*2^k
    test(1.72544037e18f, "172544043900521676700000e-5")

    // k < 0, e < 0, f*10^e < m*2^k
    test(1.7254404e-18f, "1.725440493251219023E-18")

    /* Attempt at k < 0, e >= 0, f*10^e < m*2^k, but e is adjusted downwards to
     * compensate the number of digits after the '.', so it ends up being
     * negative anyway. I am not sure we can craft an example that would
     * actually use that code path.
     */
    test(1.7254404e-18f, "0.00000000000000000000001725440493251219023e5")

    // the limit between MaxValue and PositiveInfinity
    test(Float.MaxValue, "3.4028235677973366e38")
    test(Float.MaxValue, "3.4028235677973366163e38")
    test(Float.PositiveInfinity, "3.4028235677973366164e38")
    test(Float.PositiveInfinity, "3.4028235677973367e38")

    // Hex notation with an exact representation (the input is exactly `toHexString(expected)`)
    test(0.0f, "0x0.0p0")
    test(-0.0f, "-0x0.0p0")
    test(1.0f, "0x1.0p0")
    test(-1.0f, "-0x1.0p0")
    test(2.0f, "0x1.0p1")
    test(3.0f, "0x1.8p1")
    test(0.5f, "0x1.0p-1")
    test(0.25f, "0x1.0p-2")
    test(8.003937f, "0x1.00204p3")
    test(5.785e-42f, "0x0.00204p-126")
    test(Float.MaxValue, "0x1.fffffep127")
    test(java.lang.Float.MIN_NORMAL, "0x1.0p-126")
    test(1.1754942e-38f, "0x0.fffffep-126")
    test(Float.MinPositiveValue, "0x0.000002p-126")

    // the limit between MaxValue and PositiveInfinity
    test(Float.MaxValue, "0x1.fffffefp127")
    //test(Float.MaxValue, "0x1.fffffeffffffffffffffffffffffffffp127")
    test(Float.PositiveInfinity, "0x1.ffffff00000000000000p127")
    test(Float.PositiveInfinity, "0x1.ffffff00000000000001p127")

    /* Generated with:
     *
     *   import scala.util.Random
     *   def randDecDigit(): Char = (Random.nextInt(10) + '0').toChar
     *   def randHexDigit(): Char = {
     *     val x = Random.nextInt(16)
     *     if (x < 10) (x + '0').toChar else (x - 10 + 'a').toChar
     *   }
     *   def timesRand(min: Int, max: Int, f: () => Char): String = {
     *     val count = Random.nextInt(max - min + 1) + min
     *     Array.fill(count)(f()).mkString
     *   }
     *   def randInput(): String = {
     *     "0x" + timesRand(0, 20, randHexDigit) + "." +
     *     timesRand(0, 20, randHexDigit) + "p" +
     *     (if (Random.nextBoolean()) "-" else "") +
     *     timesRand(1, 3, randDecDigit)
     *   }
     *   for (_ <- 0 until 100) {
     *     val input = randInput()
     *     val r = java.lang.Float.parseFloat(input)
     *     val rstr =
     *       if (r == Float.PositiveInfinity) "Float.PositiveInfinity"
     *       else r.toString().toLowerCase() + "f"
     *     println(s"""    test($rstr, "$input")""")
     *   }
     */

    test(7.7130426e21f, "0x0d10ffc.78b61fb6e3p49")
    test(4.418296e8f, "0xd2ae470fc6e59fe1bfd2.761972p-51")
    test(509129.66f, "0x7.c4c9a8d8e4d632p16")
    test(0.0f, "0xbf0fd97e83.b3d869055e8p-917")
    test(5.0665498e13f, "0xb851ec7d7054e.f95cb90ee067c5571dp-6")
    test(Float.PositiveInfinity, "0xbef990999045de.aaf7596651p337")
    test(Float.PositiveInfinity, "0xc072a8d60f98.1a50e162c0f174dp252")
    test(0.0f, "0x7298b175bd0.4248f438fp-372")
    test(8.6459403e11f, "0x64a6e8c9.26ff20d04b266dp9")
    test(5.8642432e10f, "0x6d3ae3d.c261a7e4e4fbc4p9")
    test(Float.PositiveInfinity, "0xd7a082b11.7bc6ap685")
    test(8.6811844e15f, "0xf6bbee2e73a99f.0643fb98ddcp-3")
    test(1.5566609e21f, "0xa8c604957de1aa987d7.09ea924d7d34a6289p-5")
    test(2.3215817e22f, "0x4ea882d26db60f1f.p12")
    test(6990793.5f, "0xd557.92905760f52cp7")
    test(0.0f, "0x1e00f2.a2bbd135b8d55f68p-897")
    test(1.59086732e13f, "0xe7806ccae.1cde3ac06cp8")
    test(1.71995341e10f, "0x8025824a082.103b1d6a12d52p-9")
    test(63.986023f, "0xff.f1bp-2")
    test(Float.PositiveInfinity, "0xb4c2.04628ce3e5c92f9p831")
    test(5.0318215e-7f, "0x21c49a9064.7ee57561df94567p-58")
    test(7.132301e-16f, "0xcd9318034553b55885e.p-126")
    test(4.2603957e20f, "0xb8c3e4d1adfc674d831.b4f3944605e11d6c44p-7")
    test(1.2994218e22f, "0x580d603b4c280f5db5d.a4b2f609cfd5614618p-01")
    test(1.0028e-41f, "0x1bf406179fa425c5.809p-197")
    test(7.8854745e34f, "0xf2fd6d6e8f.38746e5c92466114dc33p76")
    test(8.454455e-24f, "0xa388.6fp-92")
    test(Float.PositiveInfinity, "0xa8dfc739f2521bfc2314.ap947")
    test(Float.PositiveInfinity, "0x8bd91ade496261a15.c1e2aa689ca0p387")
    test(Float.PositiveInfinity, "0x4b5.5cp352")
    test(208953.42f, "0x198.1cb6ea50921307decp9")
    test(6.2066941e13f, "0xe1cc57bd9d120.d44d44a11cdc52p-6")
    test(Float.PositiveInfinity, "0x0398a97e93bc5af19392.a7a7bbp67")
    test(1.2792656e-11f, "0x38434.p-54")
    test(6.9988051e11f, "0xa2f4209476.343d38a849p-0")
    test(3.416361e-24f, "0x4.215p-80")
    test(2.32393534e17f, "0xce.682c15ae2b1ae8p50")
    test(6.7282637e8f, "0xa06a101db.870845f012ep-6")
    test(8.9688505e13f, "0x.a3247710c92p47")
    test(Float.PositiveInfinity, "0x915e6803d5d139f.9ebe8p288")
    test(5527947.5f, "0xa8b316.cd54128p-1")
    test(4.67517525e14f, "0xd49a346790157e.e197d0p-7")
    test(Float.PositiveInfinity, "0xa2a1ff8623f6a26dea4.e2e9515c71470dp92")
    test(1.79808806e17f, "0x4fd9e5077f53f4d.534cfc7600f96p-1")
    test(8.8284704e7f, "0xa863c3e13ef.eb71f7e0eb92p-17")
    test(86761.195f, "0xa974.995683p1")
    test(9.56837e-35f, "0x7f2f79dc231c411463a.edfff8c2fbb18cacp-188")
    test(0.0f, "0x4db94.32ca76e01f2p-309")
    test(2.9829555e-19f, "0xb015.25adbee2p-77")
    test(0.0f, "0x0fa02746f.fef5p-991")
    test(Float.PositiveInfinity, "0x31b238fd6449331877.fc5e2e7802p86")
    test(1.57897099e18f, "0xaf4d0d99babad42.5p1")
    test(1.0527979e-6f, "0x8d4ddc424ac6ea56467.3ecfc792601a3aap-95")
    test(1104859.6f, "0x86d.edcedc0e105bp9")
    test(4.799154e-22f, "0x910b.a84f692p-086")
    test(2.10316288e18f, "0xe97f7c1653a483b1.aeb2ef94fd4b9p-3")
    test(7.8215464e-8f, "0xa7f767d246872f4e5.e85009p-91")
    test(1.04264643e17f, "0xb9361267a25c.4b2a2223929cp9")
    test(3.122e-42f, "0x22.d1efd86df85932dc709p-143")
    test(0.0f, "0xf726f7cee01e1347f.4190cb23ae5ddb35f5abp-666")
    test(0.0f, "0x873eb84b618a7ef48.33c522p-584")
    test(2.0163967e24f, "0x6abf454652a7805907cc.4253eeccep2")
    test(242.05287f, "0xf20d8.8d47p-12")
    test(1.81299076e14f, "0xa4e3fafb755355.3f714d7954620ce7f8fp-8")
    test(1.05632464e14f, "0x30093cc8a7dd.3f790368898fee09b914p1")
    test(2.56492462e10f, "0x5f8d073aca9d14c456.d9p-36")
    test(0.0f, "0x349da.691c578399fp-504")
    test(1.2006156e26f, "0xc6a00be6a36ddbc74f7d.ep7")
    test(Float.PositiveInfinity, "0xc88498c0145f13a1.7ecda4p475")
    test(9.0430286e24f, "0xef5df1b98faaa9cbfa93.ab40dcd70c97p3")
    test(2.9899642f, "0x2fd6e.4a6a4cbb8e1ep-16")
    test(0.24777621f, "0x1fb7.218cp-15")
    test(0.515625f, "0x.84p-0")
    test(6.1116556e20f, "0x1090d0.139f8cp049")
    test(2.210929e19f, "0x13.2d3f9819d161a1p60")
    test(0.0f, "0x5430c367fcc2122fa9.2442aeaf6dbc1e3b8a5p-706")
    test(2.11213148e10f, "0x9d5db5b4.p3")
    test(1.91006708e14f, "0xadb836d1e864b0.0de397p-8")
    test(Float.PositiveInfinity, "0x9ad56.bd93ccc424eb96c8p744")
    test(0.0f, "0x6917505f.457224cp-762")
    test(1.735055e26f, "0x23e14ded6.3b47280p54")
    test(0.011165324f, "0x5b7761942.55b91p-41")
    test(2.71142646e10f, "0x65022db5.02bafaf3a3e686dcee2p4")
    test(483.03305f, "0x1e308.758ce9b779c2032ebcebp-08")
    test(5.4584883e21f, "0x24fcf7d78.9a7f13d00f6dp39")
    test(5.457768e13f, "0xc68d6c9e62.8bp6")
    test(2.3357802e7f, "0xb234b4f.8p-3")
    test(624020.2f, "0x985942f.406fb81p-8")
    test(2.86250877e17f, "0x.fe3dea3f8eccbfp58")
    test(Float.PositiveInfinity, "0x144d12b4a2e105072.p691")
    test(Float.PositiveInfinity, "0xba085db2cff8523be.81cfd0def6p753")
    test(0.0f, "0x8d1616.29fcae7eaa0cbfp-557")
    test(0.09765658f, "0xc.8002bb84f15bdcp-07")
    test(9.688656e27f, "0x3e9c8.9611f821442e8edd0p75")
    test(Float.PositiveInfinity, "0xf11d0c63a6c34.9p83")
    test(0.0f, "0x1265612751cb.1a741dd32edacc981567p-647")
    test(0.0f, "0xd068a9e5546703d73.7c98a95p-619")
    test(Float.PositiveInfinity, "0xb2aac3336353.22ccfcp348")
    test(1.7088932e34f, "0xd2a32940c6119cc833.c49202ep42")
    test(1.4970953e-24f, "0x39ea8380.0ee8d51c1aacep-109")

    /* One test where parsing the mantissa on its own would produce
     * overflow/underflow, but where the binary exponent "saves" it.
     */
    test(Float.PositiveInfinity, "0x123456789abcdef" + "0" * 18 + "p0")
    test(1.9358286e38f, "0x123456789abcdef" + "0" * 18 + "p-1")
    test(0.0f, "0x0." + "0" * 37 + "123456789abcdefp1")
    test(1.4e-45f, "0x0." + "0" * 37 + "123456789abcdefp2")

    // Underflow preserves the sign of the 0
    test(-0.0f, "-0x0." + "0" * 40 + "123456789abcdefp0")

    /* Mantissa of 0 with overflowing/underflowing binary exponent.
     * Note that Math.pow(2, 1000 / 3).toFloat is Infinity.
     */
    test(0.0f, "0x0p1000")
    test(-0.0f, "-0x0p1000")
    test(0.0f, "0x0p-1000")
    test(-0.0f, "-0x0p-1000")

    // #1053 (and #4431) When a character very far in the string can make a difference

    // even is upward -> far character cannot make a difference
    test(4.8238544e15f, "0x1.123452fffffffffffffffffffffffffffffffp52")
    test(4.8238549e15f, "0x1.1234530000000000000000000000000000000p52")
    test(4.8238549e15f, "0x1.1234530000000000000000000000000000001p52")

    // even is downward -> far character *can* make a difference
    test(4.8238549e15f, "0x1.123454fffffffffffffffffffffffffffffffp52")
    test(4.8238549e15f, "0x1.1234550000000000000000000000000000000p52")
    test(4.8238555e15f, "0x1.1234550000000000000000000000000000001p52")

    // even is "upward" (towards -Infinity) -> far character cannot make a difference
    test(-4.8238544e15f, "-0x1.123452fffffffffffffffffffffffffffffffp52")
    test(-4.8238549e15f, "-0x1.1234530000000000000000000000000000000p52")
    test(-4.8238549e15f, "-0x1.1234530000000000000000000000000000001p52")

    // even is "downward" (towards 0) -> far character *can* make a difference
    test(-4.8238549e15f, "-0x1.123454fffffffffffffffffffffffffffffffp52")
    test(-4.8238549e15f, "-0x1.1234550000000000000000000000000000000p52")
    test(-4.8238555e15f, "-0x1.1234550000000000000000000000000000001p52")
  }

  @Test def parseFloatInvalidThrows(): Unit = {
    def test(s: String): Unit =
      assertThrows(classOf[NumberFormatException], JFloat.parseFloat(s))

    test("4.3.5")
    test("4e3.5")
    test("hello world")
    test("--4")
    test("4E-3.2")
    test(".")
    test("-.e5")

    test("NaNf")
    test("Infinityf")
  }

  @Test def compareTo(): Unit = {
    def compare(x: Float, y: Float): Int =
      new JFloat(x).compareTo(new JFloat(y))

    assertTrue(compare(0.0f, 5.5f) < 0)
    assertTrue(compare(10.5f, 10.2f) > 0)
    assertTrue(compare(-2.1f, -1.0f) < 0)
    assertEquals(0, compare(3.14f, 3.14f))

    // From compareTo's point of view, NaN is equal to NaN
    assertEquals(0, compare(Float.NaN, Float.NaN))

    // And -0.0 < 0.0
    assertTrue(compare(-0.0f, 0.0f) < 0)
    assertTrue(compare(0.0f, -0.0f) > 0)
  }

  @Test def compareToAnyAny(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0.0f, 5.5f) < 0)
    assertTrue(compare(10.5f, 10.2f) > 0)
    assertTrue(compare(-2.1f, -1.0f) < 0)
    assertEquals(0, compare(3.14f, 3.14f))

    // From compareTo's point of view, NaN is equal to NaN
    assertEquals(0, compare(Float.NaN, Float.NaN))

    // And -0.0 < 0.0
    assertTrue(compare(-0.0f, 0.0f) < 0)
    assertTrue(compare(0.0f, -0.0f) > 0)
  }

  @Test def isInfinite_Issue515(): Unit = {
    assertTrue(Float.PositiveInfinity.isInfinite)
    assertTrue(Float.NegativeInfinity.isInfinite)
    assertTrue((1f/0).isInfinite)
    assertTrue((-1f/0).isInfinite)
    assertFalse(0f.isInfinite)
  }

  @Test def isNaNTest(): Unit = {
    def f(v: Float): Boolean = {
      var v2 = v // do not inline
      v2.isNaN
    }

    assertTrue(f(Float.NaN))

    assertFalse(f(Float.PositiveInfinity))
    assertFalse(f(Float.NegativeInfinity))
    assertFalse(f(1f / 0))
    assertFalse(f(-1f / 0))
    assertFalse(f(0f))
    assertFalse(f(3f))
    assertFalse(f(-1.5f))
  }

  @Test def intBitsToFloat(): Unit = {
    def isZero(v: Float, neg: Boolean): Boolean = {
      (v == 0.0f) && (1 / v == (
          if (neg) Float.NegativeInfinity
          else Float.PositiveInfinity))
    }

    import JFloat.{intBitsToFloat => f}

    // Specials
    assertEquals(Float.PositiveInfinity, f(0x7f800000), 0.0f)
    assertEquals(Float.NegativeInfinity, f(0xff800000), 0.0f)
    assertTrue(isZero(f(0x00000000), false))
    assertTrue(isZero(f(0x80000000), true))
    assertTrue(f(0x7fc00000).isNaN) // canonical NaN

    // Non-canonical NaNs
    assertTrue(f(0x7f800001).isNaN) // smallest positive NaN
    assertTrue(f(0x7f915ab5).isNaN) // an arbitrary positive NaN
    assertTrue(f(0x7fffffff).isNaN) // largest positive NaN
    assertTrue(f(0xff800001).isNaN) // smallest negative NaN
    assertTrue(f(0xff915ab5).isNaN) // an arbitrary negative NaN
    assertTrue(f(0xffffffff).isNaN) // largest negative NaN

    // Normal forms
    assertEquals(1.17549435e-38f, f(0x00800000), 0.0f)  // smallest pos normal form
    assertEquals(3.4028234e38f, f(0x7f7fffff), 0.0f)    // largest pos normal form
    assertEquals(1.53376384e8f, f(0x4d124568), 0.0f)    // an arbitrary pos normal form
    assertEquals(-1.17549435e-38f, f(0x80800000), 0.0f) // smallest neg normal form
    assertEquals(-3.4028234e38f, f(0xff7fffff), 0.0f)   // largest neg normal form
    assertEquals(-1.53376384e8f, f(0xcd124568), 0.0f)   // an arbitrary neg normal form

    // Subnormal forms
    assertEquals(Float.MinPositiveValue, f(0x00000001), 0.0f)  // smallest pos subnormal form
    assertEquals(1.1754942e-38f, f(0x007fffff), 0.0f)          // largest pos subnormal form
    assertEquals(1.1421059e-38f, f(0x007c5d44), 0.0f)          // an arbitrary pos subnormal form
    assertEquals(-Float.MinPositiveValue, f(0x80000001), 0.0f) // smallest neg subnormal form
    assertEquals(-1.1754942e-38f, f(0x807fffff), 0.0f)         // largest neg subnormal form
    assertEquals(-1.1421059e-38f, f(0x807c5d44), 0.0f)         // an arbitrary neg subnormal form
  }

  @Test def floatToIntBits(): Unit = {
    import JFloat.{floatToIntBits => f}

    // Specials
    assertEquals(0x7f800000, f(Float.PositiveInfinity))
    assertEquals(0xff800000, f(Float.NegativeInfinity))
    assertEquals(0x00000000, f(0.0f))
    assertEquals(0x80000000, f(-0.0f))
    assertEquals(0x7fc00000, f(Float.NaN)) // canonical NaN

    // Normal forms
    assertEquals(0x00800000, f(1.17549435e-38f))  // smallest pos normal form
    assertEquals(0x7f7fffff, f(3.4028234e38f))    // largest pos normal form
    assertEquals(0x4d124568, f(1.53376384e8f))    // an arbitrary pos normal form
    assertEquals(0x80800000, f(-1.17549435e-38f)) // smallest neg normal form
    assertEquals(0xff7fffff, f(-3.4028234e38f))   // largest neg normal form
    assertEquals(0xcd124568, f(-1.53376384e8f))   // an arbitrary neg normal form

    // Subnormal forms
    assertEquals(0x00000001, f(Float.MinPositiveValue))  // smallest pos subnormal form
    assertEquals(0x007fffff, f(1.1754942e-38f))          // largest pos subnormal form
    assertEquals(0x007c5d44, f(1.1421059e-38f))          // an arbitrary pos subnormal form
    assertEquals(0x80000001, f(-Float.MinPositiveValue)) // smallest neg subnormal form
    assertEquals(0x807fffff, f(-1.1754942e-38f))         // largest neg subnormal form
    assertEquals(0x807c5d44, f(-1.1421059e-38f))         // an arbitrary neg subnormal form
  }

  @Test def isFinite(): Unit = {
    assertFalse(JFloat.isFinite(Float.PositiveInfinity))
    assertFalse(JFloat.isFinite(Float.NegativeInfinity))
    assertFalse(JFloat.isFinite(Float.NaN))
    assertFalse(JFloat.isFinite(1f/0))
    assertFalse(JFloat.isFinite(-1f/0))

    assertTrue(JFloat.isFinite(0f))
    assertTrue(JFloat.isFinite(1f))
    assertTrue(JFloat.isFinite(123456f))
    assertTrue(JFloat.isFinite(Float.MinValue))
    assertTrue(JFloat.isFinite(Float.MaxValue))
    assertTrue(JFloat.isFinite(Float.MinPositiveValue))
  }

  @Test def testStaticHashCode(): Unit = {
    assumeFalse("Hash codes for doubles are different in JS than on the JVM",
        executingInJVM)

    def test(x: Float, expected: Int): Unit =
      assertEquals(expected, JFloat.hashCode(x))

    test(0.0f, 0)
    test(-0.0f, -2147483648)
    test(1234.0f, 1234)
    test(1.5f, 1073217536)
    test(-54.0f, -54)

    test(Float.MinPositiveValue, 916455424)
    test(Float.MinValue, 670040063)
    test(Float.MaxValue, -1477443585)

    test(Float.NaN, 2146959360)
    test(Float.PositiveInfinity, 2146435072)
    test(Float.NegativeInfinity, -1048576)
  }

  // The following tests are only to make sure that things link

  @Test def sum(): Unit = {
    assertEquals(12f, JFloat.sum(5f, 7f), 0f)
  }

  @Test def max(): Unit = {
    assertEquals(7f, JFloat.max(5f, 7f), 0f)
  }

  @Test def min(): Unit = {
    assertEquals(5f, JFloat.min(5f, 7f), 0f)
  }
}
