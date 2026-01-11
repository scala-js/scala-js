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

package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

/** Tests exercising the various encoding strategies for constant arrays.
 *
 *  See `CoreJSLib.defineConstantArrayMakers()` for details.
 */
class ConstantArrayTest {
  import ConstantArrayTest._

  /** Tests that some elements of an array are what they should be.
   *
   *  We avoid `assertArrayEquals` on purpose, or anything else that could
   *  create a double-mistake situation that cancels out.
   */
  @noinline
  def testSomeElems[A](xs: Array[A], expectedLength: Int)(elems: (Int, A)*): Unit = {
    assertEquals(expectedLength, xs.size)
    for ((i, elem) <- elems)
      assertEquals(s"at index $i", elem, xs(i))
  }

  @Test def byteArrayRaw(): Unit = {
    // All the elements must be actual Bytes for the compiler to optimize as an ArrayValue
    val xs: Array[Byte] = Array(
        -30.b, 34.b, -83.b, 74.b, 35.b, -30.b, -22.b, -117.b, 63.b, 16.b, 88.b,
        -32.b, 72.b, -70.b, -127.b, -86.b, 83.b, -21.b, 80.b, -71.b, -108.b,
        6.b, 28.b, 46.b, 42.b, 91.b, -69.b, 76.b, 3.b, 69.b, -12.b, -122.b,
        69.b, 83.b, -91.b, 68.b, 95.b, 25.b)

    testSomeElems(xs, 38)(
      0 -> -30,
      1 -> 34,
      6 -> -22,
      10 -> 88,
      37 -> 25
    )
  }

  @Test def shortArrayRaw(): Unit = {
    // All the elements must be actual Shorts for the compiler to optimize as an ArrayValue
    val xs: Array[Short] = Array(
        8707.s, -32254.s, 26549.s, -3652.s, 10929.s, -10006.s, -2057.s,
        10176.s, -15373.s, -5342.s, -13741.s, 11917.s, 28981.s, 3600.s,
        -32052.s, -32096.s, 7093.s, -17013.s, -13941.s, -9259.s, -11140.s,
        26818.s, 24562.s, 7908.s, 16779.s, -22180.s, -3344.s, 25390.s, 10835.s,
        18656.s, 16022.s, 4282.s, -10390.s, -18345.s, 22250.s, 23161.s,
        14716.s)

    testSomeElems(xs, 37)(
      0 -> 8707,
      1 -> -32254,
      6 -> -2057,
      10 -> -13741,
      36 -> 14716
    )
  }

  @Test def charArrayRaw(): Unit = {
    val xs: Array[Char] = Array(
        '\ude30', '\u29fb', '\u98d1', '\u0b7f', '\u5e62', '\u8f35', '\u4858',
        '\u642a', '\uc0d7', '\u77f6', '\ub0e0', '\u6aa7', '\u1a08', '\u1560',
        '\u78b3', '\u2286', '\u19e4', '\u0770', '\ua81f', '\u82f5', '\ua4f6',
        '\ue94f', '\ue134', '\u4d2e', '\u5bc5', '\ucee8', '\ucfcb', '\u5630',
        '\ub47e', '\u87ba', '\u15ca', '\u37b1', '\u8ad0', '\ubaee', '\u37a7',
        '\u9c52', '\u4a17', '\u0949', '\uc09a', '\ua645', '\u3fce'
    )

    testSomeElems(xs, 41)(
      0 -> '\ude30',
      1 -> '\u29fb',
      6 -> '\u4858',
      10 -> '\ub0e0',
      40 -> '\u3fce'
    )
  }

  @Test def intArrayRaw(): Unit = {
    val xs: Array[Int] = Array(
        -227399637, 2011237127, 1697043722, -612460322, 372891359, -1706904192,
        -465102836, 91588223, -488927274, 800813188, -1005538152, 897642319,
        -2032192123, 751990757, -273976263, -892733973, -831818136, 810070132,
        2065957472, -520998968, 103970121, -608539397, -533725119, 1738470723,
        1445257665, 73666159, 834990842, 1960910582, 1831275775, -1668025498,
        900558842, -741686196, -931395896, -1878562106, -1877333088,
        -1685467305, 173475518, 1192730047, 1021496866, 158824928, 1665377764,
        -1451842310
    )

    testSomeElems(xs, 42)(
      0 -> -227399637,
      1 -> 2011237127,
      6 -> -465102836,
      10 -> -1005538152,
      41 -> -1451842310
    )
  }

  @Test def longArrayRaw(): Unit = {
    val xs: Array[Long] = Array(
        4540950791249856447L, 9086483257025641746L, -1789067218867760531L,
        2177903297296649795L, 2879330872772524503L, 8077834881349761888L,
        -2855680043296469388L, 3243621517113502730L, 6683973210411714713L,
        2909515105351681566L, 3679392946828157195L, -5786730716041966993L,
        3592897934268063446L, -6773833663431554892L, -5637263264372920740L,
        1806082478410260164L, 4578896761148806576L, -9092289643932436736L,
        3012176357354330660L, -6167112132848567793L, 3534160124614388512L,
        8651303251409577936L, -6137441487777667531L, 322029710798097292L,
        -9027713890840934287L, -609674473011265685L, 887940347180851444L,
        3865950873111540521L, -7884530694354675517L, -2747149101397251575L,
        -2267319450482826277L, -2454110733448354807L, -5695520160295161975L,
        8939085496925873750L, 8257631973351285240L
    )

    testSomeElems(xs, 35)(
      0 -> 4540950791249856447L,
      1 -> 9086483257025641746L,
      6 -> -2855680043296469388L,
      10 -> 3679392946828157195L,
      34 -> 8257631973351285240L
    )
  }

  @Test def intArrayUVals(): Unit = {
    val xs: Array[Int] = Array(
        473, 10, 980, 1184019, 203129, 20104, 46, 158558375, 1, 268270758, 1,
        121764, 79829, 235, 3, 27574, 664, 5792353, 120594, 16, 1, 349602436,
        838570, 1976, 2870, 6575304, 90182733, 61, 732, 30580, 6125, 9, 27,
        274538451, 15011088, 53, 1481321528, 6, 123616, 19111, 1152, 96318,
        1537589989, 642654, 2, 93162055, 786105, 333582054, 44520, 127176,
        52199
    )

    testSomeElems(xs, 51)(
      0 -> 473,
      1 -> 10,
      7 -> 158558375,
      10 -> 1,
      50 -> 52199
    )
  }

  @Test def intArraySVals(): Unit = {
    val xs: Array[Int] = Array(
        42735868, -1993754, 152454578, 0, -797455942, -11921, -330, -1981884,
        0, -4084, -83, 372885229, -145463112, -7858271, -2, 4, 107599019,
        12072, 3431967, 30, 17661, -85977, -685808, 28680, -3, 106746, 56651,
        -46340072, -2, -11170, -164738, -1, 94078958, -276, 14, 86211705, 0, 1,
        -7, -273499033, 120596335, 12742, -10809745, -3684, -240, 615815082,
        12922047, 5, -13073620
    )

    testSomeElems(xs, 49)(
      0 -> 42735868,
      1 -> -1993754,
      7 -> -1981884,
      10 -> -83,
      47 -> 5,
      48 -> -13073620
    )
  }

  @Test def intArrayUDiffs(): Unit = {
    val xs: Array[Int] = Array(
        431, 437, 532, 1001, 1314, 1506, 1588, 1715, 2184, 2310, 2322, 2435,
        2593, 3043, 3265, 3388, 3511, 3969, 4458, 4552, 4935, 5421, 5881, 5918,
        6395, 6638, 6797, 6931, 7037, 7473, 7673, 8156, 8358, 8423, 8900, 8920,
        9371, 9835, 10237, 10546, 10997, 11112, 11286, 11567, 12045
    )

    testSomeElems(xs, 45)(
      0 -> 431,
      1 -> 437,
      7 -> 1715,
      10 -> 2322,
      43 -> 11567,
      44 -> 12045
    )
  }

  @Test def intArraySDiffs(): Unit = {
    val xs: Array[Int] = Array(
        213, 342, -52, -511, -643, -186, -557, -532, -579, -545, -468, -83, -7,
        -486, -987, -872, -1127, -643, -744, -275, -597, -708, -649, -501,
        -725, -1063, -592, -415, -251, 9, 371, 87, -333, -98, -232, -502, -932,
        -1313, -1058, -1261, -1445, -1768, -2249, -1839, -2225
    )

    testSomeElems(xs, 45)(
      0 -> 213,
      1 -> 342,
      7 -> -532,
      10 -> -468,
      43 -> -1839,
      44 -> -2225
    )
  }

  @Test def longArrayUVals(): Unit = {
    val xs: Array[Long] = Array(
        15237023850391754L, 3624L, 3148606L, 242981697L, 236566L,
        3070708282666L, 27254407759L, 115160157377L, 200L, 4L, 4477746712303L,
        3620920387263L, 909450894512109L, 36409L, 1642883064429635L,
        391852203431952L, 15345672567L, 53434217293575321L, 135780975260937L,
        38834741L, 0L, 300536311L, 8699L, 522329785250569772L,
        1045990580850127812L, 434284847765579269L, 522456601306589L,
        6200076636194180700L, 473051132951L, 5L, 13L, 55003331L, 3687567L,
        13252671L, 710482723099L, 350L, 4017206L, 1245449241211101881L,
        128284L, 250775715064890168L, 1L, 1288658L, 6651051537405220L, 1L,
        16956385L
    )

    testSomeElems(xs, 45)(
      0 -> 15237023850391754L,
      1 -> 3624L,
      7 -> 115160157377L,
      10 -> 4477746712303L,
      43 -> 1L,
      44 -> 16956385L
    )
  }

  @Test def longArraySVals(): Unit = {
    val xs: Array[Long] = Array(
        11L, -26239413L, -197783244831L, 64704640238L, -2104L,
        -8867972175578983931L, -8L, -245756476L, -7721997428L,
        185688314605282L, -269162703201L, 931L, -9870446511898059L,
        18927466245257473L, -74046L, 21280957L, -2349000319L, 2920L, -1L,
        -6901246L, 2815004470303L, 11082899465991L, -2L, 4L,
        -489791423341575221L, 95809630L, -728460893L, 1248239032L, 239983L,
        -78323286294L, 183233L, 13L, -42L, 7L, 979L, -64188985580202L,
        -8404275L, 567872888957546136L, 360740409L, -25834101909700L,
        -3481903L, -88714L, -480012068153681698L, -11855L, 23L,
        -266265449842954L, 1021817858690427132L, 87823879L
    )

    testSomeElems(xs, 48)(
      0 -> 11L,
      1 -> -26239413L,
      7 -> -245756476L,
      10 -> -269162703201L,
      46 -> 1021817858690427132L,
      47 -> 87823879L
    )
  }

  @Test def longArrayUDiffs(): Unit = {
    /* Due to how we compute diffs, i.e., by chunks of 32 bits, the xDiffs
     * stategies for Longs are very artificial, and are unlikely to happen in
     * practice.
     */
    val xs: Array[Long] = Array(
        0x34a00000160L, 0x495000003b6L, 0x72c00000557L, 0x98a0000089fL,
        0xb51000009edL, 0xe2e00000d30L, 0x102500000f6fL, 0x11a100001164L,
        0x15010000130cL, 0x1687000015d7L, 0x187d00001698L, 0x1b6800001a60L,
        0x1d5800001bfaL, 0x1f2800001e98L, 0x215a00001f5fL, 0x2484000022d5L,
        0x25ab0000259fL, 0x271e000026dbL, 0x29a2000027d8L, 0x2aab000029d4L,
        0x2e1100002c53L, 0x2f7900002f10L, 0x302100003018L, 0x30f500003021L,
        0x3288000031a2L, 0x34d800003475L, 0x35b1000034eeL, 0x394800003774L,
        0x3d0700003b2cL, 0x3e9500003da6L, 0x41130000407aL, 0x440a0000430dL,
        0x4635000045b8L, 0x49690000478aL, 0x4b77000049d6L, 0x4ce300004bc3L,
        0x4edb00004e61L, 0x519200004fddL, 0x545200005275L, 0x55f50000556fL,
        0x561700005610L, 0x58a7000056d7L, 0x5c4800005a76L
    )

    testSomeElems(xs, 43)(
      0 -> 0x34a00000160L,
      1 -> 0x495000003b6L,
      7 -> 0x11a100001164L,
      10 -> 0x187d00001698L,
      41 -> 0x58a7000056d7L,
      42 -> 0x5c4800005a76L
    )
  }

  @Test def longArraySDiffs(): Unit = {
    /* Due to how we compute diffs, i.e., by chunks of 32 bits, the xDiffs
     * stategies for Longs are very artificial, and are unlikely to happen in
     * practice.
     */
    val xs: Array[Long] = Array(
        0x5L, 0x800000026L, 0x2a900006d57L, 0xc0e48000c0e57L, 0xc11d7000c120aL,
        0xc1242001720faL, 0x177eab00177fd6L, 0x1f2b5300200b45L,
        0x200d061f5084beL, 0x1f5089f422a51641L, 0x3e5b5b503f1b5f38L,
        0x3f2ab7753f2ab787L, 0x3f2aed1f3f2ea749L, 0x3f2f151a3f2f5487L,
        0x884363f5b1a432baL, 0xda18ce7add629baaL, 0xdd629db1dd629e2aL,
        0xdd90e9a3dd90e9aaL, 0xdd90e9dedd90e9e2L, 0xdd913ab5dd913abcL,
        0xdd913f32dd915704L, 0xe2573aefe263b7ddL, 0xe263b7e4e263b9c1L,
        0xe263b9cd96d706caL, 0x8275fbc08275fbeL, 0x3569c45f3594251fL,
        0x3594253035942556L, 0x3594257535c82945L, 0x36000dbe360062a1L,
        0x5709c9ffc04aa641L, 0xc094d506c097aebeL, 0xc097af16c146c40aL,
        0xc146c40ccbe1074dL, 0xd140ab9fd140ad69L, 0xd14112cdd4afb843L,
        0xd4afc68cd4afc6ccL, 0xd4b00377d4b0542fL, 0xd4b3aff3d4b3b0adL,
        0xd4c88675d4c88681L, 0xd4f7b9d3c419dd5dL, 0xc419e244c41d7541L,
        0xc41d7545c50d05deL, 0xc735f672c7b59abcL, 0x4051f9734078c5baL,
        0x4078c5e84078c63eL, 0x4078e5814078e842L, 0x407ae4e24194e856L,
        0x4194ecf54194eebbL, 0x41987f774234993fL, 0x423523da428f9676L,
        0x428f967a42af6005L, 0x42c1cdbb42c1cef3L, 0x46130ac846134de7L
    )

    testSomeElems(xs, 53)(
      0 -> 0x5L,
      1 -> 0x800000026L,
      7 -> 0x1f2b5300200b45L,
      10 -> 0x3e5b5b503f1b5f38L,
      51 -> 0x42c1cdbb42c1cef3L,
      52 -> 0x46130ac846134de7L
    )
  }
}

object ConstantArrayTest {
  private implicit class IntExt(private val x: Int) extends AnyVal {
    @inline def b: Byte = x.toByte

    @inline def s: Short = x.toShort
  }
}
