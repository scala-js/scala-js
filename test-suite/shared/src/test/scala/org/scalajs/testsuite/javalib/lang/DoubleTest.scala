/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import java.lang.{Double => JDouble}

import scala.util.Try

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class DoubleTest {

  @Test def proper_equals(): Unit = {
    assertTrue(0.0.equals(0.0))
    assertTrue((-0.0).equals(-0.0))
    assertFalse(0.0.equals(-0.0))
    assertTrue(Double.NaN.equals(Double.NaN))
  }

  @Test def hashCodeTest(): Unit = {
    def hashCodeNotInlined(x: Any): Int = {
      var y = x // do not inline
      y.hashCode
    }

    def test(x: Double, expected: Int): Unit = {
      assertEquals(expected, x.hashCode)
      assertEquals(expected, hashCodeNotInlined(x))
    }

    if (!executingInJVM) {
      test(0.0, 0)
      test(-0.0, -2147483648)
      test(1234.0, 1234)
      test(1.5, 1073217536)
      test(Math.PI, 340593891)
      test(-54.0, -54)

      test(Double.MinPositiveValue, 1)
      test(Double.MinValue, 1048576)
      test(Double.MaxValue, -2146435072)

      test(Double.NaN, 2146959360)
      test(Double.PositiveInfinity, 2146435072)
      test(Double.NegativeInfinity, -1048576)
    }
  }

  @Test def toString_with_integer_values_when_an_integer(): Unit = {
    if (executingInJVM) {
      assertEquals("0.0", 0.0.toString)
      assertEquals("-0.0", (-0.0).toString)
    } else {
      assertEquals("0", 0.0.toString)
      assertEquals("0", (-0.0).toString)
    }
    assertEquals("NaN", Double.NaN.toString)
    assertEquals("Infinity", Double.PositiveInfinity.toString)
    assertEquals("-Infinity", Double.NegativeInfinity.toString)
    if (executingInJVM) {
      assertEquals("5.0", 5.0.toString)
      assertEquals("-5.0", (-5.0).toString)
    } else {
      assertEquals("5", 5.0.toString)
      assertEquals("-5", (-5.0).toString)
    }
    assertEquals("1.2", 1.2.toString)
  }

  @Test def toHexStringTest(): Unit = {
    import java.lang.Double.toHexString

    assertEquals("0x0.0p0", toHexString(0.0))
    assertEquals("-0x0.0p0", toHexString(-0.0))
    assertEquals("NaN", toHexString(Double.NaN))
    assertEquals("Infinity", toHexString(Double.PositiveInfinity))
    assertEquals("-Infinity", toHexString(Double.NegativeInfinity))
    assertEquals("0x1.0p0", toHexString(1.0))
    assertEquals("-0x1.0p0", toHexString(-1.0))
    assertEquals("0x1.0p1", toHexString(2.0))
    assertEquals("0x1.8p1", toHexString(3.0))
    assertEquals("0x1.0p-1", toHexString(0.5))
    assertEquals("0x1.0p-2", toHexString(0.25))
    assertEquals("0x1.00204p3", toHexString(8.003936767578125))
    assertEquals("0x0.00204p-1022", toHexString(1.094949828138e-311))
    assertEquals("0x1.fffffffffffffp1023", toHexString(Double.MaxValue))
    assertEquals("0x1.0p-1022", toHexString(java.lang.Double.MIN_NORMAL))
    assertEquals("0x0.fffffffffffffp-1022", toHexString(2.225073858507201E-308))
    assertEquals("0x0.0000000000001p-1022", toHexString(Double.MinPositiveValue))
  }

  @Test def should_parse_strings(): Unit = {
    // scalastyle:off line.size.limit

    /* First, a selection of large categories for which test the combination of
     * - paddings
     * - entry points to the API
     * on a moderate set of test inputs.
     */

    for (padding <- List("", "  ", (0 to 0x20).map(x => x.toChar).mkString)) {
      def pad(s: String): String = padding + s + padding

      def testFull(s: String, v: Double): Unit = {
        val s2 = pad(s)
        val r1 = JDouble.parseDouble(s2)
        assertTrue(s"""Double.parseDouble("$s2") must be $v, was $r1""",
            r1.equals(v))
        val r2 = JDouble.valueOf(s2)
        assertTrue(s"""Double.valueOf("$s2") must be $v, was $r2""",
            r2.equals(v))
        val r3 = new JDouble(s2)
        assertTrue(s"""new Double("$s2") must be $v, was $r3""",
            r3.equals(v))
      }

      // Specials
      testFull("NaN", Double.NaN)
      testFull("Infinity", Double.PositiveInfinity)
      testFull("-Infinity", Double.NegativeInfinity)

      // Decimal notation
      testFull("0.0", 0.0)
      testFull("-0.0", -0.0)
      testFull("0", 0.0)
      testFull("5.3", 5.3)
      testFull("127e2", 12700.0)
      testFull("127E-2", 1.27)
      testFull("1E+1", 10)
      testFull("-123.4", -123.4)
      testFull("65432.1", 65432.10)
      testFull("-87654.321", -87654.321)
      testFull("+.3f", 0.3)

      // Hex notation, with exactly the output of toHexString()
      testFull("0x0.0p0", 0.0)
      testFull("-0x0.0p0", -0.0)
      testFull("0x1.0p0", 1.0)
      testFull("-0x1.0p0", -1.0)
      testFull("0x1.0p1", 2.0)
      testFull("0x1.8p1", 3.0)
      testFull("0x1.0p-1", 0.5)
      testFull("0x1.0p-2", 0.25)
      testFull("0x1.00204p3", 8.003936767578125)
      testFull("0x0.00204p-1022", 1.094949828138e-311)
      testFull("0x1.fffffffffffffp1023", Double.MaxValue)
      testFull("0x1.0p-1022", java.lang.Double.MIN_NORMAL)
      testFull("0x0.fffffffffffffp-1022", 2.225073858507201E-308)
      testFull("0x0.0000000000001p-1022", Double.MinPositiveValue)
    }

    /* Then, a larger set of input strings, which we only test without padding
     * and with only one entry point.
     */

    def test(s: String, v: Double): Unit = {
      val r = JDouble.parseDouble(s)
      assertTrue(s"""Double.parseDouble("$s") must be $v, was $r""",
          r.equals(v))
    }

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
     *     "0x" + timesRand(0, 50, randHexDigit) + "." +
     *     timesRand(0, 50, randHexDigit) + "p" +
     *     (if (Random.nextBoolean()) "-" else "") +
     *     timesRand(1, 4, randDecDigit)
     *   }
     *   for (_ <- 0 until 100) {
     *     val input = randInput()
     *     println(s"""    test("$input", ${java.lang.Double.parseDouble(input)})""")
     *   }
     */

    test("0x3bcc9c4f57.f9a4f7c38944030572c19c235c9c7fa195p4351", Double.PositiveInfinity)
    test("0x5a5e28727048d5fc7a412e24b8d43eb896e97aa35cd3.63511582434bd029b800daf1a050abb7de2p882", Double.PositiveInfinity)
    test("0x29087f7ee0bd6aad502b60de6dfd8793.43f94d82f523d1752400e968cap1", 1.090849442407432E38)
    test("0x.c22e399bp45", 2.6687973842944E13)
    test("0x79b19c420.p-714", 3.790455421249137E-205)
    test("0xb0ce07bedeccdee4ea23651efe67375c.dfee4d87p510", 7.877552996709097E191)
    test("0xc1bde6a227c610729.18838356d2d80db2bf863b3f8b861faf4p5", 7.147810818070931E21)
    test("0x0b87187c64b5b3716.581fc68p3", 1.063244260886767E20)
    test("0x18d9a8b51232ac492f546528d14e47cc17d6a5010f2559a474.c27bfadc62e1fe00e23cdeep-74", 8.257905526970714E36)
    test("0x020151c74df677121e69301eeb602.76p-9618", 0.0)
    test("0xe536c72c7c.993a58b4ca9e979c9p680", 4.938533550967572E216)
    test("0xcfd63cf55ba4baae14e00.e25e0e6ad14d0079590d8f3d4a23757ae2dp-41", 7.141220395741147E12)
    test("0xcd2bf6a48f5b9a419e911332e103fe2bd75.34774cfd3db6065c14652acc6d45e23228bccd8801p-8451", 0.0)
    test("0x58abe6d66a0086f85cbe.7d4p0994", Double.PositiveInfinity)
    test("0x007c6c67312cdb3c3303800836c80b62943e836.7b74087bffp-3255", 0.0)
    test("0x62263ea3ba241e0bdce84d7d6936861c81.00927d354639e8360ee01edp2", 1.3359403146110964E41)
    test("0x49002fa5938.b54da0c70b9e77ef171e402d359ec72cp39", 2.7578894924960133E24)
    test("0xdd88d292163fbcf8a089b097f7275b7e5b42303c7c43.2a406a51865909e0a3070b07389b0d0690p-42", 1.8846067860572248E40)
    test("0x833163291091d66a87d44b296ba97f8977a32a9a5eda831bae.10796333823419a154ee0d55p7", 1.0540944659455885E62)
    test("0x6a7302a49cc8a6773b9dcb87f54179.15eee40f51c953beffc3cdf4e0e412ab793b67f7p-1", 2.7635807675525916E35)
    test("0xdf8b942d99aa42a96e9748b9fe5cea8e1.0b9ecaaf5985dd3984b79b38265d7b7e2a7ee7f6ed57b81p-4", 2.9714257773281826E38)
    test("0xad82fedb784e34be1b5bf0dc5cca18bdbba36311c1c6636e.fbd56a6f30cf3c943e89ecp8322", Double.PositiveInfinity)
    test("0x6328d473cc9bf5bae48aa4e37e3e2660865cdf8d.a9c05efad8b3ddf34134efce48921ffc19b31f4f2bee3p-428", 8.166867110480941E-82)
    test("0x3af268e735192f7a3733.90798aa580d3d6504053df967e554760938ea85823eadp9", 1.425248906962734E26)
    test("0xa2368682d42e4408e1.7706c0e74aeaba33a0baff87e22e3acd191211e2f9f59p-4738", 0.0)
    test("0x783e5869aea0daffecb432b6de1bc40226c3568fd2edc5139.7341a4d9d452e7c8345b023477e6d05919f7689bba1e9a7p-6", 7.370907344617178E56)
    test("0xfe5bb4c989c1a259aeda07111f.6cd2be7ef12p60", 2.3234060358298473E49)
    test("0xdd33a181f84bf67207cb05998997d54fd1.p06", 4.817346068045414E42)
    test("0x77a7f7e2a8.4d3de3b5f195p39", 2.8253003987295953E23)
    test("0x5de55cbb5787d.3d66f01p-14", 1.0082000456588264E11)
    test("0x67a4b03a69d77f5f66395c.2p8187", Double.PositiveInfinity)
    test("0xf28d5c5.42643b02820bf9333e33e60f66p715", 4.3838129311043347E223)
    test("0xc30a63276908dbe614c73950569ab952722814fd5d2d.afp-9102", 0.0)
    test("0xdc2de1a4e4d938f2652a5262b3b6361937c533.bc81d6feab43adb2dd65c97c466d5d2ef6d263595p231", 1.6944342110198636E115)
    test("0x6755.4f9bee8bp95", 1.0479236104077999E33)
    test("0xc3be28219dcc00557aaaae8c056e7416f0ed13db4.9e9cb950p-790", 2.745797751832513E-189)
    test("0xa39f4cc8c01885613c9c.cfedda7b9278e19a96834742f4bp-6188", 0.0)
    test("0x2488f0a650e.38dbf866364d34bfcaf93dd954e9p-03", 3.138325904977778E11)
    test("0x19d580936.17db26207be41de75a4d508cfbp3", 5.54780778727455E10)
    test("0xcc860b8f.258d4dfc274ee61cf0ae5p6757", Double.PositiveInfinity)
    test("0xab.fp64", 3.171687059173436E21)
    test("0x412d914b7c98153161f4a600e4da152a.57b5e3d6e2847e670f9ac808c1ef90fee196356p-20", 8.262292863342124E31)
    test("0x0112e744ac19b8a158c4712d39f5ae9ff254f9dab9082.67b1e05b429ec492fc873p2628", Double.PositiveInfinity)
    test("0x4124a6dc2f89206034099fcad671552f78e6.21351c7e6e8582c60285ap8", 1.452741259747921E45)
    test("0x6f6945546e2800a514fc.faabf4c000f4a6fcd089cb925da85a4cc91fcf3e2d6fd81287p7", 6.734394668355518E25)
    test("0x36a895940260d6055175338e411db0c.5c5fa36p5", 1.4530730290030868E38)
    test("0x322be50cbd1f8130319a61db6d004164f288673bc13c0305.3a77e1de3e9ab9870e8810838fc52a3a1fp-22", 2.9330270580017486E50)
    test("0x664cc5afa7473f34fbbcf3.c172e182p-8", 4.830975785903766E23)
    test("0xadc638d3d1300cc8f48b9.77ba213b7704311bf91913536df8cce03f9aa8f11e79cc3p-1", 6.565007613676269E24)
    test("0xea09c946afe882d2e9ba28f9f3bb8f6.34af0p-6821", 0.0)
    test("0xbf476a9c79896b2d0cddd0fdeb8d965e4c56c0348802611f5.6c53c3p-5", 2.3450733549226435E57)
    test("0xc59f164c0ee8495143225ce5.268820f13cp3680", Double.PositiveInfinity)
    test("0x6d1eaee6d1e5c0251f1885371.764d29da90690a9b78bc45edbp-441", 9.515580739963098E-104)
    test("0x7d3419f95aefe95444d4c48.5bd1275ab00b3ac92e217da51542e089ca80c7634ap4224", Double.PositiveInfinity)
    test("0x08d4a636.c721dbfbba482d510d1b7366p-053", 1.6448388293384176E-8)
    test("0x36beea0aa7cc593a23f47ecd401b38fc5b9bd4ed2.6c14d1c991f72p7", 6.400881277193871E50)
    test("0xbafc02016adba217f7d5e10fe938ca2a96bb4e26a38eb8b6.25p0", 4.584844284745914E57)
    test("0xe0.76e5af7fb57213c8be7796434496p00", 224.46444222324266)
    test("0x6b7334ad12263.90c9a635c2c8208b942fc1913199cp3", 1.5122246293590812E16)
    test("0x3.e98c39dd610752033bab414c78p-450", 1.345654750738825E-135)
    test("0xc2a17805e94ae.f6ef0a0ffe5703112872a5p-67", 2.3201791960838845E-5)
    test("0xd4af.7b7a572f663p-08", 212.68547787312073)
    test("0xef7007bab0a3eea.61e7119bd29653a81c31ef1e263d1b9p9", 5.521055574676869E20)
    test("0xdbce4587c9c.b77b2c4d92daacd9e393dc98f6bp-0", 1.5104936017052717E13)
    test("0xfbbc30cf74818.2fd1f12cc035c03dfca5830d50164cb79b868c87p8", 1.13371419166953267E18)
    test("0x149bf7.787b98fd06ee25d8e270518ep-6", 21103.866728686717)
    test("0xca2f5e9d6b2f7a391417fc49583eabe7ac309.54f03855fd29b1ef1a0p86", 2.180359130716814E70)
    test("0xfbd176f0e5c8e17760821b27.03f0f39433p-10", 7.6107387348123395E25)
    test("0xd4e9c94d3ddda786d3f154698d2360fffa8253c805cb.3e75dcf8a2cbb9bd556bbf81c223b3531b9098416f6p-9370", 0.0)
    test("0x2b37a4d81db15847db688b5bd807f334de1a2f6d6b9bdc3.70204da9114c1c76c1p979", Double.PositiveInfinity)
    test("0x746.8e9f4093d3d737b1b6670p-015", 0.05684073234547659)
    test("0xe0afae54cbd689d3e1068.a5ce3ead8c8e9ca3d1c10365da3afeab99dc4f39p-74", 898.7450153341871)
    test("0xa83d02b83427292.bd7da68df2p7068", Double.PositiveInfinity)
    test("0x643bdb8b5900d5eeb064c89ad09e286.4762c36b028afc237dab44bad9819713ba5c41b0fp-95", 2.102055534184586E8)
    test("0x1acf3001c46eb5908cc67d6c87d0192.bca7427dfdf814b6e4a63f54cd51aa3cffa14ad5p-978", 8.718256412074298E-259)
    test("0x7657db92eaaa.92902feb89bc1a352e52b917c3b7cfe4dp5", 4.1638309782705465E15)
    test("0x575c272512a00bd2f8d1a14f280d0987f7c53e12d21.f30p-9", 3.989898204914359E48)
    test("0x30398ddf709b895ce4.1b0ca907d7a1a69061b8c6abcp-86", 1.149769331391146E-5)
    test("0xc63f6429eb3f8491854.8060a9d4e97d13c1a0cd68312e82dca5605e2589ef8d720dfep-258", 1.2633067328500596E-55)
    test("0x7b1a87c5c9508.d4196605914de3e310df9696ebf99c048929a3efe9e1e4de2p194", 5.437632369872385E73)
    test("0xbed724343613.8746f2f7edf13abf35ea02ae381bfd433758f9d8015deee07p-2", 5.245780866189288E13)
    test("0x753dde30cf11b0073.f95e0330b0353fd9219d602ccf6p-93", 1.364872693205332E-8)
    test("0x0a005b9d1e9b0c8c461900ed564d93c8.5fp8072", Double.PositiveInfinity)
    test("0x09fff743267.22ffa43ca933345fd786cd8196cf9e82fb7c49dd5599b5fdp104", 1.3937779918868934E43)
    test("0x937255dff8ec78da95ac1d9.1d1e12373360ep19", 1.4952863996276116E33)
    test("0xbed0f063058a8370898708f730dac9135a0a0.bd157546ef4701eda08dd55ca9612a94dcep7023", Double.PositiveInfinity)
    test("0xa95f3cc152bbcf98.c78d78aec3p800", 8.138005097516387E259)
    test("0x5189a6e80fff17ffd7a10bcf873ba90aa684545104.f8p-5905", 0.0)
    test("0xd0c23d1d739.14af693c2da7f6272fbba85p3144", Double.PositiveInfinity)
    test("0xb904b6c4f518c7.dc2e488212f140253073da2180b7e6b74d6336p-754", 5.495889783734122E-211)
    test("0x99826bcb8bfd8a22e0765ee7fcefd57e3.798eefea2dbdadp-44", 1.8558154651252258E26)
    test("0xdaa946f53a4e18b6d1b07190aca94ad13374d5134.631e65342f4d6bdc9p-4930", 0.0)
    test("0xf15fbca5010a4249d98c5137f214d3d.458d525617f62001db6f2p68", 5.918472578157268E57)
    test("0x052cdde16bb8e7a6265046639.a2f897e77aa1a5d03fa5dap-759", 8.451331130505346E-201)
    test("0x770a211be78479bf8d942bad9f8fc6ace3.292c025df6b532f961ac48f99e92223p2122", Double.PositiveInfinity)
    test("0x07a598b38eb427b3b5433d.ab5912c87465ac2c2a76585ea9dd9bfb38881e72ca0529fp-4", 5.777805031594976E23)
    test("0x8698b81c9c16e5a8decab9b6d.b028dc31b3952b24068e3d34e7ca98eb83ccp82", 3.2229472965243904E54)
    test("0x662c64a84437ed9cdf306b8a5ffba3ff030c814761ac.037c61e9ad5bec868efp-745", 2.0655247050064035E-172)
    test("0xe882d86a9deee6.a63cf9e373467af776ec826bc3800p016", 4.2890730281470667E21)
    test("0x3d220eb5a59840.0ebfa1ff6d6951bc9b8b679p-1359", 0.0)

    /* One test where parsing the mantissa on its own would produce
     * overflow/underflow, but where the binary exponent "saves" it.
     */
    test("0x123456789abcdef" + "0" * 242 + "p0", Double.PositiveInfinity)
    test("0x123456789abcdef" + "0" * 242 + "p-1", 1.0226876500550064E308)
    test("0x0." + "0" * 268 + "123456789abcdefp0", 0.0)
    test("0x0." + "0" * 268 + "123456789abcdefp1", 4.9E-324)

    // Underflow preserves the sign of the 0
    test("-0x0." + "0" * 268 + "123456789abcdefp0", -0.0)

    /* Mantissa of 0 with overflowing/underflowing binary exponent.
     * Note that Math.pow(2, 10000 / 3) is Infinity.
     */
    test("0x0p10000", 0.0)
    test("-0x0p10000", -0.0)
    test("0x0p-10000", 0.0)
    test("-0x0p-10000", -0.0)

    // scalastyle:on line.size.limit
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    for (padding <- List("", "  ", (0 to 0x20).map(x => x.toChar).mkString)) {
      def pad(s: String): String = padding + s + padding

      def test(s: String): Unit =
        expectThrows(classOf[NumberFormatException], JDouble.parseDouble(pad(s)))

      test("asdf")
      test("4.3.5")
      test("4e3.5")
      test("hello world")
      test("--4")
      test("4E-3.2")
      test("1af") // hex digits without 0x
      test("0x.p1") // hex notation with both integral and fractional parts empty
      test("0x1.2") // missing 'p'
    }
  }

  @Test def compareTo(): Unit = {
    def compare(x: Double, y: Double): Int =
      new JDouble(x).compareTo(new JDouble(y))

    assertTrue(compare(0.0, 5.5) < 0)
    assertTrue(compare(10.5, 10.2) > 0)
    assertTrue(compare(-2.1, -1.0) < 0)
    assertEquals(0, compare(3.14, 3.14))

    // From compareTo's point of view, NaN is equal to NaN
    assertEquals(0, compare(Double.NaN, Double.NaN))

    // And -0.0 < 0.0
    assertTrue(compare(-0.0, 0.0) < 0)
    assertTrue(compare(0.0, -0.0) > 0)
  }

  @Test def compareToConvertedFromInt_issue_3085(): Unit = {
    @noinline
    def foo(x: Int): Unit =
      bar(x.toDouble)

    @inline
    def bar(x: Double): Unit = {
      assertTrue(x.compareTo(5.5) < 0)
      foobar(x)
    }

    @inline
    def foobar(x: Comparable[java.lang.Double]): Unit =
      assertTrue(x.compareTo(5.5) < 0)

    foo(5)
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0.0, 5.5) < 0)
    assertTrue(compare(10.5, 10.2) > 0)
    assertTrue(compare(-2.1, -1.0) < 0)
    assertEquals(0, compare(3.14, 3.14))

    // From compareTo's point of view, NaN is equal to NaN
    assertEquals(0, compare(Double.NaN, Double.NaN))

    // And -0.0 < 0.0
    assertTrue(compare(-0.0, 0.0) < 0)
    assertTrue(compare(0.0, -0.0) > 0)
  }

  @Test def `isInfinite_- #515`(): Unit = {
    assertTrue(Double.PositiveInfinity.isInfinite)
    assertTrue(Double.NegativeInfinity.isInfinite)
    assertTrue((1.0/0).isInfinite)
    assertTrue((-1.0/0).isInfinite)
    assertFalse((0.0).isInfinite)
  }

  @Test def isNaNTest(): Unit = {
    def f(v: Double): Boolean = {
      var v2 = v // do not inline
      v2.isNaN
    }

    assertTrue(f(Double.NaN))

    assertFalse(f(Double.PositiveInfinity))
    assertFalse(f(Double.NegativeInfinity))
    assertFalse(f(1.0 / 0))
    assertFalse(f(-1.0 / 0))
    assertFalse(f(0.0))
    assertFalse(f(3.0))
    assertFalse(f(-1.5))
  }

  @Test def longBitsToDouble(): Unit = {
    def isZero(v: Double, neg: Boolean): Boolean = {
      (v == 0.0) && (1 / v == (
          if (neg) Double.NegativeInfinity
          else Double.PositiveInfinity))
    }

    import JDouble.{longBitsToDouble => f}

    // Specials
    assertEquals(Double.PositiveInfinity, f(0x7ff0000000000000L), 0.0)
    assertEquals(Double.NegativeInfinity, f(0xfff0000000000000L), 0.0)
    assertTrue(isZero(f(0x0000000000000000L), false))
    assertTrue(isZero(f(0x8000000000000000L), true))
    assertTrue(f(0x7ff8000000000000L).isNaN) // canonical NaN

    // Non-canonical NaNs
    assertTrue(f(0x7ff0000000000001L).isNaN) // smallest positive NaN
    assertTrue(f(0x7ff15ab515d3cca1L).isNaN) // an arbitrary positive NaN
    assertTrue(f(0x7fffffffffffffffL).isNaN) // largest positive NaN
    assertTrue(f(0xfff0000000000001L).isNaN) // smallest negative NaN
    assertTrue(f(0xfff15ab515d3cca1L).isNaN) // an arbitrary negative NaN
    assertTrue(f(0xffffffffffffffffL).isNaN) // largest negative NaN

    // Normal forms
    assertEquals(2.2250738585072014e-308, f(0x0010000000000000L), 0.0)  // smallest pos normal form
    assertEquals(1.7976931348623157e308, f(0x7fefffffffffffffL), 0.0)   // largest pos normal form
    assertEquals(1.8790766677624813e63, f(0x4d124568bc6584caL), 0.0)    // an arbitrary pos normal form
    assertEquals(-2.2250738585072014e-308, f(0x8010000000000000L), 0.0) // smallest neg normal form
    assertEquals(-1.7976931348623157e308, f(0xffefffffffffffffL), 0.0)  // largest neg normal form
    assertEquals(-1.8790766677624813e63, f(0xcd124568bc6584caL), 0.0)   // an arbitrary neg normal form

    // Subnormal forms
    assertEquals(Double.MinPositiveValue, f(0x0000000000000001L), 0.0)  // smallest pos subnormal form
    assertEquals(2.225073858507201e-308, f(0x000fffffffffffffL), 0.0)   // largest pos subnormal form
    assertEquals(1.719471609939382e-308, f(0x000c5d44ae45cb60L), 0.0)   // an arbitrary pos subnormal form
    assertEquals(-Double.MinPositiveValue, f(0x8000000000000001L), 0.0) // smallest neg subnormal form
    assertEquals(-2.225073858507201e-308, f(0x800fffffffffffffL), 0.0)  // largest neg subnormal form
    assertEquals(-1.719471609939382e-308, f(0x800c5d44ae45cb60L), 0.0)  // an arbitrary neg subnormal form
  }

  @Test def doubleToLongBits(): Unit = {
    import JDouble.{doubleToLongBits => f}

    // Specials
    assertEquals(0x7ff0000000000000L, f(Double.PositiveInfinity))
    assertEquals(0xfff0000000000000L, f(Double.NegativeInfinity))
    assertEquals(0x0000000000000000L, f(0.0))
    assertEquals(0x8000000000000000L, f(-0.0))
    assertEquals(0x7ff8000000000000L, f(Double.NaN)) // canonical NaN

    // Normal forms
    assertEquals(0x0010000000000000L, f(2.2250738585072014e-308))  // smallest pos normal form
    assertEquals(0x7fefffffffffffffL, f(1.7976931348623157e308))   // largest pos normal form
    assertEquals(0x4d124568bc6584caL, f(1.8790766677624813e63))    // an arbitrary pos normal form
    assertEquals(0x8010000000000000L, f(-2.2250738585072014e-308)) // smallest neg normal form
    assertEquals(0xffefffffffffffffL, f(-1.7976931348623157e308))  // largest neg normal form
    assertEquals(0xcd124568bc6584caL, f(-1.8790766677624813e63))   // an arbitrary neg normal form

    // #2911 Normal form very close under a power of 2
    assertEquals(4845873199050653695L, f(9007199254740991.0))

    // Subnormal forms
    assertEquals(0x0000000000000001L, f(Double.MinPositiveValue))  // smallest pos subnormal form
    assertEquals(0x000fffffffffffffL, f(2.225073858507201e-308))   // largest pos subnormal form
    assertEquals(0x000c5d44ae45cb60L, f(1.719471609939382e-308))   // an arbitrary pos subnormal form
    assertEquals(0x8000000000000001L, f(-Double.MinPositiveValue)) // smallest neg subnormal form
    assertEquals(0x800fffffffffffffL, f(-2.225073858507201e-308))  // largest neg subnormal form
    assertEquals(0x800c5d44ae45cb60L, f(-1.719471609939382e-308))  // an arbitrary neg subnormal form
  }
}
