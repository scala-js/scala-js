/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

/** Tests the compiler re-patching of native longs to
 *  scala.scalajs.runtime.Long
 *  see org.scalajs.testsuite.jsinterop.RuntimeLongTest
 *  for a test of the implementation itself
 */
class LongTest {
  import LongTest._

  @Test def `should_correctly_handle_literals`(): Unit = {
    assertEquals(105L, 5L + 100L)
    assertEquals(2147483651L, 2147483649L + 2L)
    assertEquals(-8589934592L, -2147483648L * 4)
    assertEquals(-18014398509482040L, 4503599627370510L * (-4))
  }

  @Test def `should_correctly_dispatch_unary_ops_on_Longs`(): Unit = {
    val x = 10L
    assertEquals(-10L, -x)
    val y = 5L
    assertEquals(-5L, -y)
    assertEquals(5L, +y)
    assertEquals(-6L, ~y)
  }

  @Test def `should_correctly_dispatch_binary_ops_on_Longs`(): Unit = {
    assertEquals(25F, 5L * 5F, 0F)
    assertEquals(1F, 5L % 4F, 0F)
    assertEquals(20F, 5F * 4L, 0F)
  }

  @Test def `should_support_shifts_with_Longs_#622`(): Unit = {
    def l(x: Long): Long = x
    def i(x: Int): Int = x

    assertEquals(268435455L, l(-7L) >>> 100L)
    assertEquals(-1L, l(-7L) >> 100L)
    assertEquals(-1L, l(-7L) >> 100)
    assertEquals(268435455L, l(-7L) >>> 100)
    assertEquals(-481036337152L, l(-7L) << 100L)
    assertEquals(-481036337152L, l(-7L) << 100)
    assertEquals(481036337152L, l(7L) << 100L)
    assertEquals(549755813888L, l(8L) << 100L)
    assertEquals(1152921504606846975L, l(-7L) >>> 4)

    assertEquals(112, i(7) << 100)
    assertEquals(-1, i(-7) >> 100)
    assertEquals(268435455, i(-7) >>> 100)
    assertEquals(-5, i(-65) >> 100)
    assertEquals(-5, i(-65) >> 4)
  }

  @Test def times_*(): Unit = {
    @inline def test(a: Long, b: Long, expected: Long): Unit = {
      @noinline def hideFromOptimizer(x: Long): Long = x

      assertEquals(expected, a * b)
      assertEquals(expected, hideFromOptimizer(a) * b)
      assertEquals(expected, a * hideFromOptimizer(b))
      assertEquals(expected, hideFromOptimizer(a) * hideFromOptimizer(b))
    }

    // Random tests
    test(-304510477859059605L, -504694402761190L, 8433193943336928478L)
    test(-253162060478L, 50291L, -12731773183499098L)
    test(0L, -13850059L, 0L)
    test(-8660470952582643L, -874L, 7569251612557229982L)
    test(-1332L, 9L, -11988L)
    test(-29568141078178L, 1526544L, -8243580206627053600L)
    test(328926390054L, -3638668370L, 2184313243348463060L)
    test(205L, 228496L, 46841680L)
    test(-19460467868573L, -81306407837343422L, 740594256954004614L)
    test(515501176792L, -14676235751610L, 1686350941924289808L)
    test(414866483823975467L, 470120246452409879L, 6344118389112076765L)
    test(-117L, 97L, -11349L)
    test(30L, -513425L, -15402750L)
    test(1375729456898L, 623181770548L, -1358824991029065112L)
    test(80638733077L, 195L, 15724552950015L)
    test(-12740384618206L, -4199L, 53496875011846994L)
    test(521743856055513645L, -2105465236503908L, 4976657084923555180L)
    test(-8L, -1L, 8L)
    test(-34L, -18232708L, 619912072L)
    test(-4512416881041611L, 16599101951L, -4326473264912647477L)
    test(-2433585405235L, -645367L, 1570555712220296245L)
    test(-774590L, -22277506028L, 17255933394228520L)
    test(36307900L, -19149614702538L, 5693979142683511208L)
    test(22169162314093L, 18534166L, 5058564788733665886L)
    test(-3848931303L, 82157326906201261L, -1912529786602316571L)
    test(47326191440L, -39094697833L, -5528746562555987920L)
    test(-14641542L, -3714491797523081344L, -2254385599727553792L)
    test(771303L, 10199L, 7866519297L)
    test(0L, -14806105L, 0L)
    test(3964448L, 12L, 47573376L)
    test(77505454872L, -33668L, -2609453654630496L)
    test(14929228532112L, 6555913938L, -3686637842539497440L)
    test(431219964946864070L, 1181669L, 4853210716974444062L)
    test(2820049084807L, -5L, -14100245424035L)
    test(-16830140929953126L, 93975690486771L, 8725676311339308590L)
    test(206188838L, 1249841574949634L, 2367313232506909772L)
    test(16674057030L, 1104000290638571L, -8124607316971866814L)
    test(-64710169253973867L, -23079009995647L, 6446979988520042261L)
    test(31096512L, 21048L, 654519384576L)
    test(22L, 6963814099146552L, 153203910181224144L)
    test(1260318190682L, 1123567398313107L, 1436126772314869678L)
    test(-5L, 15132314L, -75661570L)
    test(9099845427374L, -126975734L, 6685324216344409292L)
    test(5L, -1420058L, -7100290L)
    test(-28274L, 68638918056024L, -1940696769116022576L)
    test(193355246244L, 36593871833L, -7932625013377175292L)
    test(1L, -7L, -7L)
    test(66789957775108L, -724L, -48355929429178192L)
    test(10521672279471L, -1L, -10521672279471L)
    test(-8747667231979L, -105906241L, 4095350372293300139L)

    // Random power of 2 tests
    test(100259248L, 1048576L, 105129441230848L)
    test(1048576L, 100259248L, 105129441230848L)
    test(72L, 18014398509481984L, 1297036692682702848L)
    test(18014398509481984L, 72L, 1297036692682702848L)
    test(-22253267L, 536870912L, -11947131749269504L)
    test(536870912L, -22253267L, -11947131749269504L)
    test(23022568162358L, 8388608L, 8659858730206101504L)
    test(8388608L, 23022568162358L, 8659858730206101504L)
    test(1548271L, 134217728L, 207805415948288L)
    test(134217728L, 1548271L, 207805415948288L)
    test(-55880L, -9223372036854775808L, 0L)
    test(-9223372036854775808L, -55880L, 0L)
    test(-1L, 2199023255552L, -2199023255552L)
    test(2199023255552L, -1L, -2199023255552L)
    test(13L, 65536L, 851968L)
    test(65536L, 13L, 851968L)
    test(-1L, 17592186044416L, -17592186044416L)
    test(17592186044416L, -1L, -17592186044416L)
    test(222527207082L, 4398046511104L, -7000097952840548352L)
    test(4398046511104L, 222527207082L, -7000097952840548352L)
    test(34L, 72057594037927936L, 2449958197289549824L)
    test(72057594037927936L, 34L, 2449958197289549824L)
    test(-4058427375959L, 4096L, -16623318531928064L)
    test(4096L, -4058427375959L, -16623318531928064L)
    test(-2214324316485807900L, 281474976710656L, -8006274237557899264L)
    test(281474976710656L, -2214324316485807900L, -8006274237557899264L)
    test(-14335L, 4294967296L, -61568356188160L)
    test(4294967296L, -14335L, -61568356188160L)
    test(-48456017090L, 64L, -3101185093760L)
    test(64L, -48456017090L, -3101185093760L)
    test(-158203838L, 128L, -20250091264L)
    test(128L, -158203838L, -20250091264L)
    test(-115995L, 1024L, -118778880L)
    test(1024L, -115995L, -118778880L)
    test(-483041L, 8388608L, -4052041596928L)
    test(8388608L, -483041L, -4052041596928L)
    test(186114971352L, 35184372088832L, 2511601217189183488L)
    test(35184372088832L, 186114971352L, 2511601217189183488L)
    test(8064516387533822L, 32L, 258064524401082304L)
    test(32L, 8064516387533822L, 258064524401082304L)
    test(-816627104025L, 16384L, -13379618472345600L)
    test(16384L, -816627104025L, -13379618472345600L)
    test(-106L, 536870912L, -56908316672L)
    test(536870912L, -106L, -56908316672L)
    test(0L, 4096L, 0L)
    test(4096L, 0L, 0L)
    test(-61230193L, 8388608L, -513636086841344L)
    test(8388608L, -61230193L, -513636086841344L)
    test(41500264L, 8796093022208L, -3894698884001169408L)
    test(8796093022208L, 41500264L, -3894698884001169408L)
    test(32992L, 32L, 1055744L)
    test(32L, 32992L, 1055744L)
    test(65071L, 8589934592L, 558955633836032L)
    test(8589934592L, 65071L, 558955633836032L)
    test(76048351L, 137438953472L, -7994738298998226944L)
    test(137438953472L, 76048351L, -7994738298998226944L)
    test(-2L, 2L, -4L)
    test(2L, -2L, -4L)
    test(-8318677645L, 512L, -4259162954240L)
    test(512L, -8318677645L, -4259162954240L)
    test(-60717806L, 2251799813685248L, 2922836158163451904L)
    test(2251799813685248L, -60717806L, 2922836158163451904L)
    test(0L, 8192L, 0L)
    test(8192L, 0L, 0L)
    test(164844L, 33554432L, 5531246788608L)
    test(33554432L, 164844L, 5531246788608L)
    test(-678234761L, 72057594037927936L, 8574853690513424384L)
    test(72057594037927936L, -678234761L, 8574853690513424384L)
    test(-103657850088L, 2251799813685248L, -2828260565988671488L)
    test(2251799813685248L, -103657850088L, -2828260565988671488L)
    test(138748537820112453L, -9223372036854775808L, -9223372036854775808L)
    test(-9223372036854775808L, 138748537820112453L, -9223372036854775808L)
    test(0L, 17179869184L, 0L)
    test(17179869184L, 0L, 0L)
    test(-2489041709915087415L, 2147483648L, 5526109039206858752L)
    test(2147483648L, -2489041709915087415L, 5526109039206858752L)
    test(9L, 1073741824L, 9663676416L)
    test(1073741824L, 9L, 9663676416L)
    test(379085341609132041L, 72057594037927936L, 648518346341351424L)
    test(72057594037927936L, 379085341609132041L, 648518346341351424L)
    test(218625848802439L, 65536L, -4118880446592909312L)
    test(65536L, 218625848802439L, -4118880446592909312L)
    test(-422887732952L, 16777216L, -7094878839486021632L)
    test(16777216L, -422887732952L, -7094878839486021632L)
    test(77839L, 268435456L, 20894747459584L)
    test(268435456L, 77839L, 20894747459584L)
    test(-3953804003778L, 2251799813685248L, -5624995934585749504L)
    test(2251799813685248L, -3953804003778L, -5624995934585749504L)
    test(141564L, 536870912L, 76001593786368L)
    test(536870912L, 141564L, 76001593786368L)
    test(11068003L, 536870912L, 5942088864628736L)
    test(536870912L, 11068003L, 5942088864628736L)
    test(-816398911L, 512L, -417996242432L)
    test(512L, -816398911L, -417996242432L)
    test(79084L, 128L, 10122752L)
    test(128L, 79084L, 10122752L)
    test(166L, 4294967296L, 712964571136L)
    test(4294967296L, 166L, 712964571136L)
    test(-120218862620908531L, 4294967296L, -3013501831155286016L)
    test(4294967296L, -120218862620908531L, -3013501831155286016L)
  }

  @Test def `primitives_should_convert_to_Long`(): Unit = {
    // Byte
    assertEquals(112L, 112.toByte.toLong)
    // Short
    assertEquals(-10L, (-10).toShort.toLong)
    // Char
    assertEquals(65L, 'A'.toLong)
    // Int
    assertEquals(5L, 5.toLong)
    // Long
    assertEquals(10L, 10L.toLong)
    // Float
    assertEquals(100000L, 100000.6f.toLong)
    // Double
    assertEquals(100000L, 100000.6.toLong)
  }

  @Test def `should_support_hashCode`(): Unit = {
    assertEquals(0, 0L.hashCode())
    assertEquals(55, 55L.hashCode())
    assertEquals(11, (-12L).hashCode())
    assertEquals(10006548, 10006548L.hashCode())
    assertEquals(1098747, (-1098748L).hashCode())

    assertEquals(-825638905, 613354684553L.hashCode())
    assertEquals(1910653900, 9863155567412L.hashCode())
    assertEquals(1735398658, 3632147899696541255L.hashCode())
    assertEquals(-1689438124, 7632147899696541255L.hashCode())
  }

  @Test def `should_support_hash_hash`(): Unit = {
    assertEquals(0, 0L.##)
    assertEquals(55, 55L.##)
    assertEquals(-12, (-12L).##)
    assertEquals(10006548, 10006548L.##)
    assertEquals(-1098748, (-1098748L).##)

    assertEquals(1910653900, 9863155567412L.##)
    assertEquals(1735398658, 3632147899696541255L.##)

    // These two (correctly) give different results on 2.10 and 2.11
    //assertEquals(-825638905, 613354684553L.##)  // xx06 on 2.10
    //assertEquals(-1689438124, 7632147899696541255L.##) // xx25 on 2.10
  }

  @Test def `should_have_correct_hash_in_case_classes`(): Unit = {
    assertEquals(-1669410282, HashTestBox(0L).##)
    assertEquals(-1561146018, HashTestBox(55L).##)
    assertEquals(-1266055417, HashTestBox(-12L).##)
    assertEquals(-1383472436, HashTestBox(10006548L).##)
    assertEquals(1748124846, HashTestBox(-1098748L).##)

    assertEquals(1291324266, HashTestBox(9863155567412L).##)
    assertEquals(-450677189, HashTestBox(3632147899696541255L).##)

    assertEquals(259268522, HashTestBox(1461126709984L).##)
    assertEquals(818387364, HashTestBox(1L).##)
  }

  @Test def `should_correctly_concat_to_string`(): Unit = {
    val x = 20L
    assertEquals("asdf520hello", "asdf" + 5L + x + "hello")
    assertEquals("20hello", x + "hello")
  }

  @Test def `string_should_convert_to_Long`(): Unit = {
    assertEquals(45678901234567890L, "45678901234567890".toLong)
  }

  @Test def `should_correctly_implement_is/asInstanceOf_Longs`(): Unit = {
    val dyn:  Any  = 5L
    val stat: Long = 5L

    assertEquals(5L, stat.asInstanceOf[Long])
    // models current scala behavior. See SI-1448
    assertEquals(5, stat.asInstanceOf[Int])

    assertTrue(stat.isInstanceOf[Long])
    assertFalse(stat.isInstanceOf[Int])

    assertEquals(5L, dyn.asInstanceOf[Long])

    assertTrue(dyn.isInstanceOf[Long])
    assertFalse(dyn.isInstanceOf[Int])
  }

  @Test def `should_correctly_compare_to_other_numeric_types`(): Unit = {
    assertTrue(5L == 5)
    assertTrue(5 == 5L)
    assertTrue(4 != 5L)
    assertTrue('A' == 65L)
  }
}

object LongTest {

  case class HashTestBox(long: Long)

}
