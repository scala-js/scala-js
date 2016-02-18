/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Assert._
import org.junit.Test

import java.util.Random

import org.scalajs.testsuite.utils.AssertThrows._

class RandomTest {

  @Test def should_produce_bits_according_to_spec_with_seed_10(): Unit = {
    val random = new HackRandom(10)

    assertEquals(747, random.next(10))
    assertEquals(0, random.next(1))
    assertEquals(16, random.next(6))
    assertEquals(432970, random.next(20))
    assertEquals(254270492, random.next(32))
  }

  @Test def should_produce_bits_according_to_spec_with_seed_neg5(): Unit = {
    val random = new HackRandom(-5)

    assertEquals(275, random.next(10))
    assertEquals(0, random.next(1))
    assertEquals(21, random.next(6))
    assertEquals(360349, random.next(20))
    assertEquals(1635930704, random.next(32))
  }

  @Test def should_produce_bits_according_to_spec_withseedMaxLong(): Unit = {
    val random = new HackRandom(Long.MaxValue)

    assertEquals(275, random.next(10))
    assertEquals(0, random.next(1))
    assertEquals(0, random.next(6))
    assertEquals(574655, random.next(20))
    assertEquals(-1451336087, random.next(32))
  }

  @Test def should_produce_bits_according_to_spec_withseedMinInt(): Unit = {
    val random = new HackRandom(Int.MinValue)

    assertEquals(388, random.next(10))
    assertEquals(0, random.next(1))
    assertEquals(25, random.next(6))
    assertEquals(352095, random.next(20))
    assertEquals(-2140124682, random.next(32))
  }

  @Test def should_allow_resetting_the_seed(): Unit = {
    val random = new HackRandom(11)
    assertEquals(747, random.next(10))
    assertEquals(1, random.next(1))
    assertEquals(27, random.next(6))

    random.setSeed(11)
    assertEquals(747, random.next(10))
    assertEquals(1, random.next(1))
    assertEquals(27, random.next(6))
  }

  @Test def should_reset_nextNextGaussian_when_setting_the_seed(): Unit = {
    val random = new Random(-1)
    assertEquals(1.7853314409882288, random.nextGaussian(), 0.0)
    random.setSeed(-1)
    assertEquals(1.7853314409882288, random.nextGaussian(), 0.0)
  }

  @Test def should_correctly_implement_nextDouble(): Unit = {
    val random = new Random(-45)
    assertEquals(0.27288421395636253, random.nextDouble(), 0.0)
    assertEquals(0.5523165360074201, random.nextDouble(), 0.0)
    assertEquals(0.5689979434708298, random.nextDouble(), 0.0)
    assertEquals(0.9961166166874871, random.nextDouble(), 0.0)
    assertEquals(0.5368984665202684, random.nextDouble(), 0.0)
    assertEquals(0.19849067496547423, random.nextDouble(), 0.0)
    assertEquals(0.6021019223595357, random.nextDouble(), 0.0)
    assertEquals(0.06132131151816378, random.nextDouble(), 0.0)
    assertEquals(0.7303867762743866, random.nextDouble(), 0.0)
    assertEquals(0.7426529384056163, random.nextDouble(), 0.0)
  }

  @Test def should_correctly_implement_nextBoolean(): Unit = {
    val random = new Random(4782934)
    assertFalse(random.nextBoolean())
    assertTrue(random.nextBoolean())
    assertTrue(random.nextBoolean())
    assertFalse(random.nextBoolean())
    assertFalse(random.nextBoolean())
    assertFalse(random.nextBoolean())
    assertTrue(random.nextBoolean())
    assertFalse(random.nextBoolean())
  }

  @Test def should_correctly_implement_nextInt(): Unit = {
    val random = new Random(-84638)
    assertEquals(-1217585344, random.nextInt())
    assertEquals(1665699216, random.nextInt())
    assertEquals(382013296, random.nextInt())
    assertEquals(1604432482, random.nextInt())
    assertEquals(-1689010196, random.nextInt())
    assertEquals(1743354032, random.nextInt())
    assertEquals(454046816, random.nextInt())
    assertEquals(922172344, random.nextInt())
    assertEquals(-1890515287, random.nextInt())
    assertEquals(1397525728, random.nextInt())
  }

  @Test def should_correctly_implement_nextInt_of_n(): Unit = {
    val random = new Random(7)
    assertEquals(32736, random.nextInt(76543))
    assertThrows(classOf[Exception], random.nextInt(0))
    assertEquals(29, random.nextInt(45))
    assertEquals(60, random.nextInt(945))
    assertEquals(20678044, random.nextInt(35694839))
    assertEquals(23932, random.nextInt(35699))
    assertEquals(2278, random.nextInt(3699))
    assertEquals(8, random.nextInt(10))
  }

  @Test def should_correctly_implement_nextInt_for_powers_of_2(): Unit = {
    val random = new Random(-56938)

    assertEquals(8, random.nextInt(32))
    assertEquals(3, random.nextInt(8))
    assertEquals(3, random.nextInt(128))
    assertEquals(1950, random.nextInt(4096))
    assertEquals(3706, random.nextInt(8192))
    assertEquals(4308, random.nextInt(8192))
    assertEquals(3235, random.nextInt(8192))
    assertEquals(7077, random.nextInt(8192))
    assertEquals(2392, random.nextInt(8192))
    assertEquals(31, random.nextInt(32))
  }

  @Test def should_correctly_implement_nextLong(): Unit = {
    val random = new Random(205620432625028L)
    assertEquals(3710537363280377478L, random.nextLong())
    assertEquals(4121778334981170700L, random.nextLong())
    assertEquals(289540773990891960L, random.nextLong())
    assertEquals(307008980197674441L, random.nextLong())
    assertEquals(7527069864796025013L, random.nextLong())
    assertEquals(-4563192874520002144L, random.nextLong())
    assertEquals(7619507045427546529L, random.nextLong())
    assertEquals(-7888117030898487184L, random.nextLong())
    assertEquals(-3499168703537933266L, random.nextLong())
    assertEquals(-1998975913933474L, random.nextLong())
  }

  @Test def should_correctly_implement_nextFloat(): Unit = {
    val random = new Random(-3920005825473L)
    assertEquals(0.059591234f, random.nextFloat(), 0.0f)
    assertEquals(0.7007871f, random.nextFloat(), 0.0f)
    assertEquals(0.39173192f, random.nextFloat(), 0.0f)
    assertEquals(0.0647918f, random.nextFloat(), 0.0f)
    assertEquals(0.9029677f, random.nextFloat(), 0.0f)
    assertEquals(0.18226051f, random.nextFloat(), 0.0f)
    assertEquals(0.94444054f, random.nextFloat(), 0.0f)
    assertEquals(0.008844078f, random.nextFloat(), 0.0f)
    assertEquals(0.08891684f, random.nextFloat(), 0.0f)
    assertEquals(0.06482434f, random.nextFloat(), 0.0f)
  }

  @Test def should_correctly_implement_nextBytes(): Unit = {
    val random = new Random(7399572013373333L)

    def test(exps: Int*): Unit = {
      val exp = exps.map(_.toByte).toArray
      val buf = new Array[Byte](exp.length)
      random.nextBytes(buf)
      assertArrayEquals(exp, buf)
    }

    test(62, 89, 68, -91, 10, 0, 85)
    test(-89, -76, 88, 121, -25, 47, 58, -8, 78, 20, -77, 84, -3,
        -33, 58, -9, 11, 57, -118, 40, -74, -86, 78, 123, 58)
    test(-77, 112, -116)
    test()
    test(-84, -96, 108)
    test(57, -106, 42, -100, -47, -84, 67, -48, 45)
  }

  @Test def should_correctly_implement_nextGaussian(): Unit = {
    val random = new Random(2446004)
    assertEquals(-0.5043346938630431, random.nextGaussian(), 0.0)
    assertEquals(-0.3250983270156675, random.nextGaussian(), 0.0)
    assertEquals(-0.23799457294994966, random.nextGaussian(), 0.0)
    assertEquals(0.4164610631507695, random.nextGaussian(), 0.0)
    assertEquals(0.22086348814760687, random.nextGaussian(), 0.0)
    assertEquals(-0.706833209972521, random.nextGaussian(), 0.0)
    assertEquals(0.6730758289772553, random.nextGaussian(), 0.0)
    assertEquals(0.2797393696191283, random.nextGaussian(), 0.0)
    assertEquals(-0.2979099632667685, random.nextGaussian(), 0.0)
    assertEquals(0.37443415981434314, random.nextGaussian(), 0.0)
    assertEquals(0.9584801742918951, random.nextGaussian(), 0.0)
    assertEquals(1.1762179112229345, random.nextGaussian(), 0.0)
    assertEquals(0.8736960092848826, random.nextGaussian(), 0.0)
    assertEquals(0.12301554931271008, random.nextGaussian(), 0.0)
    assertEquals(-0.6052081187207353, random.nextGaussian(), 0.0)
    assertEquals(-0.2015925608755316, random.nextGaussian(), 0.0)
    assertEquals(-1.0071216119742104, random.nextGaussian(), 0.0)
    assertEquals(0.6734222041441913, random.nextGaussian(), 0.0)
    assertEquals(0.3990565555091522, random.nextGaussian(), 0.0)
    assertEquals(2.0051627385915154, random.nextGaussian(), 0.0)
  }

  /** Helper class to access next */
  class HackRandom(seed: Long) extends Random(seed) {
    override def next(bits: Int): Int = super.next(bits)
  }

}
