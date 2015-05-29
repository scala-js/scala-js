/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import org.scalajs.jasminetest.JasmineTest

import java.util.Random

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object RandomTest extends JasmineTest {

  describe("java.util.Random") {

    it("should produce bits according to spec with seed=10") {
      val random = new HackRandom(10)

      expect(random.next(10)).toBe(747)
      expect(random.next(1)).toBe(0)
      expect(random.next(6)).toBe(16)
      expect(random.next(20)).toBe(432970)
      expect(random.next(32)).toBe(254270492)
    }

    it("should produce bits according to spec with seed=-5") {
      val random = new HackRandom(-5)

      expect(random.next(10)).toBe(275)
      expect(random.next(1)).toBe(0)
      expect(random.next(6)).toBe(21)
      expect(random.next(20)).toBe(360349)
      expect(random.next(32)).toBe(1635930704)
    }

    it("should produce bits according to spec with seed=MaxLong") {
      val random = new HackRandom(Long.MaxValue)

      expect(random.next(10)).toBe(275)
      expect(random.next(1)).toBe(0)
      expect(random.next(6)).toBe(0)
      expect(random.next(20)).toBe(574655)
      expect(random.next(32)).toBe(-1451336087)
    }

    it("should produce bits according to spec with seed=MinInt") {
      val random = new HackRandom(Int.MinValue)

      expect(random.next(10)).toBe(388)
      expect(random.next(1)).toBe(0)
      expect(random.next(6)).toBe(25)
      expect(random.next(20)).toBe(352095)
      expect(random.next(32)).toBe(-2140124682)
    }

    it("should allow resetting the seed") {
      val random = new HackRandom(11)
      expect(random.next(10)).toBe(747)
      expect(random.next(1)).toBe(1)
      expect(random.next(6)).toBe(27)

      random.setSeed(11)
      expect(random.next(10)).toBe(747)
      expect(random.next(1)).toBe(1)
      expect(random.next(6)).toBe(27)
    }

    it("should reset nextNextGaussian when setting the seed") {
      val random = new Random(-1)
      expect(random.nextGaussian()).toBe(1.7853314409882288)
      random.setSeed(-1)
      expect(random.nextGaussian()).toBe(1.7853314409882288)
    }

    it("should correctly implement nextDouble") {
      val random = new Random(-45)
      expect(random.nextDouble()).toBe(0.27288421395636253)
      expect(random.nextDouble()).toBe(0.5523165360074201)
      expect(random.nextDouble()).toBe(0.5689979434708298)
      expect(random.nextDouble()).toBe(0.9961166166874871)
      expect(random.nextDouble()).toBe(0.5368984665202684)
      expect(random.nextDouble()).toBe(0.19849067496547423)
      expect(random.nextDouble()).toBe(0.6021019223595357)
      expect(random.nextDouble()).toBe(0.06132131151816378)
      expect(random.nextDouble()).toBe(0.7303867762743866)
      expect(random.nextDouble()).toBe(0.7426529384056163)
    }

    it("should correctly implement nextBoolean") {
      val random = new Random(4782934)
      expect(random.nextBoolean()).toBe(false)
      expect(random.nextBoolean()).toBe(true)
      expect(random.nextBoolean()).toBe(true)
      expect(random.nextBoolean()).toBe(false)
      expect(random.nextBoolean()).toBe(false)
      expect(random.nextBoolean()).toBe(false)
      expect(random.nextBoolean()).toBe(true)
      expect(random.nextBoolean()).toBe(false)
    }

    it("should correctly implement nextInt") {
      val random = new Random(-84638)
      expect(random.nextInt()).toBe(-1217585344)
      expect(random.nextInt()).toBe(1665699216)
      expect(random.nextInt()).toBe(382013296)
      expect(random.nextInt()).toBe(1604432482)
      expect(random.nextInt()).toBe(-1689010196)
      expect(random.nextInt()).toBe(1743354032)
      expect(random.nextInt()).toBe(454046816)
      expect(random.nextInt()).toBe(922172344)
      expect(random.nextInt()).toBe(-1890515287)
      expect(random.nextInt()).toBe(1397525728)
    }

    it("should correctly implement nextInt(n)") {
      val random = new Random(7)
      expect(random.nextInt(76543)).toBe(32736)
      expect(() => random.nextInt(0)).toThrow
      expect(random.nextInt(45)).toBe(29)
      expect(random.nextInt(945)).toBe(60)
      expect(random.nextInt(35694839)).toBe(20678044)
      expect(random.nextInt(35699)).toBe(23932)
      expect(random.nextInt(3699)).toBe(2278)
      expect(random.nextInt(10)).toBe(8)
    }

    it("should correctly implement nextInt(n) for powers of 2") {
      val random = new Random(-56938)

      expect(random.nextInt(32)).toBe(8)
      expect(random.nextInt(8)).toBe(3)
      expect(random.nextInt(128)).toBe(3)
      expect(random.nextInt(4096)).toBe(1950)
      expect(random.nextInt(8192)).toBe(3706)
      expect(random.nextInt(8192)).toBe(4308)
      expect(random.nextInt(8192)).toBe(3235)
      expect(random.nextInt(8192)).toBe(7077)
      expect(random.nextInt(8192)).toBe(2392)
      expect(random.nextInt(32)).toBe(31)
    }

    it("should correctly implement nextLong") {
      val random = new Random(205620432625028L)
      expect(random.nextLong()).toBe(3710537363280377478L)
      expect(random.nextLong()).toBe(4121778334981170700L)
      expect(random.nextLong()).toBe(289540773990891960L)
      expect(random.nextLong()).toBe(307008980197674441L)
      expect(random.nextLong()).toBe(7527069864796025013L)
      expect(random.nextLong()).toBe(-4563192874520002144L)
      expect(random.nextLong()).toBe(7619507045427546529L)
      expect(random.nextLong()).toBe(-7888117030898487184L)
      expect(random.nextLong()).toBe(-3499168703537933266L)
      expect(random.nextLong()).toBe(-1998975913933474L)
    }

    it("should correctly implement nextFloat") {
      val random = new Random(-3920005825473L)
      expect(random.nextFloat()).toBe(0.059591234f)
      expect(random.nextFloat()).toBe(0.7007871f)
      expect(random.nextFloat()).toBe(0.39173192f)
      expect(random.nextFloat()).toBe(0.0647918f)
      expect(random.nextFloat()).toBe(0.9029677f)
      expect(random.nextFloat()).toBe(0.18226051f)
      expect(random.nextFloat()).toBe(0.94444054f)
      expect(random.nextFloat()).toBe(0.008844078f)
      expect(random.nextFloat()).toBe(0.08891684f)
      expect(random.nextFloat()).toBe(0.06482434f)
    }

    it("should correctly implement nextBytes") {
      val random = new Random(7399572013373333L)

      def test(exps: Int*): Unit = {
        val exp = js.Array(exps.map(_.toByte): _*)
        val buf = new Array[Byte](exp.length)
        random.nextBytes(buf)
        expect(buf.toJSArray).toEqual(exp)
      }

      test(62, 89, 68, -91, 10, 0, 85)
      test(-89, -76, 88, 121, -25, 47, 58, -8, 78, 20, -77, 84, -3,
        -33, 58, -9, 11, 57, -118, 40, -74, -86, 78, 123, 58)
      test(-77, 112, -116)
      test()
      test(-84, -96, 108)
      test(57, -106, 42, -100, -47, -84, 67, -48, 45)
    }

    it("should correctly implement nextGaussian") {
      val random = new Random(2446004)
      expect(random.nextGaussian()).toBe(-0.5043346938630431)
      expect(random.nextGaussian()).toBe(-0.3250983270156675)
      expect(random.nextGaussian()).toBe(-0.23799457294994966)
      expect(random.nextGaussian()).toBe(0.4164610631507695)
      expect(random.nextGaussian()).toBe(0.22086348814760687)
      expect(random.nextGaussian()).toBe(-0.706833209972521)
      expect(random.nextGaussian()).toBe(0.6730758289772553)
      expect(random.nextGaussian()).toBe(0.2797393696191283)
      expect(random.nextGaussian()).toBe(-0.2979099632667685)
      expect(random.nextGaussian()).toBe(0.37443415981434314)
      expect(random.nextGaussian()).toBe(0.9584801742918951)
      expect(random.nextGaussian()).toBe(1.1762179112229345)
      expect(random.nextGaussian()).toBe(0.8736960092848826)
      expect(random.nextGaussian()).toBe(0.12301554931271008)
      expect(random.nextGaussian()).toBe(-0.6052081187207353)
      expect(random.nextGaussian()).toBe(-0.2015925608755316)
      expect(random.nextGaussian()).toBe(-1.0071216119742104)
      expect(random.nextGaussian()).toBe(0.6734222041441913)
      expect(random.nextGaussian()).toBe(0.3990565555091522)
      expect(random.nextGaussian()).toBe(2.0051627385915154)
    }

  }

  /** Helper class to access next */
  class HackRandom(seed: Long) extends Random(seed) {
    override def next(bits: Int): Int = super.next(bits)
  }

}
