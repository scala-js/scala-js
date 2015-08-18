/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.scalajs.jasminetest.JasmineTest

object IntegerTestOnJDK8 extends JasmineTest {

  // Explicitly define these as `var`'s to avoid any compile-time constant folding
  val MaxValue: Int = Int.MaxValue
  val MinValue: Int = Int.MinValue

  describe("java.lang.Integer on JDK8 or higher") {
    it("should parse uInt strings") {
      def test(s: String, v: Int, radix: Int = 10): Unit = {
        expect(Integer.parseUnsignedInt(s, radix)).toEqual(v)
      }

      test("0", 0)
      test("5", 5)
      test("127", 127)
      test("+100", 100)
      test("30000", 30000)
      test("Kona", 411787, 27)
      test("+42", 42)
      test("+0", 0)
      test("FF", 255, 16)
      test("4000000000", 0xEE6B2800)
      test("4294967295", 0xFFFFFFFF)
    }

    it("should reject invalid uInt strings when parsing") {
      def test(s: String, radix: Int = 10): Unit =
        expect(() => Integer.parseUnsignedInt(s, radix)).toThrow

      test("abc")
      test("5a")
      test("99", 8)
      test("4294967296")
      test("-30000")
      test("+")
      test("-")
      test("-0")
      test("0.0")
    }

    it("should parse strings in base 16") {
      def test(s: String, v: Int): Unit = {
        expect(Integer.parseUnsignedInt(s, 16)).toEqual(v)
      }

      test("0", 0x0)
      test("5", 0x5)
      test("ff", 0xff)
      test("24", 0x24)
      test("30000", 0x30000)
      test("90000", 0x90000)
      test("EE6B2800", 0xEE6B2800)
      test("FFFFFFFF", 0xFFFFFFFF)
    }

    it("should provide `compareUnsigned`") {
      def compare(x: Int, y: Int): Int =
        Integer.compareUnsigned(x, y)

      expect(compare(0, 5)).toBeLessThan(0)
      expect(compare(10, 9)).toBeGreaterThan(0)
      expect(compare(3, 3)).toEqual(0)
      expect(compare(0xFFFFFFFF, 0xFFFFFFFF)).toEqual(0)
      expect(compare(0xEE6B2800, 0xFFFFFFFF)).toBeLessThan(0)
      expect(compare(0xFFFFFFFF, 0xEE6B2800)).toBeGreaterThan(0)
      expect(compare(0xEE6B2800, 3)).toBeGreaterThan(0)
      expect(compare(3, 0xEE6B2800)).toBeLessThan(0)
    }

    it("should provide `toUnsignedLong`") {
      def test(x: Int, y: Long): Unit =
        expect(Integer.toUnsignedLong(x) == y).toBeTruthy

      test(0, 0L)
      test(5, 5L)
      test(43345, 43345L)
      test(0xEE6B2800, 0xEE6B2800L)
      test(0xFFFFFFFF, 0xFFFFFFFFL)
    }

    it("should provide `divideUnsigned`") {
      def test(dividend: Int, divisor: Int, result: Int): Unit =
        expect(Integer.divideUnsigned(dividend, divisor)).toEqual(result)

      test(1, 1, 1)
      test(4, 2, 2)
      test(3, 2, 1)
      test(0xFFFFFFFF, 7, 613566756)
      test(0xFFFFFFFF, 0xEE6B2800, 1)
      test(0xEE6B2800, 2, 2000000000)
    }

    it("should provide `remainderUnsigned`") {
      def test(dividend: Int, divisor: Int, result: Int): Unit =
        expect(Integer.remainderUnsigned(dividend, divisor)).toEqual(result)

      test(1, 1, 0)
      test(4, 2, 0)
      test(3, 2, 1)
      test(0xFFFFFFFF, 7, 3)
      test(0xFFFFFFFF, 0xEE6B2800, 294967295)
      test(0xEE6B2800, 2, 0)
    }

    it("should provide `toUnsignedString` without radix") {
      expect(Integer.toUnsignedString(0)).toEqual("0")
      expect(Integer.toUnsignedString(12345)).toEqual("12345")
      expect(Integer.toUnsignedString(242134)).toEqual("242134")
      expect(Integer.toUnsignedString(Integer.MAX_VALUE)).toEqual("2147483647")
      expect(Integer.toUnsignedString(0xFFFFFFFF)).toEqual("4294967295")
      expect(Integer.toUnsignedString(0xEE6B2800)).toEqual("4000000000")

    }

    it("should provide `toUnsignedString` with radix") {
      expect(Integer.toUnsignedString(2147483647, 8)).toEqual("17777777777")
      expect(Integer.toUnsignedString(2147483647, 16)).toEqual("7fffffff")
      expect(Integer.toUnsignedString(2147483647, 2)).toEqual(
          "1111111111111111111111111111111")
      expect(Integer.toUnsignedString(2147483647, 10)).toEqual("2147483647")
      expect(Integer.toUnsignedString(0xFFFFFFFF, 16)).toEqual("ffffffff")
      expect(Integer.toUnsignedString(0xFFFFFFFF, 10)).toEqual("4294967295")
      expect(Integer.toUnsignedString(0xEE6B2800, 16)).toEqual("ee6b2800")
      expect(Integer.toUnsignedString(0xEE6B2800, 10)).toEqual("4000000000")
    }

    it("should provide `hashCode` as a static function") {
      for (i <- -256 to 256)
        expect(Integer.hashCode(i)).toEqual(i.hashCode())
      expect(Integer.hashCode(Int.MaxValue)).toEqual(Int.MaxValue.hashCode)
      expect(Integer.hashCode(Int.MinValue)).toEqual(Int.MinValue.hashCode)
    }

    it("should provide `sum` as a static function") {
      // 20 ramdomly generated cases
      expect(Integer.sum(1456847510, -1943375243)).toEqual(-486527733)
      expect(Integer.sum(-1675020769, -102165163)).toEqual(-1777185932)
      expect(Integer.sum(-492132773, 109679490)).toEqual(-382453283)
      expect(Integer.sum(-894160208, -1251837027)).toEqual(-2145997235)
      expect(Integer.sum(-1194861016, 1027987866)).toEqual(-166873150)
      expect(Integer.sum(-1898001389, 2065481406)).toEqual(167480017)
      expect(Integer.sum(-311003114, 984492586)).toEqual(673489472)
      expect(Integer.sum(-295074587, -1093289488)).toEqual(-1388364075)
      expect(Integer.sum(2022415614, 88678395)).toEqual(2111094009)
      expect(Integer.sum(-245624037, -1083032743)).toEqual(-1328656780)
      expect(Integer.sum(-1075180485, 1712078245)).toEqual(636897760)
      expect(Integer.sum(-1177939094, 1998208415)).toEqual(820269321)
      expect(Integer.sum(-1682860108, 632247105)).toEqual(-1050613003)
      expect(Integer.sum(1738870504, -1209620801)).toEqual(529249703)
      expect(Integer.sum(1763433497, 954469217)).toEqual(-1577064582)
      expect(Integer.sum(1576449779, -441737187)).toEqual(1134712592)
      expect(Integer.sum(2067118443, -213593276)).toEqual(1853525167)
      expect(Integer.sum(-1087866031, -762848293)).toEqual(-1850714324)
      expect(Integer.sum(2107199426, 642617926)).toEqual(-1545149944)
      expect(Integer.sum(-928260456, -2054005907)).toEqual(1312700933)
    }

    it("should provide `max` as a static function") {
      // 20 ramdomly generated cases
      expect(Integer.max(-1790671798, -270277483)).toEqual(-270277483)
      expect(Integer.max(1571368144, -695891091)).toEqual(1571368144)
      expect(Integer.max(-488353138, -1038365399)).toEqual(-488353138)
      expect(Integer.max(-1299154858, -1746941781)).toEqual(-1299154858)
      expect(Integer.max(-1330811400, -415165707)).toEqual(-415165707)
      expect(Integer.max(-222101245, -1612799352)).toEqual(-222101245)
      expect(Integer.max(6223768, -251871910)).toEqual(6223768)
      expect(Integer.max(-1807128180, 289107587)).toEqual(289107587)
      expect(Integer.max(1419004964, 1391551452)).toEqual(1419004964)
      expect(Integer.max(770531115, 1407516948)).toEqual(1407516948)
      expect(Integer.max(-1353241025, 127943959)).toEqual(127943959)
      expect(Integer.max(1079220095, -715415624)).toEqual(1079220095)
      expect(Integer.max(-1758211842, -451651341)).toEqual(-451651341)
      expect(Integer.max(-719501136, -720273331)).toEqual(-719501136)
      expect(Integer.max(136611495, 82825750)).toEqual(136611495)
      expect(Integer.max(-572096554, -1266456161)).toEqual(-572096554)
      expect(Integer.max(247666619, -599014758)).toEqual(247666619)
      expect(Integer.max(979958171, -773699262)).toEqual(979958171)
      expect(Integer.max(915015222, -895428609)).toEqual(915015222)
      expect(Integer.max(-2023661282, 1214239393)).toEqual(1214239393)
    }

    it("should provide `min` as a static function") {
      // 20 ramdomly generated cases
      expect(Integer.min(1070612756, -1360305565)).toEqual(-1360305565)
      expect(Integer.min(-1185998566, -943883433)).toEqual(-1185998566)
      expect(Integer.min(-741471209, -1767105808)).toEqual(-1767105808)
      expect(Integer.min(-586878137, 1591634109)).toEqual(-586878137)
      expect(Integer.min(1017257927, -1366663787)).toEqual(-1366663787)
      expect(Integer.min(-1769768449, -1206771005)).toEqual(-1769768449)
      expect(Integer.min(-516274758, 125028855)).toEqual(-516274758)
      expect(Integer.min(1929097253, 450306051)).toEqual(450306051)
      expect(Integer.min(1232270613, -2141159510)).toEqual(-2141159510)
      expect(Integer.min(1466133314, 456228627)).toEqual(456228627)
      expect(Integer.min(1643492178, -1549637221)).toEqual(-1549637221)
      expect(Integer.min(535997424, 1403224346)).toEqual(535997424)
      expect(Integer.min(1100365123, -1441182511)).toEqual(-1441182511)
      expect(Integer.min(1752406139, -778397275)).toEqual(-778397275)
      expect(Integer.min(-1083524011, 906792532)).toEqual(-1083524011)
      expect(Integer.min(-674955836, 100476859)).toEqual(-674955836)
      expect(Integer.min(702254105, -33102740)).toEqual(-33102740)
      expect(Integer.min(-1266058648, 1907502126)).toEqual(-1266058648)
      expect(Integer.min(-1750379520, 1293903630)).toEqual(-1750379520)
      expect(Integer.min(-335824862, -641887949)).toEqual(-641887949)
    }
  }
}
