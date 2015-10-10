/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.scalajs.jasminetest.JasmineTest
import scala.scalajs.js

object IntegerTest extends JasmineTest {

  describe("java.lang.Integer") {

    // Explicitly define these as `var`'s to avoid any compile-time constant folding
    val MaxValue: Int = Int.MaxValue
    val MinValue: Int = Int.MinValue

    it("should provide `reverseBytes` used by scala.Enumeration") {
      expect(Integer.reverseBytes(0xdeadbeef)).toEqual(0xefbeadde)
    }

    it("should provide `rotateLeft`") {
      expect(Integer.rotateLeft(0x689cd401, 0)).toEqual(0x689cd401)
      expect(Integer.rotateLeft(0x689cd401, 1)).toEqual(0xd139a802)
      expect(Integer.rotateLeft(0x689cd401, 8)).toEqual(0x9cd40168)
      expect(Integer.rotateLeft(0x689cd401, 13)).toEqual(0x9a802d13)
      expect(Integer.rotateLeft(0x689cd401, 32)).toEqual(0x689cd401)
      expect(Integer.rotateLeft(0x689cd401, 33)).toEqual(0xd139a802)
      expect(Integer.rotateLeft(0x689cd401, 43)).toEqual(0xe6a00b44)
      expect(Integer.rotateLeft(0x689cd401, -1)).toEqual(0xb44e6a00)
      expect(Integer.rotateLeft(0x689cd401, -28)).toEqual(0x89cd4016)
      expect(Integer.rotateLeft(0x689cd401, -39)).toEqual(0x2d139a8)
    }

    it("should provide `rotateRight`") {
      expect(Integer.rotateRight(0x689cd401, 0)).toEqual(0x689cd401)
      expect(Integer.rotateRight(0x689cd401, 1)).toEqual(0xb44e6a00)
      expect(Integer.rotateRight(0x689cd401, 8)).toEqual(0x1689cd4)
      expect(Integer.rotateRight(0x689cd401, 13)).toEqual(0xa00b44e6)
      expect(Integer.rotateRight(0x689cd401, 32)).toEqual(0x689cd401)
      expect(Integer.rotateRight(0x689cd401, 33)).toEqual(0xb44e6a00)
      expect(Integer.rotateRight(0x689cd401, 43)).toEqual(0x802d139a)
      expect(Integer.rotateRight(0x689cd401, -1)).toEqual(0xd139a802)
      expect(Integer.rotateRight(0x689cd401, -28)).toEqual(0x1689cd40)
      expect(Integer.rotateRight(0x689cd401, -39)).toEqual(0x4e6a00b4)
    }

    it("should provide `bitCount`") {
      expect(Integer.bitCount(0)).toEqual(0)
      expect(Integer.bitCount(1)).toEqual(1)
      expect(Integer.bitCount(2)).toEqual(1)
      expect(Integer.bitCount(3)).toEqual(2)
      expect(Integer.bitCount(Int.MaxValue)).toEqual(31)
      expect(Integer.bitCount(Int.MinValue)).toEqual(1)
      expect(Integer.bitCount(Int.MinValue + 1)).toEqual(2)
      expect(Integer.bitCount(-1)).toEqual(32)
      expect(Integer.bitCount(-2)).toEqual(31)

      expect(Integer.bitCount(-155937076)).toEqual(18)
      expect(Integer.bitCount(830524462)).toEqual(12)
      expect(Integer.bitCount(-1468950275)).toEqual(17)
      expect(Integer.bitCount(1878189982)).toEqual(22)
      expect(Integer.bitCount(1369853111)).toEqual(16)
      expect(Integer.bitCount(993872011)).toEqual(16)
      expect(Integer.bitCount(-419203945)).toEqual(17)
      expect(Integer.bitCount(-1529972891)).toEqual(18)
      expect(Integer.bitCount(-560981166)).toEqual(15)
      expect(Integer.bitCount(-1083297551)).toEqual(19)
      expect(Integer.bitCount(-1513915437)).toEqual(19)
      expect(Integer.bitCount(-774144288)).toEqual(19)
      expect(Integer.bitCount(1617041908)).toEqual(13)
      expect(Integer.bitCount(-799619923)).toEqual(15)
      expect(Integer.bitCount(1630552297)).toEqual(11)
      expect(Integer.bitCount(1893565724)).toEqual(15)
      expect(Integer.bitCount(-167512165)).toEqual(20)
      expect(Integer.bitCount(-1226735856)).toEqual(17)
      expect(Integer.bitCount(-1602623352)).toEqual(13)
      expect(Integer.bitCount(73385109)).toEqual(14)
      expect(Integer.bitCount(1843595740)).toEqual(17)
      expect(Integer.bitCount(-2005160623)).toEqual(16)
      expect(Integer.bitCount(-220474394)).toEqual(21)
      expect(Integer.bitCount(-1474261577)).toEqual(14)
      expect(Integer.bitCount(-2015504620)).toEqual(18)
      expect(Integer.bitCount(1450835633)).toEqual(20)
      expect(Integer.bitCount(-1877059561)).toEqual(13)
      expect(Integer.bitCount(-864957023)).toEqual(16)
      expect(Integer.bitCount(-1423863837)).toEqual(17)
      expect(Integer.bitCount(661877472)).toEqual(16)
      expect(Integer.bitCount(-1297344862)).toEqual(14)
      expect(Integer.bitCount(-1084965589)).toEqual(20)
      expect(Integer.bitCount(-169792549)).toEqual(20)
      expect(Integer.bitCount(-1175303521)).toEqual(19)
      expect(Integer.bitCount(-2075407535)).toEqual(14)
      expect(Integer.bitCount(-34407382)).toEqual(21)
      expect(Integer.bitCount(-686482061)).toEqual(18)
      expect(Integer.bitCount(-1280254298)).toEqual(18)
      expect(Integer.bitCount(-1236753591)).toEqual(14)
      expect(Integer.bitCount(-629695246)).toEqual(20)
      expect(Integer.bitCount(-1041379007)).toEqual(15)
      expect(Integer.bitCount(1133674695)).toEqual(12)
      expect(Integer.bitCount(-673156775)).toEqual(17)
      expect(Integer.bitCount(1634657308)).toEqual(15)
      expect(Integer.bitCount(-1634571160)).toEqual(15)
      expect(Integer.bitCount(-1394160814)).toEqual(17)
      expect(Integer.bitCount(57693078)).toEqual(13)
      expect(Integer.bitCount(788250760)).toEqual(15)
      expect(Integer.bitCount(-1217888690)).toEqual(19)
      expect(Integer.bitCount(-1568144709)).toEqual(12)
      expect(Integer.bitCount(827170343)).toEqual(16)
      expect(Integer.bitCount(-341950555)).toEqual(21)
      expect(Integer.bitCount(1287166354)).toEqual(14)
      expect(Integer.bitCount(-1639223942)).toEqual(19)
      expect(Integer.bitCount(532815708)).toEqual(17)
      expect(Integer.bitCount(-768179729)).toEqual(17)
      expect(Integer.bitCount(760154173)).toEqual(15)
      expect(Integer.bitCount(2000995890)).toEqual(15)
      expect(Integer.bitCount(1468010757)).toEqual(10)
      expect(Integer.bitCount(295957433)).toEqual(17)
      expect(Integer.bitCount(-1594421450)).toEqual(17)
      expect(Integer.bitCount(-1110692843)).toEqual(16)
      expect(Integer.bitCount(72567553)).toEqual(10)
      expect(Integer.bitCount(1008258604)).toEqual(14)
      expect(Integer.bitCount(1017279739)).toEqual(18)
      expect(Integer.bitCount(-649579130)).toEqual(14)
      expect(Integer.bitCount(-1743090924)).toEqual(12)
      expect(Integer.bitCount(-1321851761)).toEqual(15)
      expect(Integer.bitCount(1995849614)).toEqual(21)
      expect(Integer.bitCount(1874069759)).toEqual(19)
      expect(Integer.bitCount(57468414)).toEqual(18)
      expect(Integer.bitCount(-159055416)).toEqual(13)
      expect(Integer.bitCount(-770646612)).toEqual(15)
      expect(Integer.bitCount(1274257460)).toEqual(15)
      expect(Integer.bitCount(-1728268856)).toEqual(17)
      expect(Integer.bitCount(-131769823)).toEqual(14)
      expect(Integer.bitCount(1810706244)).toEqual(18)
      expect(Integer.bitCount(881236344)).toEqual(14)
      expect(Integer.bitCount(-536176288)).toEqual(11)
      expect(Integer.bitCount(-371993265)).toEqual(20)
      expect(Integer.bitCount(-1257692889)).toEqual(13)
      expect(Integer.bitCount(38550368)).toEqual(11)
      expect(Integer.bitCount(-196060824)).toEqual(14)
      expect(Integer.bitCount(-218909520)).toEqual(18)
      expect(Integer.bitCount(-735195141)).toEqual(21)
      expect(Integer.bitCount(-1122922843)).toEqual(16)
      expect(Integer.bitCount(-269171126)).toEqual(19)
      expect(Integer.bitCount(2002409940)).toEqual(18)
      expect(Integer.bitCount(-106797451)).toEqual(18)
      expect(Integer.bitCount(-1412648370)).toEqual(17)
      expect(Integer.bitCount(-342432881)).toEqual(20)
      expect(Integer.bitCount(-294768321)).toEqual(20)
      expect(Integer.bitCount(586296006)).toEqual(14)
      expect(Integer.bitCount(-1627992562)).toEqual(19)
      expect(Integer.bitCount(-1567624079)).toEqual(17)
      expect(Integer.bitCount(453182827)).toEqual(13)
      expect(Integer.bitCount(-704549035)).toEqual(16)
      expect(Integer.bitCount(1722304234)).toEqual(15)
      expect(Integer.bitCount(-747553362)).toEqual(19)
      expect(Integer.bitCount(-1535508973)).toEqual(18)
    }

    it("should provide `numberOfTrailingZeros`") {
      expect(Integer.numberOfTrailingZeros(0xa3c49000)).toEqual(12)
      expect(Integer.numberOfTrailingZeros(0x43f49020)).toEqual(5)
      expect(Integer.numberOfTrailingZeros(0x43c08000)).toEqual(15)
      expect(Integer.numberOfTrailingZeros(0)).toEqual(32)
    }

    it("should provide `toBinaryString` for values in range") {
      expect(Integer.toBinaryString(-1)).toEqual("11111111111111111111111111111111")
      expect(Integer.toBinaryString(-10001)).toEqual("11111111111111111101100011101111")
      expect(Integer.toBinaryString(MinValue)).toEqual("10000000000000000000000000000000")
      expect(Integer.toBinaryString(MaxValue)).toEqual("1111111111111111111111111111111")
    }

    it("should provide `toHexString` for values in range") {
      expect(Integer.toHexString(-1)).toEqual("ffffffff")
      expect(Integer.toHexString(-10001)).toEqual("ffffd8ef")
      expect(Integer.toHexString(MinValue)).toEqual("80000000")
      expect(Integer.toHexString(-2147000002)).toEqual("8007613e")
      expect(Integer.toHexString(MaxValue)).toEqual("7fffffff")
    }

    it("should provide `toOctalString` for values in range") {
      expect(Integer.toOctalString(-1)).toEqual("37777777777")
      expect(Integer.toOctalString(-10001)).toEqual("37777754357")
      expect(Integer.toOctalString(MinValue)).toEqual("20000000000")
      expect(Integer.toOctalString(MaxValue)).toEqual("17777777777")
    }

    it("should provide `compareTo`") {
      def compare(x: Int, y: Int): Int =
        new Integer(x).compareTo(new Integer(y))

      expect(compare(0, 5)).toBeLessThan(0)
      expect(compare(10, 9)).toBeGreaterThan(0)
      expect(compare(-2, -1)).toBeLessThan(0)
      expect(compare(3, 3)).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare(0, 5)).toBeLessThan(0)
      expect(compare(10, 9)).toBeGreaterThan(0)
      expect(compare(-2, -1)).toBeLessThan(0)
      expect(compare(3, 3)).toEqual(0)
    }

    it("should parse strings") {
      def test(s: String, v: Int, radix: Int = 10): Unit = {
        expect(Integer.parseInt(s, radix)).toEqual(v)
        expect(Integer.valueOf(s, radix).intValue()).toEqual(v)
        if (radix == 10)
          expect(new Integer(s).intValue()).toEqual(v)
      }

      test("0", 0)
      test("5", 5)
      test("127", 127)
      test("-100", -100)
      test("30000", 30000)
      test("-90000", -90000)
      test("Kona", 411787, 27)
      test("+42", 42)
      test("-0", 0)
      test("-FF", -255, 16)
    }

    it("should reject invalid strings when parsing") {
      def test(s: String, radix: Int = 10): Unit =
        expect(() => Integer.parseInt(s, radix)).toThrow

      test("abc")
      test("5a")
      test("2147483648")
      test("99", 8)
      test("-")
      test("")
    }

    it("should parse strings in base 16") {
      def test(s: String, v: Int): Unit = {
        expect(Integer.parseInt(s, 16)).toEqual(v)
        expect(Integer.valueOf(s, 16).intValue()).toEqual(v)
      }

      test("0", 0x0)
      test("5", 0x5)
      test("ff", 0xff)
      test("-24", -0x24)
      test("30000", 0x30000)
      test("-90000", -0x90000)
    }

    it("should provide `highestOneBit`") {
      /* Spec ported from
       * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
       */
      expect(Integer.highestOneBit(0)).toEqual(0)
      expect(Integer.highestOneBit(-1)).toEqual(Integer.MIN_VALUE)
      expect(Integer.highestOneBit(-256)).toEqual(Integer.MIN_VALUE)
      expect(Integer.highestOneBit(1)).toEqual(1)
      expect(Integer.highestOneBit(0x88)).toEqual(0x80)
      expect(Integer.highestOneBit(Int.MaxValue)).toEqual(0x40000000)
      expect(Integer.highestOneBit(Int.MinValue)).toEqual(Int.MinValue)
    }

    it("should provide `lowestOneBit`") {
      expect(Integer.lowestOneBit(0)).toEqual(0)
      expect(Integer.lowestOneBit(-1)).toEqual(1)
      expect(Integer.lowestOneBit(-256)).toEqual(256)
      expect(Integer.lowestOneBit(12)).toEqual(4)
      expect(Integer.lowestOneBit(0x88)).toEqual(0x8)
      expect(Integer.lowestOneBit(Int.MaxValue)).toEqual(1)
      expect(Integer.lowestOneBit(Int.MinValue)).toEqual(Int.MinValue)
    }

    it("should provide `toString` without radix") {
      /* Spec ported from
       * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
       */
      expect(new Integer(12345).toString).toEqual("12345")
      expect(new Integer("-12345").toString).toEqual("-12345")
      expect(Integer.toString(-80765)).toEqual("-80765")
      expect(Integer.toString(Integer.MAX_VALUE)).toEqual("2147483647")
      expect(Integer.toString(-Integer.MAX_VALUE)).toEqual("-2147483647")
      expect(Integer.toString(Integer.MIN_VALUE)).toEqual("-2147483648")
      expect(Integer.toString(0)).toEqual("0")
    }

    it("should provide `toString` with radix") {
      /* Spec ported from
       * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
       */
      expect(Integer.toString(2147483647, 8)).toEqual("17777777777")
      expect(Integer.toString(2147483647, 16)).toEqual("7fffffff")
      expect(Integer.toString(2147483647, 2)).toEqual("1111111111111111111111111111111")
      expect(Integer.toString(2147483647, 10)).toEqual("2147483647")
      expect(Integer.toString(-2147483647, 8)).toEqual("-17777777777")
      expect(Integer.toString(-2147483647, 16)).toEqual("-7fffffff")
      expect(Integer.toString(-2147483647, 2)).toEqual("-1111111111111111111111111111111")
      expect(Integer.toString(-2147483647, 10)).toEqual("-2147483647")
      expect(Integer.toString(-2147483648, 8)).toEqual("-20000000000")
      expect(Integer.toString(-2147483648, 16)).toEqual("-80000000")
      expect(Integer.toString(-2147483648, 2)).toEqual("-10000000000000000000000000000000")
      expect(Integer.toString(-2147483648, 10)).toEqual("-2147483648")
    }
  }
}
