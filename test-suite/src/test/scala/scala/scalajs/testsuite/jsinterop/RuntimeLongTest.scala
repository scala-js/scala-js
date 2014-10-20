/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.jsinterop

import scala.scalajs.runtime.RuntimeLong

import org.scalajs.jasmine.JasmineExpectation
import org.scalajs.jasminetest.JasmineTest

import scala.util.Try

/**
 * test the runtime Long implementation directly
 * does not depend on magic compiler Long rewriting
 */
object RuntimeLongTest extends JasmineTest {

  import RuntimeLong.fromDouble

  /** overload expect for long to add toString */
  def expect(l: RuntimeLong): JasmineExpectation = expect(l.toHexString)

  describe("scala.scalajs.runtime.RuntimeLong") {

    def fromInt(x: Int): RuntimeLong = new RuntimeLong(x)

    val maxInt  = fromInt(Int.MaxValue)
    val minInt  = fromInt(Int.MinValue)
    val one     = fromInt(1)
    val billion = fromInt(1000000000)

    val `4503599627370510L`    = new RuntimeLong(     14,       0,    256)
    val `613354684553L`        = new RuntimeLong( 639113,  146235,      0)
    val `9863155567412L`       = new RuntimeLong(2247476, 2351559,      0)
    val `3632147899696541255L` = new RuntimeLong(1568327, 2954580, 206463)
    val `7632147899696541255L` = new RuntimeLong(2616903, 1593290, 433837)

    it("should correctly implement negation") {
      expect(-fromInt(5)).toEqual("fffffffffffffffb")
      expect(-fromInt(0)).toEqual("0")
      expect(-minInt    ).toEqual("80000000")
    }

    it("should correctly implement comparison") {
      expect(fromInt(7)  <  fromInt(15)).toBe(true)
      expect(fromInt(15) <  fromInt(15)).toBe(false)
      expect(fromInt(15) <= fromInt(15)).toBe(true)
      expect(fromInt(14) <= fromInt(15)).toBe(true)
      expect(fromInt(15) >  fromInt(15)).toBe(false)
      expect(fromInt(14) >  fromInt(15)).toBe(false)
      expect(fromInt(16) >  fromInt(15)).toBe(true)
      expect(fromInt(15) >= fromInt(15)).toBe(true)
      expect(fromInt(14) >= fromInt(15)).toBe(false)
      expect(fromInt(16) >= fromInt(15)).toBe(true)
    }

    it("should correctly implement addition") {
      expect(fromInt(7) + fromInt(15)).toEqual("16")
      expect(    maxInt + maxInt     ).toEqual("fffffffe")
      expect(    maxInt + one        ).toEqual("80000000")
    }

    it("should correctly implement subtraction") {
      expect(fromInt(7) - fromInt(15)).toEqual("fffffffffffffff8")
      expect(    maxInt - maxInt    ).toEqual("0")
    }

    it("should correctly implement multiplication") {
      expect(fromInt(7)  * fromInt(15)).toEqual("69")
      expect(fromInt(-7) * fromInt(15)).toEqual("ffffffffffffff97")
      expect(    maxInt  * maxInt     ).toEqual("3fffffff00000001")
      expect(`4503599627370510L` * fromInt(-4)).toEqual("ffbfffffffffffc8")
    }

    it("should correctly implement division") {
      expect( fromInt(7)  / fromInt(15)).toEqual("0")
      expect( fromInt(24) / fromInt(5) ).toEqual("4")
      expect( fromInt(24) / fromInt(-5)).toEqual("fffffffffffffffc")
      expect(      maxInt / fromInt(-5)).toEqual("ffffffffe6666667")
      expect(      maxInt / billion    ).toEqual("2")
      expect((maxInt+one) / billion    ).toEqual("2")
    }

    it("should correctly implement modulus") {
      expect( fromInt(7)  % fromInt(15)).toEqual("7")
      expect( fromInt(24) % fromInt(5) ).toEqual("4")
      expect( fromInt(24) % fromInt(-5)).toEqual("4")
      expect(      maxInt % billion    ).toEqual("8ca6bff")
      expect((maxInt+one) % billion    ).toEqual("8ca6c00")
      expect(      maxInt % fromInt(-5)).toEqual("2")
    }

    it("should correctly implement toString") {
      expect(maxInt.toString).toEqual("2147483647")
      expect(fromInt(-50).toString).toEqual("-50")
      expect(fromInt(-1000000000).toString).toEqual("-1000000000")
      expect((maxInt+one).toString).toEqual("2147483648")
      expect(minInt.toString).toEqual("-2147483648")
    }

    it("should correctly implement fromDouble") {
      expect(fromDouble( 4.5)).toEqual("4")
      expect(fromDouble(-4.5)).toEqual("fffffffffffffffc")
    }

    it("should correctly implement toDouble") {
      expect(fromInt(5).toDouble).toEqual(5.0)
      expect((maxInt+one).toDouble).toEqual(2147483648.0)
    }

    it("should correctly implement numberOfLeadingZeros") {
      expect(fromInt( 0).numberOfLeadingZeros).toEqual(64)
      expect(fromInt( 1).numberOfLeadingZeros).toEqual(63)
      expect(fromInt(-1).numberOfLeadingZeros).toEqual(0)
      expect(fromInt( 2).numberOfLeadingZeros).toEqual(62)
    }

    it("should implement hashCode() according to spec in j.l.Long") {
      expect(fromInt(0       ).hashCode()).toEqual(0)
      expect(fromInt(55      ).hashCode()).toEqual(55)
      expect(fromInt(-12     ).hashCode()).toEqual(11)
      expect(fromInt(10006548).hashCode()).toEqual(10006548)
      expect(fromInt(-1098748).hashCode()).toEqual(1098747)

      expect(`613354684553L`       .hashCode()).toEqual(-825638905)
      expect(`9863155567412L`      .hashCode()).toEqual(1910653900)
      expect(`3632147899696541255L`.hashCode()).toEqual(1735398658)
      expect(`7632147899696541255L`.hashCode()).toEqual(-1689438124)
    }

  }

}


