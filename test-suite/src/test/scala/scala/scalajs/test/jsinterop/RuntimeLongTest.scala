/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package jsinterop

import scala.scalajs.runtime.RuntimeLong
import org.scalajs.jasmine.JasmineExpectation

import scala.util.Try

/**
 * test the runtime Long implementation directly
 * does not depend on magic compiler Long rewriting
 */
object RuntimeLongTest extends JasmineTest {

  import RuntimeLong.{
    fromByte, fromShort, fromInt, fromDouble, fromString, fromHexString
  }

  /** overload expect for long to add toString */
  def expect(l: RuntimeLong): JasmineExpectation = expect(l.toHexString)

  describe("scala.scalajs.runtime.Long") {

    val maxInt  = fromInt(Int.MaxValue)
    val minInt  = fromInt(Int.MinValue)
    val one     = fromInt(1)
    val billion = fromInt(1000000000)

    it("should correctly implement negation") {
      expect(-fromInt(5)).toEqual("fffffffffffffffb")
      expect(-fromInt(0)).toEqual("0000000000000000")
      expect(-minInt    ).toEqual("0000000080000000")
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
      expect(fromInt(7) + fromInt(15)).toEqual("0000000000000016")
      expect(    maxInt + maxInt     ).toEqual("00000000fffffffe")
      expect(    maxInt + one        ).toEqual("0000000080000000")
    }

    it("should correctly implement subtraction") {
      expect(fromInt(7) - fromInt(15)).toEqual("fffffffffffffff8")
      expect(    maxInt - maxInt    ).toEqual("0000000000000000")
    }

    it("should correctly implement multiplication") {
      expect(fromInt(7)  * fromInt(15)).toEqual("0000000000000069")
      expect(fromInt(-7) * fromInt(15)).toEqual("ffffffffffffff97")
      expect(    maxInt  * maxInt     ).toEqual("3fffffff00000001")
      expect(fromHexString("001000000000000e") *
             fromInt(-4)).toEqual("ffbfffffffffffc8")
    }

    it("should correctly implement division") {
      expect( fromInt(7)  / fromInt(15)).toEqual("0000000000000000")
      expect( fromInt(24) / fromInt(5) ).toEqual("0000000000000004")
      expect( fromInt(24) / fromInt(-5)).toEqual("fffffffffffffffc")
      expect(      maxInt / fromInt(-5)).toEqual("ffffffffe6666667")
      expect(      maxInt / billion    ).toEqual("0000000000000002")
      expect((maxInt+one) / billion    ).toEqual("0000000000000002")
    }

    it("should correctly implement modulus") {
      expect( fromInt(7)  % fromInt(15)).toEqual("0000000000000007")
      expect( fromInt(24) % fromInt(5) ).toEqual("0000000000000004")
      expect( fromInt(24) % fromInt(-5)).toEqual("0000000000000004")
      expect(      maxInt % billion    ).toEqual("0000000008ca6bff")
      expect((maxInt+one) % billion    ).toEqual("0000000008ca6c00")
      expect(      maxInt % fromInt(-5)).toEqual("0000000000000002")
    }

    it("should correctly implement toString") {
      expect(maxInt.toString).toEqual("2147483647")
      expect(fromInt(-50).toString).toEqual("-50")
      expect(fromInt(-1000000000).toString).toEqual("-1000000000")
      expect((maxInt+one).toString).toEqual("2147483648")
      expect(minInt.toString).toEqual("-2147483648")
    }

    it("should correctly implement fromDouble") {
      expect(fromDouble( 4.5)).toEqual("0000000000000004")
      expect(fromDouble(-4.5)).toEqual("fffffffffffffffc")
    }

    it("should correctly implement toDouble") {
      expect(fromInt(5).toDouble).toEqual(5.0)
      expect((maxInt+one).toDouble).toEqual(2147483648.0)
    }

    it("should correctly implement fromString") {
      expect(fromString("4")                 ).toEqual("0000000000000004")
      expect(fromString("-4")                ).toEqual("fffffffffffffffc")
      expect(fromString("4000000000")        ).toEqual("00000000ee6b2800")
      expect(fromString("-18014398509482040")).toEqual("ffbfffffffffffc8")

      expect(Try(fromString("asdf")).isFailure).toBeTruthy
    }

    it("should correctly implement numberOfLeadingZeros") {
      expect(fromInt( 0).numberOfLeadingZeros).toEqual(64)
      expect(fromInt( 1).numberOfLeadingZeros).toEqual(63)
      expect(fromInt(-1).numberOfLeadingZeros).toEqual(0)
      expect(fromInt( 2).numberOfLeadingZeros).toEqual(62)
    }

  }

}


