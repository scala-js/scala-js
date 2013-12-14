/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

import java.util.{ Formatter, Formattable, FormattableFlags }

import java.lang.{
  Double  => JDouble,
  Float   => JFloat,
  Integer => JInteger,
  Long    => JLong,
  Byte    => JByte,
  Boolean => JBoolean,
  String  => JString
}


object FormatterTest extends JasmineTest {

  class HelperClass
  class FormattableClass extends Formattable {
    var frm: Formatter = _
    var flags: Int = _
    var width: Int = _
    var precision: Int = _
    var calls = 0
    def formatTo(frm: Formatter, flags: Int, width: Int, precision: Int) = {
      this.calls += 1
      this.flags = flags
      this.width = width
      this.precision = precision
      frm.out().append("foobar")
    }

    def expectCalled(times: Int, flags: Int, width: Int, precision: Int) = {
      expect(this.calls).toEqual(times)
      expect(this.flags).toEqual(flags)
      expect(this.width).toEqual(width)
      expect(this.precision).toEqual(precision)
    }

  }

  def expectF(format: String, args: AnyRef*) = {
    val fmt = new Formatter()
    val res = fmt.format(format, args:_*).toString()
    fmt.close()
    expect(res)
  }

  def expectFC(format: String, flags: Int, width: Int, precision: Int) = {
    val fc = new FormattableClass
    val exp = expectF(format, fc)
    fc.expectCalled(1, flags, width, precision)
    exp
  }

  def expectThrow(format: String, args: AnyRef*) = {
    val fmt = new Formatter()
    expect(() => fmt.format(format, args:_*)).toThrow
  }

  describe("java.util.Formatter") {

    it("should provide 'b' conversion") {
      expectF("%b", null).toEqual("false")
      expectF("%b", true: JBoolean).toEqual(JString.valueOf(true))
      expectF("%b", false: JBoolean).toEqual(JString.valueOf(false))
      expectF("%b", new HelperClass).toEqual("true")
    }

    it("should provide 'h' conversion") {
      val x = new HelperClass
      expectF("%h", x).toEqual(Integer.toHexString(x.hashCode()))
      expectF("%H", x).toEqual(Integer.toHexString(x.hashCode()).toUpperCase())
      expectF("%h", null).toEqual("null")
    }

    it("should provide 's' conversion") {
      expectFC("%s", 0, -1, -1).toEqual("foobar")
      expectFC("%-s", FormattableFlags.LEFT_JUSTIFY, -1, -1).toEqual("foobar")
      expectFC("%-10s", FormattableFlags.LEFT_JUSTIFY, 10, -1).toEqual("foobar")
      expectFC("%#-10.2s", FormattableFlags.LEFT_JUSTIFY |
               FormattableFlags.ALTERNATE, 10, 2).toEqual("foobar")
      expectFC("%#10.2S", FormattableFlags.UPPERCASE |
               FormattableFlags.ALTERNATE, 10, 2).toEqual("foobar")
      expectF("%10s", "hello").toEqual("     hello")
      expectF("%-10s", "hello").toEqual("hello     ")
      expectThrow("%#s", "hello")
    }

    it("should provide 'c' conversion") {
      expectF("%-5c", new Character('!')).toEqual("!    ")
    }

    it("should provide 'd' conversion") {
      expectF("%d",   new Integer(5)).toEqual("5")
      expectF("%05d", new Integer(5)).toEqual("00005")
      expectF("%5d",  new Integer(-10)).toEqual("  -10")
      expectF("%05d", new Integer(-10)).toEqual("-0010")
    }

    it("should provide 'o' conversion") {
      expectF("%o",   new JInteger(8)).toEqual("10")
      expectF("%05o", new JInteger(16)).toEqual("00020")
      expectF("%5o",  new JInteger(-10)).toEqual("37777777766")
      expectF("%05o", new JInteger(-10)).toEqual("37777777766")
      expectF("%05o", new JLong(-5L)).toEqual("1777777777777777777773")
    }

    it("should provide 'x' conversion") {
      expectF("%0#5x", new JInteger(5)).toEqual("0x005")
      expectF("%#5x",  new JInteger(5)).toEqual("  0x5")
      expectF("%#5X",  new JInteger(5)).toEqual("  0X5")
      expectF("%x",    new JInteger(-3)).toEqual("fffffffd")
      expectF("%x",    new JByte(-4.toByte)).toEqual("fc")
      expectF("%x",    new JLong(-5L)).toEqual("fffffffffffffffb")
      expectF("%X",    new JLong(26L)).toEqual("1A")
    }

    it("should provide 'e' conversion") {
      expectF("%e",       new JDouble(1000)).toEqual("1.000000e+03")
      expectF("%.0e",     new JDouble(1.2e100)).toEqual("1e+100")
      // We use 1.51e100 in this test, since we seem to have a floating
      // imprecision at exactly 1.5e100 that yields to a rounding error
      // towards (1e+100 instead of 2e+100)
      expectF("%.0e",     new JDouble(1.51e100)).toEqual("2e+100")
      expectF("%10.2e",   new JDouble(1.2e100)).toEqual(" 1.20e+100")
      expectF("%012.4e",  new JFloat(1.2e-21f)).toEqual("001.2000e-21")
      expectF("%012.4E",  new JFloat(1.2e-21f)).toEqual("001.2000E-21")
      expectF("%(015.4e", new JFloat(-1.2e-21f)).toEqual("(0001.2000e-21)")

      // Tests with infinity and NaN
      expectF("%e",    new JDouble(Double.PositiveInfinity)).toEqual("Infinity")
      expectF("%e",    new JDouble(Double.NegativeInfinity)).toEqual("-Infinity")
      expectF("%010e", new JDouble(Double.PositiveInfinity)).toEqual("  Infinity")
      expectF("%-10e", new JDouble(Double.PositiveInfinity)).toEqual("Infinity  ")
      expectF("%(e",   new JDouble(Double.NegativeInfinity)).toEqual("(Infinity)")
      expectF("%010e", new JDouble(Double.NaN)).toEqual("       NaN")
    }

    it("should provide 'g' conversion") {
      expectF("%g",   new JDouble(.5e-4)).toEqual("5.00000e-05")
      expectF("%g",   new JDouble(3e-4)).toEqual("0.000300000")
      expectF("%.3g", new JDouble(3e-4)).toEqual("0.000300")
      expectF("%.2g", new JDouble(1e-3)).toEqual("0.0010")
      expectF("%g",   new JDouble(3e5)).toEqual("300000")
      expectF("%.3g", new JDouble(3e5)).toEqual("3.00e+05")
      expectF("%04g", new JDouble(Double.NaN)).toEqual(" NaN")
    }

    it("should provide 'f' conversion") {
      expectF("%f",      new JDouble(3.3)).toEqual("3.300000")
      expectF("%0(9.4f", new JDouble(-4.6)).toEqual("(04.6000)")
      expectF("%f",      new JFloat(3e10f)).toEqual("30000001024.000000")
      expectF("%f",      new JDouble(3e10)).toEqual("30000000000.000000")
      expectF("%04f",    new JDouble(Double.NaN)).toEqual(" NaN")
    }

    it("should allow positional arguments") {
      expectF("%2$d %1$d",    new JInteger(1), new JInteger(2)).toEqual("2 1")
      expectF("%2$d %2$d %d", new JInteger(1), new JInteger(2)).toEqual("2 2 1")
      expectF("%2$d %<d %d",  new JInteger(1), new JInteger(2)).toEqual("2 2 1")
    }

    it("should fail when called after close") {
      val f = new Formatter()
      f.close()
      expect(() => f.toString()).toThrow
    }

    it("should fail with bad format specifier") {
      expectThrow("hello world%")
      expectThrow("%%%")
      expectThrow("%q")
      expectThrow("%1")
      expectThrow("%_f")
    }

    it("should fail with not enough arguments") {
      expectThrow("%f")
      expectThrow("%d%d%d", new JInteger(1), new JInteger(1))
      expectThrow("%10$d", new JInteger(1))
    }

  }


}

