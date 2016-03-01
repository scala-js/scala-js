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

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import java.util._

import java.lang.{
Double  => JDouble,
Float   => JFloat,
Integer => JInteger,
Long    => JLong,
Byte    => JByte,
Short   => JShort,
Boolean => JBoolean,
String  => JString
}

class FormatterTest {

  class HelperClass
  class FormattableClass extends Formattable {
    var frm: Formatter = _
    var flags: Int = _
    var width: Int = _
    var precision: Int = _
    var calls = 0

    def formatTo(frm: Formatter, flags: Int, width: Int, precision: Int): Unit = {
      this.calls += 1
      this.flags = flags
      this.width = width
      this.precision = precision
      frm.out().append("foobar")
    }

    def expectCalled(times: Int, flags: Int, width: Int, precision: Int): Unit = {
      assertEquals(times, this.calls)
      assertEquals(flags, this.flags)
      assertEquals(width, this.width)
      assertEquals(precision, this.precision)
    }

  }

  def assertF(expected: String, format: String, args: AnyRef*): Unit = {
    val fmt = new Formatter()
    val res = fmt.format(format, args:_*).toString()
    fmt.close()
    assertEquals(expected, res)
  }

  def assertFC(expected: String, format: String, flags: Int, width: Int,
      precision: Int): Unit = {
    val fc = new FormattableClass
    assertF(expected, format, fc)
    fc.expectCalled(1, flags, width, precision)
  }

  def expectFormatterThrows[T <: Throwable](exeption: Class[T], format: String,
      args: AnyRef*): Unit = {
    val fmt = new Formatter()
    assertThrows(exeption, fmt.format(format, args:_*))
  }

  // Explicitly define these as `var`'s to avoid any compile-time constant folding
  var IntMax: Int = Int.MaxValue
  var IntMin: Int = Int.MinValue
  var ByteMax: Byte = Byte.MaxValue
  var ByteMin: Byte = Byte.MinValue
  var ShortMax: Short = Short.MaxValue
  var ShortMin: Short = Short.MinValue

  @Test def `should_provide_b_conversion`(): Unit = {
    assertF("false", "%b", null)
    assertF(JString.valueOf(true), "%b", true: JBoolean)
    assertF(JString.valueOf(false), "%b", false: JBoolean)
    assertF("true", "%b", new HelperClass)
  }

  @Test def `should_provide_h_conversion`(): Unit = {
    val x = new HelperClass
    assertF(Integer.toHexString(x.hashCode()), "%h", x)
    assertF(Integer.toHexString(x.hashCode()).toUpperCase(), "%H", x)
    assertF("null", "%h", null)
  }

  @Test def `should_provide_s_conversion`(): Unit = {
    assertFC("foobar", "%s", 0, -1, -1)
    assertFC("foobar", "%-10s", FormattableFlags.LEFT_JUSTIFY, 10, -1)
    assertFC("foobar", "%#-10.2s",
        FormattableFlags.LEFT_JUSTIFY | FormattableFlags.ALTERNATE, 10, 2)
    assertFC("foobar", "%#10.2S",
        FormattableFlags.UPPERCASE | FormattableFlags.ALTERNATE, 10, 2)
    assertF("     hello", "%10s", "hello")
    assertF("hello     ", "%-10s", "hello")
    if (!executingInJVMOnJDK6)
      expectFormatterThrows(classOf[Exception], "%#s", "hello")
  }

  @Test def `should_fail_s_conversions_without_width`(): Unit = {
    // Issue #2246
    expectFormatterThrows(classOf[MissingFormatWidthException], "%-s", "abc")
  }

  @Test def `should_provide_c_conversion`(): Unit = {
    assertF("!    ", "%-5c", new Character('!'))
  }

  @Test def `should_provide_d_conversion`(): Unit = {
    assertF("5", "%d", new Integer(5))
    assertF("00005", "%05d", new Integer(5))
    assertF("  -10", "%5d", new Integer(-10))
    assertF("-0010", "%05d", new Integer(-10))
  }

  @Test def `should_provide_o_conversion`(): Unit = {
    assertF("10", "%o", new JInteger(8))
    assertF("00020", "%05o", new JInteger(16))
    assertF("37777777766", "%5o", new JInteger(-10))
    assertF("37777777766", "%05o", new JInteger(-10))
    assertF("10", "%o", new JByte(8.toByte))
    assertF("00020", "%05o", new JByte(16.toByte))
    assertF("10", "%o", new JShort(8.toShort))
    assertF("00020", "%05o", new JShort(16.toShort))
    if (!executingInJVM) {
      // expected:<   [377777777]66> but was:<   [        3]66>
      assertF("   37777777766", "%14o", new JByte(-10.toByte))
      // expected:<[377777777]66> but was:<[003]66>
      assertF("37777777766", "%05o", new JByte(-10.toByte))
      // expected:<[377777]77766> but was:<[1]77766>
      assertF("1777777777777777777773", "%05o", new JLong(-5L))
      // expected:<0000[377777]77766> but was:<0000[000001]77766>
      assertF("37777777766", "%5o", new JShort(-10.toShort))
      // expected:<0000[377777]77766> but was:<0000[000001]77766>
      assertF("000037777777766", "%015o",new JShort(-10.toShort))
    }
  }

  @Test def `should_provide_x_conversion`(): Unit = {
    assertF("0x005", "%0#5x", new JInteger(5))
    assertF("  0x5", "%#5x", new JInteger(5))
    assertF("  0X5", "%#5X", new JInteger(5))
    assertF("fffffffd", "%x", new JInteger(-3))
    if (!executingInJVM) {
      // expected:<f[ffffff]c> but was:<f[]c>
      assertF("fffffffc", "%x", new JByte(-4.toByte))
    }
    assertF("0x005", "%0#5x", new JByte(5.toByte))
    assertF("  0x5", "%#5x", new JByte(5.toByte))
    assertF("  0X5", "%#5X", new JByte(5.toByte))
    if (!executingInJVM) {
      // expected:<f[ffffff]d> but was:<f[]d>
      assertF("fffffffd", "%x", new JByte(-3.toByte))
    }
    assertF("0x005", "%0#5x", new JShort(5.toShort))
    assertF("  0x5", "%#5x", new JShort(5.toShort))
    assertF("  0X5", "%#5X", new JShort(5.toShort))
    if (!executingInJVM) {
      // expected:<fff[ffff]d> but was:<fff[]d>
      assertF("fffffffd", "%x", new JShort(-3.toShort))
    }
    assertF("fffffffffffffffb", "%x", new JLong(-5L))
    assertF("1A", "%X", new JLong(26L))
  }

  @Test def `should_provide_e_conversion`(): Unit = {
    assertF("1.000000e+03", "%e", new JDouble(1000))
    assertF("1e+100", "%.0e", new JDouble(1.2e100))
    // We use 1.51e100 in this test, since we seem to have a floating
    // imprecision at exactly 1.5e100 that yields to a rounding error
    // towards (1e+100 instead of 2e+100)
    assertF("2e+100", "%.0e", new JDouble(1.51e100))
    assertF(" 1.20e+100", "%10.2e", new JDouble(1.2e100))
    assertF("001.2000e-21", "%012.4e", new JFloat(1.2e-21f))
    assertF("001.2000E-21", "%012.4E", new JFloat(1.2e-21f))
    assertF("(0001.2000e-21)", "%(015.4e", new JFloat(-1.2e-21f))

    // Tests with infinity and NaN
    assertF("Infinity", "%e", new JDouble(Double.PositiveInfinity))
    assertF("-Infinity", "%e", new JDouble(Double.NegativeInfinity))
    assertF("  Infinity", "%010e", new JDouble(Double.PositiveInfinity))
    assertF("Infinity  ", "%-10e", new JDouble(Double.PositiveInfinity))
    assertF("(Infinity)", "%(e", new JDouble(Double.NegativeInfinity))
    assertF("       NaN", "%010e", new JDouble(Double.NaN))
  }

  @Test def `should_provide_g_conversion`(): Unit = {
    assertF("5.00000e-05", "%g", new JDouble(.5e-4))
    assertF("0.000300000", "%g", new JDouble(3e-4))
    assertF("0.000300", "%.3g", new JDouble(3e-4))
    assertF("0.0010", "%.2g", new JDouble(1e-3))
    assertF("300000", "%g", new JDouble(3e5))
    assertF("3.00e+05", "%.3g", new JDouble(3e5))
    assertF(" NaN", "%04g", new JDouble(Double.NaN))
  }

  @Test def `should_provide_f_conversion`(): Unit = {
    assertF("3.300000", "%f", new JDouble(3.3))
    assertF("(04.6000)", "%0(9.4f", new JDouble(-4.6))
    assertF("30000001024.000000", "%f", new JFloat(3e10f))
    assertF("30000000000.000000", "%f", new JDouble(3e10))
    assertF(" NaN", "%04f", new JDouble(Double.NaN))
  }

  @Test def `should_support_%%`(): Unit = {
    assertF("1%2", "%d%%%d", new JInteger(1), new JInteger(2))
  }

  @Test def `should_support_%n`(): Unit = {
    assertF("1\n2", "%d%n%d", new JInteger(1), new JInteger(2))
  }

  @Test def `should_survive_null`(): Unit = {
    assertF("null", "%s", null)
  }

  @Test def `should_allow_f_string_interpolation_to_survive_null`(): Unit = {
    assertEquals("null", f"${null}%s")
  }

  @Test def should_allow_positional_arguments(): Unit = {
    assertF("2 1", "%2$d %1$d", new JInteger(1), new JInteger(2))
    assertF("2 2 1", "%2$d %2$d %d", new JInteger(1), new JInteger(2))
    assertF("2 2 1", "%2$d %<d %d", new JInteger(1), new JInteger(2))
  }

  @Test def should_fail_when_called_after_close(): Unit = {
    val f = new Formatter()
    f.close()
    assertThrows(classOf[FormatterClosedException], f.toString())
  }

  @Test def should_fail_with_bad_format_specifier(): Unit = {
    expectFormatterThrows(classOf[Exception], "hello world%")
    expectFormatterThrows(classOf[Exception], "%%%")
    expectFormatterThrows(classOf[Exception], "%q")
    expectFormatterThrows(classOf[Exception], "%1")
    expectFormatterThrows(classOf[Exception], "%_f")
  }

  @Test def should_fail_with_not_enough_arguments(): Unit = {
    expectFormatterThrows(classOf[MissingFormatArgumentException], "%f")
    expectFormatterThrows(classOf[MissingFormatArgumentException], "%d%d%d",
      new JInteger(1), new JInteger(1))
    expectFormatterThrows(classOf[MissingFormatArgumentException], "%10$d",
      new JInteger(1))
  }
}
