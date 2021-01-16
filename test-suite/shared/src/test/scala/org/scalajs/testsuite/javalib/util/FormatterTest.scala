/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.javalib.util

import java.math.BigInteger

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import java.util._

class FormatterTest {
  import FormatterTest._

  def assertF(expected: String, format: String, args: Any*): Unit = {
    val fmt = new Formatter()
    val res = fmt.format(format, args.asInstanceOf[Seq[AnyRef]]: _*).toString()
    fmt.close()
    assertEquals(expected, res)
  }

  def testWithInfinityAndNaN(conversion: Char, acceptSharp: Boolean = true,
      acceptComma: Boolean = true, acceptUpperCase: Boolean = true): Unit = {

    import Double.{NaN, PositiveInfinity => PosInf, NegativeInfinity => NegInf}

    assertF("Infinity", "%" + conversion, PosInf)
    assertF("-Infinity", "%" + conversion, NegInf)
    assertF("  Infinity", "%010" + conversion, PosInf)
    assertF(" -Infinity", "%010" + conversion, NegInf)
    assertF("Infinity  ", "%-10" + conversion, PosInf)
    assertF("(Infinity)", "%(" + conversion, NegInf)
    assertF("     (Infinity)", "%(15" + conversion, NegInf)
    assertF("     (Infinity)", "%(015" + conversion, NegInf)
    assertF("       NaN", "%10" + conversion, NaN)
    assertF("       NaN", "%010" + conversion, NaN)

    assertF("+Infinity", "%+" + conversion, PosInf)
    assertF(" Infinity", "% " + conversion, PosInf)
    assertF("-Infinity", "%+" + conversion, NegInf)
    assertF("-Infinity", "% " + conversion, NegInf)
    assertF("NaN", "%+" + conversion, NaN)
    assertF("NaN", "% " + conversion, NaN)

    assertF("+Infinity", "%+(" + conversion, PosInf)
    assertF(" Infinity", "% (" + conversion, PosInf)
    assertF("(Infinity)", "%+(" + conversion, NegInf)
    assertF("(Infinity)", "% (" + conversion, NegInf)

    if (acceptSharp)
      assertF("Infinity", "%#" + conversion, PosInf)

    if (acceptComma)
      assertF("Infinity", "%," + conversion, PosInf)

    if (acceptUpperCase) {
      val upConversion = conversion.toUpper
      assertF("INFINITY", "%" + upConversion, PosInf)
      assertF("-INFINITY", "%" + upConversion, NegInf)
      assertF("NAN", "%" + upConversion, NaN)
    }
  }

  /* Every conversion accepts `null` as input. Other than `%b`, which formats
   * `null` as `"false"`, they all format it as `"null"`. While they reject
   * flags and/or precision according to the conversion, they will then all
   * handle the width, the `-` flag and the precision as if the conversion were
   * `%s`. Notably, the precision truncates the string.
   */
  def testWithNull(conversion: Char, flags: String,
      acceptPrecision: Boolean = true,
      acceptUpperCase: Boolean = true): Unit = {

    assertF("null", "%" + conversion, null)

    for (flag <- flags)
      assertF("null", "%" + flag + "1" + conversion, null)

    assertF("  null", "%6" + conversion, null)
    assertF("null  ", "%-6" + conversion, null)

    if (acceptPrecision) {
      assertF("nul", "%.3" + conversion, null)
      assertF("  nul", "%5.3" + conversion, null)
      assertF("nul  ", "%-5.3" + conversion, null)
    }

    if (acceptUpperCase) {
      val upConversion = conversion.toUpper
      assertF("NULL", "%" + upConversion, null)
      assertF("  NULL", "%6" + upConversion, null)
    }
  }

  def expectFormatterThrows[T <: Throwable](exeption: Class[T], format: String,
      args: Any*): T = {
    val fmt = new Formatter()
    expectThrows(exeption,
        fmt.format(format, args.asInstanceOf[Seq[AnyRef]]: _*))
  }

  def expectFormatFlagsConversionMismatch(conversion: Char,
      invalidFlags: String, arg: Any): Unit = {

    for (flag <- invalidFlags) {
      val e = expectFormatterThrows(
          classOf[FormatFlagsConversionMismatchException],
          "%" + flag + conversion, arg)
      assertEquals(flag.toString, e.getFlags)
      assertEquals(conversion, e.getConversion)
    }
  }

  def expectIllegalFormatFlags(format: String, flags: String,
      arg: Any): Unit = {
    val e = expectFormatterThrows(classOf[IllegalFormatFlagsException],
        format, arg)
    assertEquals(flags, e.getFlags)
  }

  def expectIllegalFormatPrecision(conversion: Char, arg: Any): Unit = {
    val e = expectFormatterThrows(classOf[IllegalFormatPrecisionException],
        "%.5" + conversion, arg)
    assertEquals(5, e.getPrecision)
  }

  def expectIllegalFormatWidth(conversion: Char, arg: Any): Unit = {
    val e = expectFormatterThrows(classOf[IllegalFormatWidthException],
        "%5" + conversion, arg)
    assertEquals(5, e.getWidth)
  }

  def expectIllegalFormatConversion(conversion: Char, arg: Any): Unit = {
    val e = expectFormatterThrows(classOf[IllegalFormatConversionException],
        "%" + conversion, arg)
    assertEquals(conversion, e.getConversion)
    assertEquals(arg.getClass, e.getArgumentClass)
  }

  def expectUnknownFormatConversion(format: String, conversion: Char): Unit = {
    val e = expectFormatterThrows(classOf[UnknownFormatConversionException],
        format, 1, 2, 3)
    assertEquals(conversion.toString, e.getConversion)
  }

  @Test def formatB(): Unit = {
    assertF("false", "%b", null)
    assertF("true", "%b", true)
    assertF("false", "%b", false)
    assertF("true", "%b", new HelperClass)

    assertF("  false", "%7b", false)
    assertF("   true", "%7b", true)
    assertF("false  ", "%-7b", false)
    assertF("true   ", "%-7b", true)

    assertF("FALSE", "%B", false)

    assertF("tru", "%.3b", true)
    assertF("     tru", "%8.3b", true)
    assertF("fal", "%.3b", null)
    assertF("     fal", "%8.3b", null)

    expectFormatFlagsConversionMismatch('b', "#+ 0,(", true)
  }

  @Test def formatH(): Unit = {
    val x = new HelperClass
    assertF("f1e2a3", "%h", x)
    assertF("F1E2A3", "%H", x)

    assertF("  f1e2a3", "%8h", x)

    assertF("f1e2a", "%.5h", x)

    testWithNull('h', "")

    expectFormatFlagsConversionMismatch('h', "#+ 0,(", x)
  }

  @Test def formatSWithNonFormattable(): Unit = {
    assertF("abcdef", "ab%sef", "cd")
    assertF("true", "%s", true)
    assertF("12345", "%s", 12345)

    assertF("ABCDEF", "%S", "aBcdeF")

    assertF("     hello", "%10s", "hello")
    assertF("hello     ", "%-10s", "hello")

    assertF("hel", "%.3s", "hello")
    assertF("    HEL", "%7.3S", "hello")

    testWithNull('s', "")

    expectFormatFlagsConversionMismatch('s',
        if (executingInJVMOnJDK6) "+ 0,(" else "#+ 0,(", "hello")
  }

  @Test def formatSWithFormattable(): Unit = {
    import FormattableFlags._

    class FormattableClass extends Formattable {
      private var flags: Int = _
      private var width: Int = _
      private var precision: Int = _
      private var calls: Int = 0

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

      def expectNotCalled(): Unit =
        assertEquals(0, this.calls)
    }

    def test(format: String, flags: Int, width: Int, precision: Int): Unit = {
      val fc = new FormattableClass
      assertF("foobar", format, fc)
      fc.expectCalled(1, flags, width, precision)
    }

    test("%s", 0, -1, -1)
    test("%-10s", LEFT_JUSTIFY, 10, -1)
    test("%#-10.2s", LEFT_JUSTIFY | ALTERNATE, 10, 2)
    test("%#10.2S", UPPERCASE | ALTERNATE, 10, 2)

    val x = new FormattableClass
    expectFormatFlagsConversionMismatch('s', "+ 0,(", x)
    x.expectNotCalled()
  }

  @Test def formatC(): Unit = {
    assertF("a", "%c", 'a')
    assertF("A", "%C", 'A')
    assertF("A", "%c", 65)
    assertF("\ud83d\udca9", "%c", 0x1f4a9)

    assertF("    !", "%5c", '!')
    assertF("!    ", "%-5c", '!')

    testWithNull('c', "", acceptPrecision = false)

    expectFormatFlagsConversionMismatch('c', "#+ 0,(", 'A')
    expectIllegalFormatPrecision('c', 'A')

    val e = expectFormatterThrows(classOf[IllegalFormatCodePointException],
        "%c", 0x123456)
    assertEquals(0x123456, e.getCodePoint)
  }

  @Test def formatD(): Unit = {
    assertF("5", "%d", 5)
    assertF("-5", "%d", -5)
    assertF("5", "%d", new BigInteger("5"))
    assertF("-5", "%d", new BigInteger("-5"))


    assertF("00005", "%05d", 5)
    assertF("  -10", "%5d", -10)
    assertF("-0010", "%05d", -10)

    assertF("00005", "%05d", new BigInteger("5"))
    assertF("  -10", "%5d", new BigInteger("-10"))
    assertF("-0010", "%05d", new BigInteger("-10"))

    assertF("345,678", "%,d", 345678)
    assertF("-345,678", "%,d", -345678)
    assertF("12,345,678", "%,d", 12345678)
    assertF("    12,345,678", "%,14d", 12345678)
    assertF("000012,345,678", "%,014d", 12345678)
    assertF("   -12,345,678", "%,14d", -12345678)

    assertF("0012,345,678,987,654", "%,020d", new BigInteger("12345678987654"))
    assertF(" -12,345,678,987,654", "%,20d", new BigInteger("-12345678987654"))

    assertF("2,345,678", "%,d", 2345678)
    assertF("345,678", "%,d", 345678)
    assertF("45,678", "%,d", 45678)
    assertF("5,678", "%,d", 5678)
    assertF("678", "%,d", 678)
    assertF("78", "%,d", 78)
    assertF("8", "%,d", 8)

    assertF("56", "%(d", 56)
    assertF("(56)", "%(d", -56)
    assertF("    (56)", "%(8d", -56)
    assertF("(000056)", "%(08d", -56)

    assertF(" 56", "% d", 56)
    assertF("-56", "% d", -56)
    assertF("+56", "%+d", 56)
    assertF("-56", "%+d", -56)

    assertF(" 00056", "% 0,6d", 56)
    assertF("-00056", "% 0,6d", -56)
    assertF(" 00056", "% 0,(6d", 56)
    assertF("(0056)", "% 0,(6d", -56)

    assertF("56    ", "%-6d", 56)
    assertF("-56   ", "%-6d", -56)

    assertF("43212345678987654321", "%d", new BigInteger("43212345678987654321"))

    testWithNull('d', "+ (", acceptPrecision = false, acceptUpperCase = false)

    expectIllegalFormatFlags("%+- (5d", "-+ (", 56)
    expectIllegalFormatFlags("%+-0(5d", "-+0(", 56)
    expectIllegalFormatPrecision('d', 5)
  }

  @Test def formatO(): Unit = {
    assertF("10", "%o", 8)
    assertF("52", "%o", new BigInteger("42"))

    assertF("00020", "%05o", 16)
    assertF("37777777766", "%5o", -10)
    assertF("37777777766", "%05o", -10)

    assertF("00020", "%05o", new BigInteger("16"))
    assertF("  -12", "%5o", new BigInteger("-10"))
    assertF("-0012", "%05o", new BigInteger("-10"))

    assertF("   21", "%5o", 17)
    assertF("21   ", "%-5o", 17)

    assertF("10", "%o", 8.toByte)
    assertF("00020", "%05o", 16.toByte)
    assertF("10", "%o", 8.toShort)
    assertF("00020", "%05o", 16.toShort)

    assertF("30071", "%o", 12345L)
    assertF("1777777777777777747707", "%o", -12345L)

    assertF("     30071", "%10o", 12345L)
    assertF("1777777777777777747707", "%10o", -12345L)
    assertF("0000030071", "%010o", 12345L)
    assertF("1777777777777777747707", "%010o", -12345L)

    assertF("0664", "%#o", 436)
    assertF("    0664", "%#8o", 436)
    assertF("00000664", "%#08o", 436)

    assertF("0664", "%#o", new BigInteger("436"))
    assertF("-0664", "%#o", new BigInteger("-436"))
    assertF("    0664", "%#8o", new BigInteger("436"))
    assertF("   -0664", "%#8o", new BigInteger("-436"))
    assertF("00000664", "%#08o", new BigInteger("436"))
    assertF("-0000664", "%#08o", new BigInteger("-436"))

    assertF("04536610567107334372261", "%#o", new BigInteger("43212345678987654321"))

    // #4351 Unlike Ints and Longs, BigIntegers support "+ ("
    assertF("+664", "%+(o", new BigInteger("436"))
    assertF("(664)", "%+(o", new BigInteger("-436"))
    assertF(" 664", "% (o", new BigInteger("436"))
    assertF("(664)", "% (o", new BigInteger("-436"))

    /* Negative Bytes and Shorts are formatted as if they were Ints.
     * This is a consequence of the non-boxing behavior of numbers in Scala.js.
     */
    assertF("   37777777766", "%14o", asIntOnJVM(-10.toByte))
    assertF("37777777766", "%05o", asIntOnJVM(-10.toByte))
    assertF("37777777766", "%5o", asIntOnJVM(-10.toShort))
    assertF("000037777777766", "%015o", asIntOnJVM(-10.toShort))

    testWithNull('o', "#+ 0(", acceptPrecision = false, acceptUpperCase = false)

    expectFormatFlagsConversionMismatch('o', "+ ,(", 5)
    expectFormatFlagsConversionMismatch('o', "+ ,(", 5L)
    expectFormatFlagsConversionMismatch('o', ",", new BigInteger("5"))
    expectIllegalFormatPrecision('o', 5)
  }

  @Test def formatX(): Unit = {
    assertF("d431", "%x", 54321)
    assertF("ffff2bcf", "%x", -54321)
    assertF("D431", "%X", 54321)
    assertF("FFFF2BCF", "%X", -54321)
    assertF("2a", "%x", new BigInteger("42"))
    assertF("2A", "%X", new BigInteger("42"))

    assertF("0xd431", "%#x", 54321)
    assertF("0xffff2bcf", "%#x", -54321)
    assertF("0XD431", "%#X", 54321)
    assertF("0XFFFF2BCF", "%#X", -54321)

    assertF("0xd431", "%#x", new BigInteger("54321"))
    assertF("-0xd431", "%#x", new BigInteger("-54321"))
    assertF("0XD431", "%#X", new BigInteger("54321"))
    assertF("-0XD431", "%#X", new BigInteger("-54321"))

    assertF("         d431", "%13x", 54321)
    assertF("     ffff2bcf", "%13x", -54321)
    assertF("         D431", "%13X", 54321)
    assertF("     FFFF2BCF", "%13X", -54321)
    assertF("       0xd431", "%#13x", 54321)
    assertF("   0xffff2bcf", "%#13x", -54321)

    assertF("d431         ", "%-13x", 54321)
    assertF("ffff2bcf     ", "%-13x", -54321)
    assertF("0xd431       ", "%-#13x", 54321)
    assertF("0xffff2bcf   ", "%-#13x", -54321)

    assertF("000000000d431", "%013x", 54321)
    assertF("00000ffff2bcf", "%013x", -54321)
    assertF("0x0000000d431", "%0#13x", 54321)
    assertF("0x000ffff2bcf", "%0#13x", -54321)

    assertF("000003ade68b1", "%013x", new BigInteger("987654321"))
    assertF("-00003ade68b1", "%013x", new BigInteger("-987654321"))
    assertF("0x0003ade68b1", "%0#13x", new BigInteger("987654321"))
    assertF("-0x003ade68b1", "%0#13x", new BigInteger("-987654321"))

    assertF("fffffffc", "%x", asIntOnJVM(-4.toByte))
    assertF("0x005", "%0#5x", 5.toByte)
    assertF("  0x5", "%#5x", 5.toByte)
    assertF("  0X5", "%#5X", 5.toByte)
    assertF("fffffffd", "%x", asIntOnJVM(-3.toByte))

    assertF("0x005", "%0#5x", 5.toShort)
    assertF("  0x5", "%#5x", 5.toShort)
    assertF("  0X5", "%#5X", 5.toShort)
    assertF("fffffffd", "%x", asIntOnJVM(-3.toShort))

    assertF("ffffffffffff2bcf", "%x", -54321L)
    assertF("28EEA4CB1", "%X", 10987654321L)

    assertF("0x257b117723b71f4b1", "%#x", new BigInteger("43212345678987654321"))

    // #4351 Unlike Ints and Longs, BigIntegers support "+ ("
    assertF("+1b4", "%+(x", new BigInteger("436"))
    assertF("(1b4)", "%+(x", new BigInteger("-436"))
    assertF(" 1b4", "% (x", new BigInteger("436"))
    assertF("(1b4)", "% (x", new BigInteger("-436"))

    testWithNull('x', "#+ 0(", acceptPrecision = false)

    expectFormatFlagsConversionMismatch('x', "+ ,(", 5)
    expectFormatFlagsConversionMismatch('x', "+ ,(", 5L)
    expectFormatFlagsConversionMismatch('x', ",", new BigInteger("5"))
    expectIllegalFormatPrecision('x', 5)
  }

  @Test def formatE(): Unit = {
    assertF("1.000000e+03", "%e", 1000.0)
    assertF("1e+100", "%.0e", 1.2e100)
    assertF("0.000e+00", "%.3e", 0.0)
    assertF("-0.000e+00", "%.3e", -0.0)

    /* We use 1.51e100 in this test, since we seem to have a floating point
     * imprecision at exactly 1.5e100 that yields to a rounding error towards
     * 1e+100 instead of 2e+100.
     */
    assertF("2e+100", "%.0e", 1.51e100)

    assertF(" 1.20e+100", "%10.2e", 1.2e100)

    // #3202 Corner case of round-half-up (currently non-compliant)
    if (executingInJVM) {
      assertF("7.6543215e-20    ", "%-17.7e", 7.65432145e-20)
      assertF("-7.6543215e-20   ", "%-17.7e", -7.65432145e-20)
      assertF("7.6543216e-20    ", "%-17.7e", 7.65432155e-20)
      assertF("-7.6543216e-20   ", "%-17.7e", -7.65432155e-20)
    } else {
      assertF("7.6543214e-20    ", "%-17.7e", 7.65432145e-20)
      assertF("-7.6543214e-20   ", "%-17.7e", -7.65432145e-20)
      assertF("7.6543215e-20    ", "%-17.7e", 7.65432155e-20)
      assertF("-7.6543215e-20   ", "%-17.7e", -7.65432155e-20)
    }

    assertF("001.2000e-21", "%012.4e", 1.2e-21f)
    assertF("001.2000E-21", "%012.4E", 1.2e-21f)
    assertF("(0001.2000e-21)", "%(015.4e", -1.2e-21f)

    assertF("1.e+100", "%#.0e", 1.2e100)

    assertF("+1.234560e+30", "%+e", 1.23456e30)
    assertF("-1.234560e+30", "%+e", -1.23456e30)
    assertF(" 1.234560e+30", "% e", 1.23456e30)
    assertF("-1.234560e+30", "% e", -1.23456e30)

    testWithInfinityAndNaN('e', acceptComma = false)
    testWithNull('e', "#+ 0(")

    expectFormatFlagsConversionMismatch('e', ",", 5.5)
    expectIllegalFormatFlags("%-05e", "-0", 5.5)
    expectIllegalFormatFlags("% +e", "+ ", 5.5)
  }

  @Test def formatG(): Unit = {
    assertF("5.00000e-05", "%g", 0.5e-4)
    assertF("-5.00000e-05", "%g", -0.5e-4)
    assertF("0.000300000", "%g", 3e-4)
    assertF("0.000300", "%.3g", 3e-4)
    assertF("10.0000", "%g", 10.0)
    assertF("10.00", "%.4g", 10.0)
    assertF("0.0010", "%.2g", 1e-3)
    assertF("300000", "%g", 3e5)
    assertF("3.00e+05", "%.3g", 3e5)

    assertF("005.00000e-05", "%013g", 0.5e-4)
    assertF("-05.00000e-05", "%013g", -0.5e-4)
    assertF("0000000300000", "%013g", 3e5)
    assertF("-000000300000", "%013g", -3e5)

    assertF("00003.00e+05", "%012.3g", 3e5)
    assertF("-0003.00e+05", "%012.3g", -3e5)

    assertF("5.00000e-05", "%(g", 0.5e-4)
    assertF("(5.00000e-05)", "%(g", -0.5e-4)
    assertF("300000", "%(g", 3e5)
    assertF("(300000)", "%(g", -3e5)

    assertF("+5.00000e-05", "%(+g", 0.5e-4)
    assertF("(5.00000e-05)", "%(+g", -0.5e-4)
    assertF("+300000", "%(+g", 3e5)
    assertF("(300000)", "%(+g", -3e5)

    assertF(" 5.00000e-05", "% g", 0.5e-4)
    assertF("-5.00000e-05", "% g", -0.5e-4)
    assertF(" 300000", "% g", 3e5)
    assertF("-300000", "% g", -3e5)

    assertF("    5.00000e-05", "%15g", 0.5e-4)
    assertF("   -5.00000e-05", "%15g", -0.5e-4)
    assertF("         300000", "%15g", 3e5)
    assertF("        -300000", "%15g", -3e5)

    assertF("    5.00000e-05", "%(15g", 0.5e-4)
    assertF("  (5.00000e-05)", "%(15g", -0.5e-4)
    assertF("         300000", "%(15g", 3e5)
    assertF("       (300000)", "%(15g", -3e5)

    assertF("5.00000e-05    ", "%-15g", 0.5e-4)
    assertF("-5.00000e-05   ", "%-15g", -0.5e-4)
    assertF("300000         ", "%-15g", 3e5)
    assertF("-300000        ", "%-15g", -3e5)

    assertF("300,000", "%,g", 3e5)
    assertF("00000300,000", "%0,12g", 3e5)

    testWithInfinityAndNaN('g', acceptSharp = false)
    testWithNull('g', "+ 0,(")

    expectFormatFlagsConversionMismatch('g', "#", 5.5)
    expectIllegalFormatFlags("%-05g", "-0", 5.5)
    expectIllegalFormatFlags("% +g", "+ ", 5.5)
  }

  @Test def formatF(): Unit = {
    assertF("3.300000", "%f", 3.3)
    assertF("(04.6000)", "%0(9.4f", -4.6)

    assertF("30000001024.000000", "%f", 3e10f)

    assertF("30000000000.000000", "%f", 3e10)

    assertF("30,000,000,000.000000", "%,f", 3e10)

    assertF("00000000.000050", "%015f", 0.5e-4)
    assertF("-0000000.000050", "%015f", -0.5e-4)
    assertF("00300000.000000", "%015f", 3e5)
    assertF("-0300000.000000", "%015f", -3e5)

    assertF("00300000.000", "%012.3f", 3e5)
    assertF("-0300000.000", "%012.3f", -3e5)

    assertF("0.000050", "%(f", 0.5e-4)
    assertF("(0.000050)", "%(f", -0.5e-4)
    assertF("300000.000000", "%(f", 3e5)
    assertF("(300000.000000)", "%(f", -3e5)

    assertF("+0.000050", "%(+f", 0.5e-4)
    assertF("(0.000050)", "%(+f", -0.5e-4)
    assertF("+300000.000000", "%(+f", 3e5)
    assertF("(300000.000000)", "%(+f", -3e5)

    assertF(" 0.000050", "% f", 0.5e-4)
    assertF("-0.000050", "% f", -0.5e-4)
    assertF(" 300000.000000", "% f", 3e5)
    assertF("-300000.000000", "% f", -3e5)

    assertF("       0.000050", "%15f", 0.5e-4)
    assertF("      -0.000050", "%15f", -0.5e-4)
    assertF("  300000.000000", "%15f", 3e5)
    assertF(" -300000.000000", "%15f", -3e5)

    assertF("       0.000050", "%(15f", 0.5e-4)
    assertF("     (0.000050)", "%(15f", -0.5e-4)
    assertF("  300000.000000", "%(15f", 3e5)
    assertF("(300000.000000)", "%(15f", -3e5)

    assertF("0.000050       ", "%-15f", 0.5e-4)
    assertF("-0.000050      ", "%-15f", -0.5e-4)
    assertF("300000.000000  ", "%-15f", 3e5)
    assertF("-300000.000000 ", "%-15f", -3e5)

    assertF("300,000.000000", "%,f", 3e5)
    assertF("00000300,000.000000", "%0,19f", 3e5)

    // #3202
    if (executingInJVM) {
      assertF("66380.78812500000", "%.11f", 66380.788125)
    } else {
      assertF("66380.78812500001", "%.11f", 66380.788125)
    }

    testWithInfinityAndNaN('f', acceptUpperCase = false)
    testWithNull('f', "#+ 0,(", acceptUpperCase = false)

    expectIllegalFormatFlags("%-05f", "-0", 5.5)
    expectIllegalFormatFlags("% +f", "+ ", 5.5)
  }

  @Test def formatPercentPercent(): Unit = {
    assertF("1%2", "%d%%%d", 1, 2)

    /* https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8204229
     * 'width' is ignored before JDK 11.
     */
    if (!executingInJVM) {
      assertF("    %", "%5%")
      assertF("%    ", "%-5%")
    }

    expectIllegalFormatFlags("%0,+< (#%", "#+ 0,(<", null)
    expectIllegalFormatPrecision('%', null)
  }

  @Test def formatPercentN(): Unit = {
    assertF("1\n2", "%d%n%d", 1, 2)

    expectIllegalFormatFlags("%0-,+< (#n", "-#+ 0,(<", null)
    expectIllegalFormatPrecision('%', null)
    expectIllegalFormatWidth('n', null)
  }

  @Test def formatPositional(): Unit = {
    assertF("2 1", "%2$d %1$d", 1, 2)
    assertF("2 2 1", "%2$d %2$d %d", 1, 2)
    assertF("2 2 1", "%2$d %<d %d", 1, 2)
  }

  @Test def formatUnknown(): Unit = {
    // Correct format, unknown conversion
    expectUnknownFormatConversion("abc%udf", 'u')
    expectUnknownFormatConversion("abc%2$-(<034.12udf", 'u')

    // Unknown conversion *and* the index (45$) is too large
    expectUnknownFormatConversion("abc%45$-(<034.12udf", 'u')

    // Incorrect format, the reported conversion is the character after '%'
    expectUnknownFormatConversion("abc%2$-(_034.12udf", '2')

    // Weird case: for a trailing '%', the reported unknown conversion is '%'
    expectUnknownFormatConversion("abc%", '%')
  }

  @Test def leftAlignWithoutWidthThrows(): Unit = {
    for (conversion <- "bBhHsHcCdoxXeEgGf%") {
      val fmt = "ab%-" + conversion + "cd"
      val arg: Any = conversion match {
        case 'e' | 'E' | 'g' | 'G' | 'f' => 5.5
        case _                           => 5
      }
      val e =
        expectFormatterThrows(classOf[MissingFormatWidthException], fmt, arg)
      assertEquals(fmt, "%-" + conversion, e.getFormatSpecifier)
    }
  }

  @Test def indexTooLargeUsesLastIndex(): Unit = {
    expectFormatterThrows(classOf[MissingFormatArgumentException],
        "%9876543210$d", 56, 78)

    assertF("56 56", "%d %9876543210$d", 56, 78)
  }

  @Test def widthOrPrecisionTooLargeIsIgnored(): Unit = {
    assertF("56 78", "%d %9876543210d", 56, 78)
    assertF("56 78", "%d %.9876543210d", 56, 78)
  }

  @Test def closeThenUseThrows(): Unit = {
    val f = new Formatter()
    f.close()
    assertThrows(classOf[FormatterClosedException], f.toString())
  }

  @Test def formatBadFormatStringThrows(): Unit = {
    expectFormatterThrows(classOf[Exception], "hello world%")
    expectFormatterThrows(classOf[Exception], "%%%")
    expectFormatterThrows(classOf[Exception], "%q")
    expectFormatterThrows(classOf[Exception], "%1")
    expectFormatterThrows(classOf[Exception], "%_f")
  }

  @Test def formatNotEnoughArgumentsThrows(): Unit = {
    expectFormatterThrows(classOf[MissingFormatArgumentException], "%f")
    expectFormatterThrows(classOf[MissingFormatArgumentException], "%d%d%d",
        1, 1)
    expectFormatterThrows(classOf[MissingFormatArgumentException], "%10$d", 1)
  }
}

object FormatterTest {

  def asIntOnJVM(x: Byte): Any =
    if (executingInJVM) x.toInt
    else x

  def asIntOnJVM(x: Short): Any =
    if (executingInJVM) x.toInt
    else x

  class HelperClass {
    override def hashCode(): Int = 0xf1e2a3

    // Soothe Scalastyle (equals and hashCode should be declared together)
    override def equals(that: Any): Boolean = super.equals(that)
  }

}
