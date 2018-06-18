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

class FormatterTest {
  import FormatterTest._

  def assertF(expected: String, format: String, args: Any*): Unit = {
    val fmt = new Formatter()
    val res = fmt.format(format, args.asInstanceOf[Seq[AnyRef]]: _*).toString()
    fmt.close()
    assertEquals(expected, res)
  }

  def testWithInfinityAndNaN(conversion: Char,
      acceptUpperCase: Boolean = true): Unit = {

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

    assertF("+Infinity", "%+(" + conversion, PosInf)
    assertF(" Infinity", "% (" + conversion, PosInf)
    assertF("(Infinity)", "%+(" + conversion, NegInf)
    assertF("(Infinity)", "% (" + conversion, NegInf)

    if (acceptUpperCase) {
      val upConversion = conversion.toUpper
      assertF("INFINITY", "%" + upConversion, PosInf)
      assertF("-INFINITY", "%" + upConversion, NegInf)
      assertF("NAN", "%" + upConversion, NaN)
    }
  }

  def expectFormatterThrows[T <: Throwable](exeption: Class[T], format: String,
      args: Any*): T = {
    val fmt = new Formatter()
    expectThrows(exeption,
        fmt.format(format, args.asInstanceOf[Seq[AnyRef]]: _*))
  }

  @Test def `should_provide_b_conversion`(): Unit = {
    assertF("false", "%b", null)
    assertF("true", "%b", true)
    assertF("false", "%b", false)
    assertF("true", "%b", new HelperClass)

    assertF("  false", "%7b", false)
    assertF("   true", "%7b", true)
    assertF("false  ", "%-7b", false)
    assertF("true   ", "%-7b", true)

    assertF("FALSE", "%B", false)
  }

  @Test def `should_provide_h_conversion`(): Unit = {
    val x = new HelperClass
    assertF("f1e2a3", "%h", x)
    assertF("F1E2A3", "%H", x)

    assertF("  f1e2a3", "%8h", x)

    assertF("null", "%h", null)
  }

  @Test def sConversionWithNonFormattable(): Unit = {
    assertF("abcdef", "ab%sef", "cd")
    assertF("true", "%s", true)
    assertF("12345", "%s", 12345)

    assertF("ABCDEF", "%S", "aBcdeF")

    assertF("     hello", "%10s", "hello")
    assertF("hello     ", "%-10s", "hello")

    assertF("null", "%s", null)

    if (!executingInJVMOnJDK6)
      expectFormatterThrows(classOf[Exception], "%#s", "hello")
  }

  @Test def sConversionWithFormattable(): Unit = {
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
  }

  @Test def `should_provide_c_conversion`(): Unit = {
    assertF("a", "%c", 'a')
    assertF("A", "%C", 'A')
    assertF("A", "%c", 65)

    assertF("    !", "%5c", '!')
    assertF("!    ", "%-5c", '!')
  }

  @Test def `should_provide_d_conversion`(): Unit = {
    assertF("5", "%d", 5)
    assertF("-5", "%d", -5)

    assertF("00005", "%05d", 5)
    assertF("  -10", "%5d", -10)
    assertF("-0010", "%05d", -10)

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
  }

  @Test def `should_provide_o_conversion`(): Unit = {
    assertF("10", "%o", 8)

    assertF("00020", "%05o", 16)
    assertF("37777777766", "%5o", -10)
    assertF("37777777766", "%05o", -10)

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

    /* Negative Bytes and Shorts are formatted as if they were Ints.
     * This is a consequence of the non-boxing behavior of numbers in Scala.js.
     */
    assertF("   37777777766", "%14o", asIntOnJVM(-10.toByte))
    assertF("37777777766", "%05o", asIntOnJVM(-10.toByte))
    assertF("37777777766", "%5o", asIntOnJVM(-10.toShort))
    assertF("000037777777766", "%015o", asIntOnJVM(-10.toShort))
  }

  @Test def `should_provide_x_conversion`(): Unit = {
    assertF("d431", "%x", 54321)
    assertF("ffff2bcf", "%x", -54321)
    assertF("D431", "%X", 54321)
    assertF("FFFF2BCF", "%X", -54321)

    assertF("0xd431", "%#x", 54321)
    assertF("0xffff2bcf", "%#x", -54321)
    assertF("0XD431", "%#X", 54321)
    assertF("0XFFFF2BCF", "%#X", -54321)

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
  }

  @Test def `should_provide_e_conversion`(): Unit = {
    assertF("1.000000e+03", "%e", 1000.0)
    assertF("1e+100", "%.0e", 1.2e100)
    assertF("0.000e+00", "%.3e", 0.0)

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

    assertF("+1.234560e+30", "%+e", 1.23456e30)
    assertF("-1.234560e+30", "%+e", -1.23456e30)
    assertF(" 1.234560e+30", "% e", 1.23456e30)
    assertF("-1.234560e+30", "% e", -1.23456e30)

    testWithInfinityAndNaN('e')
  }

  @Test def `should_provide_g_conversion`(): Unit = {
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

    testWithInfinityAndNaN('g')
  }

  @Test def `should_provide_f_conversion`(): Unit = {
    assertF("3.300000", "%f", 3.3)
    assertF("(04.6000)", "%0(9.4f", -4.6)

    assertF("30000001024.000000", "%f", 3e10f)

    assertF("30000000000.000000", "%f", 3e10)

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

    // #3202
    if (executingInJVM) {
      assertF("66380.78812500000", "%.11f", 66380.788125)
    } else {
      assertF("66380.78812500001", "%.11f", 66380.788125)
    }

    testWithInfinityAndNaN('f', acceptUpperCase = false)
  }

  @Test def `should_support_%%`(): Unit = {
    assertF("1%2", "%d%%%d", 1, 2)
  }

  @Test def `should_support_%n`(): Unit = {
    assertF("1\n2", "%d%n%d", 1, 2)
  }

  @Test def should_allow_positional_arguments(): Unit = {
    assertF("2 1", "%2$d %1$d", 1, 2)
    assertF("2 2 1", "%2$d %2$d %d", 1, 2)
    assertF("2 2 1", "%2$d %<d %d", 1, 2)
  }

  @Test def leftAlignWithoutWidthThrows(): Unit = {
    for (conversion <- "bBhHsHcCdoxXeEgGf") {
      val fmt = "ab%-" + conversion + "cd"
      val arg: Any = conversion match {
        case 'e' | 'E' | 'g' | 'G' | 'f' => 5.5
        case _                           => 5
      }
      expectFormatterThrows(classOf[MissingFormatWidthException], fmt, arg)
    }
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
