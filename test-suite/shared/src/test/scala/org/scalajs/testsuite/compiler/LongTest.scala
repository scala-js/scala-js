/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

/** Tests the compiler re-patching of native longs to
 *  scala.scalajs.runtime.Long
 *  see org.scalajs.testsuite.jsinterop.RuntimeLongTest
 *  for a test of the implementation itself
 */
class LongTest {
  @Test def `should_correctly_handle_literals`(): Unit = {
    assertEquals(105L, 5L + 100L)
    assertEquals(2147483651L, 2147483649L + 2L)
    assertEquals(-8589934592L, -2147483648L * 4)
    assertEquals(-18014398509482040L, 4503599627370510L * (-4))
  }

  @Test def `should_correctly_dispatch_unary_ops_on_Longs`(): Unit = {
    val x = 10L
    assertEquals(-10L, -x)
    val y = 5L
    assertEquals(-5L, -y)
    assertEquals(5L, +y)
    assertEquals(-6L, ~y)
  }

  @Test def `should_correctly_dispatch_binary_ops_on_Longs`(): Unit = {
    assertEquals(25F, 5L * 5F, 0F)
    assertEquals(1F, 5L % 4F, 0F)
    assertEquals(20F, 5F * 4L, 0F)
  }

  @Test def `should_support_shifts_with_Longs_#622`(): Unit = {
    def l(x: Long): Long = x
    def i(x: Int): Int = x

    assertEquals(268435455L, l(-7L) >>> 100L)
    assertEquals(-1L, l(-7L) >> 100L)
    assertEquals(-1L, l(-7L) >> 100)
    assertEquals(268435455L, l(-7L) >>> 100)
    assertEquals(-481036337152L, l(-7L) << 100L)
    assertEquals(-481036337152L, l(-7L) << 100)
    assertEquals(481036337152L, l(7L) << 100L)
    assertEquals(549755813888L, l(8L) << 100L)
    assertEquals(1152921504606846975L, l(-7L) >>> 4)

    assertEquals(112, i(7) << 100)
    assertEquals(-1, i(-7) >> 100)
    assertEquals(268435455, i(-7) >>> 100)
    assertEquals(-5, i(-65) >> 100)
    assertEquals(-5, i(-65) >> 4)
  }

  @Test def `primitives_should_convert_to_Long`(): Unit = {
    // Byte
    assertEquals(112L, 112.toByte.toLong)
    // Short
    assertEquals(-10L, (-10).toShort.toLong)
    // Char
    assertEquals(65L, 'A'.toLong)
    // Int
    assertEquals(5L, 5.toLong)
    // Long
    assertEquals(10L, 10L.toLong)
    // Float
    assertEquals(100000L, 100000.6f.toLong)
    // Double
    assertEquals(100000L, 100000.6.toLong)
  }

  @Test def `should_support_hashCode`(): Unit = {
    assertEquals(0, 0L.hashCode())
    assertEquals(55, 55L.hashCode())
    assertEquals(11, (-12L).hashCode())
    assertEquals(10006548, 10006548L.hashCode())
    assertEquals(1098747, (-1098748L).hashCode())

    assertEquals(-825638905, 613354684553L.hashCode())
    assertEquals(1910653900, 9863155567412L.hashCode())
    assertEquals(1735398658, 3632147899696541255L.hashCode())
    assertEquals(-1689438124, 7632147899696541255L.hashCode())
  }

  @Test def `should_support_hash_hash`(): Unit = {
    assertEquals(0, 0L.##)
    assertEquals(55, 55L.##)
    assertEquals(-12, (-12L).##)
    assertEquals(10006548, 10006548L.##)
    assertEquals(-1098748, (-1098748L).##)

    assertEquals(1910653900, 9863155567412L.##)
    assertEquals(1735398658, 3632147899696541255L.##)

    // These two (correctly) give different results on 2.10 and 2.11
    //assertEquals(-825638905, 613354684553L.##)  // xx06 on 2.10
    //assertEquals(-1689438124, 7632147899696541255L.##) // xx25 on 2.10
  }

  @Test def `should_correctly_concat_to_string`(): Unit = {
    val x = 20L
    assertEquals("asdf520hello", "asdf" + 5L + x + "hello")
    assertEquals("20hello", x + "hello")
  }

  @Test def `string_should_convert_to_Long`(): Unit = {
    assertEquals(45678901234567890L, "45678901234567890".toLong)
  }

  @Test def `should_correctly_implement_is/asInstanceOf_Longs`(): Unit = {
    val dyn:  Any  = 5L
    val stat: Long = 5L

    assertEquals(5L, stat.asInstanceOf[Long])
    // models current scala behavior. See SI-1448
    assertEquals(5, stat.asInstanceOf[Int])

    assertTrue(stat.isInstanceOf[Long])
    assertFalse(stat.isInstanceOf[Int])

    assertEquals(5L, dyn.asInstanceOf[Long])

    assertTrue(dyn.isInstanceOf[Long])
    assertFalse(dyn.isInstanceOf[Int])
  }

  @Test def `should_correctly_compare_to_other_numeric_types`(): Unit = {
    assertTrue(5L == 5)
    assertTrue(5 == 5L)
    assertTrue(4 != 5L)
    assertTrue('A' == 65L)
  }
}
