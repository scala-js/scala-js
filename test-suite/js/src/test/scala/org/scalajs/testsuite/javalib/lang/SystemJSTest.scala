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

package org.scalajs.testsuite.javalib.lang

import org.scalajs.testsuite.utils.Platform

import scala.scalajs.js
import scala.scalajs.LinkingInfo

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

class SystemJSTest {

  @Test def identityHashCodeForJSObjects(): Unit = {
    if (Platform.assumeES2015 || js.typeOf(js.Dynamic.global.WeakMap) != "undefined") {
      /* This test is more restrictive than the spec, but we know our
       * implementation will always pass the test.
       */
      val x1 = new js.Object
      val x2 = new js.Object
      val x1FirstHash = x1.hashCode()
      assertEquals(x1FirstHash, x1.hashCode())
      assertNotEquals(x1.hashCode(), x2.hashCode())
      assertEquals(x1FirstHash, x1.hashCode())

      assertEquals(x1FirstHash, System.identityHashCode(x1))
      assertEquals(x2.hashCode(), System.identityHashCode(x2))
    } else {
      val x1 = new js.Object
      val x1FirstHash = x1.hashCode()
      assertEquals(x1FirstHash, x1.hashCode())
      assertEquals(x1FirstHash, System.identityHashCode(x1))
    }
  }

  @Test def identityHashCodeOfValuesImplementedAsJSPrimitives(): Unit = {
    /* None of the specific values here are by-spec. This test is highly
     * implementation-dependent. It is written like this to make sure that we
     * are returning different values for different arguments, but the specific
     * values are irrelevant and could be changed at any time.
     */

    @noinline def test(hash: Int, x: Any): Unit =
      assertEquals(hash, System.identityHashCode(x))

    test(101574, "foo")
    test(0, "")

    test(1237, false)
    test(1231, true)

    test(5, 5)
    test(789456, 789456)

    test(0, 0.0)
    test(-2147483648, -0.0)
    test(1234, 1234.0)
    test(1073217536, 1.5)
    test(340593891, Math.PI)
    test(-54, -54.0)

    test(1, Double.MinPositiveValue)
    test(1048576, Double.MinValue)
    test(-2146435072, Double.MaxValue)

    test(2146959360, Double.NaN)
    test(2146435072, Double.PositiveInfinity)
    test(-1048576, Double.NegativeInfinity)

    test(0, ())

    if (js.typeOf(0L) == "bigint") {
      test(0, 0L)
      test(1, 1L)
      test(0, -1L)

      test(-1746700373, 4203407456681260900L)
      test(1834237377, -4533628472446063315L)
      test(1917535332, -8078028383605336161L)
      test(1962981592, -1395767907951999837L)
      test(1771769687, 4226100786750107409L)
      test(-1655946833, 8283069451989884520L)
      test(969818862, -4956907030691723841L)
      test(-614637591, 7053247622210876606L)
      test(1345794172, 4113526825251053222L)
      test(-575359500, 7285869072471305893L)
    }
  }

  @Test def identityHashCodeOfSymbols(): Unit = {
    /* None of the specific values here are by-spec. This test is highly
     * implementation-dependent. It is written like this to make sure that we
     * are returning different values for different arguments, but the specific
     * values are irrelevant and could be changed at any time.
     *
     * By-spec, however, hashCode() delegates to System.identityHashCode() for
     * symbols, since they are not Scala objects nor primitives that correspond
     * to a hijacked class. So the values here must be equal to those in
     * `ObjectJSTest.hashCodeOfSymbols()`.
     */

    assumeTrue("requires JS symbols", Platform.jsSymbols)

    @noinline def test(hash: Int, x: js.Symbol): Unit =
      assertEquals(hash, System.identityHashCode(x))

    // unfortunately, all symbols without description hash to the same value
    test(0, js.Symbol())

    test(0, js.Symbol(""))
    test(-1268878963, js.Symbol("foobar"))
    test(93492084, js.Symbol("babar"))
    test(3392903, js.Symbol(null))

    test(-1268878963, js.Symbol.forKey("foobar"))
    test(93492084, js.Symbol.forKey("babar"))
    test(3392903, js.Symbol.forKey(null))
  }

  @Test def identityHashCodeOfBigInts(): Unit = {
    /* None of the specific values here are by-spec. This test is highly
     * implementation-dependent. It is written like this to make sure that we
     * are returning different values for different arguments, but the specific
     * values are irrelevant and could be changed at any time.
     *
     * By-spec, however, hashCode() delegates to System.identityHashCode() for
     * bigints, since they are not Scala objects nor primitives that correspond
     * to a hijacked class (except for those that fit in a Long when we
     * implement Longs as bigints). So the values here must be equal to those
     * in `SystemJSTest.identityHashCodeOfBigInts()`.
     */

    assumeTrue("requires JS bigints", Platform.jsBigInts)

    @noinline def test(hash: Int, x: js.BigInt): Unit =
      assertEquals(hash, System.identityHashCode(x))

    test(0, js.BigInt("0"))
    test(1, js.BigInt("1"))
    test(0, js.BigInt("-1"))

    test(-1746700373, js.BigInt("4203407456681260900"))
    test(1834237377, js.BigInt("-4533628472446063315"))
    test(1917535332, js.BigInt("-8078028383605336161"))
    test(1962981592, js.BigInt("-1395767907951999837"))
    test(1771769687, js.BigInt("4226100786750107409"))
    test(-1655946833, js.BigInt("8283069451989884520"))
    test(969818862, js.BigInt("-4956907030691723841"))
    test(-614637591, js.BigInt("7053247622210876606"))
    test(1345794172, js.BigInt("4113526825251053222"))
    test(-575359500, js.BigInt("7285869072471305893"))

    test(-413046144, js.BigInt("52943860994923075240706774564564704640410650435892"))
    test(-726153056, js.BigInt("-89593710930720640163135273078359588137037151908747"))
  }

  @Test def systemProperties(): Unit = {
    def get(key: String): String = java.lang.System.getProperty(key)

    // Defined in System.scala

    assertEquals("1.8", get("java.version"))
    assertEquals("1.8", get("java.vm.specification.version"))
    assertEquals("Oracle Corporation", get("java.vm.specification.vendor"))
    assertEquals("Scala.js", get("java.vm.name"))
    assertEquals("1.8", get("java.specification.version"))
    assertEquals("/", get("file.separator"))
    assertEquals(":", get("path.separator"))
    assertEquals("\n", get("line.separator"))
    assertEquals(LinkingInfo.linkerVersion, get("java.vm.version"))
  }
}
