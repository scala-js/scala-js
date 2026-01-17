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

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform

import scala.scalajs.js

class ObjectJSTest {

  @Test def everythingButNullIsAnObject(): Unit = {
    assertTrue((new js.Object: Any).isInstanceOf[Object])
    assertTrue((js.Array(5): Any).isInstanceOf[Object])
  }

  @Test def everythingCanCastToObjectSuccessfullyIncludingNull(): Unit = {
    (new js.Object: Any).asInstanceOf[Object]
    (js.Array(5): Any).asInstanceOf[Object]
  }

  @Test def cloneOnNonScalaObject(): Unit = {
    class CloneOnNonScalaObject extends js.Object {
      def boom(): Any = this.clone()
    }

    val obj = new CloneOnNonScalaObject()
    assertThrows(classOf[CloneNotSupportedException], obj.boom())
  }

  @Test def hashCodeOfSymbols(): Unit = {
    /* None of the specific values here are by-spec. This test is highly
     * implementation-dependent. It is written like this to make sure that we
     * are returning different values for different arguments, but the specific
     * values are irrelevant and could be changed at any time.
     *
     * By-spec, however, hashCode() delegates to System.identityHashCode() for
     * symbols, since they are not Scala objects nor primitives that correspond
     * to a hijacked class. So the values here must be equal to those in
     * `SystemJSTest.identityHashCodeOfSymbols()`.
     */

    assumeTrue("requires JS symbols", Platform.jsSymbols)

    @noinline def test(hash: Int, x: js.Symbol): Unit =
      assertEquals(hash, x.hashCode())

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

  @Test def hashCodeOfBigInts(): Unit = {
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
}
