/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class ThrowAndCatchTest {
  import ThrowAndCatchTest._

  @Test def testJSThrowThrowableScalaCatch(): Unit = {
    val e = new Exception("boom")
    try {
      jsThrow(e)
    } catch {
      case e2: Throwable =>
        assertSame(e, e2)
    }
  }

  @Test def testJSThrowTypeErrorScalaCatch(): Unit = {
    val e = new js.TypeError("boom")
    try {
      jsThrow(e)
    } catch {
      case js.JavaScriptException(e2) =>
        assertSame(e, e2)
    }
  }

  @Test def testJSThrowOptionScalaCatch(): Unit = {
    val e = Some("boom")
    try {
      jsThrow(e)
    } catch {
      case js.JavaScriptException(e2) =>
        assertSame(e, e2)
    }
  }

  @Test def testScalaThrowThrowableJSCatch(): Unit = {
    val e = new Exception("boom")
    val e2 = jsCatch(() => throw e)
    assertSame(e, e2)
  }

  @Test def testScalaThrowTypeErrorJSCatch(): Unit = {
    val e = new js.TypeError("boom")
    val e2 = jsCatch(() => throw js.JavaScriptException(e))
    assertSame(e, e2)
  }

  @Test def testScalaThrowOptionJSCatch(): Unit = {
    val e = Some("boom")
    val e2 = jsCatch(() => throw js.JavaScriptException(e))
    assertSame(e, e2)
  }

  @Test def testScalaThrowThrowableInJavaScriptExceptionJSCatch(): Unit = {
    // This is evil, but spec'ed nevertheless
    val e = new Exception("boom")
    val e2 = jsCatch(() => throw js.JavaScriptException(e))
    assertSame(e, e2)
  }

}

object ThrowAndCatchTest {
  private val jsThrow: js.Function1[Any, Nothing] =
    new js.Function("e", "throw e;").asInstanceOf[js.Function1[Any, Nothing]]

  private val jsCatch: js.Function1[js.Function0[_], Any] = {
    new js.Function("f",
        """
          |try {
          |  f();
          |} catch (e) {
          |  return e;
          |}
          |throw new Error("Did not catch anything");
        """.stripMargin).asInstanceOf[js.Function1[js.Function0[_], Any]]
  }
}
