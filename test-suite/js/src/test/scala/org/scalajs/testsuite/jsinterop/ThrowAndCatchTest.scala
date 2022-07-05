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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

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

  @Test def testJSThrowTypeErrorScalaCatchFuture(): Unit = {
    val e = new js.TypeError("boom")
    try {
      jsThrow(e)
    } catch {
      case JSExceptionFuture(e2) =>
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

  @Test def testJSThrowOptionScalaCatchFuture(): Unit = {
    val e = Some("boom")
    try {
      jsThrow(e)
    } catch {
      case JSExceptionFuture(e2) =>
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

  @Test def testScalaThrowTypeErrorJSCatchFuture(): Unit = {
    val e = new js.TypeError("boom")
    val e2 = jsCatch(() => throw JSExceptionFuture(e))
    assertSame(e, e2)
  }

  @Test def testScalaThrowOptionJSCatch(): Unit = {
    val e = Some("boom")
    val e2 = jsCatch(() => throw js.JavaScriptException(e))
    assertSame(e, e2)
  }

  @Test def testScalaThrowOptionJSCatchFuture(): Unit = {
    val e = Some("boom")
    val e2 = jsCatch(() => throw JSExceptionFuture(e))
    assertSame(e, e2)
  }

  @Test def testScalaThrowThrowableInJavaScriptExceptionJSCatch(): Unit = {
    // This is evil, but spec'ed nevertheless
    val e = new Exception("boom")
    val e2 = jsCatch(() => throw js.JavaScriptException(e))
    assertSame(e, e2)
  }

  @Test def testScalaThrowThrowableInJavaScriptExceptionJSCatchFuture(): Unit = {
    // This is evil, but spec'ed nevertheless
    val e = new Exception("boom")
    val e2 = jsCatch(() => throw JSExceptionFuture(e))
    assertSame(e, e2)
  }

}

object ThrowAndCatchTest {
  @noinline
  def jsThrow(ex: Any): Nothing =
    js.special.`throw`(ex)

  @noinline
  def jsCatch(body: js.Function0[Any]): Any = {
    val thrown = js.special.tryCatch[Option[Any]] { () =>
      body()
      None
    } { (ex: Any) =>
      Some(ex)
    }
    thrown.getOrElse {
      throw new AssertionError("Did not catch anything")
    }
  }

  /** `js.JavaScriptException` as we would define it in Scala.js 2.x. */
  object JSExceptionFuture {
    @inline def apply(ex: Any): Throwable = js.special.wrapAsThrowable(ex)

    @inline def unapply(th: Throwable): Option[Any] = {
      val ex = js.special.unwrapFromThrowable(th)
      if (th eq ex.asInstanceOf[AnyRef])
        None
      else
        Some(ex)
    }
  }
}
