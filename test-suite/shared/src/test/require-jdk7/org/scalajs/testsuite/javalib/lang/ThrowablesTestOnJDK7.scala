/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Assert._
import org.junit.Test

class ThrowablesTestOnJDK7 {

  @Test def throwable_message_issue_2559(): Unit = {
    val t0 = new Throwable
    val t1 = new Throwable("foo")

    def test0(newThrowable: Throwable): Unit = {
      assertNull(newThrowable.getMessage)
    }

    def test1(newThrowable: String => Throwable): Unit = {
      assertEquals("foo", newThrowable("foo").getMessage)
    }

    def test2(newThrowable: Throwable => Throwable): Unit = {
      assertEquals(t0.getClass.getName, newThrowable(t0).getMessage)
      assertEquals(t0.getClass.getName + ": foo", newThrowable(t1).getMessage)
    }

    def test3(newThrowable: (String, Throwable) => Throwable): Unit = {
      assertEquals("bar", newThrowable("bar", t0).getMessage)
      assertEquals("bar", newThrowable("bar", t1).getMessage)
      assertNull(newThrowable(null, t0).getMessage)
      assertNull(newThrowable(null, t1).getMessage)
    }

    // java.lang

    test0(new BootstrapMethodError)
    test1(new BootstrapMethodError(_))
    test2(new BootstrapMethodError(_))
    test3(new BootstrapMethodError(_, _))

    test0(new ReflectiveOperationException)
    test1(new ReflectiveOperationException(_))
    test2(new ReflectiveOperationException(_))
    test3(new ReflectiveOperationException(_, _))
  }

  @Test def assertionErrorCtorWithStringThrowable(): Unit = {
    val th = new RuntimeException("kaboom")
    val e = new AssertionError("boom", th)
    assertEquals("boom", e.getMessage)
    assertSame(th, e.getCause)
  }

  @Test def noWritableStackTrace(): Unit = {
    class NoStackTraceException(msg: String)
        extends Throwable(msg, null, true, false) {

      override def fillInStackTrace(): Throwable = {
        fail("NoStackTraceException.fillInStackTrace() must not be called")
        this
      }
    }

    val e = new NoStackTraceException("error")
    assertEquals(0, e.getStackTrace().length)

    e.setStackTrace(Array(new StackTraceElement("class", "method", "file", 0)))
    assertEquals(0, e.getStackTrace().length)
  }

  @Test def suppression(): Unit = {
    val e = new Exception("error")
    assertEquals(0, e.getSuppressed().length)

    val suppressed1 = new IllegalArgumentException("suppressed 1")
    val suppressed2 = new UnsupportedOperationException("suppressed 2")

    // There is no ordering guarantee in suppressed exceptions, so we compare sets

    e.addSuppressed(suppressed1)
    assertEquals(Set(suppressed1), e.getSuppressed().toSet)

    e.addSuppressed(suppressed2)
    assertEquals(Set(suppressed1, suppressed2), e.getSuppressed().toSet)
  }

  @Test def noSuppression(): Unit = {
    class NoSuppressionException(msg: String)
        extends Throwable(msg, null, false, true)

    val e = new NoSuppressionException("error")
    assertEquals(0, e.getSuppressed().length)

    e.addSuppressed(new Exception("suppressed"))
    assertEquals(0, e.getSuppressed().length)
  }
}
