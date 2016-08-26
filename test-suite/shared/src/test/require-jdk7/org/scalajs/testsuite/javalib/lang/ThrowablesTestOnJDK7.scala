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
}
