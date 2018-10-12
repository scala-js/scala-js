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

class ThrowablesTest {

  @Test def should_define_all_java_lang_Errors_and_Exceptions(): Unit = {
    new ArithmeticException()
    new ArrayIndexOutOfBoundsException()
    new ArrayStoreException()
    new ClassCastException()
    new ClassNotFoundException()
    new CloneNotSupportedException()
    // Needs an instance of java.lang.Enum.
    // import scala.language.existentials
    // new EnumConstantNotPresentException(null.asInstanceOf[Class[_ <: Enum[T] forSome { type T <: Enum[T] }]], null)
    new Exception()
    new IllegalAccessException()
    new IllegalArgumentException()
    new IllegalMonitorStateException()
    new IllegalStateException()
    new IllegalThreadStateException()
    new IndexOutOfBoundsException()
    new InstantiationException()
    new InterruptedException()
    new NegativeArraySizeException()
    new NoSuchFieldException()
    new NoSuchMethodException()
    new NullPointerException()
    new NumberFormatException()
    new RuntimeException()
    new SecurityException()
    new StringIndexOutOfBoundsException()
    new TypeNotPresentException(null, null)
    new UnsupportedOperationException()
    new AbstractMethodError()
    new AssertionError()
    new ClassCircularityError()
    new ClassFormatError()
    new Error()
    new ExceptionInInitializerError()
    new IllegalAccessError()
    new IncompatibleClassChangeError()
    new InstantiationError()
    new InternalError()
    new LinkageError()
    new NoClassDefFoundError()
    new NoSuchFieldError()
    new NoSuchMethodError()
    new OutOfMemoryError()
    new StackOverflowError()
    new UnknownError()
    new UnsatisfiedLinkError()
    new UnsupportedClassVersionError()
    new VerifyError()
    new VirtualMachineError() {}
  }

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

    test0(new Throwable)
    test1(new Throwable(_))
    test2(new Throwable(_))
    test3(new Throwable(_, _))

    test0(new Exception)
    test1(new Exception(_))
    test2(new Exception(_))
    test3(new Exception(_, _))

    test0(new IllegalArgumentException)
    test1(new IllegalArgumentException(_))
    test2(new IllegalArgumentException(_))
    test3(new IllegalArgumentException(_, _))

    test0(new IllegalStateException)
    test1(new IllegalStateException(_))
    test2(new IllegalStateException(_))
    test3(new IllegalStateException(_, _))

    test0(new RuntimeException)
    test1(new RuntimeException(_))
    test2(new RuntimeException(_))
    test3(new RuntimeException(_, _))

    test0(new SecurityException)
    test1(new SecurityException(_))
    test2(new SecurityException(_))
    test3(new SecurityException(_, _))

    test0(new UnsupportedOperationException)
    test1(new UnsupportedOperationException(_))
    test2(new UnsupportedOperationException(_))
    test3(new UnsupportedOperationException(_, _))

    // java.io

    import java.io.IOException
    test0(new IOException)
    test1(new IOException(_))
    test2(new IOException(_))
    test3(new IOException(_, _))

    // java.util

    import java.util.InvalidPropertiesFormatException
    test1(new InvalidPropertiesFormatException(_))
    test2(new InvalidPropertiesFormatException(_))

    import java.util.concurrent.ExecutionException
    test2(new ExecutionException(_))
    test3(new ExecutionException(_, _))
  }

  @Test def assertionErrorsPeculiarConstructors(): Unit = {
    def assertMessageNoCause(expectedMessage: String, e: AssertionError): Unit = {
      assertEquals(expectedMessage, e.getMessage)
      assertNull(e.getCause)
    }

    assertMessageNoCause(null, new AssertionError())

    assertMessageNoCause("boom", new AssertionError("boom"))
    assertMessageNoCause("Some(5)", new AssertionError(Some(5)))
    assertMessageNoCause("null", new AssertionError(null: Object))

    assertMessageNoCause("true", new AssertionError(true))
    assertMessageNoCause("5", new AssertionError(5.toByte))
    assertMessageNoCause("6", new AssertionError(6.toShort))
    assertMessageNoCause("7", new AssertionError(7))
    assertMessageNoCause("8", new AssertionError(8L))
    assertMessageNoCause("1.5", new AssertionError(1.5f))
    assertMessageNoCause("2.5", new AssertionError(2.5))

    val th = new RuntimeException("kaboom")
    val e = new AssertionError(th)
    assertEquals(th.toString, e.getMessage)
    assertSame(th, e.getCause)
  }
}
