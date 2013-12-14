/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

object ThrowablesTest extends JasmineTest {
  describe("java.lang.Throwables, java.util.Throwables") {

    it("should define all java.lang and java.util Errors/Exceptions") {
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

      import java.util._
      new ServiceConfigurationError("")
      new ConcurrentModificationException()
      new DuplicateFormatFlagsException("")
      new EmptyStackException()
      new FormatFlagsConversionMismatchException("", '\0')
      new FormatterClosedException()
      new IllegalFormatCodePointException(0)
      new IllegalFormatConversionException('\0', new Object().getClass)
      new IllegalFormatFlagsException("")
      new IllegalFormatPrecisionException(0)
      new IllegalFormatWidthException(0)
      new InputMismatchException()
      // Needs java.io.IOException.
      // new InvalidPropertiesFormatException("")
      new MissingFormatArgumentException("")
      new MissingFormatWidthException("")
      new MissingResourceException(null, null, null)
      new NoSuchElementException()
      new TooManyListenersException()
      new UnknownFormatConversionException("")
      new UnknownFormatFlagsException("")
    }
  }
}
