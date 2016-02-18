/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Test

class ThrowablesTest {

  @Test def shouldDefineAllJavaUtilErrorsAndExceptions(): Unit = {
    import java.util._
    new ServiceConfigurationError("")
    new ConcurrentModificationException()
    new DuplicateFormatFlagsException("")
    new EmptyStackException()
    new FormatFlagsConversionMismatchException("", '\u0000')
    new FormatterClosedException()
    new IllegalFormatCodePointException(0)
    new IllegalFormatConversionException('\u0000', new Object().getClass)
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
