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
