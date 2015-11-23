/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.io

import org.junit.Test

class ThrowablesTest {
  @Test def should_define_all_java_io_Errors_and_Exceptions(): Unit = {
    import java.io._
    new IOException("", new Exception())
    new EOFException("")
    new UTFDataFormatException("")
    new UnsupportedEncodingException("")
    new NotSerializableException("")
  }
}
