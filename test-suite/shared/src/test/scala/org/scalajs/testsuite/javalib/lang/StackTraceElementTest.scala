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

class StackTraceElementTest {

  @Test def should_leave_toString_unmodified_if_columnNumber_is_not_specified(): Unit = {
    val st = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
    assertEquals("MyClass.myMethod(myFile.scala:1)", st.toString)
  }
}
