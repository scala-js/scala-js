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

class StackTraceElementJSTest {

  import scala.scalajs.runtime.StackTrace.Implicits._

  @Test def should_use_the_additional_columnNumber_field_in_its_toString(): Unit = {
    val st = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
    st.setColumnNumber(5)
    assertEquals("MyClass.myMethod(myFile.scala:1:5)", st.toString)
  }
}
