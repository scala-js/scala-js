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

import org.junit.Assert._
import org.junit.Test

class StackTraceElementTest {

  @Test def should_leave_toString_unmodified_if_columnNumber_is_not_specified(): Unit = {
    val st = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
    assertEquals("MyClass.myMethod(myFile.scala:1)", st.toString)
  }
}
