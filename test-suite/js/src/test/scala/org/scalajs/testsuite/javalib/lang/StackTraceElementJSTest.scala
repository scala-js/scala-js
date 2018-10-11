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

class StackTraceElementJSTest {

  import scala.scalajs.runtime.StackTrace.Implicits._

  @Test def should_use_the_additional_columnNumber_field_in_its_toString(): Unit = {
    val st = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
    st.setColumnNumber(5)
    assertEquals("MyClass.myMethod(myFile.scala:1:5)", st.toString)
  }
}
