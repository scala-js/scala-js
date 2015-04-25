/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

object StackTraceElementTest extends JasmineTest {

  import scala.scalajs.runtime.StackTrace.Implicits._

  describe("java.lang.StackTraceElement") {
    it("should use the additional columnNumber field in its toString") {
      val st = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
      st.setColumnNumber(5)
      expect(st.toString).toEqual("MyClass.myMethod(myFile.scala:1:5)")
    }

    it("should leave toString unmodified if columnNumber is not specified") {
      val st = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
      expect(st.toString).toEqual("MyClass.myMethod(myFile.scala:1)")
    }
  }

}
