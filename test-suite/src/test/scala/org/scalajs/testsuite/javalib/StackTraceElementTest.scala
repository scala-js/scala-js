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

  describe("java.lang.StackTraceElement") {
    it("should use the magic columnNumber field in its toString") {
      val st = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
      st.asInstanceOf[js.Dynamic].columnNumber = 5
      expect(st.toString).toEqual("MyClass.myMethod(myFile.scala:1:5)")
    }

    it("should leave toString unmodified without magic columnNumber") {
      val st = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
      expect(st.toString).toEqual("MyClass.myMethod(myFile.scala:1)")
    }
  }

}
