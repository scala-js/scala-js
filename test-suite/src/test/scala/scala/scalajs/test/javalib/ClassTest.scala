/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.test.JasmineTest
import scala.scalajs.js

object ClassTest extends JasmineTest {

  describe("java.lang.Class") {

    it("should provide getSimpleName()") {
      expect(classOf[java.lang.Integer].getSimpleName()).toEqual("Integer")
      expect(classOf[java.lang.Class[_]].getSimpleName()).toEqual("Class")
      expect(classOf[scala.collection.Map[_, _]].getSimpleName()).toEqual("Map")
      expect(classOf[ClassTestClass#InnerClass].getSimpleName()).toEqual("InnerClass")
    }

  }

}

class ClassTestClass {
  class InnerClass
}
