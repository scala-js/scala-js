/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import org.scalajs.jasminetest.JasmineTest
import scala.scalajs.js

import scala.runtime.BoxedUnit

object ClassTest extends JasmineTest {

  describe("java.lang.Class") {

    it("should provide getSimpleName()") {
      expect(classOf[java.lang.Integer].getSimpleName()).toEqual("Integer")
      expect(classOf[java.lang.Class[_]].getSimpleName()).toEqual("Class")
      expect(classOf[scala.collection.Map[_, _]].getSimpleName()).toEqual("Map")
      expect(classOf[ClassTestClass#InnerClass].getSimpleName()).toEqual("InnerClass")
    }

    it("getComponentType()") {
      @noinline
      def testNoInline(clazz: Class[_], componentType: Class[_]): Unit =
        expect(clazz.getComponentType == componentType).toBeTruthy()

      @inline
      def test(clazz: Class[_], componentType: Class[_]): Unit = {
        testNoInline(clazz, componentType)
        expect(clazz.getComponentType == componentType).toBeTruthy()
      }

      test(classOf[Array[Object]], classOf[Object])
      test(classOf[Array[Int]], classOf[Int])
      test(classOf[Array[String]], classOf[String])
      test(classOf[Array[Seq[_]]], classOf[Seq[_]])
      test(classOf[Array[js.Date]], classOf[js.Date])
      test(classOf[Array[js.Dictionary[_]]], classOf[js.Dictionary[_]])
      test(classOf[Array[Unit]], classOf[BoxedUnit]) // not Unit

      test(classOf[Array[Array[Object]]], classOf[Array[Object]])
      test(classOf[Array[Array[Int]]], classOf[Array[Int]])
      test(classOf[Array[Array[String]]], classOf[Array[String]])
      test(classOf[Array[Array[Seq[_]]]], classOf[Array[Seq[_]]])
      test(classOf[Array[Array[js.Date]]], classOf[Array[js.Date]])
      test(classOf[Array[Array[js.Dictionary[_]]]], classOf[Array[js.Dictionary[_]]])
      test(classOf[Array[Array[Unit]]], classOf[Array[Unit]])

      test(classOf[Object], null)
      test(classOf[Int], null)
      test(classOf[String], null)
      test(classOf[Seq[_]], null)
      test(classOf[js.Date], null)
      test(classOf[js.Dictionary[_]], null)
      test(classOf[Unit], null)
    }

  }

}

class ClassTestClass {
  class InnerClass
}
