/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.runtime.BoxedUnit

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

object ReflectArrayTest extends JasmineTest {

  import java.lang.reflect.Array.newInstance

  describe("java.lang.reflect.Array") {

    it("newInstance(clazz, length)") {
      @inline
      def testBase(clazz: Class[_], length: Int, expectedClazz: Class[_],
          sampleElem: Any): Unit = {
        val array = newInstance(clazz, length).asInstanceOf[Array[_]]
        expect(array.getClass == expectedClazz).toBeTruthy
        expect(array.getClass.isArray).toBeTruthy
        expect(array.length).toEqual(length)
        for (i <- 0 until array.length)
          expect(array(i) == sampleElem).toBeTruthy
      }

      @noinline
      def testNoInline(clazz: Class[_], length: Int, expectedClazz: Class[_],
          sampleElem: Any): Unit = {
        testBase(clazz, length, expectedClazz, sampleElem)
      }

      @inline
      def test(clazz: Class[_], expectedClazz: Class[_],
          sampleElem: Any): Unit = {
        testNoInline(clazz, length = 2, expectedClazz, sampleElem)
        testBase(clazz, length = 2, expectedClazz, sampleElem)

        testNoInline(clazz, length = 0, expectedClazz, sampleElem)
        testBase(clazz, length = 0, expectedClazz, sampleElem)
      }

      test(classOf[Int], classOf[Array[Int]], 0)
      test(classOf[Char], classOf[Array[Char]], '\0')
      test(classOf[Long], classOf[Array[Long]], 0L)
      test(classOf[Boolean], classOf[Array[Boolean]], false)

      test(classOf[BoxedUnit], classOf[Array[Unit]], null) // yes, null

      test(classOf[Object], classOf[Array[Object]], null)
      test(classOf[String], classOf[Array[String]], null)
      test(classOf[js.Date], classOf[Array[js.Date]], null)
      test(classOf[js.Dictionary[_]], classOf[Array[js.Dictionary[_]]], null)

      test(classOf[java.lang.Integer], classOf[Array[java.lang.Integer]], null)
      test(classOf[java.lang.Long], classOf[Array[java.lang.Long]], null)

      test(classOf[Array[Object]], classOf[Array[Array[Object]]], null)
      test(classOf[Array[Int]], classOf[Array[Array[Int]]], null)
      test(classOf[Array[String]], classOf[Array[Array[String]]], null)
    }

  }

}
