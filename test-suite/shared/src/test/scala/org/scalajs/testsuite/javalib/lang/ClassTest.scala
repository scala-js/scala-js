/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

import scala.runtime.BoxedUnit

class ClassTest {

  @Test def getSimpleName(): Unit = {
    assertEquals("Integer", classOf[java.lang.Integer].getSimpleName())
    assertEquals("Class", classOf[java.lang.Class[_]].getSimpleName())
    assertEquals("Map", classOf[scala.collection.Map[_, _]].getSimpleName())
    assertEquals("InnerClass", classOf[ClassTestClass#InnerClass].getSimpleName())
  }

  @Test def getComponentType(): Unit = {
    @noinline
    def testNoInline(clazz: Class[_], componentType: Class[_]): Unit =
      assertEquals(componentType, clazz.getComponentType)

    @inline
    def test(clazz: Class[_], componentType: Class[_]): Unit = {
      testNoInline(clazz, componentType)
      assertEquals(componentType, clazz.getComponentType)
    }

    test(classOf[Array[Object]], classOf[Object])
    test(classOf[Array[Int]], classOf[Int])
    test(classOf[Array[String]], classOf[String])
    test(classOf[Array[Seq[_]]], classOf[Seq[_]])
    test(classOf[Array[Unit]], classOf[BoxedUnit]) // not Unit

    test(classOf[Array[Array[Object]]], classOf[Array[Object]])
    test(classOf[Array[Array[Int]]], classOf[Array[Int]])
    test(classOf[Array[Array[String]]], classOf[Array[String]])
    test(classOf[Array[Array[Seq[_]]]], classOf[Array[Seq[_]]])
    test(classOf[Array[Array[Unit]]], classOf[Array[Unit]])

    test(classOf[Object], null)
    test(classOf[Int], null)
    test(classOf[String], null)
    test(classOf[Seq[_]], null)
    test(classOf[Unit], null)
  }
}

class ClassTestClass {
  class InnerClass
}
