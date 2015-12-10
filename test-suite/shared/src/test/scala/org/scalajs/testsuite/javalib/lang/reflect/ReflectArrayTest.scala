/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang.reflect

import scala.runtime.BoxedUnit

import org.junit.Test
import org.junit.Assert._

class ReflectArrayTest {

  @inline
  private def testBase(clazz: Class[_], length: Int, expectedClazz: Class[_],
      sampleElem: Any): Unit = {
    val array =
      java.lang.reflect.Array.newInstance(clazz, length).asInstanceOf[Array[_]]
    assertEquals(expectedClazz, array.getClass)
    assertTrue(array.getClass.isArray)
    assertEquals(length, array.length)
    for (i <- 0 until array.length)
      assertEquals(sampleElem, array(i))
  }

  @noinline
  private def testNewInstanceNoInline(clazz: Class[_], length: Int, expectedClazz: Class[_],
      sampleElem: Any): Unit = {
    testBase(clazz, length, expectedClazz, sampleElem)
  }

  @inline
  def testNewInstance(clazz: Class[_], expectedClazz: Class[_],
      sampleElem: Any): Unit = {
    testNewInstanceNoInline(clazz, length = 2, expectedClazz, sampleElem)
    testBase(clazz, length = 2, expectedClazz, sampleElem)

    testNewInstanceNoInline(clazz, length = 0, expectedClazz, sampleElem)
    testBase(clazz, length = 0, expectedClazz, sampleElem)
  }

  @Test def newInstance(): Unit = {
    testNewInstance(classOf[Int], classOf[Array[Int]], 0)
    testNewInstance(classOf[Char], classOf[Array[Char]], '\0')
    testNewInstance(classOf[Long], classOf[Array[Long]], 0L)
    testNewInstance(classOf[Boolean], classOf[Array[Boolean]], false)

    testNewInstance(classOf[BoxedUnit], classOf[Array[Unit]], null) // yes, null

    testNewInstance(classOf[Object], classOf[Array[Object]], null)
    testNewInstance(classOf[String], classOf[Array[String]], null)

    testNewInstance(classOf[java.lang.Integer], classOf[Array[java.lang.Integer]], null)
    testNewInstance(classOf[java.lang.Long], classOf[Array[java.lang.Long]], null)

    testNewInstance(classOf[Array[Object]], classOf[Array[Array[Object]]], null)
    testNewInstance(classOf[Array[Int]], classOf[Array[Array[Int]]], null)
    testNewInstance(classOf[Array[String]], classOf[Array[Array[String]]], null)
  }
}
