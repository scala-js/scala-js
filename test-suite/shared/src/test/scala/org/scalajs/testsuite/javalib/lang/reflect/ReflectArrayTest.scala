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

package org.scalajs.testsuite.javalib.lang.reflect

import scala.runtime.BoxedUnit

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

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
    testNewInstance(classOf[Char], classOf[Array[Char]], '\u0000')
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

  @Test def newInstanceNegativeArraySize(): Unit = {
    import java.lang.{reflect => jlr}

    assumeTrue("Assuming compliant array errors", hasCompliantArrayErrors)

    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Int], -5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Boolean], -5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[AnyRef], -5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[String], -5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Array[AnyRef]], -5))

    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Int], -5, 5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Boolean], -5, 5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[AnyRef], -5, 5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[String], -5, 5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Array[AnyRef]], -5, 5))

    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Int], 5, -5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Boolean], 5, -5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[AnyRef], 5, -5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[String], 5, -5))
    assertThrows(classOf[NegativeArraySizeException], jlr.Array.newInstance(classOf[Array[AnyRef]], 5, -5))
  }
}
