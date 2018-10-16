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

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js

class ClassJSTest {

  @Test def getComponentType(): Unit = {
    @noinline
    def testNoInline(clazz: Class[_], componentType: Class[_]): Unit =
      assertEquals(componentType, clazz.getComponentType)

    @inline
    def test(clazz: Class[_], componentType: Class[_]): Unit = {
      testNoInline(clazz, componentType)
      assertEquals(componentType, clazz.getComponentType)
    }

    test(classOf[Array[js.Date]], classOf[js.Date])
    test(classOf[Array[js.Dictionary[_]]], classOf[js.Dictionary[_]])

    test(classOf[Array[Array[js.Date]]], classOf[Array[js.Date]])
    test(classOf[Array[Array[js.Dictionary[_]]]], classOf[Array[js.Dictionary[_]]])

    test(classOf[js.Date], null)
    test(classOf[js.Dictionary[_]], null)
  }
}
