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

  @Test def isAssignableFrom(): Unit = {
    /* isAssignableFrom should respect the JVM rules even for JS types,
     * although isInstance doesn't. The reason is that it provides reflection
     * capabilities on the *class hierarchy*, so the *declared* classes matter.
     */

    // Positive tests

    assertTrue(classOf[Object].isAssignableFrom(classOf[js.Object]))
    assertTrue(classOf[Object].isAssignableFrom(classOf[js.EvalError]))
    assertTrue(classOf[Object].isAssignableFrom(classOf[js.Iterator[_]]))

    assertTrue(classOf[js.Object].isAssignableFrom(classOf[js.Object]))
    assertTrue(classOf[js.EvalError].isAssignableFrom(classOf[js.EvalError]))
    assertTrue(classOf[js.Iterator[_]].isAssignableFrom(classOf[js.Iterator[_]]))

    assertTrue(classOf[js.Error].isAssignableFrom(classOf[js.EvalError]))
    assertTrue(classOf[js.Iterable[_]].isAssignableFrom(classOf[js.Array[_]]))
    assertTrue(classOf[js.Any].isAssignableFrom(classOf[js.Dictionary[_]]))

    assertTrue(classOf[Array[js.Object]].isAssignableFrom(classOf[Array[js.Date]]))
    assertTrue(classOf[Array[js.Iterable[_]]].isAssignableFrom(classOf[Array[js.Array[_]]]))
    assertTrue(classOf[Array[js.Any]].isAssignableFrom(classOf[Array[js.Dictionary[_]]]))

    assertTrue(classOf[Array[Object]].isAssignableFrom(classOf[Array[js.Object]]))
    assertTrue(classOf[Array[Object]].isAssignableFrom(classOf[Array[Array[js.Object]]]))

    // Negative tests

    assertFalse(classOf[js.Date].isAssignableFrom(classOf[js.Object]))
    assertFalse(classOf[js.Iterator[_]].isAssignableFrom(classOf[js.Object]))
    assertFalse(classOf[js.Date].isAssignableFrom(classOf[js.Iterator[_]]))
    assertFalse(classOf[js.Iterator[_]].isAssignableFrom(classOf[js.Iterable[_]]))

    assertFalse(classOf[js.Object].isAssignableFrom(classOf[js.Iterator[_]]))
    assertFalse(classOf[js.Date].isAssignableFrom(classOf[js.Iterator[_]]))
    assertFalse(classOf[Array[js.Object]].isAssignableFrom(classOf[Array[js.Iterator[_]]]))

    assertFalse(classOf[js.Object].isAssignableFrom(classOf[Object]))
    assertFalse(classOf[js.Object].isAssignableFrom(classOf[Throwable]))
    assertFalse(classOf[js.Error].isAssignableFrom(classOf[Throwable]))

    assertFalse(classOf[Array[js.Date]].isAssignableFrom(classOf[Array[js.Object]]))
    assertFalse(classOf[Array[Array[Object]]].isAssignableFrom(classOf[Array[js.Object]]))
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

    test(classOf[Array[js.Date]], classOf[js.Date])
    test(classOf[Array[js.Dictionary[_]]], classOf[js.Dictionary[_]])

    test(classOf[Array[Array[js.Date]]], classOf[Array[js.Date]])
    test(classOf[Array[Array[js.Dictionary[_]]]], classOf[Array[js.Dictionary[_]]])

    test(classOf[js.Date], null)
    test(classOf[js.Dictionary[_]], null)
  }
}
