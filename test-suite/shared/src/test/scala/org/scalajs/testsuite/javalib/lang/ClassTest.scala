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

import scala.runtime.BoxedUnit

class ClassTest {

  private val PrimitiveClassOfs = Seq(
      classOf[Unit],
      classOf[Boolean],
      classOf[Char],
      classOf[Byte],
      classOf[Short],
      classOf[Int],
      classOf[Long],
      classOf[Float],
      classOf[Double]
  )

  @Test def getPrimitiveTypeName(): Unit = {
    assertEquals("void", classOf[Unit].getName)
    assertEquals("boolean", classOf[Boolean].getName)
    assertEquals("char", classOf[Char].getName)
    assertEquals("byte", classOf[Byte].getName)
    assertEquals("short", classOf[Short].getName)
    assertEquals("int", classOf[Int].getName)
    assertEquals("long", classOf[Long].getName)
    assertEquals("float", classOf[Float].getName)
    assertEquals("double", classOf[Double].getName)
  }

  @Test def wellKnownClasses(): Unit = {
    assertSame(classOf[Unit], scala.runtime.BoxedUnit.TYPE)
    assertSame(classOf[Unit], java.lang.Void.TYPE)
    assertSame(classOf[Boolean], java.lang.Boolean.TYPE)
    assertSame(classOf[Char], java.lang.Character.TYPE)
    assertSame(classOf[Byte], java.lang.Byte.TYPE)
    assertSame(classOf[Short], java.lang.Short.TYPE)
    assertSame(classOf[Int], java.lang.Integer.TYPE)
    assertSame(classOf[Long], java.lang.Long.TYPE)
    assertSame(classOf[Float], java.lang.Float.TYPE)
    assertSame(classOf[Double], java.lang.Double.TYPE)

    assertNotSame(classOf[java.lang.Void], scala.runtime.BoxedUnit.TYPE)
    assertNotSame(classOf[java.lang.Void], java.lang.Void.TYPE)
    assertNotSame(classOf[java.lang.Boolean], java.lang.Boolean.TYPE)
    assertNotSame(classOf[java.lang.Character], java.lang.Character.TYPE)
    assertNotSame(classOf[java.lang.Byte], java.lang.Byte.TYPE)
    assertNotSame(classOf[java.lang.Short], java.lang.Short.TYPE)
    assertNotSame(classOf[java.lang.Integer], java.lang.Integer.TYPE)
    assertNotSame(classOf[java.lang.Long], java.lang.Long.TYPE)
    assertNotSame(classOf[java.lang.Float], java.lang.Float.TYPE)
    assertNotSame(classOf[java.lang.Double], java.lang.Double.TYPE)
  }

  object TestObject

  @Test def getSimpleName(): Unit = {
    assertEquals("Integer", classOf[java.lang.Integer].getSimpleName())
    assertEquals("Class", classOf[java.lang.Class[_]].getSimpleName())
    assertEquals("Map", classOf[scala.collection.Map[_, _]].getSimpleName())
    assertEquals("InnerClass", classOf[ClassTestClass#InnerClass].getSimpleName())
    assertEquals("TestObject$", TestObject.getClass.getSimpleName)
  }

  @Test def isAssignableFrom(): Unit = {
    val SelectedClassOfs =
      PrimitiveClassOfs ++ Seq(classOf[Object], classOf[String])

    // All Classes are assignable from themselves
    for (cls <- SelectedClassOfs) {
      assertTrue(s"$cls should be assignable from itself",
          cls.isAssignableFrom(cls))
    }

    // Otherwise, if one side is a primitive, the result must be false
    for {
      left <- SelectedClassOfs
      right <- SelectedClassOfs
      if (left ne right) && (left.isPrimitive || right.isPrimitive)
    } {
      assertFalse(
          s"$left.isAssignableFrom($right) should be false",
          left.isAssignableFrom(right))
    }

    assertTrue(classOf[Object].isAssignableFrom(classOf[String]))
    assertTrue(classOf[Seq[_]].isAssignableFrom(classOf[List[_]]))
    assertTrue(classOf[Object].isAssignableFrom(classOf[Array[String]]))
    assertTrue(classOf[Array[Seq[_]]].isAssignableFrom(classOf[Array[List[_]]]))

    assertFalse(classOf[String].isAssignableFrom(classOf[Object]))
    assertFalse(classOf[List[_]].isAssignableFrom(classOf[Seq[_]]))
    assertFalse(classOf[Array[String]].isAssignableFrom(classOf[Object]))
    assertFalse(classOf[Array[List[_]]].isAssignableFrom(classOf[Array[Seq[_]]]))
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
