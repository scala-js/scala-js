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
