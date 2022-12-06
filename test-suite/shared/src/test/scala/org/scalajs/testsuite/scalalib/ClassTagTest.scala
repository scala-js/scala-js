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

package org.scalajs.testsuite.scalalib

import scala.reflect._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

class ClassTagTest {

  @Test def applyReturnsExistingInstancesForPredefinedClassTags(): Unit = {
    assertSame(ClassTag.Byte, ClassTag(classOf[Byte]))
    assertSame(ClassTag.Short, ClassTag(classOf[Short]))
    assertSame(ClassTag.Char, ClassTag(classOf[Char]))
    assertSame(ClassTag.Int, ClassTag(classOf[Int]))
    assertSame(ClassTag.Long, ClassTag(classOf[Long]))
    assertSame(ClassTag.Float, ClassTag(classOf[Float]))
    assertSame(ClassTag.Double, ClassTag(classOf[Double]))
    assertSame(ClassTag.Boolean, ClassTag(classOf[Boolean]))
    assertSame(ClassTag.Unit, ClassTag(classOf[Unit]))
    assertSame(ClassTag.Object, ClassTag(classOf[Object]))
    assertSame(ClassTag.Nothing, ClassTag(classOf[Nothing]))
    assertSame(ClassTag.Null, ClassTag(classOf[Null]))

    assertSame(ClassTag.Byte, classTag[Byte])
    assertSame(ClassTag.Short, classTag[Short])
    assertSame(ClassTag.Char, classTag[Char])
    assertSame(ClassTag.Int, classTag[Int])
    assertSame(ClassTag.Long, classTag[Long])
    assertSame(ClassTag.Float, classTag[Float])
    assertSame(ClassTag.Double, classTag[Double])
    assertSame(ClassTag.Boolean, classTag[Boolean])
    assertSame(ClassTag.Unit, classTag[Unit])
    assertSame(ClassTag.Any, classTag[Any])
    assertSame(ClassTag.Object, classTag[Object])
    assertSame(ClassTag.AnyVal, classTag[AnyVal])
    assertSame(ClassTag.AnyRef, classTag[AnyRef])
  }

  @Test def runtimeClass(): Unit = {
    assertSame(classOf[Byte], ClassTag.Byte.runtimeClass)
    assertSame(classOf[Short], ClassTag.Short.runtimeClass)
    assertSame(classOf[Char], ClassTag.Char.runtimeClass)
    assertSame(classOf[Int], ClassTag.Int.runtimeClass)
    assertSame(classOf[Long], ClassTag.Long.runtimeClass)
    assertSame(classOf[Float], ClassTag.Float.runtimeClass)
    assertSame(classOf[Double], ClassTag.Double.runtimeClass)
    assertSame(classOf[Boolean], ClassTag.Boolean.runtimeClass)
    assertSame(classOf[Unit], ClassTag.Unit.runtimeClass)
    assertSame(classOf[Any], ClassTag.Any.runtimeClass)
    assertSame(classOf[Object], ClassTag.Object.runtimeClass)
    assertSame(classOf[AnyVal], ClassTag.AnyVal.runtimeClass)
    assertSame(classOf[AnyRef], ClassTag.AnyRef.runtimeClass)
    assertSame(classOf[Nothing], ClassTag.Nothing.runtimeClass)
    assertSame(classOf[Null], ClassTag.Null.runtimeClass)

    assertSame(classOf[String], classTag[String].runtimeClass)
    assertSame(classOf[Integer], classTag[Integer].runtimeClass)
    assertSame(classOf[Seq[_]], classTag[Seq[_]].runtimeClass)

    assertSame(classOf[Array[Object]], classTag[Array[Object]].runtimeClass)
    assertSame(classOf[Array[String]], classTag[Array[String]].runtimeClass)
    assertSame(classOf[Array[Int]], classTag[Array[Int]].runtimeClass)
    assertSame(classOf[Array[Unit]], classTag[Array[Unit]].runtimeClass)

    assertSame(classOf[String], ClassTag(classOf[String]).runtimeClass)
    assertSame(classOf[Integer], ClassTag(classOf[Integer]).runtimeClass)
    assertSame(classOf[Seq[_]], ClassTag(classOf[Seq[_]]).runtimeClass)

    assertSame(classOf[Array[_]], ClassTag(classOf[Array[_]]).runtimeClass)
    assertSame(classOf[Array[Object]], ClassTag(classOf[Array[Object]]).runtimeClass)
    assertSame(classOf[Array[_ <: AnyRef]], ClassTag(classOf[Array[_ <: AnyRef]]).runtimeClass)
    assertSame(classOf[Array[String]], ClassTag(classOf[Array[String]]).runtimeClass)
    assertSame(classOf[Array[_ <: Seq[_]]], ClassTag(classOf[Array[_ <: Seq[_]]]).runtimeClass)
    assertSame(classOf[Array[Int]], ClassTag(classOf[Array[Int]]).runtimeClass)
    assertSame(classOf[Array[Unit]], ClassTag(classOf[Array[Unit]]).runtimeClass)

    // These work as expected, though
    assertSame(classOf[Array[Nothing]], ClassTag(classOf[Array[Nothing]]).runtimeClass)
    assertSame(classOf[Array[Null]], ClassTag(classOf[Array[Null]]).runtimeClass)
  }

  @Test def classTagBasedPatternMatchingOfPrimitives(): Unit = {
    def test[A: ClassTag](x: Any): Boolean = x match {
      case x: A => true
      case _    => false
    }

    def testTrue[A: ClassTag](x: Any): Unit =
      assertTrue(s"$x must be a ${classTag[A]}", test[A](x))

    def testFalse[A: ClassTag](x: Any): Unit =
      assertFalse(s"$x must not be a ${classTag[A]}", test[A](x))

    def testTrueOnJS[A: ClassTag](x: Any): Unit = {
      if (executingInJVM)
        testFalse[A](x)
      else
        testTrue[A](x)
    }

    testTrue[Unit](())
    testTrue[scala.runtime.BoxedUnit](())
    testTrue[Boolean](false)
    testTrue[java.lang.Boolean](false)
    testTrue[Char]('A')
    testTrue[Character]('A')
    testTrue[Byte](5.toByte)
    testTrue[java.lang.Byte](5.toByte)
    testTrue[Int](5)
    testTrue[Integer](5)
    testTrue[Int](2000000000)
    testTrue[Float](1.5f)
    testTrue[Double](5.4)

    testTrueOnJS[Void](())
    testTrueOnJS[Byte](5)
    testTrueOnJS[java.lang.Byte](5)
    testTrueOnJS[Int](5.toByte)
    testTrueOnJS[Integer](5.toByte)
    testTrueOnJS[Double](5)
    testTrueOnJS[Float](1.5)

    testFalse[Unit](5)
    testFalse[scala.runtime.BoxedUnit](5)
    testFalse[Void](5)
    testFalse[Boolean](5)
    testFalse[java.lang.Boolean](5)
    testFalse[Char](5)
    testFalse[Character](5)
    testFalse[Byte](300)
    testFalse[java.lang.Byte](300)
    testFalse[Int]('A')
    testFalse[Integer]('A')
    testFalse[Int](1.5)
    testFalse[Double]('A')
  }
}
