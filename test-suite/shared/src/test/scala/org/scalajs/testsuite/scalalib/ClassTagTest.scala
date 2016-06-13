/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import scala.language.implicitConversions

import scala.reflect._

import org.junit.Test
import org.junit.Assert._

class ClassTagTest {

  @Test def apply_should_get_the_existing_instances_for_predefined_ClassTags(): Unit = {
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
    assertSame(ClassTag.Nothing, classTag[Nothing])
    assertSame(ClassTag.Null, classTag[Null])
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

    assertSame(classOf[Array[_]], classTag[Array[_]].runtimeClass)
    assertSame(classOf[Array[Object]], classTag[Array[Object]].runtimeClass)
    assertSame(classOf[Array[_ <: AnyRef]], classTag[Array[_ <: AnyRef]].runtimeClass)
    assertSame(classOf[Array[String]], classTag[Array[String]].runtimeClass)
    assertSame(classOf[Array[_ <: Seq[_]]], classTag[Array[_ <: Seq[_]]].runtimeClass)
    assertSame(classOf[Array[Int]], classTag[Array[Int]].runtimeClass)
    assertSame(classOf[Array[Unit]], classTag[Array[Unit]].runtimeClass)

    // Weird, those two return Array[s.r.Nothing$] instead of Array[Object]
    // The same happens on the JVM
    assertSame(classOf[Array[scala.runtime.Nothing$]], classTag[Array[Nothing]].runtimeClass)
    assertSame(classOf[Array[scala.runtime.Null$]], classTag[Array[Null]].runtimeClass)

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
}
