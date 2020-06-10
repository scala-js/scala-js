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
import org.junit.Assert.assertSame

class ClassTagTestScala2 {

  /**
   * This is a Scala 2.x only test because:
   * Dotty does not have [[ClassTag]] instances for [[Nothing]] or for [[Null]].
   * @see [[https://github.com/lampepfl/dotty/issues/1730]]
   */
  @Test def apply_should_get_the_existing_instances_for_predefined_ClassTags(): Unit = {
    assertSame(ClassTag.Nothing, classTag[Nothing])
    assertSame(ClassTag.Null, classTag[Null])
  }

  /**
   * This is a Scala 2.x only test because:
   * Dotty does not have [[ClassTag]] instances for [[Nothing]] or for [[Null]].
   * The [[Array]] needs the [[ClassTag]] for the parameterized type.
   * @see [[https://github.com/lampepfl/dotty/issues/1730]]
   */
  @Test def runtimeClass(): Unit = {
    assertSame(classOf[Array[_]], classTag[Array[_]].runtimeClass)
    assertSame(classOf[Array[_ <: AnyRef]], classTag[Array[_ <: AnyRef]].runtimeClass)
    assertSame(classOf[Array[_ <: Seq[_]]], classTag[Array[_ <: Seq[_]]].runtimeClass)

    // Weird, those two return Array[s.r.Nothing$] instead of Array[Object]
    // The same happens on the JVM
    assertSame(classOf[Array[scala.runtime.Nothing$]], classTag[Array[Nothing]].runtimeClass)
    assertSame(classOf[Array[scala.runtime.Null$]], classTag[Array[Null]].runtimeClass)
  }
}
