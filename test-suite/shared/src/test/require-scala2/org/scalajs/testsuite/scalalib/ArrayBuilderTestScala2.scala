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

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder

import org.junit.Test
import org.junit.Assert.assertSame

class ArrayBuilderTestScala2 {

  @inline
  def makeNoInline[T](implicit ct: ClassTag[T]): ArrayBuilder[T] = {
    /* The dance in this method is to be source compatible with the old and
     * new collections. In the new collections, ArrayBuilder.make[T] doesn't
     * take an explicit () parameter list, but it does in the old collections.
     */

    @noinline def ctNoInline = ct

    {
      implicit val ct = ctNoInline
      ArrayBuilder.make[T]
    }
  }

  /** This is a Scala 2.x only test because:
   *  Dotty does not have [[ClassTag]] instances for [[Nothing]] or for [[Null]].
   *  @see [[https://github.com/lampepfl/dotty/issues/1730]]
   */
  @Test def Nothing_and_Null(): Unit = {
    assertSame(classOf[Array[Nothing]], ArrayBuilder.make[Nothing].result().getClass)
    assertSame(classOf[Array[Null]], ArrayBuilder.make[Null].result().getClass)

    assertSame(classOf[Array[Nothing]], makeNoInline[Nothing].result().getClass)
    assertSame(classOf[Array[Null]], makeNoInline[Null].result().getClass)
  }
}
