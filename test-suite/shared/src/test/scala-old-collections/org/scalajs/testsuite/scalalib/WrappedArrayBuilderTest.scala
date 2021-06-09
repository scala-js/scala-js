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

import scala.collection.mutable
import scala.reflect.ClassTag

import org.junit.Assert._
import org.junit.Test

class WrappedArrayBuilderTest {

  @Test def emptyResult_Issue4507(): Unit = {
    def test[A](implicit ct: ClassTag[A]): Unit = {
      val result = new mutable.WrappedArrayBuilder(ct).result()
      assertEquals(0, result.size)

      val expectedComponentType: Class[_] =
        if (ct.runtimeClass == classOf[Unit]) classOf[scala.runtime.BoxedUnit]
        else ct.runtimeClass
      assertSame(expectedComponentType, result.array.getClass().getComponentType())
    }

    test[Int]
    test[String]
    test[List[Int]]
    test[Char]
    test[Unit]
    test[scala.runtime.BoxedUnit]
    test[java.lang.Void]
    test[Array[Int]]
    test[Array[String]]
  }

}
