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

package org.scalajs.testsuite.javalib.util.function

import java.util.{Arrays, ArrayList, List => JavaList}
import java.util.function.{Consumer, ObjDoubleConsumer}
import org.junit.Assert._
import org.junit.Test

class ObjDoubleConsumerTest {
  import ObjDoubleConsumerTest._

  @Test def accept(): Unit = {
    val arr: JavaList[Double] = Arrays.asList(3.0d, 2.0d, 5.0d, 7.0d, 4.0d)

    // Side-effects
    var current: ArrayList[Double] = new ArrayList[Double](arr.size())
    val double: ObjDoubleConsumer[JavaList[Double]] = makeConsumer { (t: JavaList[Double], num) =>
      t.forEach(
        new Consumer[Double] {
          def accept(value: Double): Unit = {
            current.add(value * num)
            ()
          }
        }
      )
    }

    double.accept(arr, 1.0d)
    assertArrayEquals(arr.toArray, current.toArray)

    current = new ArrayList[Double](arr.size())
    double.accept(arr, 2.0d)
    assertArrayEquals(Arrays.asList(6.0d, 4.0d, 10.0d, 14.0d, 8.0d).toArray, current.toArray)
  }
}

object ObjDoubleConsumerTest {
  def makeConsumer[T](f: (T, Double) => Unit): ObjDoubleConsumer[T] = {
    new ObjDoubleConsumer[T] {
      def accept(t: T, value: Double): Unit = f(t, value)
    }
  }
}
