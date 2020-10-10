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
import java.util.function.{Consumer, ObjLongConsumer}
import org.junit.Assert._
import org.junit.Test

class ObjLongConsumerTest {
  import ObjLongConsumerTest._

  @Test def accept(): Unit = {
    val arr: JavaList[Long] = Arrays.asList(3L, 2L, 5L, 7L, 4L)

    // Side-effects
    var current: ArrayList[Long] = new ArrayList[Long](arr.size())
    val double: ObjLongConsumer[JavaList[Long]] = makeConsumer { (t: JavaList[Long], num) =>
      t.forEach(
        new Consumer[Long] {
          def accept(value: Long): Unit = {
            current.add(value * num)
            ()
          }
        }
      )
    }

    double.accept(arr, 1L)
    assertArrayEquals(arr.toArray, current.toArray)

    current = new ArrayList[Long](arr.size())
    double.accept(arr, 2L)
    assertArrayEquals(Arrays.asList(6L, 4L, 10L, 14L, 8L).toArray, current.toArray)
  }
}

object ObjLongConsumerTest {
  def makeConsumer[T](f: (T, Long) => Unit): ObjLongConsumer[T] = {
    new ObjLongConsumer[T] {
      def accept(t: T, value: Long): Unit = f(t, value)
    }
  }
}
