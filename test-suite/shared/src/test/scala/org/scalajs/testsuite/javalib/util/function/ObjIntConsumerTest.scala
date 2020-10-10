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
import java.util.function.{Consumer, ObjIntConsumer}
import org.junit.Assert._
import org.junit.Test

class ObjIntConsumerTest {
  import ObjIntConsumerTest._

  @Test def accept(): Unit = {
    val arr: JavaList[Int] = Arrays.asList(3, 2, 5, 7, 4)

    // Side-effects
    var current: ArrayList[Int] = new ArrayList[Int](arr.size())
    val double: ObjIntConsumer[JavaList[Int]] = makeConsumer { (t: JavaList[Int], num) =>
      t.forEach(
        new Consumer[Int] {
          def accept(value: Int): Unit = {
            current.add(value * num)
            ()
          }
        }
      )
    }

    double.accept(arr, 1)
    assertArrayEquals(arr.toArray, current.toArray)

    current = new ArrayList[Int](arr.size())
    double.accept(arr, 2)
    assertArrayEquals(Arrays.asList(6, 4, 10, 14, 8).toArray, current.toArray)
  }
}

object ObjIntConsumerTest {
  def makeConsumer[T](f: (T, Int) => Unit): ObjIntConsumer[T] = {
    new ObjIntConsumer[T] {
      def accept(t: T, value: Int): Unit = f(t, value)
    }
  }
}
