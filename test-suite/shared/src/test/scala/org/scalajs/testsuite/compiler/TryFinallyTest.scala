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

package org.scalajs.testsuite.compiler

import scala.collection.mutable

import org.junit.Test
import org.junit.Assert._

// Much of the point of this test class is to test `return`s inside `try..finally`s
// scalastyle:off return

class TryFinallyTest {

  /* Some of these tests are ported from the partest run/finally.scala.
   * We have copies of them in our own test suite to more quickly identify
   * any issues with our compilation scheme for try..finally. On JS it is
   * straightforward, but it is a huge beast in Wasm.
   */

  type SideEffect = Any => Unit

  @noinline
  def test(body: SideEffect => Unit)(expectedSideEffects: String*): Unit = {
    val sideEffects = mutable.ListBuffer.empty[String]

    try {
      body(x => sideEffects += ("" + x))
    } catch {
      case e: Throwable =>
        sideEffects += ("CAUGHT: " + e)
    }

    if (!sideEffects.sameElements(expectedSideEffects)) {
      // Custom message for easier debugging
      fail(
          "Expected side effects:" +
          expectedSideEffects.mkString("\n* ", "\n* ", "\n") +
          "but got:" +
          sideEffects.mkString("\n* ", "\n* ", "\n"))
    }
  }

  // test that finally is not covered by any exception handlers.
  @Test
  def throwCatchFinally(): Unit = {
    test { println =>
      def bar(): Unit = {
        try {
          println("hi")
        } catch {
          case e: Throwable => println("SHOULD NOT GET HERE")
        } finally {
          println("In Finally")
          throw new RuntimeException("ouch")
        }
      }

      try {
        bar()
      } catch {
        case e: Throwable => println(e)
      }
    } (
      "hi",
      "In Finally",
      "java.lang.RuntimeException: ouch"
    )
  }

  // test that finally is not covered by any exception handlers.
  // return in catch (finally is executed)
  @Test
  def retCatch(): Unit = {
    test { println =>
      def retCatchInner(): Unit = {
        try {
          throw new Exception
        } catch {
          case e: Throwable =>
            println(e)
            return
        } finally {
          println("in finally")
        }
      }

      retCatchInner()
    } (
      "java.lang.Exception",
      "in finally"
    )
  }

  // throw in catch (finally is executed, exception propagated)
  @Test
  def throwCatch(): Unit = {
    test { println =>
      try {
        throw new Exception
      } catch {
        case e: Throwable =>
          println(e)
          throw e
      } finally {
        println("in finally")
      }
    } (
      "java.lang.Exception",
      "in finally",
      "CAUGHT: java.lang.Exception"
    )
  }

  // return inside body (finally is executed)
  @Test
  def retBody(): Unit = {
    test { println =>
      def retBodyInner(): Unit = {
        try {
          return
        } catch {
          case e: Throwable =>
            println(e)
            throw e
        } finally println("in finally")
      }

      retBodyInner()
    } (
      "in finally"
    )
  }

  // throw inside body (finally and catch are executed)
  @Test
  def throwBody(): Unit = {
    test { println =>
      try {
        throw new Exception
      } catch {
        case e: Throwable =>
          println(e)
      } finally {
        println("in finally")
      }
    } (
      "java.lang.Exception",
      "in finally"
    )
  }

  // return inside finally (each finally is executed once)
  @Test
  def retFinally(): Unit = {
    test { println =>
      def retFinallyInner(): Unit = {
        try {
          try {
            println("body")
          } finally {
            println("in finally 1")
            return
          }
        } finally {
          println("in finally 2")
        }
      }

      retFinallyInner()
    } (
      "body",
      "in finally 1",
      "in finally 2"
    )
  }

  // throw inside finally (finally is executed once, exception is propagated)
  @Test
  def throwFinally(): Unit = {
    test { println =>
      try {
        try {
          println("body")
        } finally {
          println("in finally")
          throw new Exception
        }
      } catch {
        case e: Throwable => println(e)
      }
    } (
      "body",
      "in finally",
      "java.lang.Exception"
    )
  }

  // nested finally blocks with return value
  @Test
  def nestedFinallyBlocks(): Unit = {
    test { println =>
      def nestedFinallyBlocksInner(): Int = {
        try {
          try {
            return 10
          } finally {
            try { () } catch { case _: Throwable => () }
            println("in finally 1")
          }
        } finally {
          println("in finally 2")
        }
      }

      assertEquals(10, nestedFinallyBlocksInner())
    } (
      "in finally 1",
      "in finally 2"
    )
  }

  @Test
  def nonDefaultableTryResultType_Issue5165(): Unit = {
    test { println =>
      // after the optimizer, some has type Some! (a non-nullable reference type)
      val some = try {
        println("in try")
        Some(1)
      } finally {
        println("in finally")
      }
      assertEquals(1, some.value)
    } (
      "in try",
      "in finally"
    )
  }

  @Test
  def nonDefaultableLabeledResultType_Issue5165(): Unit = {
    test { println =>
      /* After the optimizer, the result type of the Labeled block that gets
       * inlined is a Some! (a non-nullable reference type).
       */
      @inline def nonDefaultableLabeledResultTypeInner(): Some[Int] = {
        try {
          println("in try")
          return Some(1)
        } finally {
          println("in finally")
        }
      }

      val some = nonDefaultableLabeledResultTypeInner()
      assertEquals(1, some.value)
    } (
      "in try",
      "in finally"
    )
  }
}
