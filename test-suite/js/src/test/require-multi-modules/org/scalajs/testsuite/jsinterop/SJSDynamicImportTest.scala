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

package org.scalajs.testsuite.jsinterop

import scala.concurrent.Future

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.LinkingInfo

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{BeforeClass, Test}

import org.scalajs.junit.async._

object SJSDynamicImportTest {
  @BeforeClass
  def assumeRuntimeSupportsPromise(): Unit = {
    assumeTrue("Assume ES6", LinkingInfo.assumingES6)
  }
}

class SJSDynamicImportTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def basic(): AsyncResult = await {
    val promise = js.dynamicImport {
      1 + 1
    }

    promise.toFuture.map { r =>
      assertEquals(2, r)
    }
  }

  @Test
  def withCaptures(): AsyncResult = await {
    var x = 1

    val promise = js.dynamicImport {
      x = 2
    }

    promise.toFuture.map { _ =>
      assertEquals(2, x)
    }
  }

  @Test
  def referenceEnclosing(): AsyncResult = await {
    class Test {
      private var _x = false

      def call(): Future[Unit] = {
        js.dynamicImport {
          _x = true
        }.toFuture
      }

      def x: Boolean = _x
    }

    val t = new Test

    t.call().map { _ =>
      assertTrue(t.x)
    }
  }

  @Test
  def actuallyDynamic(): AsyncResult = await {
    assertDynamicLoad {
      js.dynamicImport {
        FailureOnLoad
      }
    }
  }

  @Test
  def localDefDynamic(): AsyncResult = await {
    assertDynamicLoad {
      js.dynamicImport {
        def test(x: Int) = FailureOnLoad
        test(2)
      }
    }
  }

  @Test
  def localScalaClassDynamic(): AsyncResult = await {
    assertDynamicLoad {
      js.dynamicImport {
        class Local {
          FailureOnLoad
        }
        new Local
      }
    }
  }

  @Test
  def localJSClassDynamic(): AsyncResult = await {
    assertDynamicLoad {
      js.dynamicImport {
        class Local extends FailureOnLoad
        new Local
      }
    }
  }

  @Test
  def anonFunDynamic(): AsyncResult = await {
    assertDynamicLoad {
      js.dynamicImport {
        val x = () => FailureOnLoad
        x()
      }
    }
  }

  @Test
  def nested(): AsyncResult = await {
    // Ludicrously complicated nested dynamic imports.

    var x = 1

    val promise = js.dynamicImport {
      def foo(y: Int) = x += y

      val a = assertDynamicLoad {
        js.dynamicImport { FailureOnLoad }
      }

      val b = js.dynamicImport {
        def bar(z: Int) = foo(z + 1)

        bar(1)
      }

      Future.sequence(List(a, b.toFuture))
    }

    // Future#flatten, but that's not available on 2.11.
    for (i <- promise.toFuture; _ <- i) yield {
      assertEquals(3, x)
    }
  }


  @Test // #4385
  def capturesInLoop(): AsyncResult = await {
    val futures = List.newBuilder[Future[Any]]
    val effects = List.newBuilder[Int]

    var i = 0
    while (i != 5) {
      val s = i
      futures += js.dynamicImport(effects += s).toFuture
      i += 1
    }

    for {
      _ <- Future.sequence(futures.result())
    } yield {
      assertEquals(List(0, 1, 2, 3, 4), effects.result().sorted)
    }
  }

  private def assertDynamicLoad[T](promise: js.Promise[T]): Future[Unit] = {
    promise.toFuture
      .map(_ => fail("expected failure"))
      .recover {
        case js.JavaScriptException(e: js.Error)
            if e.message == "load failure for test" =>
      }
  }
}

/** A native object that fails when imported.
 *
 *  We use this to make sure `dynamicImport` actually imports dynamically.
 */
@JSImport("../test-classes/fail-load.js", JSImport.Default)
@js.native
private object FailureOnLoad extends js.Object

@JSImport("../test-classes/fail-load.js", JSImport.Default)
@js.native
private class FailureOnLoad extends js.Object
