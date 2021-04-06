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
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{BeforeClass, Test}

import org.scalajs.junit.async._

import org.scalajs.testsuite.utils.Platform._

object SJSDynamicImportTest {
  @BeforeClass
  def assumeRuntimeSupportsPromise(): Unit = {
    assumeTrue("Requires Promises",
        assumeES2015 || js.typeOf(js.Dynamic.global.Promise) != "undefined")
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

  @Test // #4386
  def sharedDependencyWithPublicModule(): AsyncResult = await {
    /* This test is trying to trigger a condition where the MaxModuleAnalyzer is
     * introducing a dependency of a dynamically loaded internal module to the
     * public module that loads it. This happens if the public module and the
     * internal module share a dependency (in this case `SharedDependency`).
     *
     * Take the following class graph:
     *
     * A -- dyn --> B
     * |            |
     * |            |
     * +---> C <----+
     *
     * where `dyn` denotes a dynamic import.
     *
     * The optimal grouping here is different if `A` is an entry point or not.
     *
     * If `A` is not a direct entry point, `A` and `C` should be grouped
     * together into an internal module. `B` should depend on this module. This
     * avoids an additional module for `C` which would only be loaded iff `A` is
     * loaded (because `B` can only be loaded via `A`).
     *
     * However, if `A` is a direct entry point (and hence in a public module),
     * `C` must be put into a separate internal module so it can be imported by
     * `B` (recall that public modules cannot be imported by Scala.js generated
     * modules).
     *
     * To trigger this scenario in the large test suite, we must create a
     * dedicated entry point for this test. Because tests are loaded
     * reflectively all tests are reachable by all modules (due to their static
     * initializers). So if we were to try to use the test itself as an entry
     * point, it would be put in an internal module and not trigger the above
     * condition. By using the indirection via an export, we can avoid this and
     * trigger the relevant condition.
     */
    ExportsTest
      .exportsNameSpace("shared_dep_mod")
      .useSharedDependencyInPublicModule()
      .asInstanceOf[js.Promise[Int]]
      .toFuture
      .map(assertEquals(2, _))
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

private object UseSharedDependencyInPublicModule {
  @JSExportTopLevel("useSharedDependencyInPublicModule", "shared_dep_mod")
  def useSharedDependency(): js.Promise[Int] = {
    val x = SharedDependency.calculate()

    js.dynamicImport {
      SharedDependency.calculate() + x
    }
  }
}

private object SharedDependency {
  @noinline
  def calculate(): Int = 1
}
