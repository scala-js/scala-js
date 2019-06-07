/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import org.junit.Assert._
import org.junit.Test

/* This is currently hard-coded for Node.js modules in particular.
 * We are importing built-in Node.js modules, because we do not have any
 * infrastructure to load non-built-in modules. In the future, we should use
 * our own user-defined ES6 modules written in JavaScript.
 */
class DynamicImportTest {

  @Test def testDynImportParsesAndExecutes(): Unit = {
    /* Since we do not have support for asynchronous tests, all we can do here
     * is test that `js.import` parses, links, and executes without error,
     * returning a `Promise`. We can't actually wait for the promise to
     * resolve and hence cannot test its result.
     *
     * We will be able to revisit this in Scala.js 1.x.
     */

    def isPromise(x: Any): Boolean = x.isInstanceOf[js.Promise[_]]

    assertTrue(isPromise(js.`import`[js.Any]("fs")))

    val failedPromise = js.`import`[js.Any]("non-existent-module")
    assertTrue(isPromise(failedPromise))
    // Recover to avoid the unhandled rejected Promise warning of Node.js
    failedPromise.toFuture.recover {
      case th: Throwable => ()
    }
  }

}
