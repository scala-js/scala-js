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

import org.scalajs.junit.async._

/* This is currently hard-coded for Node.js modules in particular.
 * We are importing built-in Node.js modules, because we do not have any
 * infrastructure to load non-built-in modules. In the future, we should use
 * our own user-defined ES6 modules written in JavaScript.
 */
class DynamicImportTest {
  import DynamicImportTest._

  @Test def testSuccessfulImport(): AsyncResult = await {
    js.`import`[QueryStringAPI]("querystring").toFuture.map { qs =>
      assertEquals("object", js.typeOf(qs))

      val dict = js.Dictionary("foo" -> "bar", "baz" -> "qux")

      assertEquals("foo=bar&baz=qux", qs.stringify(dict))
      assertEquals("foo:bar;baz:qux", qs.stringify(dict, ";", ":"))
    }
  }

  @Test(expected = classOf[js.JavaScriptException])
  def testFailedImport(): AsyncResult = await {
    js.`import`[js.Any]("non-existent-module").toFuture
  }
}

object DynamicImportTest {
  trait QueryStringAPI extends js.Any {
    def stringify(obj: js.Dictionary[String]): String
    def stringify(obj: js.Dictionary[String], sep: String, eq: String): String
  }
}
