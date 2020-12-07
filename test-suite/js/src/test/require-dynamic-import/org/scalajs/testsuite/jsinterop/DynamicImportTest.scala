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

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.async._

class DynamicImportTest {
  import DynamicImportTest._

  @Test def testSuccessfulImport(): AsyncResult = await {
    js.`import`[ModulesTestModuleAPI]("../test-classes/modules-test.js").toFuture.map { m =>
      assertEquals("object", js.typeOf(m))
      assertEquals(5, m.ssum(2))
      assertEquals(13, m.ssum(2, 3))
    }
  }

  @Test(expected = classOf[js.JavaScriptException])
  def testFailedImport(): AsyncResult = await {
    js.`import`[js.Any]("non-existent-module").toFuture
  }
}

object DynamicImportTest {
  trait ModulesTestModuleAPI extends js.Any {
    def ssum(x: Int): Int
    def ssum(x: Int, y: Int): Int
  }
}
