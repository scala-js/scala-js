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

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.Platform._

class ImportMetaTest {
  @Test def testImportMeta(): Unit = {
    val meta = js.`import`.meta
    assertEquals("object", js.typeOf(meta))

    assertSame(meta, js.`import`.meta)

    if (executingInNodeJS)
      assertEquals("string", js.typeOf(meta.url))
  }
}
